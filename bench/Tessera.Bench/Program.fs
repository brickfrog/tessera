/// A benchmark of the local DAX engine on the spaceparts sample model (see bench/README.md).
///
///   dotnet run -c Release --project bench/Tessera.Bench -- <model> <fixture dir> [options]
///
/// Every query runs in a child process, so one that runs too long is killed (--timeout) and the
/// rest still run. Each result is fingerprinted (a hash of its JSON, or of its error) so two runs
/// can be compared for a changed answer (--compare).
module Tessera.Bench.Program

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions
open Tessera

type Query = { Name: string; Dax: string }

let private quote (s: string) = s.Replace("'", "''")
let private str (s: string) = "\"" + s.Replace("\"", "\"\"") + "\""

/// One column of each dimension of spaceparts: what the measures are grouped by.
let private dimensions = [
    "Brand", "Brand"
    "Customer", "Account Type"
    "Date", "Calendar Year (ie 2021)"
    "Invoice Document Type", "Group"
    "Product", "Type"
    "Region", "System"
    "Budget Rate", "Currency System"
]

let private stations (n: int) =
    String.Join(", ", [ for i in 1..n -> str $"Station {i}" ])

let private matrix (filter: string) =
    "EVALUATE SUMMARIZECOLUMNS ( 'Region'[Station], "
    + filter
    + "\"Min\", [CF - Bar Chart X Min], \"Max\", [CF - Bar Chart X Max] )"

let queries (db: Microsoft.AnalysisServices.Tabular.Database) : Query list =
    let measures = [
        for t in db.Model.Tables do
            for m in t.Measures do
                m.Name
    ]

    [
        for n in [ 2; 5; 10; 20 ] do
            {
                Name = $"station-matrix/{n}"
                Dax = matrix $"TREATAS ( {{ {stations n} }}, 'Region'[Station] ), "
            }
        {
            Name = "station-matrix/all"
            Dax = matrix ""
        }
        // The same shape over measures whose values differ by row (at these data sizes the
        // matrix's measures are 0 everywhere, so its answers say little).
        for n, filter in [ "20", $"TREATAS ( {{ {stations 20} }}, 'Region'[Station] ), "; "all", "" ] do
            {
                Name = $"allselected-shape/{n}"
                Dax =
                    "DEFINE MEASURE 'Invoiced Sales'[Min Revenue] = MINX ( ALLSELECTED ( 'Region'[Station] ), [Revenue] ) "
                    + "MEASURE 'Invoiced Sales'[Max GM] = MAXX ( ALLSELECTED ( 'Region'[Station] ), [Gross Margin %] ) "
                    + "EVALUATE SUMMARIZECOLUMNS ( 'Region'[Station], "
                    + filter
                    + "\"rev\", [Revenue], \"min\", [Min Revenue], \"max\", [Max GM] )"
            }
        {
            Name = "share-of-selection"
            Dax =
                "DEFINE MEASURE 'Invoiced Sales'[Share] = DIVIDE ( [Revenue], CALCULATE ( [Revenue], ALLSELECTED ( 'Region' ) ) ) "
                + "EVALUATE SUMMARIZECOLUMNS ( 'Region'[System], 'Product'[Type], \"share\", [Share] )"
        }
        {
            Name = "rankx-customers"
            Dax =
                "EVALUATE ADDCOLUMNS ( VALUES ( Customer[Account Name] ), \"r\", RANKX ( ALL ( Customer[Account Name] ), [Revenue] ) )"
        }
        for m in measures do
            {
                Name = $"total/{m}"
                Dax = $"EVALUATE ROW ( \"v\", [{m}] )"
            }
        for t, c in dimensions do
            for m in measures do
                {
                    Name = $"by {t}[{c}]/{m}"
                    Dax = $"EVALUATE SUMMARIZECOLUMNS ( '{quote t}'[{c}], \"v\", [{m}] )"
                }
    ]

let private fingerprint (text: string) =
    let bytes = SHA256.HashData(Encoding.UTF8.GetBytes text)
    Convert.ToHexString(bytes, 0, 6).ToLowerInvariant()

let private clock =
    Engine.Clock.fixedAt (DateTime(2026, 8, 21, 12, 0, 0, DateTimeKind.Unspecified))

let private load (model: string) =
    match ModelIO.loadPath model with
    | Ok(_, db) -> db
    | Error e -> failwith (TessError.message e)

/// The child: prepare once, then run the queries from `start` on, one line per result.
let child (model: string) (dir: string) (start: int) (filter: Regex) (check: bool) =
    let incremental = DaxEngine.IncrementalStats.Global
    incremental.Check <- check
    let db = load model
    let all = queries db

    if start = 0 then
        let sw = Stopwatch.StartNew()
        let items = Engine.coverage db
        Console.Out.WriteLine($"RESULT\t-1\tcoverage\t{sw.Elapsed.TotalMilliseconds:F1}\tok\t{items.Length}")

    let sw = Stopwatch.StartNew()

    let store =
        match Fixture.load db dir with
        | Ok l -> l.Store
        | Error e -> failwith (TessError.message e)

    let prepared =
        match
            Engine.prepareWith
                {
                    Engine.Options.defaults with
                        Clock = Some clock
                }
                db
                store
        with
        | Ok p -> p
        | Error e -> failwith (TessError.message e)

    Console.Out.WriteLine($"PREPARE\t{sw.Elapsed.TotalMilliseconds:F1}")
    Console.Out.Flush()

    all
    |> List.iteri (fun i q ->
        if i >= start && filter.IsMatch q.Name then
            Console.Out.WriteLine($"START\t{i}")
            Console.Out.Flush()

            let applied, checkedBefore, mismatched =
                incremental.Applied, incremental.Checked, incremental.Mismatched

            let sw = Stopwatch.StartNew()
            let r = Engine.evaluateQuery prepared q.Dax
            let ms = sw.Elapsed.TotalMilliseconds

            Console.Out.WriteLine(
                $"STATS\t{i}\t{incremental.Applied - applied}\t{incremental.Checked - checkedBefore}\t{incremental.Mismatched - mismatched}"
            )

            let status, print =
                match r with
                | Ok results -> "ok", fingerprint ((QueryResult.listToJson results).ToJsonString())
                | Error e -> TessError.code e, fingerprint (TessError.message e)

            Console.Out.WriteLine($"RESULT\t{i}\t{q.Name}\t{ms:F1}\t{status}\t{print}")
            Console.Out.Flush())

    Console.Out.WriteLine($"PEAK\t{Process.GetCurrentProcess().PeakWorkingSet64}")
    Console.Out.WriteLine("DONE")

type Row = {
    Name: string
    Ms: float option
    Status: string
    Print: string
}

let parent
    (model: string)
    (dir: string)
    (timeout: float)
    (filter: string)
    (out: string option)
    (compare: string option)
    (check: bool)
    =
    let db = load model
    let all = queries db |> Array.ofList
    let rows = List<Row>()
    let mutable prepare = []
    let mutable peak = 0L
    let mutable start = 0
    let mutable finished = false
    let mutable applied = 0L
    let mutable checkedCount = 0L
    let mutable mismatched = 0L
    let exe = Environment.ProcessPath
    let dll = typeof<Query>.Assembly.Location

    while not finished do
        let psi =
            ProcessStartInfo(
                exe,
                [
                    dll
                    "--child"
                    model
                    dir
                    string start
                    filter
                    (if check then "check" else "")
                ],
                RedirectStandardOutput = true,
                UseShellExecute = false
            )

        use p = Process.Start psi
        let lines = new Collections.Concurrent.BlockingCollection<string>()

        let reader =
            Threading.Thread(fun () ->
                let mutable line = p.StandardOutput.ReadLine()

                while not (isNull line) do
                    lines.Add line
                    line <- p.StandardOutput.ReadLine()

                lines.CompleteAdding())

        reader.Start()
        let mutable current = -1
        let mutable since = Stopwatch.StartNew()
        let mutable go = true

        while go do
            let mutable line = null

            // The child's peak working set, also for a child that is killed.
            (try
                p.Refresh()

                if not p.HasExited then
                    peak <- max peak p.PeakWorkingSet64
             with _ ->
                 ())

            if lines.TryTake(&line, 250) then
                let f = line.Split '\t'

                match f.[0] with
                | "START" ->
                    current <- int f.[1]
                    since <- Stopwatch.StartNew()
                | "RESULT" ->
                    let row = {
                        Name = f.[2]
                        Ms = Some(float f.[3])
                        Status = f.[4]
                        Print = f.[5]
                    }

                    rows.Add row
                    eprintfn "%10.1f ms  %-14s %s" (float f.[3]) f.[4] f.[2]
                    current <- -1
                | "STATS" ->
                    applied <- applied + int64 f.[2]
                    checkedCount <- checkedCount + int64 f.[3]
                    mismatched <- mismatched + int64 f.[4]
                | "PREPARE" -> prepare <- float f.[1] :: prepare
                | "PEAK" -> peak <- max peak (int64 f.[1])
                | "DONE" ->
                    finished <- true
                    go <- false
                | _ -> eprintfn "%s" line
            elif lines.IsCompleted then
                // The child died without saying DONE.
                if current >= 0 then
                    rows.Add {
                        Name = all.[current].Name
                        Ms = None
                        Status = "crash"
                        Print = ""
                    }

                    start <- current + 1
                else
                    finished <- true

                go <- false
            elif current >= 0 && since.Elapsed.TotalSeconds > timeout then
                (try
                    p.Kill true
                 with _ ->
                     ())

                eprintfn "%10s     %-14s %s" $">{timeout}s" "timeout" all.[current].Name

                rows.Add {
                    Name = all.[current].Name
                    Ms = None
                    Status = "timeout"
                    Print = ""
                }

                start <- current + 1
                current <- -1
                go <- false

        if start >= all.Length then
            finished <- true

    let text = StringBuilder()
    text.AppendLine($"# fixture\t{dir}") |> ignore

    let prepares =
        String.Join(",", prepare |> List.rev |> List.map (fun x -> x.ToString("F0")))

    text.AppendLine($"# prepare ms\t{prepares}") |> ignore
    text.AppendLine($"# peak working set MB\t{float peak / 1048576.0:F0}") |> ignore

    let total = rows |> Seq.sumBy (fun r -> defaultArg r.Ms 0.0)
    text.AppendLine($"# total query ms\t{total:F0}") |> ignore

    for r in rows do
        let ms =
            match r.Ms with
            | Some ms -> ms.ToString("F1", Globalization.CultureInfo.InvariantCulture)
            | None -> $">{timeout}s"

        text.AppendLine($"{r.Name}\t{ms}\t{r.Status}\t{r.Print}") |> ignore

    match out with
    | Some path -> File.WriteAllText(path, text.ToString())
    | None -> ()

    eprintfn
        "prepare %s ms; peak working set %.0f MB; total %.0f ms"
        (String.Join(",", prepare))
        (float peak / 1048576.0)
        total

    // Of the queries that finished.
    eprintfn "incremental filter contexts: %d applied, %d checked, %d mismatched" applied checkedCount mismatched

    let failed = if check && mismatched > 0L then 1 else 0

    match compare with
    | Some path ->
        let before =
            File.ReadAllLines path
            |> Array.filter (fun l -> not (l.StartsWith "#"))
            |> Array.map (fun l -> l.Split '\t')
            |> Array.map (fun f -> f.[0], (f.[1], f.[2], f.[3]))
            |> dict

        let mutable changed = 0

        for r in rows do
            match before.TryGetValue r.Name with
            | true, (_, status, print) when status <> "timeout" && r.Status <> "timeout" ->
                if status <> r.Status || print <> r.Print then
                    changed <- changed + 1
                    eprintfn "CHANGED %s: %s %s -> %s %s" r.Name status print r.Status r.Print
            | true, (ms, status, _) -> eprintfn "  %s: %s %s -> %s" r.Name status ms r.Status
            | _ -> ()

        eprintfn "%d changed answers" changed
        if changed > 0 then 1 else failed
    | None -> failed

[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    | "--child" :: model :: dir :: start :: filter :: check :: _ ->
        child model dir (int start) (Regex filter) (check = "check")
        0
    | model :: dir :: rest ->
        let rec opt name (xs: string list) =
            match xs with
            | n :: v :: _ when n = name -> Some v
            | _ :: tail -> opt name tail
            | [] -> None

        parent
            model
            dir
            (opt "--timeout" rest |> Option.map float |> Option.defaultValue 60.0)
            (opt "--filter" rest |> Option.defaultValue ".")
            (opt "--out" rest)
            (opt "--compare" rest)
            (List.contains "--check" rest)
    | _ ->
        eprintfn
            "usage: Tessera.Bench <model> <fixture dir> [--timeout s] [--filter regex] [--out file] [--compare file] [--check]"

        2
