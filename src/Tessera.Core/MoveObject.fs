/// Moving a measure to another table. It keeps its name (measure names are unique model-wide), so
/// unqualified [M] references stay; references that qualify it with its old table ('Old'[M],
/// Old[M]) are rewritten token by token, as `rename` does, and checked the same way.
module Tessera.MoveObject

open System
open Microsoft.AnalysisServices.Tabular
open Tessera.Dax
open Tessera.Inventory

type MoveSpec = {
    /// The measure, in any form `get` accepts.
    Measure: string
    /// The table to move it to.
    Table: string
    /// Also set its display folder.
    DisplayFolder: string option
}

let private plural n (word: string) =
    if n = 1 then $"1 {word}" else $"{n} {word}s"

let private eqCi (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

/// Rewrite the qualified references to the measure in one expression, right to left.
let private rewrite
    (measure: ObjectRef)
    (oldTable: string)
    (newTable: string)
    (dax: string)
    (refs: Deps.Reference list)
    =
    let allTokens = tokenize dax
    let tableText = Rename.tableReferenceText allTokens oldTable newTable

    let replacements =
        refs
        |> List.choose (fun r ->
            match r.Target, r.Start, r.Length with
            | Deps.Resolved t, Some start, Some length when t = measure ->
                match
                    tokenize (dax.Substring(start, length))
                    |> significant
                    |> List.map (fun t -> { t with Start = t.Start + start })
                with
                | [ table; name ] when Token.isName table && name.Kind = TokenKind.Bracketed ->
                    Some(table.Start, table.Length, tableText table)
                | _ -> None
            | _ -> None)
        |> List.sortByDescending (fun (start, _, _) -> start)

    let text =
        replacements
        |> List.fold
            (fun (s: string) (start, length, replacement) ->
                s.Substring(0, start) + replacement + s.Substring(start + length))
            dax

    text, replacements.Length

/// Move a measure to another table, as one edit.
let move (db: Database) (spec: MoveSpec) : Result<Mutation.Effect, TessError> =
    let tomBefore = RemoveObject.tomErrors db
    let graph = Deps.build db
    let entries = graph.Entries
    let tables = entries |> List.filter (fun e -> e.Ref.Kind = ObjectKind.Table)

    let target =
        Selector.resolve entries spec.Measure
        |> Result.bind (fun e ->
            let refuse reason =
                Error(UnsupportedMove(ObjectRef.format e.Ref, reason))

            match e.Ref.Kind with
            | ObjectKind.Measure -> Ok e
            | ObjectKind.CalculationItem -> refuse "calculation items belong to their calculation group"
            | ObjectKind.Column -> refuse "a column belongs to its table, whose partitions supply its data"
            | _ -> refuse "only measures can be moved")

    let destination (m: ObjectEntry) =
        match Selector.resolve tables spec.Table with
        | Error(ObjectNotFound _) -> Error(UnsupportedMove(ObjectRef.format m.Ref, $"there is no table '{spec.Table}'"))
        | Error e -> Error e
        | Ok t -> Ok t

    target
    |> Result.bind (fun m -> destination m |> Result.map (fun t -> m, t))
    |> Result.bind (fun (m, t) ->
        let measure = m.Object :?> Measure
        let table = t.Object :?> Table
        let oldRef = m.Ref
        let name = measure.Name
        let newRef = ObjectRef.inTable ObjectKind.Measure table.Name name

        if obj.ReferenceEquals(measure.Table, table) then
            let folder =
                match spec.DisplayFolder with
                | Some _ -> " (to change only its display folder, use `set`)"
                | None -> ""

            Ok {
                Mutation.Effect.none with
                    Notes = [
                        $"{ObjectRef.format oldRef} is already in table '{table.Name}'; nothing moved{folder}"
                    ]
            }
        elif not (isNull table.CalculationGroup) then
            Error(
                UnsupportedMove(
                    ObjectRef.format oldRef,
                    $"'{table.Name}' is a calculation group, which can't hold measures"
                )
            )
        else
            let clash =
                entries
                |> List.tryFind (fun e ->
                    (e.Ref.Kind = ObjectKind.Column || e.Ref.Kind = ObjectKind.Hierarchy)
                    && ObjectEntry.table e = Some table.Name
                    && eqCi (ObjectEntry.name e) name)

            match clash with
            | Some c -> Error(NameCollision(name, ObjectRef.format c.Ref))
            | None ->
                let oldTable = measure.Table.Name

                let byExpression =
                    graph.References
                    |> List.filter (fun r -> r.Start.IsSome)
                    |> List.groupBy (fun r -> r.From, r.Via)
                    |> Map.ofList

                let edits: Rename.Edit list =
                    Deps.daxSources entries
                    |> List.choose (fun s ->
                        match Map.tryFind (s.Owner.Ref, s.Property) byExpression with
                        | None -> None
                        | Some refs ->
                            match rewrite oldRef oldTable table.Name s.Dax refs with
                            | _, 0 -> None
                            | after, n ->
                                Some {
                                    Owner = s.Owner
                                    Property = s.Property
                                    Before = s.Dax
                                    After = after
                                    References = n
                                })

                let addedTo = Collections.Generic.List<string>()

                /// The whole edit, on the database or (for the check) a copy of it; `find` looks
                /// objects up by their refs before the move. A removed TOM object can't be added
                /// back, so the measure moves as a copy, and translations and perspective
                /// memberships are moved over to it.
                let applyMove (find: ObjectRef -> MetadataObject) =
                    addedTo.Clear()
                    Rename.applyEdits (fun e -> find e.Ref) edits
                    let m = find oldRef :?> Measure
                    let dest = find t.Ref :?> Table
                    let model = dest.Model

                    let isMeasure (o: MetadataObject) =
                        obj.ReferenceEquals(o, m)
                        || (not (isNull m.KPI) && obj.ReferenceEquals(o, m.KPI))

                    let translations = [
                        for c in model.Cultures do
                            for x in c.ObjectTranslations do
                                if isMeasure x.Object then
                                    c, x
                    ]

                    let memberships = [
                        for p in model.Perspectives do
                            for pt in p.PerspectiveTables do
                                for pm in pt.PerspectiveMeasures do
                                    if obj.ReferenceEquals(pm.Measure, m) then
                                        p, pt, pm
                    ]

                    for c, x in translations do
                        c.ObjectTranslations.Remove x |> ignore

                    for _, pt, pm in memberships do
                        pt.PerspectiveMeasures.Remove pm |> ignore

                    let moved = m.Clone()
                    m.Table.Measures.Remove m |> ignore
                    dest.Measures.Add moved
                    spec.DisplayFolder |> Option.iter (fun f -> moved.DisplayFolder <- f)

                    for c, x in translations do
                        let o: MetadataObject =
                            if obj.ReferenceEquals(x.Object, m) then
                                moved
                            else
                                moved.KPI

                        c.ObjectTranslations.Add(
                            new ObjectTranslation(Object = o, Property = x.Property, Value = x.Value)
                        )

                    // Keep the measure in every perspective it was in. If the new table wasn't,
                    // it joins with just this measure.
                    for p, _, pm in memberships do
                        let pt =
                            match p.PerspectiveTables.Find dest.Name with
                            | null ->
                                let pt = new PerspectiveTable(Table = dest)
                                p.PerspectiveTables.Add pt
                                addedTo.Add p.Name
                                pt
                            | pt -> pt

                        let copy = pm.Clone()
                        copy.Measure <- moved
                        pt.PerspectiveMeasures.Add copy

                let mapRef (r: ObjectRef) = if r = oldRef then newRef else r

                match Rename.verifyWith db graph applyMove mapRef id with
                | (_ :: _) as conflicts, _ -> Error(MoveConflict(ObjectRef.format oldRef, table.Name, conflicts))
                | [], resolvedNow ->
                    let byRef = entries |> List.map (fun e -> e.Ref, e.Object) |> dict
                    applyMove (fun r -> byRef.[r])

                    match Set.difference (RemoveObject.tomErrors db) tomBefore |> Set.toList with
                    | (_ :: _) as errors ->
                        let listed = errors |> List.map (fun e -> "\n  " + e) |> String.concat ""

                        Error(
                            UnsupportedMove(
                                ObjectRef.format oldRef,
                                $"it would leave links to the old measure that tess can't fix:{listed}"
                            )
                        )
                    | [] ->
                        let references = edits |> List.sumBy (fun e -> e.References)
                        let objects = edits |> List.map (fun e -> e.Owner.Ref) |> List.distinct

                        let updated =
                            if references = 0 then
                                "no qualified references to update"
                            else
                                $"""updated {plural references "qualified reference"} in {plural objects.Length "object"}"""

                        Ok {
                            Warnings =
                                resolvedNow
                                |> List.map (fun s -> "a reference that named nothing now resolves: " + s)
                            Renamed = [ oldRef, newRef ]
                            Notes = [
                                $"Moved {ObjectRef.format oldRef} -> {ObjectRef.format newRef}; {updated}"
                                match spec.DisplayFolder with
                                | Some f -> $"Set its display folder to '{f}'"
                                | None -> ()
                                for p in addedTo do
                                    $"Added table '{table.Name}' to perspective '{p}' (with just this measure), so the measure stays in it"
                            ]
                        })
