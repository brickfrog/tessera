module Tessera.Tests.ObjectRefTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Tessera

let private parseOk s =
    match ObjectRef.parse s with
    | Ok r -> r
    | Error e -> failtestf "expected '%s' to parse: %s" s (TessError.message e)

let private parseFails s =
    match ObjectRef.parse s with
    | Ok r -> failtestf "expected '%s' to fail, got %A" s r
    | Error(InvalidObjectRef _) -> ()
    | Error e -> failtestf "wrong error for '%s': %A" s e

/// Generates a well-formed ref: any kind, the right number of non-empty segments
/// (including ones full of '/' and '\').
type RefGen =
    static member ObjectRef() =
        gen {
            let! kind = Gen.elements ObjectKind.all

            let segment =
                Gen.oneof [
                    ArbMap.defaults |> ArbMap.generate<NonEmptyString> |> Gen.map (fun s -> s.Get)
                    Gen.elements [ "/"; "\\"; "a/b"; "a\\b"; "\\/"; "x:y"; " spaced " ]
                ]

            let! path = Gen.listOfLength (ObjectKind.arity kind) segment
            return { Kind = kind; Path = path }
        }
        |> Arb.fromGen

let private config = {
    FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<RefGen> ]
        maxTest = 500
}

[<Tests>]
let tests =
    testList "ObjectRef" [
        testCase "formats table-scoped objects as kind:table/name"
        <| fun () ->
            Expect.equal
                (ObjectRef.format (ObjectRef.inTable ObjectKind.Measure "Sales" "Total"))
                "measure:Sales/Total"
                ""

        testCase "model has no path"
        <| fun () ->
            Expect.equal (ObjectRef.format ObjectRef.model) "model" ""
            Expect.equal (parseOk "model") ObjectRef.model ""
            Expect.equal (parseOk "MODEL") ObjectRef.model "case-insensitive"

        testCase "escapes slashes and backslashes in names"
        <| fun () ->
            let r = ObjectRef.inTable ObjectKind.Column "A/B" "C\\D"
            Expect.equal (ObjectRef.format r) @"column:A\/B/C\\D" ""
            Expect.equal (parseOk @"column:A\/B/C\\D") r ""

        testCase "colons after the kind belong to the name"
        <| fun () -> Expect.equal (parseOk "measure:Sales/Ratio: A:B").Path [ "Sales"; "Ratio: A:B" ] ""

        testCase "kind aliases"
        <| fun () ->
            Expect.equal (parseOk "calculationitem:TI/YTD").Kind ObjectKind.CalculationItem ""
            Expect.equal (parseOk "udf:Double").Kind ObjectKind.Function ""
            Expect.equal (parseOk "Measure:Sales/Total").Kind ObjectKind.Measure "kind is case-insensitive"

        testCase "levels take three segments"
        <| fun () -> Expect.equal (parseOk "level:Date/Calendar/Year").Path [ "Date"; "Calendar"; "Year" ] ""

        testList "rejects" [
            for bad in
                [
                    ""
                    "   "
                    "Sales"
                    "bogus:Sales"
                    "measure:Total"
                    "measure:Sales/Total/Extra"
                    "measure:Sales/"
                    "measure:/Total"
                    @"measure:Sales/Tot\al"
                    @"measure:Sales/Total\"
                ] -> testCase $"'{bad}'" <| fun () -> parseFails bad
        ]

        testPropertyWithConfig config "parse (format r) = r"
        <| fun (r: ObjectRef) -> ObjectRef.parse (ObjectRef.format r) = Ok r

        testPropertyWithConfig config "name and tableName read the path"
        <| fun (r: ObjectRef) ->
            let nameOk =
                match r.Path with
                | [] -> ObjectRef.name r = "model"
                | p -> ObjectRef.name r = List.last p

            let tableOk =
                match ObjectRef.tableName r with
                | Some t -> t = List.head r.Path
                | None -> true

            nameOk && tableOk
    ]
