module Tessera.Tests.SelectorTests

open Expecto
open Tessera
open Tessera.Inventory
open Tessera.Tests.Fixtures

let private entries = lazy (objects (sampleDatabase ()))

let private resolves (input: string) (expected: string) =
    testCase $"{input} -> {expected}"
    <| fun () ->
        match Selector.resolve entries.Value input with
        | Ok e -> Expect.equal (ObjectRef.format e.Ref) expected ""
        | Error e -> failtestf "%s" (TessError.message e)

let private fails (input: string) (check: TessError -> bool) =
    testCase $"{input} fails"
    <| fun () ->
        match Selector.resolve entries.Value input with
        | Ok e -> failtestf "resolved to %s" (ObjectRef.format e.Ref)
        | Error e -> Expect.isTrue (check e) (sprintf "%A" e)

let private notFound =
    function
    | ObjectNotFound _ -> true
    | _ -> false

let private ambiguous =
    function
    | AmbiguousObject _ -> true
    | _ -> false

let private invalid =
    function
    | InvalidObjectRef _ -> true
    | _ -> false

[<Tests>]
let tests =
    testList "Selector" [
        testList "parse" [
            testCase "forms"
            <| fun () ->
                let p s =
                    match Selector.parse s with
                    | Ok sel -> sel
                    | Error e -> failtestf "%s" (TessError.message e)

                Expect.equal (p "'Sales'[Total]") (Qualified("Sales", "Total")) ""
                Expect.equal (p "Sales[Total]") (Qualified("Sales", "Total")) ""
                Expect.equal (p "Time Intelligence [YTD]") (Qualified("Time Intelligence", "YTD")) "space before ["
                Expect.equal (p "[Total]") (Member "Total") ""
                Expect.equal (p "'Sales'") (Bare "Sales") ""
                Expect.equal (p "Sales") (Bare "Sales") ""
                Expect.equal (p "  [Total]  ") (Member "Total") "trimmed"
                Expect.equal (p "'O''Brien'[A]]B]") (Qualified("O'Brien", "A]B")) "'' and ]] escapes"
                Expect.equal (p "measure:Sales/Total") (Exact(ObjectRef.inTable ObjectKind.Measure "Sales" "Total")) ""
                Expect.equal (p "model") (Exact ObjectRef.model) ""
                Expect.equal (p "Ratio: A") (Bare "Ratio: A") "a colon only means a ref after a known kind"
        ]

        testList "resolve" [
            resolves "measure:Sales/Total" "measure:Sales/Total"
            resolves "MEASURE:sales/total" "measure:Sales/Total"
            resolves "'Sales'[Total]" "measure:Sales/Total"
            resolves "Sales[Amount]" "column:Sales/Amount"
            resolves "sales[amount]" "column:Sales/Amount"
            resolves "'Date'[Calendar]" "hierarchy:Date/Calendar"
            resolves "[Total]" "measure:Sales/Total"
            resolves "[Total (hidden)]" "measure:Sales/Total (hidden)"
            resolves "[Amount]" "column:Sales/Amount"
            resolves "[Period]" "column:Time Intelligence/Period"
            resolves "Sales" "table:Sales"
            resolves "'Time Intelligence'" "table:Time Intelligence"
            resolves "Readers" "role:Readers"
            resolves "Double" "function:Double"
            resolves "YTD" "calcitem:Time Intelligence/YTD"
            resolves "model" "model"
            resolves @"Sales[Path/With\Slashes]" @"column:Sales/Path\/With\\Slashes"

            fails "[DateKey]" ambiguous
            fails "Year" ambiguous // a column and a level
            fails "[Nope]" notFound
            fails "Sales[Nope]" notFound
            fails "Nope" notFound
            fails "measure:Sales/Nope" notFound
            fails "[unterminated" invalid
            fails "'Sales" invalid
            fails "'Sales'x" invalid
            fails "[]" invalid
            fails "Sales[Total] extra" invalid
        ]
    ]
