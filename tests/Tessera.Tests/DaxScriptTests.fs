module Tessera.Tests.DaxScriptTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Tessera
open Tessera.DaxScript

let private parseOk (text: string) =
    match parse text with
    | Ok s -> s
    | Error e -> failtestf "parse failed at %d:%d: %s" e.Position.Line e.Position.Column e.Message

let private parseError (text: string) =
    match parse text with
    | Ok s -> failtestf "expected a parse error, got %A" s
    | Error e -> e

let private props (s: Statement) =
    s.Properties |> List.map (fun p -> p.Name, p.Value)

// Tabular Editor 3's documented examples (docs.tabulareditor.com/te3/features/dax-scripts.html).
let private te3Measure =
    """----------------------------------
-- Measure: [Internet Total Sales]
----------------------------------
MEASURE 'Internet Sales'[Internet Total Sales] = SUM('Internet Sales'[Sales Amount])
    Description = "Returns the sum of the sales amount"
    FormatString = "\$#,0.00;(\$#,0.00);\$#,0.00"
"""

let private te3Kpi =
    """--------------------------------------------------------
-- Measure: [Internet Current Quarter Sales Performance]
--------------------------------------------------------
MEASURE 'Internet Sales'[Internet Current Quarter Sales Performance] =
    IFERROR(
        [Internet Current Quarter Sales] / [Internet Previous Quarter Sales Proportion to QTD],
        BLANK()
    )
    , KpiStatusExpression =
        VAR x = [Internet Current Quarter Sales Performance]
        RETURN
            IF(ISBLANK(x), BLANK(), IF(x < 1, -1, IF(x < 1.07, 0, 1)))
    , KpiStatusGraphic = "Shapes"
    , KpiTargetExpression = 1.1
"""

let private te3CalculationGroup =
    """-----------------------------------------
-- Calculation Group: 'Time Intelligence'
-----------------------------------------
CALCULATIONGROUP 'Time Intelligence'[Period]
    Description = "Use this table to perform time calculations"

    CALCULATIONITEM "Current" = SELECTEDMEASURE()
        Ordinal = 0

    CALCULATIONITEM "MTD" = TOTALMTD(SELECTEDMEASURE(), 'Calendar'[Date])
        Ordinal = 1

    CALCULATIONITEM "YoY %" = DIVIDE(SELECTEDMEASURE() - [PY], [PY])
        Ordinal = 5
        FormatString = "Percent"
"""

[<Tests>]
let tests =
    testList "DaxScript" [
        testList "Tabular Editor 3's examples" [
            testCase "a measure with a description and a format string"
            <| fun () ->
                match parseOk te3Measure with
                | [ s ] ->
                    Expect.equal s.Type ObjectType.Measure ""
                    Expect.equal s.Table (Some "Internet Sales") ""
                    Expect.equal s.Name "Internet Total Sales" ""
                    Expect.equal s.Expression (Some "SUM('Internet Sales'[Sales Amount])") ""
                    Expect.equal s.Position { Line = 4; Column = 1 } "the keyword"

                    Expect.equal
                        (props s)
                        [
                            "Description", Value.String "Returns the sum of the sales amount"
                            "FormatString", Value.String "\\$#,0.00;(\\$#,0.00);\\$#,0.00"
                        ]
                        "a lone string is the static format string"
                | other -> failtestf "%A" other

            testCase "leading commas, a multi-line expression and KPI properties"
            <| fun () ->
                match parseOk te3Kpi with
                | [ s ] ->
                    Expect.equal
                        s.Expression
                        (Some(
                            "IFERROR(\n    [Internet Current Quarter Sales] / [Internet Previous Quarter Sales Proportion to QTD],\n    BLANK()\n)"
                        ))
                        "the next line on, without the common indentation"

                    Expect.equal
                        (props s)
                        [
                            "KpiStatusExpression",
                            Value.Expression(
                                "VAR x = [Internet Current Quarter Sales Performance]\nRETURN\n    IF(ISBLANK(x), BLANK(), IF(x < 1, -1, IF(x < 1.07, 0, 1)))"
                            )
                            "KpiStatusGraphic", Value.String "Shapes"
                            "KpiTargetExpression", Value.Expression "1.1"
                        ]
                        ""
                | other -> failtestf "%A" other

            testCase "a calculation group and its items"
            <| fun () ->
                match parseOk te3CalculationGroup with
                | [ g; current; mtd; yoy ] ->
                    Expect.equal g.Type ObjectType.CalculationGroup ""
                    Expect.equal (g.Table, g.Name) (Some "Time Intelligence", "Period") "the table and its column"
                    Expect.isNone g.Expression ""

                    Expect.equal
                        (props g)
                        [ "Description", Value.String "Use this table to perform time calculations" ]
                        ""

                    for i in [ current; mtd; yoy ] do
                        Expect.equal i.Type ObjectType.CalculationItem ""
                        Expect.equal i.Table (Some "Time Intelligence") "items belong to the group above"

                    Expect.equal current.Expression (Some "SELECTEDMEASURE()") ""
                    Expect.equal (props mtd) [ "Ordinal", Value.Int 1 ] ""
                    Expect.equal yoy.Name "YoY %" ""

                    Expect.equal
                        (props yoy)
                        [ "Ordinal", Value.Int 5; "FormatString", Value.Expression "\"Percent\"" ]
                        "an item's format string is always DAX"
                | other -> failtestf "%A" other

            testCase "an empty expression, and an omitted one"
            <| fun () ->
                let script =
                    "MEASURE 'Internet Sales'[Internet Total Sales] =\n"
                    + "    , DisplayFolder = \"Sales\"\n\n"
                    + "MEASURE 'Internet Sales'[Internet Total Sales]\n"
                    + "    DisplayFolder = \"Sales\"\n"

                match parseOk script with
                | [ empty; omitted ] ->
                    Expect.equal empty.Expression (Some "") "= with nothing after it"
                    Expect.equal omitted.Expression None "no ="
                    Expect.equal (props empty) (props omitted) ""
                | other -> failtestf "%A" other
        ]

        testList "DAX queries" [
            testCase "DEFINE objects are read; EVALUATE sections are skipped"
            <| fun () ->
                let script =
                    "DEFINE\n"
                    + "    MEASURE Sales[A] = 1 MEASURE Sales[B] = [A] + 1\n"
                    + "    COLUMN Sales[C] = Sales[Amount] * 2\n"
                    + "    TABLE Copy = Sales\n"
                    + "    FUNCTION Twice = (x: INT64) => x * 2\n"
                    + "EVALUATE\n"
                    + "    SUMMARIZECOLUMNS(\"A\", [A])\n"
                    + "ORDER BY [A]\n"
                    + "EVALUATE { 1 }\n"
                    + "DEFINE MEASURE Sales[D] = 4\n"
                    + "EVALUATE { [D] }\n"

                let read =
                    parseOk script |> List.map (fun s -> s.Type, s.Table, s.Name, s.Expression)

                Expect.equal
                    read
                    [
                        ObjectType.Measure, Some "Sales", "A", Some "1"
                        ObjectType.Measure, Some "Sales", "B", Some "[A] + 1"
                        ObjectType.Column, Some "Sales", "C", Some "Sales[Amount] * 2"
                        ObjectType.Table, None, "Copy", Some "Sales"
                        ObjectType.Function, None, "Twice", Some "(x: INT64) => x * 2"
                        ObjectType.Measure, Some "Sales", "D", Some "4"
                    ]
                    ""
        ]

        testList "delimiting expressions" [
            testCase "comments after an expression: kept on its last line or indented, not at the margin"
            <| fun () ->
                let script =
                    "MEASURE 'T'[A] =\n"
                    + "    SUM(T[x])\n"
                    + "    -- old: SUMX(T, T[x])\n"
                    + "\n"
                    + "------------\n"
                    + "-- Measure: [B]\n"
                    + "------------\n"
                    + "MEASURE 'T'[B] = 1 -- same line\n"
                    + "-- banner\n"
                    + "MEASURE 'T'[C] = 2\n"

                Expect.equal
                    (parseOk script |> List.map (fun s -> s.Expression))
                    [ Some "SUM(T[x])\n-- old: SUMX(T, T[x])"; Some "1 -- same line"; Some "2" ]
                    ""

            testCase "a DAX line that only looks like a property stays in the expression"
            <| fun () ->
                let script =
                    "MEASURE 'T'[A] =\n"
                    + "    VAR Visible = 1\n"
                    + "    RETURN\n"
                    + "    Visible = 1\n"
                    + "    Visible = FALSE\n"

                match parseOk script with
                | [ s ] ->
                    Expect.equal
                        s.Expression
                        (Some "VAR Visible = 1\nRETURN\nVisible = 1")
                        "Visible takes TRUE or FALSE"

                    Expect.equal (props s) [ "Visible", Value.Bool false ] ""
                | other -> failtestf "%A" other

            testCase "properties inside parentheses are DAX; a comma at the top level ends the expression"
            <| fun () ->
                let script =
                    "MEASURE 'T'[A] = CALCULATE(\n    [X],\n    Description = \"y\"\n) , FormatString = \"0\""

                match parseOk script with
                | [ s ] ->
                    Expect.equal s.Expression (Some "CALCULATE(\n    [X],\n    Description = \"y\"\n)") ""
                    Expect.equal (props s) [ "FormatString", Value.String "0" ] ""
                | other -> failtestf "%A" other

            testCase "multi-line strings and escaped quotes are kept exactly"
            <| fun () ->
                let script =
                    "MEASURE 'T'[A] =\n    \"line 1\n    line 2\" & \"say \"\"hi\"\"\"\n"
                    + "    Description = \"multi\nline \"\"quoted\"\"\"\n"

                match parseOk script with
                | [ s ] ->
                    Expect.equal s.Expression (Some "\"line 1\nline 2\" & \"say \"\"hi\"\"\"") "dedented as a whole"
                    Expect.equal (props s) [ "Description", Value.String "multi\nline \"quoted\"" ] ""
                | other -> failtestf "%A" other

            testCase "keywords and property names are case-insensitive; names are canonical"
            <| fun () ->
                match parseOk "measure Sales[A] = 1\n    description = \"d\"\n    VISIBLE = false" with
                | [ s ] -> Expect.equal (props s) [ "Description", Value.String "d"; "Visible", Value.Bool false ] ""
                | other -> failtestf "%A" other

            testCase "a dynamic format string is DAX that isn't a lone string"
            <| fun () ->
                match parseOk "MEASURE 'T'[A] = 1\n    FormatString = IF([A] > 1, \"0\", \"0.0\")" with
                | [ s ] -> Expect.equal (props s) [ "FormatString", Value.Expression "IF([A] > 1, \"0\", \"0.0\")" ] ""
                | other -> failtestf "%A" other

            testCase "column data types, integers and empty strings"
            <| fun () ->
                let script =
                    "COLUMN 'T'[C] = 1\n    Datatype = integer\n    Description =\n    , DisplayFolder = \"\"\n"
                    + "CALCULATIONGROUP 'G'[Name]\n    Precedence = -2"

                match parseOk script with
                | [ c; g ] ->
                    Expect.equal
                        (props c)
                        [
                            "Datatype", Value.DataType "INTEGER"
                            "Description", Value.String ""
                            "DisplayFolder", Value.String ""
                        ]
                        ""

                    Expect.equal (props g) [ "Precedence", Value.Int -2 ] ""
                | other -> failtestf "%A" other
        ]

        testList "errors carry a line and column" [
            let check name script line column (fragment: string) =
                testCase name
                <| fun () ->
                    let e = parseError script
                    Expect.equal (e.Position.Line, e.Position.Column) (line, column) e.Message
                    Expect.stringContains e.Message fragment ""

            check
                "an unknown property"
                "MEASURE 'T'[A] = 1\n  , Colour = \"red\""
                2
                5
                "'Colour' is not a property of a measure"

            check "a property of another kind" "TABLE 'T' = X\n    Datatype = STRING" 2 5 "is not a property of a table"

            check
                "nothing after a comma"
                "MEASURE 'T'[A] = 1\n    ,\n"
                3
                1
                "expected a property (Name = value) after ','"

            check "a bad boolean, after a comma" "MEASURE 'T'[A] = 1\n    , Visible = yes" 2 17 "expected TRUE or FALSE"
            check "a bad data type" "COLUMN 'T'[A] = 1\n    , Datatype = TEXT" 2 18 "expected one of BOOLEAN"
            check "an item outside a group" "\n  CALCULATIONITEM \"X\" = 1" 2 3 "must follow its CALCULATIONGROUP"
            check "a group with an expression" "CALCULATIONGROUP 'G'[Name] = 1" 1 28 "no expression of its own"
            check "a measure without a table" "MEASURE [A] = 1" 1 9 "expected a table name"
            check "a measure without a name" "MEASURE 'T' = 1" 1 13 "expected [Name] after MEASURE 'T'"
            check "something that isn't an object" "-- hi\nSELECT 1" 2 1 "expected MEASURE, COLUMN, TABLE"
            check "an unterminated table name" "MEASURE 'Sales[A] = 1" 1 9 "expected a table name"
        ]

        testList "printing" [
            testCase "banners, indentation, and a comma after an empty expression"
            <| fun () ->
                let st t table name expr ps = {
                    Type = t
                    Table = table
                    Name = name
                    Expression = expr
                    Properties =
                        ps
                        |> List.map (fun (n, v) -> {
                            Name = n
                            Value = v
                            Position = Position.none
                        })
                    Position = Position.none
                }

                let text =
                    print [
                        st ObjectType.Measure (Some "Sales") "Total" (Some "SUM(Sales[Amount])") [
                            "FormatString", Value.String "#,0"
                            "Visible", Value.Bool false
                        ]
                        st ObjectType.Measure (Some "O'Brien") "A]B" (Some "VAR x = 1\nRETURN\n\n    x") [
                            "DetailRows", Value.Expression "T\n-- note"
                        ]
                        st ObjectType.Measure (Some "Sales") "Empty" (Some "") [
                            "Description", Value.String "say \"hi\""
                        ]
                        st ObjectType.CalculationGroup (Some "TI") "Period" None [ "Precedence", Value.Int 1 ]
                        st
                            ObjectType.CalculationItem
                            (Some "TI")
                            "YTD"
                            (Some "TOTALYTD(SELECTEDMEASURE(), 'Date'[Date])")
                            [ "Ordinal", Value.Int 0 ]
                        st ObjectType.Function None "Lib.Twice" (Some "(x) => x * 2") []
                    ]

                let expected =
                    """-------------------
-- Measure: [Total]
-------------------
MEASURE 'Sales'[Total] = SUM(Sales[Amount])
    FormatString = "#,0"
    Visible = FALSE

------------------
-- Measure: [A]]B]
------------------
MEASURE 'O''Brien'[A]]B] =
    VAR x = 1
    RETURN

        x
    DetailRows =
        T
        -- note

-------------------
-- Measure: [Empty]
-------------------
MEASURE 'Sales'[Empty] =
    , Description = "say ""hi""QUOTE

--------------------------
-- Calculation Group: 'TI'
--------------------------
CALCULATIONGROUP 'TI'[Period]
    Precedence = 1

    CALCULATIONITEM "YTD" = TOTALYTD(SELECTEDMEASURE(), 'Date'[Date])
        Ordinal = 0

----------------------
-- Function: Lib.Twice
----------------------
FUNCTION Lib.Twice = (x) => x * 2
"""
                    |> fun s -> s.Replace("QUOTE", "\"")

                Expect.equal text expected ""

                Expect.equal
                    (parseOk text
                     |> List.map (fun s -> s.Type, s.Table, s.Name, s.Expression, props s))
                    [
                        ObjectType.Measure,
                        Some "Sales",
                        "Total",
                        Some "SUM(Sales[Amount])",
                        [ "FormatString", Value.String "#,0"; "Visible", Value.Bool false ]
                        ObjectType.Measure,
                        Some "O'Brien",
                        "A]B",
                        Some "VAR x = 1\nRETURN\n\n    x",
                        [ "DetailRows", Value.Expression "T\n-- note" ]
                        ObjectType.Measure, Some "Sales", "Empty", Some "", [ "Description", Value.String "say \"hi\"" ]
                        ObjectType.CalculationGroup, Some "TI", "Period", None, [ "Precedence", Value.Int 1 ]
                        ObjectType.CalculationItem,
                        Some "TI",
                        "YTD",
                        Some "TOTALYTD(SELECTEDMEASURE(), 'Date'[Date])",
                        [ "Ordinal", Value.Int 0 ]
                        ObjectType.Function, None, "Lib.Twice", Some "(x) => x * 2", []
                    ]
                    "it reads back"
        ]

        testProperty "parse never throws"
        <| fun (text: string) ->
            match parse text with
            | Ok _
            | Error _ -> true

        testProperty
            "parse never throws on script-like text"
            (Prop.forAll
                (Gen.elements [
                    "MEASURE"
                    "'T'"
                    "[A]"
                    "="
                    ","
                    "\n"
                    "    "
                    "Visible"
                    "TRUE"
                    "\"s\""
                    "("
                    ")"
                    "--c\n"
                    "/*"
                    "CALCULATIONGROUP"
                    "CALCULATIONITEM"
                    "DEFINE"
                    "EVALUATE"
                    "1"
                    "-"
                    "Ordinal"
                 ]
                 |> Gen.listOf
                 |> Gen.map (String.concat " ")
                 |> Arb.fromGen)
                (fun text ->
                    match parse text with
                    | Ok _
                    | Error _ -> true))
    ]
