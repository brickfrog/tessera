module Tessera.Tests.LintTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.Fixtures

let private measureRef name =
    ObjectRef.inTable ObjectKind.Measure "Sales" name

/// The sample model with a measure M (in Sales) holding `expr`.
let private withMeasure (expr: string) =
    let db = sampleDatabase ()
    db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "M", Expression = expr))
    db

let private findingsOn (r: ObjectRef) (db: Database) =
    Lint.lint db |> List.filter (fun f -> f.Ref = r)

/// (rule id, fixable) for each finding on M.
let private found (expr: string) =
    withMeasure expr
    |> findingsOn (measureRef "M")
    |> List.map (fun f -> f.Rule.Id, Lint.Finding.fixable f)

/// M's expression after fixing everything (unchanged when nothing applies).
let private fixedIn (db: Database) (r: ObjectRef) (property: string) (original: string) =
    let result = Lint.fix db (fun _ -> true)

    result.Fixes
    |> List.tryFind (fun f -> f.Source.Owner.Ref = r && f.Source.Property = property)
    |> Option.map (fun f -> f.After)
    |> Option.defaultValue original

let private fixedText (expr: string) =
    fixedIn (withMeasure expr) (measureRef "M") "expression" expr

let private fixesTo (before: string) (after: string) =
    Expect.equal (fixedText before) after before

let private unchanged (expr: string) = fixesTo expr expr

/// Invalid tokens and unterminated /* comments.
let private lexProblems (dax: string) =
    Dax.tokenize dax
    |> List.filter (fun t ->
        t.Kind = Dax.TokenKind.Invalid
        || (t.Kind = Dax.TokenKind.Comment
            && t.Text.StartsWith "/*"
            && (t.Length < 4 || not (t.Text.EndsWith "*/"))))
    |> List.length

[<Tests>]
let ruleTests =
    testList "Lint rules" [
        testList "TS001 non-standard whitespace" [
            testCase "no-break and zero-width spaces become regular spaces"
            <| fun () ->
                Expect.equal (found "1\u00A0+ 2") [ "TS001", true ] ""
                fixesTo "1\u00A0+ 2" "1 + 2"
                fixesTo "[Total]\u200B+\u2007\u20071" "[Total] +  1"
                fixesTo "\uFEFFSUM(Sales[Amount])" " SUM(Sales[Amount])"

            testCase "the finding names the character and is an error"
            <| fun () ->
                let f = withMeasure "1 +\u00A02" |> findingsOn (measureRef "M") |> List.exactlyOne
                Expect.equal f.Rule.Severity Validate.Severity.Error ""
                Expect.stringContains f.Message "U+00A0 (no-break space)" ""
                Expect.equal (f.Line, f.Column) (1, 4) ""

            testCase "strings, quoted names, [names] and comments are left alone"
            <| fun () -> Expect.isEmpty (found "\"a\u00A0b\" // c\u00A0d\n& \"x\" /* \u200B */") ""
        ]

        testList "DI001/DI002 unused variables" [
            testCase "TE3's DI001 example"
            <| fun () ->
                let before =
                    "VAR _internetSalesTaxed = [Total] * 1.25\nVAR _cogsTaxed = [Total] * 1.25\n"
                    + "VAR _internetSales = [Total]\nRETURN _internetSalesTaxed - _cogsTaxed"

                Expect.equal (found before) [ "DI001", true ] ""

                fixesTo
                    before
                    "VAR _internetSalesTaxed = [Total] * 1.25\nVAR _cogsTaxed = [Total] * 1.25\nRETURN _internetSalesTaxed - _cogsTaxed"

            testCase "TE3's DI002 example: variables only used by unused variables"
            <| fun () ->
                let before =
                    "VAR _internetSalesTaxed = [Total] * 1.25\nVAR _cogs = [Total]\nVAR _cogsTaxed = _cogs * 1.25\n"
                    + "RETURN _internetSalesTaxed"

                Expect.equal (found before) [ "DI002", true; "DI001", true ] ""
                fixesTo before "VAR _internetSalesTaxed = [Total] * 1.25\nRETURN _internetSalesTaxed"

            testCase "every variable unused: the block becomes its RETURN expression"
            <| fun () ->
                fixesTo "VAR a = 1 VAR b = a RETURN 123" "123"
                fixesTo "VAR a = 1\nRETURN\n    [Total] * 2" "[Total] * 2"

                fixesTo
                    "CALCULATE(\n    VAR a = 1\n    RETURN [Total],\n    Sales[Amount] > 0\n)"
                    "CALCULATE(\n    [Total],\n    Sales[Amount] > 0\n)"

            testCase "on one line, the definition and its separating space go"
            <| fun () ->
                fixesTo "VAR a = 1 VAR b = 2 RETURN b" "VAR b = 2 RETURN b"
                fixesTo "VAR b = 2 VAR a = 1\nRETURN b" "VAR b = 2\nRETURN b"

            testCase "comments on other lines stay; a trailing comment goes with its line"
            <| fun () ->
                fixesTo "VAR a = 1 // unused\n// the result\nVAR b = 2\nRETURN b" "// the result\nVAR b = 2\nRETURN b"

            testCase "CRLF line endings"
            <| fun () -> fixesTo "VAR a = 1\r\nVAR b = 2\r\nRETURN b" "VAR b = 2\r\nRETURN b"

            testCase "a block that is an operand isn't unwrapped (it would change precedence)"
            <| fun () ->
                Expect.equal (found "2 * VAR a = 1 RETURN 3 + 4") [ "DI001", false ] ""
                unchanged "2 * VAR a = 1 RETURN 3 + 4"

            testCase "used variables, and names defined twice, are not reported"
            <| fun () ->
                Expect.isEmpty (found "VAR Sales = 1 RETURN Sales + 1") "a variable named like a table"
                Expect.isEmpty (found "VAR t = FILTER(Sales, [Total] > 1) RETURN COUNTROWS(t) + SUMX(t, t[Amount])") ""
                Expect.isEmpty (found "VAR a = 1 RETURN (VAR a = 2 RETURN a)") "shadowed: not analysed"

            testCase "a nested block's unused variable"
            <| fun () -> fixesTo "VAR a = VAR b = 1 VAR c = 2 RETURN c RETURN a" "VAR a = VAR c = 2 RETURN c RETURN a"
        ]

        testList "DI003/DI004 table names" [
            testCase "a measure loses its table"
            <| fun () ->
                Expect.equal (found "'Sales'[Total] * 1.25") [ "DI003", true ] ""
                fixesTo "'Sales'[Total] * 1.25 + Sales[Total]" "[Total] * 1.25 + [Total]"

            testCase "a column gets its table, always quoted"
            <| fun () ->
                Expect.equal (found "SUMX(Sales, [Amount] * 2)") [ "DI004", true ] ""
                fixesTo "SUMX(Sales, [Amount] * [DateKey])" "SUMX(Sales, 'Sales'[Amount] * 'Sales'[DateKey])"

            testCase "in a calculated column and a row filter too"
            <| fun () ->
                let db = sampleDatabase ()
                let sales = db.Model.Tables.["Sales"]
                sales.Columns.Add(new CalculatedColumn(Name = "Doubled", Expression = "[Amount] * 2"))
                let readers = db.Model.Roles.["Readers"]
                readers.TablePermissions.Add(new TablePermission(Table = sales, FilterExpression = "[Amount] > 0"))
                let column = ObjectRef.inTable ObjectKind.Column "Sales" "Doubled"
                let role = ObjectRef.modelLevel ObjectKind.Role "Readers"
                Expect.equal (fixedIn db column "expression" "") "'Sales'[Amount] * 2" ""
                Expect.equal (fixedIn db role "tablePermissions.Sales.filterExpression" "") "'Sales'[Amount] > 0" ""

            testCase "not fixed when the unqualified name would mean something else"
            <| fun () ->
                let db = sampleDatabase ()
                db.Model.Tables.["Sales"].Measures.Add(new Measure(Name = "Year", Expression = "1"))
                // In Date, [Year] is Date's column, not the Sales measure.
                db.Model.Tables.["Date"].Measures.Add(new Measure(Name = "D", Expression = "'Sales'[Year] + 1"))
                let d = ObjectRef.inTable ObjectKind.Measure "Date" "D"
                let fs = findingsOn d db
                Expect.equal (fs |> List.map (fun f -> f.Rule.Id, Lint.Finding.fixable f)) [ "DI003", false ] ""

            testCase "not fixed when a measure has the column's name"
            <| fun () ->
                let db = withMeasure "SUM([Amount])"
                db.Model.Tables.["Date"].Measures.Add(new Measure(Name = "Amount", Expression = "1"))
                let f = findingsOn (measureRef "M") db |> List.exactlyOne
                Expect.equal (f.Rule.Id, Lint.Finding.fixable f) ("DI004", false) ""
                Expect.stringContains f.Message "a measure has the same name" ""

            testCase "references inside NAMEOF (field parameters) are left as written"
            <| fun () ->
                Expect.isEmpty (found "NAMEOF('Sales'[Total]) & NAMEOF([Amount])") ""
                Expect.equal (found "NAMEOF('Sales'[Total]) & 'Sales'[Total]") [ "DI003", true ] ""

            testCase "unqualified measures and qualified columns are fine"
            <| fun () -> Expect.isEmpty (found "[Total] + SUM(Sales[Amount]) + SUM('Date'[Year])") ""
        ]

        testList "DI008 variable in CALCULATE" [
            testCase "TE3's example: reported as a warning, not fixed"
            <| fun () ->
                let expr =
                    "VAR _salesWithTax = [Total] * 1.25\nRETURN\n    CALCULATE(_salesWithTax, 'Date'[Month] = \"Jan\")"

                let f = withMeasure expr |> findingsOn (measureRef "M") |> List.exactlyOne
                Expect.equal (f.Rule.Id, f.Rule.Severity, f.Fix) ("DI008", Validate.Severity.Warning, None) ""
                Expect.equal (f.Line, f.Column) (3, 5) ""
                Expect.stringContains f.Message "filter arguments don't change the result" ""
                unchanged expr

            testCase "CALCULATETABLE, and CALCULATE with no filters"
            <| fun () ->
                Expect.equal
                    (found "VAR t = Sales RETURN COUNTROWS(CALCULATETABLE(t, Sales[Amount] > 0))")
                    [ "DI008", false ]
                    ""

                Expect.equal (found "VAR x = [Total] RETURN CALCULATE(x)") [ "DI008", false ] ""

            testCase "measures, expressions and UDF parameters are not variables"
            <| fun () ->
                Expect.isEmpty (found "CALCULATE([Total], Sales[Amount] > 0)") ""
                Expect.isEmpty (found "VAR x = 1 RETURN CALCULATE(x + [Total], Sales[Amount] > 0)") ""
                Expect.isEmpty (found "CALCULATE(Sales, Sales[Amount] > 0)") "not a variable at all"
                let db = sampleDatabase ()

                db.Model.Functions.Add(
                    new Function(Name = "Filtered", Expression = "(x: ANYVAL EXPR) => CALCULATE(x, Sales[Amount] > 0)")
                )

                Expect.isEmpty (findingsOn (ObjectRef.modelLevel ObjectKind.Function "Filtered") db) ""
        ]

        testList "DI014 IFERROR -> DIVIDE" [
            testCase "TE3's examples"
            <| fun () ->
                Expect.equal (found "IFERROR([Total] / [Total], BLANK())") [ "DI014", true ] ""
                fixesTo "IFERROR([Total] / [Total], BLANK())" "DIVIDE([Total], [Total])"

                fixesTo
                    "IFERROR(([Total] - [Total (hidden)]) / [Total], 1)"
                    "DIVIDE([Total] - [Total (hidden)], [Total], 1)"

            testCase "multiplication before the division, signs and powers are part of the operands"
            <| fun () ->
                fixesTo "IFERROR([Total] * 2 / [Total], 0)" "DIVIDE([Total] * 2, [Total], 0)"
                fixesTo "IFERROR(-[Total] / [Total] ^ 2, 0)" "DIVIDE(-[Total], [Total] ^ 2, 0)"
                fixesTo "iferror( [Total]/[Total] , 0 )" "DIVIDE( [Total],[Total] , 0 )"

            testCase "not when the division isn't the whole first argument"
            <| fun () ->
                for expr in
                    [
                        "IFERROR([Total] / [Total] * 2, 0)"
                        "IFERROR([Total] + [Total] / 2, 0)"
                        "IFERROR([Total] / 2 / 3, 0)"
                        "IFERROR([Total], 0)"
                        "IFERROR([Total] / 2 > 1, 0)"
                        "IFERROR([Total] / 2)"
                    ] do
                    Expect.isEmpty (found expr) expr

            testCase "a comment before the closing parenthesis keeps BLANK()"
            <| fun () -> fixesTo "IFERROR([Total] / 2, BLANK() /* why */)" "DIVIDE([Total], 2, BLANK() /* why */)"
        ]

        testList "DR014 IN" [
            testCase "TE3's examples"
            <| fun () ->
                let before =
                    "IF('Date'[Month] = \"Invoice\" || 'Date'[Month] = \"Credit Note\", 1, 0)"

                Expect.equal (found before) [ "DR014", true ] ""
                fixesTo before "IF('Date'[Month] IN {\"Invoice\", \"Credit Note\"}, 1, 0)"
                fixesTo "VAR a = [Total] RETURN a = 1 || a = 2 || a = 100" "VAR a = [Total] RETURN a IN {1, 2, 100}"

            testCase "the finding is info"
            <| fun () ->
                let f =
                    withMeasure "[Total] = 1 || [Total] = 2"
                    |> findingsOn (measureRef "M")
                    |> List.exactlyOne

                Expect.equal f.Rule.Severity Validate.Severity.Info ""

            testCase "not when BLANK could match a value, or the chain is part of something bigger"
            <| fun () ->
                for expr in
                    [
                        "[Total] = 0 || [Total] = 1"
                        "[Total] = 1 || [Total] = -0"
                        "'Date'[Month] = \"\" || 'Date'[Month] = \"Jan\""
                        "[Total] = 1 || [Total] = \"1\""
                        "[Total] = 1 || [Total (hidden)] = 2"
                        "[Total] > 0 && [Total] = 1 || [Total] = 2"
                        "[Total] = 1 || [Total] = 2 && [Total] > 0"
                        "NOT [Total] = 1 || [Total] = 2"
                        "[Total] = TRUE || [Total] = FALSE"
                        "[Total] = 1"
                    ] do
                    Expect.isEmpty (found expr) expr

            testCase "comments inside the chain: reported, not fixed"
            <| fun () -> Expect.equal (found "[Total] = 1 /* one */ || [Total] = 2") [ "DR014", false ] ""
        ]
    ]

[<Tests>]
let modelTests =
    testList "Lint models" [
        testCase "the sample model is clean"
        <| fun () ->
            Expect.isEmpty (Lint.lint (sampleDatabase ())) ""
            Expect.isEmpty (Lint.fix (sampleDatabase ()) (fun _ -> true)).Fixes ""

        testCase "findings carry their position, rule and fix"
        <| fun () ->
            let f =
                withMeasure "1 +\n  SUM([Amount])"
                |> findingsOn (measureRef "M")
                |> List.exactlyOne

            Expect.equal (f.Property, f.Line, f.Column, f.Offset) ("expression", 2, 7, 10) ""
            Expect.equal f.Rule.Name "Add table name to column references" ""
            Expect.equal f.Rule.Category "Improvement" ""

            Expect.equal
                f.Fix
                (Some [
                    {
                        Lint.Start = 10
                        Lint.Length = 8
                        Lint.Text = "'Sales'[Amount]"
                    }
                ])
                ""

        testCase "fixes go through Mutation: applied, with no errors introduced"
        <| fun () ->
            let db =
                withMeasure "VAR unused = 1\nRETURN\n    IFERROR(SUM([Amount]) / 'Sales'[Total], 0)"

            let applied =
                Mutation.apply db (fun db ->
                    Lint.fix db (fun _ -> true) |> Lint.apply
                    Ok Mutation.Effect.none)
                |> Result.defaultWith (fun e -> failtestf "%A" e)

            Expect.isEmpty applied.IntroducedErrors ""

            Expect.equal
                db.Model.Tables.["Sales"].Measures.["M"].Expression
                "DIVIDE(SUM('Sales'[Amount]), [Total], 0)"
                ""

            Expect.isEmpty (findingsOn (measureRef "M") db) "nothing left to fix"

        testCase "a selection fixes only those rules"
        <| fun () ->
            let db = withMeasure "VAR unused = 1 RETURN SUM([Amount]) + 'Sales'[Total]"
            let result = Lint.fix db (fun f -> f.Rule.Id = "DI004")
            Lint.apply result

            Expect.equal
                db.Model.Tables.["Sales"].Measures.["M"].Expression
                "VAR unused = 1 RETURN SUM('Sales'[Amount]) + 'Sales'[Total]"
                ""

            Expect.equal (result.Fixes |> List.collect (fun f -> f.Fixed)) [ "DI004", 1 ] ""

        testCase "unfixable findings are reported as remaining"
        <| fun () ->
            let result =
                Lint.fix (withMeasure "VAR x = 1 RETURN CALCULATE(x, Sales[Amount] > 0)") (fun _ -> true)

            Expect.isEmpty result.Fixes ""
            Expect.equal (result.Remaining |> List.map (fun f -> f.Rule.Id)) [ "DI008" ] ""

        testCase "after fixing, bpa's qualification rules agree"
        <| fun () ->
            let db = withMeasure "SUM([Amount]) + 'Sales'[Total] + Sales[Total]"
            let qualification = [ "TESS_QUALIFY_COLUMNS"; "TESS_UNQUALIFIED_MEASURES" ]

            let bpaHits (db: Database) =
                let rules =
                    BpaRules.builtIn.Value
                    |> List.filter (fun r -> List.contains r.Id qualification)

                Bpa.analyze db rules
                |> List.collect (fun r -> r.Findings |> List.map (fun f -> r.Rule.Id, ObjectRef.format f.Ref))
                |> List.filter (fun (_, r) -> r = "measure:Sales/M")

            Expect.equal (bpaHits db |> List.map fst |> List.sort) (List.sort qualification) "before"
            Lint.fix db (fun _ -> true) |> Lint.apply
            Expect.isEmpty (bpaHits db) "after"
    ]

// ---- Properties ---------------------------------------------------------------------------------

/// DAX-shaped text built from the rules' patterns, with odd whitespace and comments mixed in.
type LintGen =
    static member Dax() =
        let ws =
            Gen.elements [
                " "
                " "
                " "
                "\n"
                "\n    "
                "\u00A0"
                "\u200B"
                " // c\n"
                " /* c */ "
                "\t"
            ]

        let name = Gen.elements [ "a"; "b"; "c"; "_x"; "Sales" ]

        let atom =
            Gen.elements [
                "[Amount]"
                "[Total]"
                "'Sales'[Total]"
                "Sales[Amount]"
                "'Date'[Month]"
                "1"
                "0"
                "2.5"
                "\"x\""
                "\"\""
                "BLANK()"
                "a"
                "b"
                "_x"
            ]

        let rec expr (size: int) : Gen<string> =
            if size <= 0 then
                atom
            else
                let sub = expr (size / 2)

                Gen.oneof [
                    atom
                    gen {
                        let! l = sub
                        let! op = Gen.elements [ " + "; " * "; " / "; " - "; " = "; " || "; " && " ]
                        let! r = sub
                        return l + op + r
                    }
                    gen {
                        let! f = Gen.elements [ "SUM"; "CALCULATE"; "IF"; "SUMX"; "COUNTROWS" ]
                        let! args = Gen.listOfLength 2 sub
                        return f + "(" + String.concat ", " args + ")"
                    }
                    gen {
                        let! vars = Gen.listOfLength 2 (Gen.zip name sub)
                        let! seps = Gen.listOfLength 3 ws
                        let! body = sub

                        let defs =
                            List.zip vars (List.take 2 seps)
                            |> List.map (fun ((n, e), s) -> $"VAR {n} = {e}{s}")
                            |> String.concat ""

                        return defs + "RETURN" + seps.[2] + body
                    }
                    gen {
                        let! a = sub
                        let! b = sub
                        let! x = Gen.elements [ "0"; "BLANK()"; "1" ]
                        return $"IFERROR({a} / {b}, {x})"
                    }
                    gen {
                        let! lhs = Gen.elements [ "[Total]"; "'Date'[Month]"; "a" ]
                        let! values = Gen.listOfLength 3 (Gen.elements [ "1"; "2"; "\"A\""; "0" ])
                        return values |> List.map (fun v -> $"{lhs} = {v}") |> String.concat " || "
                    }
                    gen {
                        let! v = name
                        let! e = sub
                        return $"CALCULATE({v}, {e})"
                    }
                    gen {
                        let! e = sub
                        let! s = ws
                        return "(" + s + e + ")"
                    }
                    gen {
                        let! l = sub
                        let! s = ws
                        let! r = sub
                        return l + s + r
                    }
                ]

        Gen.sized (fun n -> expr (min n 12)) |> Arb.fromGen

let private config = {
    FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<LintGen> ]
        maxTest = 300
}

/// One model per property, with M's expression swapped in for each case.
let private fixer () =
    let db = withMeasure "1"
    let m = db.Model.Tables.["Sales"].Measures.["M"]

    fun (expr: string) ->
        m.Expression <- expr
        let result = Lint.fix db (fun _ -> true)

        let after =
            result.Fixes
            |> List.tryFind (fun f -> f.Source.Owner.Ref = measureRef "M")
            |> Option.map (fun f -> f.After)
            |> Option.defaultValue expr

        after, result

[<Tests>]
let propertyTests =
    testList "Lint properties" [
        testPropertyWithConfig config "fixing twice changes nothing more"
        <| fun (expr: string) ->
            let fix = fixer ()
            let once, result = fix expr
            let twice, _ = fix once
            result.Unsettled.IsEmpty && twice = once

        testPropertyWithConfig config "fixed DAX lexes as cleanly as before, and has nothing fixable left"
        <| fun (expr: string) ->
            let fix = fixer ()
            let once, _ = fix expr
            let db = withMeasure once

            lexProblems once <= lexProblems expr
            && (lexProblems expr > 0 || lexProblems once = 0)
            && (findingsOn (measureRef "M") db |> List.forall (Lint.Finding.fixable >> not))

        testPropertyWithConfig config "fixed DAX refers to no object the original didn't"
        <| fun (expr: string) ->
            let fix = fixer ()
            let once, _ = fix expr

            let targets (dax: string) =
                Deps.dependsOn (Deps.build (withMeasure dax)) (measureRef "M")
                |> List.choose (fun r ->
                    match r.Target with
                    | Deps.Resolved t -> Some t
                    | Deps.Unresolved _ -> None)
                |> Set.ofList

            Set.isSubset (targets once) (targets expr)

        testPropertyWithConfig config "CRLF line endings fix the same way as LF"
        <| fun (expr: string) ->
            let fix = fixer ()
            let lf, _ = fix expr
            let crlf, _ = fix (expr.Replace("\n", "\r\n"))
            crlf = lf.Replace("\n", "\r\n")
    ]
