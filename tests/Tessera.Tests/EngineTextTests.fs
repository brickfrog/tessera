/// The text functions and FORMAT of the local DAX engine. Every expected value is one a source shows
/// or states (quoted next to it: Microsoft Learn's page of the function, or dax.guide/<function>),
/// or follows from such a rule on other data. What the sources leave open must be refused.
module Tessera.Tests.EngineTextTests

open System
open System.Globalization
open System.Threading
open Expecto
open FsCheck
open FsCheck.FSharp
open Microsoft.AnalysisServices.Tabular
open Tessera
open Tessera.Tests.EngineHelpers

/// T (4): Id, Txt, At, N, C (a fixed decimal: 12.5, 2.675, -0.125, 1234567.8912)
///   1  "Big red box"   2020-06-25 13:23:45   1234.567
///   2  BLANK           2021-01-02 00:00:00   BLANK
///   3  ""              1899-12-30 15:30:00   -1980.1    (a time alone)
///   4  "ab  "          2020-01-09 09:05:07   0.5
let private model: TestModel = {
    Tables = [
        table "T" [
            "Id", DataType.Int64
            "Txt", DataType.String
            "At", DataType.DateTime
            "N", DataType.Double
            "C", DataType.Decimal
        ] [
            [ 1; "Big red box"; box (DateTime(2020, 6, 25, 13, 23, 45)); 1234.567; 12.5M ]
            [ 2; null; box (DateTime(2021, 1, 2)); null; 2.675M ]
            [ 3; ""; box (DateTime(1899, 12, 30, 15, 30, 0)); -1980.1; -0.125M ]
            [ 4; "ab  "; box (DateTime(2020, 1, 9, 9, 5, 7)); 0.5; 1234567.8912M ]
        ]
    ]
    Rels = []
    Measures = []
    Functions = []
}

let private prepared = lazy (prepare model)
let private p () = prepared.Value

let private preparedWith (culture: string) =
    let db, store = build model
    db.Model.Culture <- culture

    match Engine.prepare db store with
    | Ok p -> p
    | Error e -> failtestf "prepare failed: %s" (TessError.message e)

let private german = lazy (preparedWith "de-DE")
let private noCulture = lazy (preparedWith null)

/// The one cell of `expr` evaluated in the row of T with this Id.
let private cellQuery (id: int) (expr: string) =
    $"EVALUATE SELECTCOLUMNS ( FILTER ( T, T[Id] = {id} ), \"r\", {expr} )"

let private isAt (id: int) (expr: string) (value: obj) =
    Expect.equal (rows (p ()) (cellQuery id expr)) [ [ qv value ] ] expr

let private refusedAt (id: int) (expr: string) (mentions: string) =
    notSupported (p ()) (cellQuery id expr) mentions

let private refused (dax: string) (mentions: string) =
    match scalarError (p ()) dax with
    | DaxNotSupported(what, _) -> Expect.stringContains what mentions dax
    | other -> failtestf "expected not-supported, got %A\n%s" other dax

let private fails (dax: string) =
    Expect.equal (TessError.code (scalarError (p ()) dax)) "dax-error" dax

let private text (dax: string) : string =
    match scalar (p ()) dax with
    | QueryValue.String s -> s
    | other -> failtestf "expected text, got %A\n%s" other dax

/// A text as a DAX literal.
let private lit (s: string) = "\"" + s.Replace("\"", "\"\"") + "\""

let private config = {
    FsCheckConfig.defaultConfig with
        maxTest = 200
}

/// Printable ASCII without characters that mean something to SEARCH.
let private asciiText: Gen<string> =
    Gen.elements ([ 'a' .. 'e' ] @ [ 'A' .. 'C' ] @ [ ' '; '-'; '1'; '"' ])
    |> Gen.listOf
    |> Gen.map (fun cs -> String(Array.ofList cs))

type AsciiText = AsciiText of string

type Generators =
    static member AsciiText() =
        Arb.fromGen (asciiText |> Gen.map AsciiText)

let private propertyConfig = {
    config with
        arbitrary = [ typeof<Generators> ]
}

[<Tests>]
let textTests =
    testList "Engine text functions" [
        testList "LEN LEFT RIGHT MID" [
            // dax.guide/len: Val = "DAX is so cool!": LEFT ( Val, 3 ) "DAX", RIGHT ( Val, 5 )
            // "cool!", MID ( Val, 11, 4 ) "cool", LEN ( Val ) 15.
            testCase "the documented example" (fun () ->
                is (p ()) "LEFT ( \"DAX is so cool!\", 3 )" "DAX"
                is (p ()) "RIGHT ( \"DAX is so cool!\", 5 )" "cool!"
                is (p ()) "MID ( \"DAX is so cool!\", 11, 4 )" "cool"
                is (p ()) "LEN ( \"DAX is so cool!\" )" 15)
            // Microsoft Learn, MID: MID("abcde",2,3) "Returns "bcd""; dax.guide/mid: SSN example.
            testCase "MID" (fun () ->
                is (p ()) "MID ( \"abcde\", 2, 3 )" "bcd"
                is (p ()) "\"XXX-XX-\" & MID ( \"123-45-6789\", 8, 3 ) & \"X\"" "XXX-XX-678X"
                is (p ()) "MID ( \"abcde\", 3, 3 )" "cde"
                is (p ()) "MID ( \"abcde\", 2, 0 )" "")
            // "(optional) The number of characters you want LEFT to extract; if omitted, 1"; "If the
            // num_chars argument is larger than the number of characters available, the function
            // returns the maximum characters available and doesn't raise an error" (Microsoft Learn).
            testCase "the count defaults to 1 and may exceed the text" (fun () ->
                is (p ()) "LEFT ( \"abc\" )" "a"
                is (p ()) "RIGHT ( \"abc\" )" "c"
                is (p ()) "LEFT ( \"abc\", 10 )" "abc"
                is (p ()) "RIGHT ( \"abc\", 10 )" "abc"
                is (p ()) "LEFT ( \"abc\", 0 )" ""
                is (p ()) "LEFT ( \"abc\", 2.0 )" "ab")
            // "Spaces count as characters" (Microsoft Learn, LEN); BLANK read as text is the empty
            // text (SQLBI, "Handling BLANK in DAX").
            testCase "LEN counts spaces, and BLANK is empty" (fun () ->
                is (p ()) "LEN ( \"ab  \" )" 4
                is (p ()) "LEN ( \"\" )" 0
                is (p ()) "LEN ( BLANK () )" 0
                isAt 2 "LEN ( T[Txt] )" 0
                isAt 1 "LEN ( T[Txt] )" 11)
            // Microsoft Learn, LEFT: "the column [ResellerKey] contains numbers such as 5, 24, and
            // 312, so the result also has variable length".
            testCase "a number is its text" (fun () ->
                is (p ()) "LEFT ( 312, 5 )" "312"
                is (p ()) "RIGHT ( 12345, 2 )" "45")
            testCase "what the pages leave open is refused" (fun () ->
                refused "MID ( \"abcde\", 4, 10 )" "past the end"
                refused "MID ( \"abcde\", 0, 2 )" "before the first character"
                refused "LEFT ( \"abc\", -1 )" "negative"
                refused "LEFT ( \"abc\", 1.5 )" "not a whole number"
                refused "RIGHT ( \"abc\", BLANK () )" "BLANK"
                refused "LEFT ( BLANK (), 1 )" "BLANK text"
                refusedAt 2 "LEFT ( T[Txt], 1 )" "BLANK text"
                refused "LEN ( 12345 )" "not text"
                refused "LEFT ( TRUE (), 1 )" "TRUE or FALSE"
                refused "LEFT ( dt\"2020-01-02\", 1 )" "date")
            // "This function returns different results depending on the UnicodeCharacterBehavior
            // setting of your model" (Microsoft Learn: LEN, LEFT, RIGHT, MID, REPLACE, FIND).
            testCase "surrogate pairs are refused where a position counts" (fun () ->
                refused "LEN ( \"a😀b\" )" "Basic Multilingual Plane"
                refused "LEFT ( \"a😀b\", 2 )" "Basic Multilingual Plane"
                refused "MID ( \"a😀b\", 2, 1 )" "Basic Multilingual Plane"
                refused "FIND ( \"b\", \"a😀b\" )" "Basic Multilingual Plane"
                refused "SEARCH ( \"b\", \"a😀b\" )" "Basic Multilingual Plane"
                refused "REPLACE ( \"a😀b\", 1, 1, \"x\" )" "Basic Multilingual Plane"
                refused "UNICODE ( \"😀\" )" "Basic Multilingual Plane")
        ]

        testList "UPPER LOWER TRIM REPT" [
            // dax.guide/upper: "UPPER = DAX IS SO COOL!", "LOWER = dax is so cool!"; Microsoft Learn,
            // LOWER: "= LOWER("123ABC") returns 123abc".
            testCase "case" (fun () ->
                is (p ()) "UPPER ( \"DAX is so cool!\" )" "DAX IS SO COOL!"
                is (p ()) "LOWER ( \"DAX is so cool!\" )" "dax is so cool!"
                is (p ()) "LOWER ( \"123ABC\" )" "123abc"
                is (p ()) "UPPER ( \"日本 123\" )" "日本 123")
            testCase "letters outside ASCII and other cultures are refused" (fun () ->
                refused "UPPER ( \"straße\" )" "outside ASCII"
                refused "LOWER ( \"École\" )" "outside ASCII"
                refused "UPPER ( BLANK () )" "BLANK text"
                notSupported german.Value "EVALUATE ROW ( \"r\", UPPER ( \"i\" ) )" "de-DE")
            // dax.guide/trim: "TRIM removes trailing and starting spaces from a string, including
            // multiple spaces within the string"; Microsoft Learn: it "does not remove this
            // nonbreaking space character".
            testCase "TRIM" (fun () ->
                is (p ()) "TRIM ( \"  DAX is          awesome !!!     \" )" "DAX is awesome !!!"
                is (p ()) "TRIM ( \"A column with trailing spaces.   \" )" "A column with trailing spaces."
                is (p ()) "TRIM ( \"   \" )" ""
                is (p ()) "LEN ( TRIM ( \" a  \" ) )" 4)
            // Microsoft Learn, REPT: REPT("85",3) is 858585; "The result of the REPT function cannot
            // be longer than 32,767 characters, or REPT returns an error".
            testCase "REPT" (fun () ->
                is (p ()) "REPT ( \"85\", 3 )" "858585"
                is (p ()) "REPT ( \"Text\", 2 )" "TextText"
                is (p ()) "REPT ( \"\", 9000000000000000 )" ""
                fails "REPT ( \"a\", 9000000000000000 )"
                is (p ()) "LEN ( REPT ( \"a\", 32767 ) )" 32767
                fails "REPT ( \"a\", 32768 )"
                is (p ()) "IFERROR ( REPT ( \"ab\", 20000 ), \"too long\" )" "too long")
            // "If num_times is 0 (zero), REPT returns a blank" / "is truncated" (Microsoft Learn)
            // against "returns an empty string" / "rounded to the closest integer" (dax.guide/rept).
            testCase "REPT refuses where the two pages disagree" (fun () ->
                refused "REPT ( \"ab\", 0 )" "zero times"
                refused "REPT ( \"ab\", 2.7 )" "not a whole number"
                refused "REPT ( \"ab\", -1 )" "negative")
        ]

        testList "SUBSTITUTE REPLACE" [
            // "If omitted, every instance of old_text is replaced"; "The SUBSTITUTE function is
            // case-sensitive" (Microsoft Learn). dax.guide/substitute: "Contoso 2G MP3 Player E200
            // Red" with "2" -> "?" is "Contoso ?G MP3 Player E?00 Red".
            testCase "SUBSTITUTE" (fun () ->
                is
                    (p ())
                    "SUBSTITUTE ( \"Contoso 2G MP3 Player E200 Red\", \"2\", \"?\" )"
                    "Contoso ?G MP3 Player E?00 Red"

                is
                    (p ())
                    "SUBSTITUTE ( \"Contoso 512MB MP3 Player E51 Blue\", \"MP3 Player\", \"Zune-like player\" )"
                    "Contoso 512MB Zune-like player E51 Blue"

                is (p ()) "SUBSTITUTE ( \"a-b-c\", \"-\", \"+\" )" "a+b+c"
                is (p ()) "SUBSTITUTE ( \"a-b-c\", \"-\", \"+\", 2 )" "a-b+c"
                is (p ()) "SUBSTITUTE ( \"a-b-c\", \"-\", \"+\", 3 )" "a-b-c"
                is (p ()) "SUBSTITUTE ( \"Red red RED\", \"red\", \"blue\" )" "Red blue RED"
                is (p ()) "SUBSTITUTE ( \"a-b\", \"-\", BLANK () )" "ab"
                is (p ()) "SUBSTITUTE ( \"abc\", \"x\", \"y\" )" "abc")
            testCase "SUBSTITUTE refuses the undocumented" (fun () ->
                refused "SUBSTITUTE ( \"abc\", \"\", \"x\" )" "empty text"
                refused "SUBSTITUTE ( \"aaa\", \"aa\", \"x\" )" "overlap"
                refused "SUBSTITUTE ( \"a-b\", \"-\", \"+\", 0 )" "below 1"
                refused "SUBSTITUTE ( BLANK (), \"a\", \"b\" )" "BLANK text")
            // dax.guide/replace: REPLACE ( "DAX is so cool !", 11, 4, "fantastic" ) is "DAX is so
            // fantastic !"; Microsoft Learn: a BLANK num_chars inserts "without replacing any
            // characters".
            testCase "REPLACE" (fun () ->
                is (p ()) "REPLACE ( \"DAX is so cool !\", 11, 4, \"fantastic\" )" "DAX is so fantastic !"
                is (p ()) "REPLACE ( \"PA1234\", 1, 2, \"OB\" )" "OB1234"
                is (p ()) "REPLACE ( \"abc\", 2, BLANK (), \"X\" )" "aXbc"
                is (p ()) "REPLACE ( \"abc\", 3, 1, \"\" )" "ab")
            testCase "REPLACE refuses past the end" (fun () ->
                refused "REPLACE ( \"abc\", 2, 5, \"X\" )" "past the end"
                refused "REPLACE ( \"abc\", 4, 0, \"X\" )" "past the end"
                refused "REPLACE ( \"abc\", 0, 1, \"X\" )" "before the first character")
        ]

        testList "SEARCH FIND" [
            // Microsoft Learn, SEARCH: "Searching for "N" finds the first occurrence of 'N' or 'n'";
            // "Twin Cycles" has "cycle" at 6, "Helmets and Cycles" at 13.
            testCase "SEARCH ignores case" (fun () ->
                is (p ()) "SEARCH ( \"cycle\", \"Twin Cycles\", 1, BLANK () )" 6
                is (p ()) "SEARCH ( \"cycle\", \"Helmets and Cycles\", 1, BLANK () )" 13
                is (p ()) "SEARCH ( \"cycle\", \"Mass Market Bikes\", 1, BLANK () )" null
                is (p ()) "SEARCH ( \"RED\", \"Big red box\" )" 5
                isAt 1 "SEARCH ( \"BOX\", T[Txt] )" 9)
            // Microsoft Learn, FIND: "Volume Bike Sellers" 8, "Mass Market Bikes" 13, "Jumbo Bikes"
            // 7; "if you used "bike" in the find_text argument, the query returns no results".
            testCase "FIND is case-sensitive" (fun () ->
                is (p ()) "FIND ( \"Bike\", \"Volume Bike Sellers\", 1, BLANK () )" 8
                is (p ()) "FIND ( \"Bike\", \"Mass Market Bikes\", 1, BLANK () )" 13
                is (p ()) "FIND ( \"Bike\", \"Jumbo Bikes\", 1, BLANK () )" 7
                is (p ()) "FIND ( \"bike\", \"Jumbo Bikes\", 1, BLANK () )" null
                is (p ()) "FIND ( \"red\", \"Red red\" )" 5)
            // "A question mark matches any single character; an asterisk matches any sequence of
            // characters ... type a tilde (~) before the character" (Microsoft Learn); dax.guide:
            // "player*blue" is at 19 in "Contoso 512MB MP3 Player E51 Blue", 16 in "Contoso 2G MP3
            // Player E200 Blue", and not in "Contoso 2G MP3 Player E200 Red"; "In order to find ~,
            // you must use ~~"; "FIND does not support wildcards".
            testCase "wildcards" (fun () ->
                is (p ()) "SEARCH ( \"player*blue\", \"Contoso 512MB MP3 Player E51 Blue\", 1, BLANK () )" 19
                is (p ()) "SEARCH ( \"player*blue\", \"Contoso 2G MP3 Player E200 Blue\", 1, BLANK () )" 16
                is (p ()) "SEARCH ( \"player*blue\", \"Contoso 2G MP3 Player E200 Red\", 1, BLANK () )" null
                is (p ()) "SEARCH ( \"b?x\", \"Big red box\" )" 9
                is (p ()) "SEARCH ( \"~?\", \"Why? a?\" )" 4
                is (p ()) "SEARCH ( \"~*\", \"a*b\" )" 2
                is (p ()) "SEARCH ( \"~~\", \"a~b\" )" 2
                is (p ()) "SEARCH ( \"?\", \"abc\", 2 )" 2
                is (p ()) "FIND ( \"*\", \"a*b\" )" 2
                is (p ()) "FIND ( \"a*d\", \"abcd\", 1, 0 )" 0)
            // "(optional) The character position in within_text at which you want to start
            // searching. If omitted, 1"; NotFoundValue: "If you don't specify it, the function
            // returns an error".
            testCase "start position and NotFoundValue" (fun () ->
                is (p ()) "SEARCH ( \"b\", \"Big red box\", 2 )" 9
                is (p ()) "FIND ( \"a\", \"banana\", 3 )" 4
                is (p ()) "FIND ( \"a\", \"banana\", , -1 )" 2
                is (p ()) "SEARCH ( \"z\", \"abc\", 1, -1 )" -1
                is (p ()) "SEARCH ( \"z\", \"abc\", 1, \"none\" )" "none"
                is (p ()) "FIND ( \"a\", \"abc\", 4, 0 )" 0
                fails "SEARCH ( \"z\", \"abc\" )"
                fails "FIND ( \"Bike\", \"bike shop\" )"
                is (p ()) "IFERROR ( FIND ( \"z\", \"abc\" ), -1 )" -1)
            // "Nulls in within_text are interpreted as an empty string in this context" (Microsoft
            // Learn, SEARCH); "Use double quotes (empty text) to match the first character in
            // within_text" (Microsoft Learn, FIND).
            testCase "BLANK and empty texts" (fun () ->
                is (p ()) "SEARCH ( \"a\", BLANK (), 1, -1 )" -1
                isAt 2 "SEARCH ( \"a\", T[Txt], 1, -1 )" -1
                is (p ()) "FIND ( \"\", \"abc\" )" 1
                refused "FIND ( \"\", \"abc\", 2 )" "empty text"
                refused "SEARCH ( \"\", \"abc\" )" "empty text")
            testCase "what the pages leave open is refused" (fun () ->
                refused "SEARCH ( \"a\", \"abc\", 0 )" "before the first character"
                refused "FIND ( \"a\", \"abc\", 5, 0 )" "after the end"
                refused "SEARCH ( \"a~b\", \"a~b\" )" "tilde"
                // "kanatype-insensitive, width-insensitive" and "accent sensitive": not folded here.
                refused "SEARCH ( \"か\", \"カ\" )" "kana"
                refused "SEARCH ( \"é\", \"École\" )" "outside ASCII"
                refused "SEARCH ( \"a\", \"á\" )" "combining mark"
                refused "SEARCH ( \"k\", \"K\" )" "outside ASCII"
                // A linguistic comparison may find "ss" in "Straße", "fi" in the ligature.
                refused "SEARCH ( \"ss\", \"Straße\", 1, 0 )" "linguistic"
                refused "CONTAINSSTRING ( \"ﬁn\", \"fi\" )" "linguistic"
                refused "SEARCH ( \"2\", \"m²\", 1, 0 )" "linguistic")
            // "Searching for "á" finds the first occurrence of 'á' but no occurrences of 'a'".
            testCase "an accented letter in the text is not its base letter" (fun () ->
                is (p ()) "SEARCH ( \"e\", \"café e\" )" 6
                is (p ()) "SEARCH ( \"caf\", \"Café\" )" 1)
        ]

        testList "EXACT CONTAINSSTRING" [
            // dax.guide/exact: A = "sqlbi", B = "SQLBI": A = B true, EXACT ( A, B ) false; "An empty
            // string and BLANK are considered the same value".
            testCase "EXACT" (fun () ->
                is (p ()) "EXACT ( \"sqlbi\", \"SQLBI\" )" false
                is (p ()) "EXACT ( \"sqlbi\", \"sqlbi\" )" true
                is (p ()) "EXACT ( BLANK (), \"\" )" true
                is (p ()) "EXACT ( BLANK (), \"a\" )" false
                refused "EXACT ( \"a \", \"a\" )" "trailing spaces"
                refused "EXACT ( 1, \"1\" )" "not text")
            // Microsoft Learn, CONTAINSSTRING: "bc" TRUE, "BC" TRUE, "a*d" TRUE, "ef" FALSE.
            // dax.guide: CONTAINSSTRING ( "Why?", "~?" ) returns TRUE; "If WithinText is BLANK or an
            // empty string, CONTAINSSTRING always returns FALSE, even when FindText is empty or "*".
            // If WithinText is not empty and FindText is BLANK or an empty string, CONTAINSSTRING
            // returns TRUE"; CONTAINSSTRING ( 12345, 34 ) returns TRUE.
            testCase "CONTAINSSTRING" (fun () ->
                is (p ()) "CONTAINSSTRING ( \"abcd\", \"bc\" )" true
                is (p ()) "CONTAINSSTRING ( \"abcd\", \"BC\" )" true
                is (p ()) "CONTAINSSTRING ( \"abcd\", \"a*d\" )" true
                is (p ()) "CONTAINSSTRING ( \"abcd\", \"ef\" )" false
                is (p ()) "CONTAINSSTRING ( \"Why?\", \"~?\" )" true
                is (p ()) "CONTAINSSTRING ( \"Grey\", \"r?y\" )" true
                is (p ()) "CONTAINSSTRING ( \"Red\", \"r?y\" )" false
                is (p ()) "CONTAINSSTRING ( BLANK (), \"\" )" false
                is (p ()) "CONTAINSSTRING ( \"\", \"*\" )" false
                is (p ()) "CONTAINSSTRING ( \"abc\", BLANK () )" true
                is (p ()) "CONTAINSSTRING ( \"abc\", \"\" )" true
                is (p ()) "CONTAINSSTRING ( 12345, 34 )" true)
            // Microsoft Learn, CONTAINSSTRINGEXACT: "bc" TRUE, "BC" FALSE, "a*d" FALSE, "ef" FALSE.
            testCase "CONTAINSSTRINGEXACT" (fun () ->
                is (p ()) "CONTAINSSTRINGEXACT ( \"abcd\", \"bc\" )" true
                is (p ()) "CONTAINSSTRINGEXACT ( \"abcd\", \"BC\" )" false
                is (p ()) "CONTAINSSTRINGEXACT ( \"abcd\", \"a*d\" )" false
                is (p ()) "CONTAINSSTRINGEXACT ( \"abcd\", \"ef\" )" false
                refused "CONTAINSSTRINGEXACT ( \"abcd\", \"\" )" "empty")
            // The pages describe SEARCH for the default collation; a case-sensitive model is not it.
            testCase "SEARCH is refused in a case-sensitive model" (fun () ->
                let db, store = build model
                db.Model.Collation <- "Latin1_General_CS_AS"

                let cs =
                    match DataStore.Create(DataStore.collationOf db, store.Tables, DataStore.relationshipsOf db) with
                    | Ok s -> s
                    | Error e -> failtest e

                match Engine.prepare db cs with
                | Ok prepared ->
                    notSupported prepared "EVALUATE ROW ( \"r\", SEARCH ( \"a\", \"abc\" ) )" "collation"
                    notSupported prepared "EVALUATE ROW ( \"r\", CONTAINSSTRING ( \"abc\", \"a\" ) )" "collation"
                | Error e -> failtestf "prepare failed: %s" (TessError.message e))
        ]

        testList "COMBINEVALUES CONCATENATE UNICHAR UNICODE" [
            // dax.guide/combinevalues: "COMBINEVALUES ( ",", BLANK (), BLANK () ) returns ",""; the
            // delimiter "must be a constant text that is not empty".
            testCase "COMBINEVALUES" (fun () ->
                is (p ()) "COMBINEVALUES ( \",\", BLANK (), BLANK () )" ","
                is (p ()) "COMBINEVALUES ( \"|\", \"a\", \"b\", \"c\" )" "a|b|c"
                is (p ()) "COMBINEVALUES ( \", \", \"January\", 2020 )" "January, 2020"
                refused "COMBINEVALUES ( \"a\" & \"b\", \"x\", \"y\" )" "text literal"

                Expect.equal
                    (TessError.code (scalarError (p ()) "COMBINEVALUES ( \"\", \"x\", \"y\" )"))
                    "dax-invalid"
                    "empty")
            // Microsoft Learn, CONCATENATE: "Hello " and "World"; "MTN" and 40 give "MTN40".
            testCase "CONCATENATE and &" (fun () ->
                is (p ()) "CONCATENATE ( \"Hello \", \"World\" )" "Hello World"
                is (p ()) "CONCATENATE ( \"MTN\", 40 )" "MTN40"
                is (p ()) "12 & 34" "1234"
                is (p ()) "\"a\" & 1.5" "a1.5"
                refused "\"a\" & TRUE ()" "TRUE or FALSE"
                refused "\"a\" & dt\"2020-01-02\"" "date")
            // The decimal separator is the culture's.
            testCase "a number with decimals becomes text only in en-US" (fun () ->
                notSupported german.Value "EVALUATE ROW ( \"r\", \"a\" & 1.5 )" "de-DE"
                notSupported noCulture.Value "EVALUATE ROW ( \"r\", \"a\" & 1.5 )" "names no culture"
                is german.Value "\"a\" & 15" "a15")
            // Microsoft Learn, UNICHAR: 65 is "A", 32 the space, 9733 "★"; zero, a partial surrogate
            // and a number out of range are errors. dax.guide: UNICODE ( "━" ) = 9473.
            testCase "UNICHAR and UNICODE" (fun () ->
                is (p ()) "UNICHAR ( 65 )" "A"
                is (p ()) "UNICHAR ( 32 )" " "
                is (p ()) "UNICHAR ( 9733 )" "★"
                is (p ()) "UNICHAR ( 9473 )" "━"
                is (p ()) "UNICODE ( \"━\" )" 9473
                is (p ()) "UNICODE ( \"ABC\" )" 65
                is (p ()) "UNICHAR ( 128512 )" "😀"
                fails "UNICHAR ( 0 )"
                fails "UNICHAR ( 55296 )"
                fails "UNICHAR ( 1114112 )"
                fails "UNICHAR ( -1 )"
                refused "UNICHAR ( 7 )" "control character"
                refused "UNICHAR ( 65.5 )" "not a whole number"
                refused "UNICODE ( \"\" )" "empty")
            testCase "TOCSV and TOJSON are refused by name" (fun () ->
                notSupported (p ()) "EVALUATE ROW ( \"r\", TOCSV ( T ) )" "TOCSV"
                notSupported (p ()) "EVALUATE ROW ( \"r\", TOJSON ( T ) )" "TOJSON")
        ]

        testList "properties" [
            // LEFT, MID and RIGHT are pieces of the text: together they are the text.
            testPropertyWithConfig propertyConfig "LEFT & MID & RIGHT recompose the text"
            <| fun (AsciiText s) (a: PositiveInt) (b: PositiveInt) ->
                let n = s.Length

                if n > 0 then
                    let i = a.Get % (n + 1)
                    let j = i + b.Get % (n - i + 1)

                    let dax =
                        $"LEFT ( {lit s}, {i} ) & MID ( {lit s}, {i + 1}, {j - i} ) & RIGHT ( {lit s}, {n - j} )"

                    // MID from the position after the last character is refused (past the end).
                    if i < n then
                        Expect.equal (text dax) s dax

            testPropertyWithConfig propertyConfig "LEN is the length, and LEFT and RIGHT have the length asked for"
            <| fun (AsciiText s) (a: NonNegativeInt) ->
                Expect.equal (scalar (p ()) $"LEN ( {lit s} )") (QueryValue.Int64(int64 s.Length)) s
                let k = a.Get % (s.Length + 2)
                Expect.equal (text $"LEFT ( {lit s}, {k} )") (s.Substring(0, min k s.Length)) s
                Expect.equal (text $"RIGHT ( {lit s}, {k} )") (s.Substring(s.Length - min k s.Length)) s

            // Replacing a text with itself changes nothing.
            testPropertyWithConfig propertyConfig "SUBSTITUTE of a text with itself is the text"
            <| fun (AsciiText s) (AsciiText piece) ->
                if piece <> "" then
                    match Engine.evaluateScalar (p ()) $"SUBSTITUTE ( {lit s}, {lit piece}, {lit piece} )" with
                    | Ok v -> Expect.equal v (QueryValue.String s) s
                    // Occurrences that overlap are refused.
                    | Error(DaxNotSupported _) -> ()
                    | Error e -> failtestf "%A" e

            // What FIND finds is there, and nothing earlier is.
            testPropertyWithConfig propertyConfig "FIND returns the first place the text is, as MID confirms"
            <| fun (AsciiText s) (AsciiText piece) ->
                if piece <> "" then
                    let expected = s.IndexOf(piece, StringComparison.Ordinal)

                    Expect.equal
                        (scalar (p ()) $"FIND ( {lit piece}, {lit s}, 1, 0 )")
                        (QueryValue.Int64(int64 expected + 1L))
                        s

                    if expected >= 0 then
                        Expect.equal (text $"MID ( {lit s}, {expected + 1}, {piece.Length} )") piece s

            // SEARCH without wildcards is FIND on the upper-cased texts; CONTAINSSTRING is SEARCH.
            testPropertyWithConfig propertyConfig "SEARCH is FIND without regard to case"
            <| fun (AsciiText s) (AsciiText piece) ->
                if piece <> "" then
                    let expected =
                        s.ToUpperInvariant().IndexOf(piece.ToUpperInvariant(), StringComparison.Ordinal)

                    Expect.equal
                        (scalar (p ()) $"SEARCH ( {lit piece}, {lit s}, 1, 0 )")
                        (QueryValue.Int64(int64 expected + 1L))
                        s

                    if s <> "" then
                        Expect.equal
                            (scalar (p ()) $"CONTAINSSTRING ( {lit s}, {lit piece} )")
                            (QueryValue.Boolean(expected >= 0))
                            s

            testPropertyWithConfig
                propertyConfig
                "TRIM leaves no space at the ends and no two together, and is idempotent"
            <| fun (AsciiText s) ->
                let t = text $"TRIM ( {lit s} )"
                Expect.isFalse (t.StartsWith ' ' || t.EndsWith ' ' || t.Contains "  ") s
                Expect.equal (t.Replace(" ", "")) (s.Replace(" ", "")) s
                Expect.equal (text $"TRIM ( {lit t} )") t s

            testPropertyWithConfig propertyConfig "UPPER and LOWER agree with each other"
            <| fun (AsciiText s) ->
                let u = text $"UPPER ( {lit s} )"
                Expect.equal u (s.ToUpperInvariant()) s
                Expect.equal (text $"UPPER ( LOWER ( {lit s} ) )") u s

                Expect.equal
                    (scalar (p ()) $"EXACT ( UPPER ( {lit s} ), LOWER ( {lit s} ) )")
                    (QueryValue.Boolean(u = s.ToLowerInvariant()))
                    s

            testProperty "UNICODE ( UNICHAR ( n ) ) is n in the Basic Multilingual Plane"
            <| fun (n: PositiveInt) ->
                let code = 32 + n.Get % 55000

                if code < 0xD800 then
                    Expect.equal (scalar (p ()) $"UNICODE ( UNICHAR ( {code} ) )") (QueryValue.Int64(int64 code)) ""
        ]
    ]

/// Runs with the thread's culture set to one that writes numbers and dates differently: nothing
/// in the engine's text may follow it.
let private inCulture (name: string) (f: unit -> unit) =
    let before = Thread.CurrentThread.CurrentCulture

    try
        Thread.CurrentThread.CurrentCulture <- CultureInfo.GetCultureInfo name
        f ()
    finally
        Thread.CurrentThread.CurrentCulture <- before

[<Tests>]
let formatTests =
    testList "Engine FORMAT" [
        testList "predefined number formats" [
            // Microsoft Learn and dax.guide/format, FORMAT ( 12345.67, ... ): General Number
            // 12345.67, Currency $12,345.67, Fixed 12345.67, Standard 12,345.67, Scientific 1.23E+04;
            // FORMAT ( 0.742, "Percent" ) is 74.20%.
            testCase "the documented examples" (fun () ->
                is (p ()) "FORMAT ( 12345.67, \"General Number\" )" "12345.67"
                is (p ()) "FORMAT ( 12345.67, \"Currency\" )" "$12,345.67"
                is (p ()) "FORMAT ( 12345.67, \"Fixed\" )" "12345.67"
                is (p ()) "FORMAT ( 12345.67, \"Standard\" )" "12,345.67"
                is (p ()) "FORMAT ( 12345.67, \"Scientific\" )" "1.23E+04"
                is (p ()) "FORMAT ( 0.742, \"Percent\" )" "74.20%")
            // "Fixed": "Displays at least one digit to the left and two digits to the right of the
            // decimal separator".
            testCase "digits are padded" (fun () ->
                is (p ()) "FORMAT ( 5, \"Fixed\" )" "5.00"
                is (p ()) "FORMAT ( 0.5, \"Fixed\" )" "0.50"
                is (p ()) "FORMAT ( 5, \"Standard\" )" "5.00"
                is (p ()) "FORMAT ( 1234567, \"Standard\" )" "1,234,567.00"
                is (p ()) "FORMAT ( 5, \"Currency\" )" "$5.00"
                is (p ()) "FORMAT ( 0.00012345, \"Scientific\" )" "1.23E-04"
                is (p ()) "FORMAT ( 0, \"Scientific\" )" "0.00E+00"
                is (p ()) "FORMAT ( 9996, \"Scientific\" )" "1.00E+04"
                is (p ()) "FORMAT ( 42, \"General Number\" )" "42"
                is (p ()) "FORMAT ( -12345.67, \"General Number\" )" "-12345.67"
                isAt 1 "FORMAT ( T[C], \"General Number\" )" "12.5"
                isAt 4 "FORMAT ( T[C], \"General Number\" )" "1234567.8912")
            // "Displays No if number is 0; otherwise, displays Yes" and so on (Microsoft Learn);
            // FORMAT ( TRUE, "True/False" ) True, FORMAT ( FALSE, "On/Off" ) Off, FORMAT ( TRUE,
            // "Yes/No" ) Yes (dax.guide/format).
            testCase "Yes/No, True/False, On/Off" (fun () ->
                is (p ()) "FORMAT ( TRUE, \"True/False\" )" "True"
                is (p ()) "FORMAT ( FALSE, \"On/Off\" )" "Off"
                is (p ()) "FORMAT ( TRUE, \"Yes/No\" )" "Yes"
                is (p ()) "FORMAT ( 0, \"Yes/No\" )" "No"
                is (p ()) "FORMAT ( -3, \"On/Off\" )" "On"
                is (p ()) "FORMAT ( 0.0, \"True/False\" )" "False")
            // "If value is BLANK, the function returns an empty string"; "If format_string is BLANK,
            // the value is formatted with a "General Number" or "General Date" format".
            testCase "BLANK value and BLANK format" (fun () ->
                is (p ()) "FORMAT ( BLANK (), \"Currency\" )" ""
                is (p ()) "FORMAT ( BLANK (), \"#,0.00\" )" ""
                is (p ()) "ISBLANK ( FORMAT ( BLANK (), \"0\" ) )" false
                isAt 2 "FORMAT ( T[N], \"0.00\" )" ""
                is (p ()) "FORMAT ( 12345.67, BLANK () )" "12345.67"
                is (p ()) "FORMAT ( 42, \"0\" ) & \"!\"" "42!")
            testCase "what the sources leave open is refused" (fun () ->
                // "1,234,567.00 %" (Microsoft Learn's example) against "1234567.00%" (dax.guide).
                refused "FORMAT ( 12345.67, \"Percent\" )" "thousand separator"
                refused "FORMAT ( -5, \"Currency\" )" "negative"
                refused "FORMAT ( 5, \"currency\" )" "predefined format is written"
                refused "FORMAT ( 1E20, \"General Number\" )" "very large"
                refused "FORMAT ( 0.1 + 0.2, \"General Number\" )" "15 significant digits"
                refused "FORMAT ( 5, \"Short Date\" )" "date format"
                refused "FORMAT ( \"abc\", \"0\" )" "text value"
                refused "FORMAT ( TRUE, \"0\" )" "TRUE or FALSE"
                refused "FORMAT ( 5, 0 )" "not text")
        ]

        testList "custom number formats" [
            // dax.guide/format: FORMAT ( 1234.567, "$#,0.00" ) $1,234.57; """US$"" #,0.00" US$
            // 1,234.57; FORMAT ( 1980.126, "#,0.00" ) 1,980.13; -1980.1 -1,980.10; 0 0.00.
            testCase "digits, thousands and literals" (fun () ->
                is (p ()) "FORMAT ( 1234.567, \"$#,0.00\" )" "$1,234.57"
                is (p ()) "FORMAT ( 1234.567, \"\"\"US$\"\" #,0.00\" )" "US$ 1,234.57"
                is (p ()) "FORMAT ( 1980.126, \"#,0.00\" )" "1,980.13"
                is (p ()) "FORMAT ( -1980.1, \"#,0.00\" )" "-1,980.10"
                is (p ()) "FORMAT ( 0, \"#,0.00\" )" "0.00"
                isAt 1 "FORMAT ( T[N], \"#,0.00\" )" "1,234.57"
                isAt 3 "FORMAT ( T[N], \"#,0.00\" )" "-1,980.10")
            // Microsoft Learn: 0 "Display a digit or a zero ... display leading or trailing zeros
            // ... round the number to as many decimal places as there are zeros ... display the
            // extra digits without modification"; # "Display a digit or nothing"; "If the format
            // expression contains only number signs to the left of this symbol, numbers smaller
            // than 1 begin with a decimal separator".
            testCase "0 and #" (fun () ->
                is (p ()) "FORMAT ( 5, \"000\" )" "005"
                is (p ()) "FORMAT ( 12345, \"000\" )" "12345"
                is (p ()) "FORMAT ( 5, \"0.00\" )" "5.00"
                is (p ()) "FORMAT ( 3.14159, \"0.00\" )" "3.14"
                is (p ()) "FORMAT ( 0.5, \"0.00\" )" "0.50"
                is (p ()) "FORMAT ( 0.5, \"#.##\" )" ".5"
                is (p ()) "FORMAT ( 0.5, \"#.00\" )" ".50"
                is (p ()) "FORMAT ( 1.5, \"0.0#\" )" "1.5"
                is (p ()) "FORMAT ( 1.257, \"0.0#\" )" "1.26"
                is (p ()) "FORMAT ( 1234567, \"#,##0\" )" "1,234,567"
                is (p ()) "FORMAT ( 1234.567, \"#,##0\" )" "1,235"
                is (p ()) "FORMAT ( 0, \"#,##0\" )" "0"
                is (p ()) "FORMAT ( 0, \"#\" )" ""
                is (p ()) "FORMAT ( 42, \"0\" )" "42")
            // dax.guide/format: "#,0.00;(#,0.00)" gives 1,980.13 / (1,980.10) / 0.00; and
            // "#,#.##;(#,#.##);-" gives 1,980.12 / (1,980.12) / -. "If you include semicolons with
            // nothing between them, the missing section is defined using the format of the positive
            // value" (Microsoft Learn).
            testCase "sections" (fun () ->
                is (p ()) "FORMAT ( 1980.126, \"#,0.00;(#,0.00)\" )" "1,980.13"
                is (p ()) "FORMAT ( -1980.1, \"#,0.00;(#,0.00)\" )" "(1,980.10)"
                is (p ()) "FORMAT ( 0, \"#,0.00;(#,0.00)\" )" "0.00"
                is (p ()) "FORMAT ( 1980.12, \"#,#.##;(#,#.##);-\" )" "1,980.12"
                is (p ()) "FORMAT ( -1980.12, \"#,#.##;(#,#.##);-\" )" "(1,980.12)"
                is (p ()) "FORMAT ( 0, \"#,#.##;(#,#.##);-\" )" "-"
                is (p ()) "FORMAT ( 0, \"$#,##0;;\\Z\\e\\r\\o\" )" "Zero"
                is (p ()) "FORMAT ( -5, \"0.0;;\\Z\" )" "-5.0"
                is (p ()) "FORMAT ( -5, \"$#,##0;($#,##0)\" )" "($5)"
                is (p ()) "FORMAT ( 1, \"0;\"\"a;b\"\"\" )" "1"
                is (p ()) "FORMAT ( -1, \"0;\"\"a;b\"\"\" )" "a;b")
            // dax.guide/format, BigNumber = 103456789102: "$#,0" $103,456,789,102; "$#,0,.0#K"
            // $103,456,789.1K; "$#,0,,.0#M" $103,456.79M; "$#,0,,,.0#B" $103.46B; "$#,0,,,,.0#T"
            // $0.1T. And 10345 $10.35K (a half rounds up), 103456 $103.46K, 1034567 $1.03M,
            // 134897891 $134.9M, 103499789102 $103.5B, 1034567891023 $1.03T, 103 with "$#,0" $103.
            // Microsoft Learn: "##0,," shows 100 million as 100.
            testCase "thousand separators that scale" (fun () ->
                is (p ()) "FORMAT ( 103456789102, \"$#,0\" )" "$103,456,789,102"
                is (p ()) "FORMAT ( 103456789102, \"$#,0,.0#K\" )" "$103,456,789.1K"
                is (p ()) "FORMAT ( 103456789102, \"$#,0,,.0#M\" )" "$103,456.79M"
                is (p ()) "FORMAT ( 103456789102, \"$#,0,,,.0#B\" )" "$103.46B"
                is (p ()) "FORMAT ( 103456789102, \"$#,0,,,,.0#T\" )" "$0.1T"
                is (p ()) "FORMAT ( 103, \"$#,0\" )" "$103"
                is (p ()) "FORMAT ( 10345, \"$#,0,.0#K\" )" "$10.35K"
                is (p ()) "FORMAT ( 103456, \"$#,0,.0#K\" )" "$103.46K"
                is (p ()) "FORMAT ( 1034567, \"$#,0,,.0#M\" )" "$1.03M"
                is (p ()) "FORMAT ( 134897891, \"$#,0,,.0#M\" )" "$134.9M"
                is (p ()) "FORMAT ( 103499789102, \"$#,0,,,.0#B\" )" "$103.5B"
                is (p ()) "FORMAT ( 1034567891023, \"$#,0,,,,.0#T\" )" "$1.03T"
                is (p ()) "FORMAT ( 100000000, \"##0,,\" )" "100"
                is (p ()) "FORMAT ( 100000, \"##0,,\" )" "0")
            // "Percentage placeholder. The expression is multiplied by 100. The percent character
            // (%) is inserted in the position where it appears in the format string."
            testCase "percent, exponent, escapes" (fun () ->
                is (p ()) "FORMAT ( 0.25, \"0%\" )" "25%"
                is (p ()) "FORMAT ( 0.256, \"0.0%\" )" "25.6%"
                is (p ()) "FORMAT ( 12345.67, \"0.00E+00\" )" "1.23E+04"
                is (p ()) "FORMAT ( 12345.67, \"0.00E-00\" )" "1.23E04"
                is (p ()) "FORMAT ( 0.00012345, \"0.0e-0\" )" "1.2e-4"
                is (p ()) "FORMAT ( 12345.67, \"0.###E+000\" )" "1.235E+004"
                is (p ()) "FORMAT ( 5, \"0 \\d\\a\\y\\s\" )" "5 days"
                is (p ()) "FORMAT ( 5, \"0 \"\"days\"\"\" )" "5 days"
                is (p ()) "FORMAT ( 5, \"0\\\\\" )" "5\\"
                // dax.guide/format: FORMAT ( INT ( DurationEvent ), "0:" ) gives "1:".
                is (p ()) "FORMAT ( 1, \"0:\" )" "1:")
            testCase "exact types are formatted exactly" (fun () ->
                is (p ()) "FORMAT ( 9007199254740993, \"#,0\" )" "9,007,199,254,740,993"
                is (p ()) "FORMAT ( 2.5, \"0\" )" "3"
                is (p ()) "FORMAT ( 0.125, \"0.00\" )" "0.13"
                // A fixed decimal is exact: 2.675 is halfway, and a half rounds up.
                isAt 2 "FORMAT ( T[C], \"0.00\" )" "2.68"
                isAt 4 "FORMAT ( T[C], \"#,0.000\" )" "1,234,567.891"
                isAt 4 "FORMAT ( T[C], \"Currency\" )" "$1,234,567.89"
                refusedAt 3 "FORMAT ( T[C], \"0.00\" )" "halfway")
            testCase "what the sources leave open is refused" (fun () ->
                // The float 2.675 is just below 2.675: 2.68 read to 15 digits, 2.67 read exactly.
                refused "FORMAT ( 2.675, \"0.00\" )" "inexactly"
                refused "FORMAT ( 0.1, \"0.00000000000000000000\" )" "inexactly"
                refused "FORMAT ( -2.5, \"0\" )" "halfway"
                refused "FORMAT ( -0.001, \"0.00\" )" "rounds to zero"
                refused "FORMAT ( 0.001, \"0.00;-0.00;\\z\" )" "rounds to zero"
                refused "FORMAT ( -5, \"$0.00\" )" "minus sign"
                refused "FORMAT ( 5, \"#.##\" )" "decimal point"
                refused "FORMAT ( 5, \"0-0\" )" "more than one place"
                refused "FORMAT ( 5, \"#0#\" )" "to the right of 0"
                refused "FORMAT ( 5, \"0.#0\" )" "to the right of #"
                refused "FORMAT ( 5, \"0%%\" )" "more than one %"
                refused "FORMAT ( 5, \"00.0E+0\" )" "scientific"
                refused "FORMAT ( 5, \"0;0;0;0\" )" "three sections"
                refused "FORMAT ( 5, \"0 days\" )" "the letter d"
                refused "FORMAT ( 5, \"yyyy\" )" "the letter y"
                refused "FORMAT ( 5, \"abc\" )" "the letter a"
                refused "FORMAT ( 5, \"\"\"x\"\"\" )" "digit placeholder"
                refused "FORMAT ( 5, \"0 \"\"x\" )" "not closed"
                refused "FORMAT ( 1 / 0, \"0\" )" "not finite")
        ]

        testList "dates" [
            // dax.guide/format, DT = DATE ( 2021, 1, 2 ) + TIME ( 15, 30, 0 ): General Date 1/2/2021
            // 3:30:00 PM, Long Date Saturday, January 2, 2021, Short Date 1/2/2021, Long Time
            // 3:30:00 PM, Medium Time 03:30 PM, Short Time 15:30.
            testCase "predefined formats" (fun () ->
                let dt = "dt\"2021-01-02T15:30:00\""
                is (p ()) $"FORMAT ( {dt}, \"General Date\" )" "1/2/2021 3:30:00 PM"
                is (p ()) $"FORMAT ( {dt}, \"Long Date\" )" "Saturday, January 2, 2021"
                is (p ()) $"FORMAT ( {dt}, \"Short Date\" )" "1/2/2021"
                is (p ()) $"FORMAT ( {dt}, \"Long Time\" )" "3:30:00 PM"
                is (p ()) $"FORMAT ( {dt}, \"Medium Time\" )" "03:30 PM"
                is (p ()) $"FORMAT ( {dt}, \"Short Time\" )" "15:30"
                // Microsoft Learn: FORMAT( dt"2020-12-15T12:30:59", BLANK(), "en-US" ) is
                // 12/15/2020 12:30:59 PM; "For example, 3/12/2008 11:07:31 AM".
                is (p ()) "FORMAT ( dt\"2020-12-15T12:30:59\", BLANK (), \"en-US\" )" "12/15/2020 12:30:59 PM"
                is (p ()) "FORMAT ( dt\"2020-12-15T12:30:59\", BLANK () )" "12/15/2020 12:30:59 PM"
                is (p ()) "FORMAT ( dt\"2008-03-12T11:07:31\", \"General Date\" )" "3/12/2008 11:07:31 AM"
                is (p ()) "FORMAT ( dt\"2008-03-12T00:07:31\", \"Long Time\" )" "12:07:31 AM")
            // General Date is shown with a date and a time only; what it leaves out is said of "c".
            testCase "General Date of a date alone or a time alone is refused" (fun () ->
                refused "FORMAT ( dt\"2021-01-02\", \"General Date\" )" "no time"
                refusedAt 3 "FORMAT ( T[At], \"General Date\" )" "no time"
                refusedAt 2 "FORMAT ( T[At], BLANK () )" "no time")
            // Microsoft Learn's table for Thursday, June 25, 2020, 1:23:45 PM (en-US).
            testCase "custom characters: the documented table" (fun () ->
                let check (format: string) (expected: string) =
                    isAt 1 $"FORMAT ( T[At], {lit format} )" expected

                check "d" "25"
                check "dd" "25"
                check "ddd" "Thu"
                check "dddd" "Thursday"
                check "dddddd" "Thursday, June 25, 2020"
                check "w" "5"
                check "ww" "26"
                check "m" "6"
                check "mm" "06"
                check "mmm" "Jun"
                check "mmmm" "June"
                check "q" "2"
                check "y" "177"
                check "yy" "20"
                check "yyyy" "2020"
                check "\"Year\" yyyy" "Year 2020"
                check "yyyy \\Qq" "2020 Q2"
                check "dd/mm/yyyy" "25/06/2020"
                check "mm/dd/yyyy" "06/25/2020"
                check "h:nn:ss" "13:23:45"
                check "h:nn:ss AMPM" "1:23:45 PM"
                check "hh:nn:ss" "13:23:45"
                check "hh:nn:ss AMPM" "01:23:45 PM"
                check "mm/dd/yyyy hh:nn:ss AMPM" "06/25/2020 01:23:45 PM"
                // "When it's formatted using format string "m/d/yyyy" it will be: ... "6/25/2020"".
                check "m/d/yyyy" "6/25/2020")
            // dax.guide/format: DATE ( 2019, 3, 28 ): "yyyy-mm-dd" 2019-03-28, "m/d/yy" 3/28/19,
            // "\QQ yyyy" Q1 2019; DATE ( 2020, 1, 2 ) + TIME ( 15, 30, 0 ): "dd/mm/yyyy" 02/01/2020,
            // "hh:nn:ss" and "hh:mm:ss" 15:30:00; D = DATE ( 2020, 1, 9 ): d 9, dd 09, ddd Thu, dddd
            // Thursday, dddddd Thursday, January 9, 2020; DATE ( 2020, 1, 2 ): m 1, mm 01, mmm Jan,
            // mmmm January; "dd mmmm yyyy" of 2020-12-15 is 15 December 2020.
            testCase "custom characters: DAX Guide's examples" (fun () ->
                is (p ()) "FORMAT ( dt\"2019-03-28\", \"yyyy-mm-dd\" )" "2019-03-28"
                is (p ()) "FORMAT ( dt\"2019-03-28\", \"m/d/yy\" )" "3/28/19"
                is (p ()) "FORMAT ( dt\"2019-03-28\", \"\\QQ yyyy\" )" "Q1 2019"
                is (p ()) "FORMAT ( dt\"2020-01-02T15:30:00\", \"dd/mm/yyyy\" )" "02/01/2020"
                is (p ()) "FORMAT ( dt\"2020-01-02T15:30:00\", \"hh:nn:ss\" )" "15:30:00"
                is (p ()) "FORMAT ( dt\"2020-01-02T15:30:00\", \"hh:mm:ss\" )" "15:30:00"
                is (p ()) "FORMAT ( dt\"2020-01-09\", \"d\" )" "9"
                is (p ()) "FORMAT ( dt\"2020-01-09\", \"dd\" )" "09"
                is (p ()) "FORMAT ( dt\"2020-01-09\", \"ddd\" )" "Thu"
                is (p ()) "FORMAT ( dt\"2020-01-09\", \"dddd\" )" "Thursday"
                is (p ()) "FORMAT ( dt\"2020-01-09\", \"dddddd\" )" "Thursday, January 9, 2020"
                is (p ()) "FORMAT ( dt\"2020-01-02\", \"m\" )" "1"
                is (p ()) "FORMAT ( dt\"2020-01-02\", \"mm\" )" "01"
                is (p ()) "FORMAT ( dt\"2020-01-02\", \"mmm\" )" "Jan"
                is (p ()) "FORMAT ( dt\"2020-01-02\", \"mmmm\" )" "January"
                is (p ()) "FORMAT ( dt\"2020-12-15T12:30:59\", \"dd mmmm yyyy\", \"en-US\" )" "15 December 2020")
            // "AM/PM ... uppercase AM with any hour before noon"; "am/pm"; "A/P"; "a/p".
            testCase "the 12-hour clock" (fun () ->
                isAt 4 "FORMAT ( T[At], \"h:nn am/pm\" )" "9:05 am"
                isAt 4 "FORMAT ( T[At], \"hh:n:s A/P\" )" "09:5:7 A"
                isAt 1 "FORMAT ( T[At], \"h a/p\" )" "1 p"
                is (p ()) "FORMAT ( dt\"2020-01-02T00:00:00\", \"h AM/PM\" )" "12 AM"
                is (p ()) "FORMAT ( dt\"2020-01-02T12:00:00\", \"h AM/PM\" )" "12 PM"
                isAt 3 "FORMAT ( T[At], \"hh:nn\" )" "15:30")
            // The week of the year, 1 to 54: weeks start on Sunday, the first has January 1.
            testCase "week numbers at the ends of the year" (fun () ->
                is (p ()) "FORMAT ( dt\"2020-01-01\", \"ww\" )" "1"
                is (p ()) "FORMAT ( dt\"2020-01-04\", \"ww\" )" "1"
                is (p ()) "FORMAT ( dt\"2020-01-06\", \"ww\" )" "2"
                // A Sunday: week 1 if weeks start on Monday, week 2 if on Sunday.
                refused "FORMAT ( dt\"2020-01-05\", \"ww\" )" "Sunday"
                is (p ()) "FORMAT ( dt\"2020-12-31\", \"ww\" )" "53"
                // 2000 began on a Saturday and was a leap year: December 31 is in week 54.
                is (p ()) "FORMAT ( dt\"2000-12-30\", \"ww\" )" "53"
                refused "FORMAT ( dt\"2000-12-31\", \"ww\" )" "Sunday"
                is (p ()) "FORMAT ( dt\"2020-12-31\", \"y\" )" "366")
            testCase "what the sources leave open is refused" (fun () ->
                let dt = "dt\"2020-06-25T13:23:45\""
                refused $"FORMAT ( {dt}, \"c\" )" "the date format c"
                refused $"FORMAT ( {dt}, \"ddddd\" )" "ddddd"
                refused $"FORMAT ( {dt}, \"ttttt\" )" "ttttt"
                refused $"FORMAT ( {dt}, \"Medium Date\" )" "Medium Date"
                refused $"FORMAT ( {dt}, \"short date\" )" "predefined format is written"
                refused "FORMAT ( dt\"2020-06-25T09:05:00\", \"Short Time\" )" "leading zero"
                refused $"FORMAT ( {dt}, \"mm:ss\" )" "minutes or the month"
                refused $"FORMAT ( {dt}, \"h m\" )" "minutes or the month"
                refused $"FORMAT ( {dt}, \"yyy\" )" "3 times"
                refused $"FORMAT ( {dt}, \"mmmmm\" )" "5 times"
                refused $"FORMAT ( {dt}, \"yyyy x\" )" "the letter x"
                refused $"FORMAT ( {dt}, \"0.00\" )" "number format for a date"
                refused $"FORMAT ( {dt}, \"Currency\" )" "number format"
                refused $"FORMAT ( {dt}, \"\" )" "empty format"
                refused $"FORMAT ( {dt}, \"Am/Pm\" )" "the letter a"
                // A time alone has no date to show; DAX is not reliable before March 1, 1900.
                refusedAt 3 "FORMAT ( T[At], \"yyyy\" )" "March 1, 1900")
            // dax.guide/format: "\QQ yyyy" is Q1 2019. Power BI's auto date/time table has
            // Month = FORMAT([Date], "MMMM") (sqlbi.com, "Continuous axis for any Date table in Power
            // BI"), and Microsoft Learn ("Auto date/time in Power BI Desktop") shows its rows for
            // January 2019 as "January".
            testCase "upper-case letters the pages show" (fun () ->
                is (p ()) "FORMAT ( dt\"2019-03-28\", \"\\QQ yyyy\" )" "Q1 2019"
                is (p ()) "FORMAT ( dt\"2019-03-28\", \"Q\" )" "1"
                is (p ()) "FORMAT ( dt\"2019-01-01\", \"MMMM\" )" "January")
            // "Unlike in previous versions of Visual Basic, these format characters are
            // case-sensitive" (VB 2005, "User-Defined Date/Time Formats (Format Function)"). So each
            // value is Microsoft Learn's for the lower-case format, for Thursday, June 25, 2020, 1:23:45 PM.
            testCase "upper-case letters read as Visual Basic reads them" (fun () ->
                let dt = "dt\"2020-06-25T13:23:45\""

                let check (format: string) (expected: string) =
                    is (p ()) $"FORMAT ( {dt}, \"{format}\" )" expected

                check "DD" "25" // "dd" 25
                check "DDD" "Thu" // "ddd" Thu
                check "DDDD" "Thursday" // "dddd" Thursday
                check "DDDDDD" "Thursday, June 25, 2020" // "dddddd"
                check "W" "5" // "w" 5
                check "MM" "06" // "mm" 06
                check "MMM" "Jun" // "mmm" Jun
                check "MMMM" "June" // "mmmm" June
                check "YY" "20" // "yy" 20
                check "YYYY" "2020" // "yyyy" 2020
                check "yyyy \\QQ" "2020 Q2" // "yyyy \Qq" 2020 Q2
                check "DD/MM/YYYY" "25/06/2020" // "dd/mm/yyyy" 25/06/2020
                check "MM/DD/YYYY" "06/25/2020" // "mm/dd/yyyy" 06/25/2020
                check "H:NN:SS" "13:23:45" // "h:nn:ss" 13:23:45
                check "HH:NN:SS" "13:23:45" // "hh:nn:ss" 13:23:45
                check "HH:mm:ss" "13:23:45" // dax.guide: "hh:mm:ss" is 15:30:00, mm after hh is minutes
                // Each run in one case; the runs may differ.
                check "MMM d, YYYY" "Jun 25, 2020"
                // The sample model's Date table (spaceparts-analytics): "MMM DD", "DDD", "DDDD".
                check "MMM DD" "Jun 25")
            testCase "upper-case letters the 2010 DAX pages read otherwise are refused" (fun () ->
                let dt = "dt\"2020-06-25T13:23:45\""
                // "The M character displays the month ... The m character displays the minutes".
                refused $"FORMAT ( {dt}, \"hh:MM\" )" "upper-case M after h"
                refused $"FORMAT ( {dt}, \"HH:MM:SS\" )" "upper-case M after h"
                // "H ... using the 24-hour clock"; h with AM/PM is the 12-hour clock in Visual Basic.
                refused $"FORMAT ( {dt}, \"H:nn AM/PM\" )" "upper-case H with AM/PM"
                refused $"FORMAT ( {dt}, \"HH:nn AMPM\" )" "upper-case H with AM/PM"
                // "The Y and y characters display the year"; y alone is the day of the year.
                refused $"FORMAT ( {dt}, \"Y-mm\" )" "the letter Y alone"
                // "M", "D" and "Y" were predefined formats.
                refused $"FORMAT ( {dt}, \"D\" )" "predefined format in the 2010 DAX pages"
                refused $"FORMAT ( {dt}, \"M\" )" "predefined format in the 2010 DAX pages"
                refused $"FORMAT ( {dt}, \"Y\" )" "predefined format in the 2010 DAX pages"
                // "Mmm" is the month then minutes there; "Hh" the 24-hour then the 12-hour clock.
                refused $"FORMAT ( {dt}, \"Mmm\" )" "upper and lower case"
                refused $"FORMAT ( {dt}, \"Hh:nn\" )" "upper and lower case"
                refused $"FORMAT ( {dt}, \"yyyy-mm-dD\" )" "upper and lower case"
                // Settled whatever the case: c, ttttt, MMMMM, and m next to seconds.
                refused $"FORMAT ( {dt}, \"C\" )" "the date format c"
                refused $"FORMAT ( {dt}, \"TTTTT\" )" "ttttt"
                refused $"FORMAT ( {dt}, \"MMMMM\" )" "5 times"
                refused $"FORMAT ( {dt}, \"MM:SS\" )" "minutes or the month"
                // Lower case is unchanged.
                is (p ()) $"FORMAT ( {dt}, \"d\" )" "25"
                is (p ()) $"FORMAT ( {dt}, \"y\" )" "177"
                is (p ()) $"FORMAT ( {dt}, \"h:nn AM/PM\" )" "1:23 PM")
        ]

        testList "culture" [
            // "Predefined format strings use the model culture property"; the locale argument names
            // another. Only en-US is written.
            testCase "every culture but en-US is refused" (fun () ->
                refused "FORMAT ( 1234.567, \"#,0.00\", \"de-DE\" )" "de-DE"
                refused "FORMAT ( dt\"2020-12-15T12:30:59\", BLANK (), \"en-GB\" )" "en-GB"
                refused "FORMAT ( dt\"2020-12-15T12:30:59\", \"dd mmmm yyyy\", \"pt-BR\" )" "pt-BR"
                refused "FORMAT ( 5, \"0\", \"\" )" "empty locale"
                is (p ()) "FORMAT ( 1234.567, \"#,0.00\", \"en-us\" )" "1,234.57"
                is (p ()) "FORMAT ( 1234.567, \"#,0.00\", BLANK () )" "1,234.57"
                notSupported german.Value "EVALUATE ROW ( \"r\", FORMAT ( 5, \"0\" ) )" "de-DE"
                notSupported noCulture.Value "EVALUATE ROW ( \"r\", FORMAT ( 5, \"0\" ) )" "names no culture"
                // The locale argument overrides the model's culture, and BLANK is always empty.
                is german.Value "FORMAT ( 1234.567, \"#,0.00\", \"en-US\" )" "1,234.57"
                is german.Value "FORMAT ( BLANK (), \"0\" )" "")
            testCase "the machine's culture changes nothing" (fun () ->
                for name in [ "de-DE"; "tr-TR"; "ar-SA" ] do
                    inCulture name (fun () ->
                        is (p ()) "FORMAT ( -1234.567, \"#,0.00\" )" "-1,234.57"
                        is (p ()) "FORMAT ( 12345.67, \"Currency\" )" "$12,345.67"
                        is (p ()) "FORMAT ( 0.00012345, \"Scientific\" )" "1.23E-04"
                        is (p ()) "FORMAT ( dt\"2021-01-02T15:30:00\", \"General Date\" )" "1/2/2021 3:30:00 PM"
                        is (p ()) "FORMAT ( dt\"2021-01-02T15:30:00\", \"Long Date\" )" "Saturday, January 2, 2021"
                        is (p ()) "FIXED ( 1234.5, 1 )" "1,234.5"
                        is (p ()) "UPPER ( \"title\" )" "TITLE"
                        is (p ()) "LOWER ( \"TITLE\" )" "title"
                        is (p ()) "SEARCH ( \"I\", \"title\" )" 2
                        is (p ()) "\"a\" & 1.5" "a1.5"))
        ]

        testList "the coverage check sees a literal format" [
            testCase "refusals told from the arguments" (fun () ->
                let db, _ =
                    build {
                        model with
                            Measures = [
                                "T", "Locale", "FORMAT ( 1, \"0\", \"de-DE\" )"
                                "T", "Short", "FORMAT ( 1, \"ddddd\" )"
                                "T", "Csv", "TOCSV ( T )"
                                "T", "Fine", "FORMAT ( 1, \"#,0.00\" ) & FORMAT ( 1, \"yyyy-mm-dd\" )"
                            ]
                    }

                let report = Engine.coverage db |> List.collect snd |> String.concat "\n"
                Expect.stringContains report "de-DE" "locale"
                Expect.stringContains report "ddddd" "format"
                Expect.stringContains report "TOCSV" "tocsv"
                Expect.isFalse (report.Contains "#,0.00" || report.Contains "digit placeholder") report)
        ]

        testList "FIXED" [
            // dax.guide/fixed, 384518.16: FIXED ( x, 2, FALSE ) 384,518.16; ( x, 2, TRUE )
            // 384518.16; ( x, 0, TRUE ) 384518; ( x, -2, FALSE ) 384,500. And 6741548.73 with 0
            // decimals is 6741549, 7192581.95 to -2 is 7,192,600. "If you omit decimals, it's
            // assumed to be 2"; "if 0 or omitted, display commas" (Microsoft Learn).
            testCase "the documented examples" (fun () ->
                is (p ()) "FIXED ( 384518.16, 2, FALSE )" "384,518.16"
                is (p ()) "FIXED ( 384518.16, 2, TRUE )" "384518.16"
                is (p ()) "FIXED ( 384518.16, 0, TRUE )" "384518"
                is (p ()) "FIXED ( 384518.16, -2, FALSE )" "384,500"
                is (p ()) "FIXED ( 6741548.73, 0, TRUE )" "6741549"
                is (p ()) "FIXED ( 7192581.95, -2, FALSE )" "7,192,600"
                is (p ()) "FIXED ( 4392768.29 )" "4,392,768.29"
                is (p ()) "FIXED ( 5, 2, 1 )" "5.00"
                is (p ()) "FIXED ( 1234, 1, 0 )" "1,234.0"
                is (p ()) "FIXED ( 0.5 )" "0.50")
            testCase "what the pages leave open is refused" (fun () ->
                refused "FIXED ( -5 )" "negative"
                refused "FIXED ( 2.5, 0 )" "halfway"
                refused "FIXED ( 2.675, 2 )" "inexactly"
                refused "FIXED ( BLANK () )" "BLANK"
                refused "FIXED ( 5, 1.5 )" "decimals"
                notSupported german.Value "EVALUATE ROW ( \"r\", FIXED ( 5 ) )" "de-DE")
        ]

        testList "properties" [
            // A whole number through "0" is its digits: reading them back gives the number.
            testProperty "FORMAT of a whole number with \"0\" reads back as the number"
            <| fun (n: int64) ->
                let n = max n (Int64.MinValue + 1L)
                let literal = n.ToString CultureInfo.InvariantCulture
                let s = text $"FORMAT ( {literal}, \"0\" )"
                Expect.equal s literal "the digits"
                Expect.equal (text $"FORMAT ( {literal}, \"General Number\" )") s "General Number"

            // Thousand separators add nothing but commas.
            testProperty "\"#,0\" is \"0\" with commas every three digits"
            <| fun (n: NonNegativeInt) ->
                let grouped = text $"FORMAT ( {n.Get}, \"#,0\" )"
                Expect.equal (grouped.Replace(",", "")) (string n.Get) grouped

                for part in grouped.Split(',') |> Array.skip 1 do
                    Expect.equal part.Length 3 grouped

            // Decimals a whole number doesn't have are zeros.
            testProperty "\"0.00\" of a whole number is the number and two zeros"
            <| fun (n: int64) ->
                let n = max n (Int64.MinValue + 1L)
                let literal = n.ToString CultureInfo.InvariantCulture
                Expect.equal (text $"FORMAT ( {literal}, \"0.00\" )") (literal + ".00") literal

            // A date written as yyyy-mm-dd hh:nn:ss is the date.
            testProperty "FORMAT of a date with yyyy-mm-dd hh:nn:ss reads back as the date"
            <| fun (days: NonNegativeInt) (seconds: NonNegativeInt) ->
                let d =
                    DateTime(1900, 3, 1).AddDays(float (days.Get % 70000)).AddSeconds(float (seconds.Get % 86400))

                let literal = d.ToString("yyyy-MM-ddTHH:mm:ss", CultureInfo.InvariantCulture)
                let s = text $"FORMAT ( dt\"{literal}\", \"yyyy-mm-dd hh:nn:ss\" )"
                Expect.equal s (d.ToString("yyyy-MM-dd HH:mm:ss", CultureInfo.InvariantCulture)) literal

                let names = text $"FORMAT ( dt\"{literal}\", \"dddd, mmmm d, yyyy h:nn:ss AM/PM\" )"
                Expect.equal names (d.ToString("dddd, MMMM d, yyyy h:mm:ss tt", CultureInfo.InvariantCulture)) literal
        ]
    ]
