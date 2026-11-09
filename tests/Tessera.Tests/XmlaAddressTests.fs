module Tessera.Tests.XmlaAddressTests

open Expecto
open Tessera
open Tessera.Tests.Fixtures

let private ok =
    function
    | Ok v -> v
    | Error e -> failtestf "%s" (TessError.message e)

let private code (r: Result<'a, TessError>) =
    match r with
    | Ok _ -> "ok"
    | Error e -> TessError.code e

[<Tests>]
let tests =
    testList "XmlaAddress" [
        testCase "every server form, with and without a database"
        <| fun () ->
            let cases = [
                "xmla:powerbi://api.powerbi.com/v1.0/myorg/Sales Team;Sales Model",
                ("powerbi://api.powerbi.com/v1.0/myorg/Sales Team", XmlaEndpoint.PowerBI, Some "Sales Model")
                "xmla:powerbi://api.powerbi.com/v1.0/myorg/Ws",
                ("powerbi://api.powerbi.com/v1.0/myorg/Ws", XmlaEndpoint.PowerBI, None)
                "xmla:asazure://westeurope.asazure.windows.net/srv;Db",
                ("asazure://westeurope.asazure.windows.net/srv", XmlaEndpoint.AzureAS, Some "Db")
                "xmla:https://host/olap/msmdpump.dll;Db",
                ("https://host/olap/msmdpump.dll", XmlaEndpoint.Http, Some "Db")
                "xmla:localhost:51234", ("localhost:51234", XmlaEndpoint.Tcp, None)
                "xmla:localhost:51234;", ("localhost:51234", XmlaEndpoint.Tcp, None)
                @"xmla:host\instance;Db", (@"host\instance", XmlaEndpoint.Tcp, Some "Db")
                "XMLA:  ssas01 ; Db ", ("ssas01", XmlaEndpoint.Tcp, Some "Db")
            ]

            for input, (dataSource, endpoint, database) in cases do
                let a = XmlaAddress.parse input |> ok
                Expect.equal (a.DataSource, a.Endpoint, a.Database) (dataSource, endpoint, database) input
                Expect.isEmpty a.Properties input

        testCase "connection strings: keys in any spelling, quoted values, extra properties kept"
        <| fun () ->
            let a =
                XmlaAddress.parse
                    "xmla:Provider=MSOLAP;Data Source=\"powerbi://api.powerbi.com/v1.0/myorg/A;B\";initial catalog='It''s';Timeout=30;Roles=Reader"
                |> ok

            Expect.equal a.DataSource "powerbi://api.powerbi.com/v1.0/myorg/A;B" ""
            Expect.equal a.Database (Some "It's") ""
            Expect.equal a.Properties [ "Timeout", "30"; "Roles", "Reader" ] ""
            Expect.equal (XmlaAddress.parse "xmla:DataSource=srv;Database=D" |> ok).Database (Some "D") ""

        testCase "credentials are refused on the command line, without echoing their value"
        <| fun () ->
            for key in [ "Password"; "pwd"; "PWD"; "User ID"; "UID"; "Access Token"; "Client Secret" ] do
                let r = XmlaAddress.parse $"xmla:Data Source=srv;{key}=hunter2-value"
                Expect.equal (code r) "secret-on-command-line" key

                match r with
                | Error e -> Expect.isFalse ((TessError.message e).Contains "hunter2") key
                | Ok _ -> ()

        testCase "a remote model needs the prefix; a server argument doesn't"
        <| fun () ->
            Expect.isFalse (XmlaAddress.isRemote "localhost:51234") ""
            Expect.isFalse (XmlaAddress.isRemote @"host\instance") ""
            Expect.isFalse (XmlaAddress.isRemote "powerbi://api.powerbi.com/v1.0/myorg/Ws") ""
            Expect.isFalse (XmlaAddress.isRemote "./xmla:odd-folder-name") ""
            Expect.isTrue (XmlaAddress.isRemote "xmla:localhost:51234") ""
            Expect.isTrue (XmlaAddress.isRemote "XMLA:localhost") ""
            Expect.equal (code (XmlaAddress.parse "localhost:51234")) "invalid-server-address" ""
            Expect.equal (XmlaAddress.parseServer "localhost:51234;Db" |> ok).Database (Some "Db") ""

        testCase "malformed addresses"
        <| fun () ->
            for bad in
                [
                    "xmla:"
                    "xmla:;Db"
                    "xmla:ftp://host/x"
                    "xmla:some/path;Db"
                    "xmla:a;b;c"
                    "xmla:Data Source=a;Data Source=b"
                    "xmla:Data Source=a;Catalog=x;Database=y"
                    "xmla:Provider=MSOLAP;Initial Catalog=x"
                    "xmla:Data Source=\"unclosed"
                    "xmla:Data Source=a;novalue"
                ] do
                Expect.equal (code (XmlaAddress.parse bad)) "invalid-server-address" bad

        testCase "display round-trips and never shows extra properties"
        <| fun () ->
            let a = XmlaAddress.parse "xmla:Data Source=srv01;Initial Catalog=Db;Roles=R" |> ok
            Expect.equal (XmlaAddress.display a) "xmla:srv01;Db" ""
            let again = XmlaAddress.parse (XmlaAddress.display a) |> ok
            Expect.equal (again.DataSource, again.Database) (a.DataSource, a.Database) ""

        testCase "ModelSource.resolve refuses a remote model without touching the file system or network"
        <| fun () ->
            match ModelSource.resolve "xmla:powerbi://api.powerbi.com/v1.0/myorg/Ws;Db" with
            | Error(RemoteSourceUnsupported _ as e) -> Expect.stringContains (TessError.message e) "tess convert" ""
            | other -> failtestf "%A" other

            Expect.equal (code (ModelIO.loadPath "xmla:localhost")) "remote-source-unsupported" ""

        testCase "a file whose name starts with xmla: is still reachable as a path"
        <| fun () ->
            if not (System.OperatingSystem.IsWindows()) then
                use t = new TempDir()
                let file = System.IO.Path.Combine(t.Path, "xmla:odd.bim")

                ModelIO.save ModelIO.BimFile ModelIO.SaveOptions.defaults false (sampleDatabase ()) file
                |> ok
                |> ignore

                let source, _ = ModelIO.loadPath file |> ok
                Expect.equal source.Format Bim ""
    ]
