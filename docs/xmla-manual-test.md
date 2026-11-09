# Manual test: connected mode against real servers

Connected mode (`connect`, `query`, `deploy`, `xmla:` models) was developed without a server. Everything below
the client libraries is tested against a fake; the live connector (`src/Tessera.Xmla/Live.fs`) has only been
exercised as far as fake credentials go:

| Checked for real (Linux, single-file binary, made-up credentials) | Never run |
|---|---|
| a service principal reaches the sign-in service and its error is reported (`AADSTS90002`) | a successful connection of any kind |
| an access token reaches the Power BI workspace lookup and its 403 is reported | reading a database, listing databases |
| device-code sign-in starts and prints the code | finishing a device-code sign-in; the token cache file; token renewal |
| SSAS over TCP and password-less HTTP are refused off Windows | running a query; several `EVALUATE`s; the value types that come back |
| HTTP with a user name and password attempts the connection | executing TMSL; everything on Windows; Azure AS; SSAS |

So expect to find bugs here. This checklist is ordered so each step depends only on the ones before it. Note what
differs from "Expect" (the exact output, with any secret removed) and stop at the first step that fails badly.

## Setup

Use a workspace and models you can afford to break. Section C deploys.

```sh
dotnet publish src/Tessera.Cli -c Release -r linux-x64 --self-contained -p:PublishSingleFile=true -o publish
alias tess=$PWD/publish/tess          # use the published binary: it is what ships
git clone --depth 1 https://github.com/TabularEditor/spaceparts-analytics /tmp/spaceparts
```

For Power BI you need:
- a workspace on Premium, Premium Per User, Embedded or Fabric capacity, with the capacity's **XMLA Endpoint**
  setting on **Read Write** (section C needs write; A and B work with **Read Only**);
- a published semantic model in it (any import model with a few tables);
- one of: the Azure CLI signed in as a workspace Member/Admin; or a service principal that is a workspace
  Member/Admin, with the tenant setting **Service principals can use Fabric APIs** enabled for it.

```sh
WS='powerbi://api.powerbi.com/v1.0/myorg/<Workspace Name>'
DB='<Model Name>'
```

## A. Connect and authenticate (Power BI)

### A1. No credentials: nothing is contacted

```sh
env -u TESS_XMLA_TOKEN -u AZURE_CLIENT_ID tess connect "$WS"; echo "exit $?"
```

Expect: `tess: No credentials for powerbi://...: set TESS_XMLA_TOKEN ...`, `exit 5`, immediately (no network
delay).

### A2. Access token

```sh
export TESS_XMLA_TOKEN="$(az account get-access-token --resource https://analysis.windows.net/powerbi/api --query accessToken -o tsv)"
tess connect "$WS"; echo "exit $?"
tess connect "$WS" --json
```

Expect: `exit 0` and

```
Server          powerbi://api.powerbi.com/v1.0/myorg/<Workspace Name>
Signed in with  access token
Product         <product name> <version>
Mode            PowerBI

<Model Name>   <compatibility level>
<other models in the workspace>
```

The JSON has `"authentication": "access token"` and one `databases[]` entry per model, each with a `source` of the
form `xmla:powerbi://...;<Model Name>`. Check: every model in the workspace is listed, and the token appears
nowhere in either output.

### A3. Service principal

```sh
unset TESS_XMLA_TOKEN
export TESS_XMLA_CLIENT_ID=<app id> TESS_XMLA_TENANT_ID=<tenant id>
printf '%s' '<secret>' > /tmp/spn-secret && chmod 600 /tmp/spn-secret
export TESS_XMLA_CLIENT_SECRET_FILE=/tmp/spn-secret
tess connect "$WS"
```

Expect: as A2, with `Signed in with  service principal <app id> in tenant <tenant id>`.

Then each failure, one at a time (restore the right value after each):

| Change | Expect (exit 5) |
|---|---|
| `TESS_XMLA_TENANT_ID=00000000-0000-0000-0000-000000000002` | `Authentication failed ... AADSTS90002 ...` then `The tenant wasn't found: check TESS_XMLA_TENANT_ID` |
| `TESS_XMLA_CLIENT_ID=00000000-0000-0000-0000-000000000001` | `Authentication failed ... AADSTS700016 ...` then the "application isn't in this tenant" hint |
| a wrong secret in the file | `Authentication failed ... AADSTS7000215 ...` then the "client secret is wrong" hint; **the wrong secret itself must not appear** |
| `WS` with a workspace name that doesn't exist | `Can't connect ... Resolving the PBI workspace ...` with a hint about the workspace name or access |
| the principal removed from the workspace | the same 403 hint |
| a workspace not on a capacity (Pro) | a hint that XMLA endpoints need Premium/PPU/Fabric capacity. **The matched text is a guess**: record the real message |

### A4. Device code

```sh
unset TESS_XMLA_CLIENT_ID TESS_XMLA_TENANT_ID TESS_XMLA_CLIENT_SECRET_FILE
export TESS_XMLA_AUTH=device-code
tess connect "$WS"
```

Expect: stderr shows `To sign in, use a web browser to open the page https://login.microsoft.com/device and enter
the code XXXXXXXXX to authenticate.`; after you do, the same output as A2 with `Signed in with  device code sign-in`.
Run it again: it asks for a code again (nothing was kept).

If the sign-in page refuses the application (tess uses the Analysis Services client libraries' own application ID,
`cf710c6e-dfcc-4fa8-a093-d47294e44c66`), record the error, then repeat with `TESS_XMLA_CLIENT_ID` set to a public
client app registration of your own that has the Power BI Service `Dataset.ReadWrite.All` delegated permission and
"Allow public client flows" on.

Then the opt-in cache:

```sh
export TESS_XMLA_TOKEN_CACHE=~/.cache/tessera/xmla-token-cache
tess connect "$WS"      # asks for a code once
tess connect "$WS"      # must not ask again
stat -c '%a %n' ~/.cache/tessera/xmla-token-cache
```

Expect: `600 .../xmla-token-cache`, and no second prompt. Delete the file; the next run prompts again.
`unset TESS_XMLA_AUTH TESS_XMLA_TOKEN_CACHE` afterwards and go back to the token (A2) for the rest.

### A5. Database names

```sh
tess info "xmla:$WS;no such model"; echo "exit $?"
tess info "xmla:$WS"; echo "exit $?"
```

Expect: `No database 'no such model' on powerbi://...; it has:` followed by every model name, `exit 5`. The second:
with several models, `... has more than one database; name one` and the list, `exit 5`; with exactly one, its info.

## B. Read, and query (Power BI)

### B1. The remote model as a source

```sh
SRC="xmla:$WS;$DB"
tess info "$SRC"
tess list "$SRC" -k measure | head
tess validate "$SRC"; tess bpa "$SRC" | tail -3; tess lint "$SRC" | tail -3
tess convert "$SRC" /tmp/remote.SemanticModel
tess diff "$SRC" /tmp/remote.SemanticModel; echo "exit $?"
```

Expect: `info` shows `Format xmla` and `Path xmla:powerbi://...;<Model Name>`, with plausible counts. The `diff`
prints nothing and exits 0: what was downloaded is what is there. Open `/tmp/remote.SemanticModel` in Power BI
Desktop or Tabular Editor to confirm it is a complete model. If you have the model's source files, also
`tess diff "$SRC" <your files> --stat` and check the differences are ones you recognise.

Things to look for: partitions and data sources present; no row-number columns or other server-generated objects;
calculated columns keep their data types.

### B2. Editing commands don't write to a server

```sh
tess rename "$SRC" "[<a measure>]" "Renamed"            # dry run: shows the change
tess rename "$SRC" "[<a measure>]" "Renamed" --save; echo "exit $?"
```

Expect: the second prints `Can't use the remote model ... --save can't write to a server ...`, `exit 3`.

### B3. Query: types

```sh
tess query "$SRC" 'EVALUATE ROW("i", 42, "d", 1.5, "c", CURRENCY(12.34), "s", "x", "t", DATE(2024,1,31), "b", TRUE(), "n", BLANK())' --json
```

Expect exactly these values (the `type` of the last column is whatever the server reports; record it):

```json
"columns": [ {"name":"[i]","type":"int64"}, {"name":"[d]","type":"double"}, {"name":"[c]","type":"decimal"},
             {"name":"[s]","type":"string"}, {"name":"[t]","type":"dateTime"}, {"name":"[b]","type":"boolean"},
             {"name":"[n]","type":"variant"} ],
"rows": [ [ 42, 1.5, 12.34, "x", "2024-01-31T00:00:00", true, null ] ],
"rowCount": 1, "truncated": false
```

This is the step most likely to surprise: record the `columns` exactly as returned. In particular whether measure
and expression columns come back typed or as `variant`, and whether `CURRENCY` comes back as `decimal`.

### B4. Query: several results, formats, limits, errors

```sh
tess query "$SRC" 'EVALUATE { 1, 2, 3 } EVALUATE ROW("a", "x,y")'
tess query "$SRC" 'EVALUATE { 1, 2, 3 } EVALUATE ROW("a", "x,y")' --csv
tess query "$SRC" 'EVALUATE { 1, 2, 3 }' --max-rows 2 --json
echo "EVALUATE TOPN(5, '<a table>')" | tess query "$SRC" -
tess query "$SRC" "EVALUATE 'no such table'"; echo "exit $?"
```

Expect: two tables separated by an empty line (`[Value]` with 1, 2, 3 right-aligned; `[a]` with `x,y`); the CSV has
`"x,y"` quoted and an empty line between results; the third has `"rowCount": 2, "truncated": true`; the fourth
prints five rows of a real table, with BLANKs as empty cells and dates as `yyyy-MM-ddTHH:mm:ss`; the last prints
`tess: Query failed: ...` with the server's own message and `exit 5`.

**If only the first result of a multi-`EVALUATE` query comes back**, that is a real finding: the reader's
`NextResult` is untested.

### B5. The gated integration tests

```sh
TESSERA_XMLA_SERVER="$WS" TESSERA_XMLA_DATABASE="$DB" \
  dotnet run --project tests/Tessera.Tests -c Release -- --filter-test-list "XMLA integration"
```

Expect: 6 tests pass. They only read.

## C. Deploy (Power BI; writes to the workspace)

### C1. Dry run against the model itself

```sh
tess convert "$SRC" /tmp/as-deployed --include-sensitive --force
tess deploy /tmp/as-deployed "$WS" "$DB"; echo "exit $?"
```

Expect: `Deploy /tmp/as-deployed -> replace the model of '<Model Name>' on powerbi://...`, some `kept:` lines,
`No changes.`, the TMSL script, and on stderr `Dry run: nothing deployed. Add --execute to deploy. It also needs
--overwrite (the database exists).`, `exit 0`. Confirm in the Power BI service that nothing changed (the model's
"Refreshed" time is untouched).

### C2. Create a new model

```sh
tess deploy /tmp/spaceparts "$WS" "tessera-test"                     # dry run: "create 'tessera-test'"
tess deploy /tmp/spaceparts "$WS" "tessera-test" --tmsl > /tmp/create.tmsl
tess deploy /tmp/spaceparts "$WS" "tessera-test" --execute; echo "exit $?"
tess connect "$WS" | grep tessera-test
tess diff /tmp/spaceparts "xmla:$WS;tessera-test" --stat
```

Expect: `Deployed 'tessera-test' to powerbi://...`, `exit 0`, and the model listed. The `diff` should show little
or nothing (record what it shows: differences here are either things the service adds or things tess dropped).
`/tmp/create.tmsl` is the script that ran; if the server rejects it (`exit 5`, `Deploying ... failed:` and its
messages), keep that file and the messages.

### C3. Redeploy a change; the server's side survives

Refresh `tessera-test` in the service if you can (or note that its tables are unprocessed). Then change one measure
in a copy of the files and:

```sh
cp -r /tmp/spaceparts /tmp/spaceparts2
tess set /tmp/spaceparts2 "[<a measure>]" -p description="changed by tess" --save
tess deploy /tmp/spaceparts2 "$WS" "tessera-test"                       # shows exactly that one change
tess deploy /tmp/spaceparts2 "$WS" "tessera-test" --execute; echo "exit $?"         # exit 4: needs --overwrite
tess deploy /tmp/spaceparts2 "$WS" "tessera-test" --execute --overwrite; echo "exit $?"
tess get "xmla:$WS;tessera-test" "[<a measure>]" -p description
```

Expect: the dry run lists one changed object; the first `--execute` is refused with `already exists`; the second
deploys; the description reads back. In the service: **the data is still there** (a deployed-and-refreshed model
must not need a refresh after a metadata-only deployment), and any role members you added in the service are still
there.

### C4. The error gate

```sh
tess add /tmp/spaceparts2 "measure:<a table>/Broken" -e "'No Such Table'[x]" --save --force
tess deploy /tmp/spaceparts2 "$WS" "tessera-test" --execute --overwrite; echo "exit $?"
```

Expect: `error: measure:<a table>/Broken: ...` and `Not deployed: the model would have 1 error ...`, `exit 1`,
nothing changed on the server.

### C5. Clean up

Delete `tessera-test` in the Power BI service (tess has no delete).

## D. SQL Server Analysis Services (Windows)

On a Windows machine with a tabular instance (and, for D3, Power BI Desktop with a file open):

```powershell
dotnet publish src/Tessera.Cli -c Release -r win-x64 --self-contained -p:PublishSingleFile=true -o publish
$env:Path = "$PWD\publish;$env:Path"
```

### D1. Integrated security

```powershell
tess connect localhost            # or host\instance
tess info "xmla:localhost;<Database>"
tess query "xmla:localhost;<Database>" "EVALUATE ROW(""a"", 1)"
```

Expect: `Signed in with  integrated (no credentials from tess)`, `Mode AnalysisServices`, the databases, and the
query result. The single-file binary has no `msasxpress.dll` or MSAL broker next to it; a failure mentioning either
is a finding.

### D2. Deploy

```powershell
tess deploy <a 1500/1600-level model> localhost TessTest
tess deploy <the same> localhost TessTest --execute
tess deploy <the same> localhost TessTest --execute --overwrite --deploy-partitions
```

Expect: create, then replace. Deploying a Power BI model (compatibility level 1700+) to SSAS should fail early with
`the server doesn't support compatibility level ...`.

### D3. Power BI Desktop

Find the port (`netstat -ano | findstr LISTENING` against the `msmdsrv.exe` PID, or DAX Studio's status bar), then:

```powershell
tess info "xmla:localhost:<port>"
tess convert "xmla:localhost:<port>" .\from-desktop.SemanticModel
```

Expect: the open file's model (the database name is a GUID; with one database it needn't be given).

### D4. The same from Linux or macOS

```sh
tess connect ssas-host; echo "exit $?"
```

Expect: `... only works on Windows ...`, `exit 5`, immediately.

If the instance is published over HTTP (`msmdpump.dll`) with Basic authentication:

```sh
export TESS_XMLA_USER='DOMAIN\user' TESS_XMLA_PASSWORD_FILE=/path/to/password
tess connect "https://ssas-host/olap/msmdpump.dll"
```

Expect: the databases, signed in as `user DOMAIN\user`.

## E. Azure Analysis Services (if you have one)

```sh
export TESS_XMLA_TOKEN="$(az account get-access-token --resource 'https://*.asazure.windows.net' --query accessToken -o tsv)"
tess connect "asazure://<region>.asazure.windows.net/<server>"
```

Expect: as A2. A service principal must be a server administrator (`app:<id>@<tenant>`).

## What to send back

For each step that differed: the command, the full output with secrets removed, and the platform. The most useful
single artifacts are the `--json` output of B3 and the `tess diff ... --stat` of C2.
