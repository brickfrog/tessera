namespace Tessera

/// Every failure tessera can report. Core code returns these instead of throwing or printing;
/// the CLI decides how to render them and which exit code to use.
type TessError =
    | PathNotFound of path: string
    | UnrecognizedModel of path: string * reason: string
    | AmbiguousModel of path: string * candidates: string list
    | InvalidPbip of path: string * reason: string
    | RemoteModelReference of reportPath: string
    | LoadFailed of path: string * message: string
    | DestinationNotEmpty of path: string
    | SaveFailed of path: string * message: string
    | InvalidObjectRef of input: string * reason: string
    | ObjectNotFound of input: string
    | AmbiguousObject of input: string * candidates: string list
    | PropertyNotFound of objectRef: string * path: string
    | InvalidPattern of pattern: string * reason: string
    | InvalidName of name: string * reason: string
    | NameCollision of name: string * existing: string
    | UnsupportedRename of objectRef: string * reason: string
    | RenameConflict of objectRef: string * newName: string * details: string list
    /// A Best Practice Analyzer rule file that can't be read as rules.
    | InvalidRules of path: string * reason: string
    /// An edit would leave the model with errors it didn't have (save refused without --force).
    | ChangeIntroducesErrors of count: int
    /// `set`: a property path the object doesn't have (with close matches).
    | UnknownProperty of objectRef: string * path: string * suggestions: string list
    /// `set`: a property that can't be written (read-only, `name`, child collections, ...).
    | ReadOnlyProperty of objectRef: string * path: string * reason: string
    /// `set`: a value the property can't take.
    | InvalidPropertyValue of objectRef: string * path: string * value: string * reason: string
    /// `add` can't create this object here (e.g. a column of a calculated table).
    | CannotAdd of objectRef: string * reason: string
    /// No object matches, but some are named similarly.
    | ObjectNotFoundSuggest of input: string * candidates: string list
    /// Objects can't be removed: others still refer to them (dependents as "ref  via  text -> target").
    | RemoveBlocked of objects: string list * dependents: string list
    | UnsupportedRemove of objectRef: string * reason: string
    | UnsupportedMove of objectRef: string * reason: string
    /// Moving a measure would change what references (its own, or others') mean.
    | MoveConflict of objectRef: string * table: string * details: string list
    /// `translations import`: a file that can't be read, or a row that can't be applied (with the
    /// line it starts on, or the item number in a JSON file).
    | InvalidTranslationFile of path: string * line: int option * reason: string
    /// `perspective add/remove`: an object that can't be added to or removed from a perspective.
    | UnsupportedPerspectiveChange of perspective: string * reason: string
    /// A DAX script that can't be read, at a 1-based line and column of `source` (a path or <stdin>).
    | InvalidScript of source: string * line: int * column: int * reason: string
    /// An object of a DAX script that can't be applied: the error, at the object's line and column.
    | ScriptObjectFailed of source: string * line: int * column: int * error: TessError
    /// `script`: an object a DAX script can't hold or change this way (e.g. a data column's expression).
    | CannotScript of objectRef: string * reason: string
    /// `bpa rules`: no rule with this ID where it was looked for (with a hint where it is, if anywhere).
    | RuleNotFound of ruleId: string * where: string * hint: string option
    /// `bpa rules add`: a rule with this ID is already there (without --replace).
    | RuleExists of ruleId: string * where: string
    /// `bpa rules ignore`: an object that can't carry an ignore annotation.
    | CannotIgnore of objectRef: string * reason: string
    /// A server address (`xmla:...`, or `deploy`'s server argument) that can't be understood.
    | InvalidServerAddress of input: string * reason: string
    /// A credential was passed on the command line (visible in `ps` and shell history).
    | SecretOnCommandLine of key: string
    /// A remote (`xmla:`) model where this command, or this way of using it, needs model files.
    | RemoteSourceUnsupported of source: string * reason: string
    /// The credential environment variables can't be used as they are (e.g. an unreadable file).
    | InvalidCredentials of reason: string
    /// The server needs credentials and none are configured.
    | AuthenticationRequired of server: string * hint: string
    /// The identity provider or the server refused the credentials.
    | AuthenticationFailed of server: string * reason: string * hint: string option
    | ConnectionFailed of server: string * reason: string * hint: string option
    /// A kind of connection this platform's client library can't make (e.g. TCP outside Windows).
    | UnsupportedConnection of server: string * reason: string
    /// No such database on the server (with the ones that exist).
    | DatabaseNotFound of server: string * database: string * existing: string list
    /// The address names no database and the server has none, or more than one.
    | DatabaseRequired of server: string * existing: string list
    | QueryFailed of reason: string
    /// `deploy`: the database exists and --overwrite wasn't given.
    | DatabaseExists of server: string * database: string
    /// `deploy`: the deployed model would have errors the server's model doesn't (without --force).
    | DeployIntroducesErrors of count: int
    /// `deploy`: the server rejected the script, or reported errors running it.
    | DeployFailed of server: string * database: string * messages: string list
    /// A fixture data directory that can't be used at all (problems inside one are findings).
    | InvalidFixture of path: string * reason: string
    /// `refresh`: something that can't be refreshed, or not in the way asked.
    | CannotRefresh of target: string * reason: string
    /// `refresh`: the server rejected the script, or reported errors running it.
    | RefreshFailed of server: string * database: string * messages: string list
    /// DAX that can't be parsed, at a 1-based line and column of `source` ("query", or an object).
    | DaxSyntax of source: string * line: int * column: int * message: string
    /// The local DAX engine doesn't implement this (a function, an argument form, a model feature),
    /// so it refuses rather than guess. `where` is "<source> <line>:<column>", or "" for the model.
    | DaxNotSupported of feature: string * where: string
    /// DAX the engine can't evaluate because it is wrong: an unknown name, the wrong arguments...
    | DaxInvalid of message: string * where: string
    /// An error raised while evaluating DAX: ERROR(), a conversion that fails, an overflow...
    | DaxEvaluation of message: string * where: string
    /// The fixture data has errors, so the local engine won't evaluate against it.
    | FixtureHasErrors of directory: string * errors: int
    /// `test`: no suite files where they were looked for.
    | NoTestSuites of where: string
    /// The model has no role of this name (`--role`).
    | RoleNotFound of role: string * existing: string list
    /// None of these roles (name, permission) may query data.
    | RoleCannotRead of roles: (string * string) list

module TessError =

    /// Stable, machine-readable identifier (used in JSON output).
    let rec code =
        function
        | PathNotFound _ -> "path-not-found"
        | UnrecognizedModel _ -> "unrecognized-model"
        | AmbiguousModel _ -> "ambiguous-model"
        | InvalidPbip _ -> "invalid-pbip"
        | RemoteModelReference _ -> "remote-model-reference"
        | LoadFailed _ -> "load-failed"
        | DestinationNotEmpty _ -> "destination-not-empty"
        | SaveFailed _ -> "save-failed"
        | InvalidObjectRef _ -> "invalid-object-ref"
        | ObjectNotFound _ -> "object-not-found"
        | AmbiguousObject _ -> "ambiguous-object"
        | PropertyNotFound _ -> "property-not-found"
        | InvalidPattern _ -> "invalid-pattern"
        | InvalidName _ -> "invalid-name"
        | NameCollision _ -> "name-collision"
        | UnsupportedRename _ -> "unsupported-rename"
        | RenameConflict _ -> "rename-conflict"
        | InvalidRules _ -> "invalid-rules"
        | ChangeIntroducesErrors _ -> "change-introduces-errors"
        | UnknownProperty _ -> "unknown-property"
        | ReadOnlyProperty _ -> "read-only-property"
        | InvalidPropertyValue _ -> "invalid-property-value"
        | CannotAdd _ -> "cannot-add"
        | ObjectNotFoundSuggest _ -> "object-not-found"
        | RemoveBlocked _ -> "remove-blocked"
        | UnsupportedRemove _ -> "unsupported-remove"
        | UnsupportedMove _ -> "unsupported-move"
        | MoveConflict _ -> "move-conflict"
        | InvalidTranslationFile _ -> "invalid-translation-file"
        | UnsupportedPerspectiveChange _ -> "unsupported-perspective-change"
        | InvalidScript _ -> "invalid-script"
        | ScriptObjectFailed(_, _, _, inner) -> code inner
        | CannotScript _ -> "cannot-script"
        | RuleNotFound _ -> "rule-not-found"
        | RuleExists _ -> "rule-exists"
        | CannotIgnore _ -> "cannot-ignore"
        | InvalidServerAddress _ -> "invalid-server-address"
        | SecretOnCommandLine _ -> "secret-on-command-line"
        | RemoteSourceUnsupported _ -> "remote-source-unsupported"
        | InvalidCredentials _ -> "invalid-credentials"
        | AuthenticationRequired _ -> "authentication-required"
        | AuthenticationFailed _ -> "authentication-failed"
        | ConnectionFailed _ -> "connection-failed"
        | UnsupportedConnection _ -> "unsupported-connection"
        | DatabaseNotFound _ -> "database-not-found"
        | DatabaseRequired _ -> "database-required"
        | QueryFailed _ -> "query-failed"
        | DatabaseExists _ -> "database-exists"
        | DeployIntroducesErrors _ -> "deploy-introduces-errors"
        | DeployFailed _ -> "deploy-failed"
        | InvalidFixture _ -> "invalid-fixture"
        | CannotRefresh _ -> "cannot-refresh"
        | RefreshFailed _ -> "refresh-failed"
        | DaxSyntax _ -> "dax-syntax"
        | DaxNotSupported _ -> "not-supported"
        | DaxInvalid _ -> "dax-invalid"
        | DaxEvaluation _ -> "dax-error"
        | FixtureHasErrors _ -> "fixture-has-errors"
        | NoTestSuites _ -> "no-test-suites"
        | RoleNotFound _ -> "role-not-found"
        | RoleCannotRead _ -> "role-cannot-read"

    let rec message =
        function
        | PathNotFound path -> $"Path not found: {path}"
        | UnrecognizedModel(path, reason) -> $"Not a recognised semantic model: {path} ({reason})"
        | AmbiguousModel(path, candidates) ->
            let list = candidates |> List.map (fun c -> $"  {c}") |> String.concat "\n"
            $"More than one semantic model found under {path}; pass one of these explicitly:\n{list}"
        | InvalidPbip(path, reason) -> $"Invalid Power BI project file {path}: {reason}"
        | RemoteModelReference reportPath ->
            $"Report {reportPath} references a published semantic model (byConnection), not a local one"
        | LoadFailed(path, msg) -> $"Failed to load {path}: {msg}"
        | DestinationNotEmpty path -> $"Destination exists and is not empty: {path} (use --force to overwrite)"
        | SaveFailed(path, msg) -> $"Failed to write {path}: {msg}"
        | InvalidObjectRef(input, reason) -> $"Invalid object reference '{input}': {reason}"
        | ObjectNotFound input -> $"No object matches '{input}'"
        | AmbiguousObject(input, candidates) ->
            let list = candidates |> List.map (fun c -> $"  {c}") |> String.concat "\n"
            $"'{input}' matches more than one object; use one of these:\n{list}"
        | PropertyNotFound(objectRef, path) -> $"{objectRef} has no property '{path}'"
        | InvalidPattern(pattern, reason) -> $"Invalid pattern '{pattern}': {reason}"
        | InvalidName(name, reason) -> $"Invalid name '{name}': {reason}"
        | NameCollision(name, existing) -> $"'{name}' is already taken by {existing} (names are case-insensitive)"
        | UnsupportedRename(objectRef, reason) -> $"Can't rename {objectRef}: {reason}"
        | RenameConflict(objectRef, newName, details) ->
            let list = details |> List.map (fun d -> $"  {d}") |> String.concat "\n"
            $"Renaming {objectRef} to '{newName}' would change what other references mean:\n{list}"
        | InvalidRules(path, reason) -> $"Invalid BPA rules in {path}: {reason}"
        | ChangeIntroducesErrors n ->
            let errors = if n = 1 then "1 error" else $"{n} errors"
            $"Not saved: the change introduces {errors} (see above; use --force to save anyway)"
        | UnknownProperty(objectRef, path, suggestions) ->
            let hint =
                match suggestions with
                | [] -> ""
                | s ->
                    " - did you mean "
                    + (s |> List.map (fun p -> $"'{p}'") |> String.concat " or ")
                    + "?"

            $"{objectRef} has no property '{path}'{hint} (`tess get <model> <object> --properties` lists them)"
        | ReadOnlyProperty(objectRef, path, reason) -> $"Can't set {path} on {objectRef}: {reason}"
        | InvalidPropertyValue(objectRef, path, value, reason) ->
            let first = value.Split('\n').[0]

            let shown =
                if first.Length > 40 then first.Substring(0, 40) + "..."
                elif first.Length < value.Length then first + "..."
                else first

            $"Can't set {path} on {objectRef} to '{shown}': {reason}"
        | CannotAdd(objectRef, reason) -> $"Can't add {objectRef}: {reason}"
        | ObjectNotFoundSuggest(input, candidates) ->
            let list = candidates |> String.concat ", "
            $"No object matches '{input}'; did you mean {list}?"
        | RemoveBlocked(objects, dependents) ->
            let list = dependents |> List.map (fun d -> $"  {d}") |> String.concat "\n"
            let it = if objects.Length = 1 then "it" else "them"
            let names = String.concat ", " objects

            $"Can't remove {names}: other objects refer to {it}:\n{list}\n"
            + $"Remove those too (or use --cascade), or use --force to remove {it} anyway and leave these references broken."
        | UnsupportedRemove(objectRef, reason) -> $"Can't remove {objectRef}: {reason}"
        | UnsupportedMove(objectRef, reason) -> $"Can't move {objectRef}: {reason}"
        | MoveConflict(objectRef, table, details) ->
            let list = details |> List.map (fun d -> $"  {d}") |> String.concat "\n"

            $"Moving {objectRef} to '{table}' would change what references mean:\n{list}\n"
            + "Qualify those references with their table ('Table'[Column]) first."
        | InvalidTranslationFile(path, line, reason) ->
            let at =
                match line with
                | Some n -> $" line {n}"
                | None -> ""

            $"{path}{at}: {reason}"
        | UnsupportedPerspectiveChange(perspective, reason) -> $"Can't change {perspective}: {reason}"
        | InvalidScript(source, line, column, reason) -> $"{source}:{line}:{column}: {reason}"
        | ScriptObjectFailed(source, line, column, inner) -> $"{source}:{line}:{column}: {message inner}"
        | CannotScript(objectRef, reason) -> $"Can't script {objectRef}: {reason}"
        | RuleNotFound(ruleId, where, hint) ->
            let hint = hint |> Option.map (fun h -> $"; {h}") |> Option.defaultValue ""
            $"No BPA rule '{ruleId}' in {where}{hint}"
        | RuleExists(ruleId, where) -> $"{where} already has a BPA rule '{ruleId}' (use --replace to replace it)"
        | CannotIgnore(objectRef, reason) -> $"Can't ignore rules on {objectRef}: {reason}"
        | InvalidServerAddress(input, reason) -> $"Invalid server address '{input}': {reason}"
        | SecretOnCommandLine key ->
            $"'{key}' must not be given on the command line (it would show in `ps` and shell history); "
            + "use the TESS_XMLA_* environment variables instead (see `tess connect --help`)"
        | RemoteSourceUnsupported(source, reason) -> $"Can't use the remote model {source} here: {reason}"
        | InvalidCredentials reason -> $"Invalid credentials configuration: {reason}"
        | AuthenticationRequired(server, hint) -> $"No credentials for {server}: {hint}"
        | AuthenticationFailed(server, reason, hint) ->
            let hint = hint |> Option.map (fun h -> $"\n{h}") |> Option.defaultValue ""
            $"Authentication failed for {server}: {reason}{hint}"
        | ConnectionFailed(server, reason, hint) ->
            let hint = hint |> Option.map (fun h -> $"\n{h}") |> Option.defaultValue ""
            $"Can't connect to {server}: {reason}{hint}"
        | UnsupportedConnection(server, reason) -> $"Can't connect to {server}: {reason}"
        | DatabaseNotFound(server, database, existing) ->
            let list =
                match existing with
                | [] -> "the server has no databases (that these credentials can see)"
                | names -> "it has:\n" + (names |> List.map (fun n -> $"  {n}") |> String.concat "\n")

            $"No database '{database}' on {server}; {list}"
        | DatabaseRequired(server, existing) ->
            match existing with
            | [] -> $"{server} has no databases (that these credentials can see)"
            | names ->
                let list = names |> List.map (fun n -> $"  {n}") |> String.concat "\n"
                $"{server} has more than one database; name one (xmla:<server>;<database>):\n{list}"
        | QueryFailed reason -> $"Query failed: {reason}"
        | DatabaseExists(server, database) ->
            $"Database '{database}' already exists on {server} (use --overwrite to replace its model)"
        | DeployIntroducesErrors n ->
            let errors = if n = 1 then "1 error" else $"{n} errors"
            $"Not deployed: the model would have {errors} the deployed one doesn't (see above; use --force to deploy anyway)"
        | DeployFailed(server, database, messages) ->
            let list = messages |> List.map (fun m -> $"  {m}") |> String.concat "\n"
            $"Deploying '{database}' to {server} failed:\n{list}"
        | InvalidFixture(path, reason) -> $"Can't use {path} as fixture data: {reason}"
        | CannotRefresh(target, reason) -> $"Can't refresh {target}: {reason}"
        | RefreshFailed(server, database, messages) ->
            let list = messages |> List.map (fun m -> $"  {m}") |> String.concat "\n"
            $"Refreshing '{database}' on {server} failed:\n{list}"
        | DaxSyntax(source, line, column, msg) -> $"{source} {line}:{column}: {msg}"
        | DaxNotSupported(feature, where) ->
            let at = if where = "" then "" else $" ({where})"
            $"Not supported by the local DAX engine: {feature}{at}"
        | DaxInvalid(msg, where) ->
            let at = if where = "" then "" else $"{where}: "
            $"{at}{msg}"
        | DaxEvaluation(msg, where) ->
            let at = if where = "" then "" else $"{where}: "
            $"{at}{msg}"
        | FixtureHasErrors(directory, n) ->
            let errors = if n = 1 then "1 error" else $"{n} errors"
            $"The fixture data in {directory} has {errors}; fix them first (`tess data check` lists them)"
        | NoTestSuites where ->
            $"No test suites (*.test.yaml, *.test.yml, *.test.json) found in {where}. `tess test init <model>` writes one to start from."
        | RoleNotFound(role, existing) ->
            match existing with
            | [] -> $"The model has no role '{role}'; it has no roles"
            | names -> $"The model has no role '{role}'; it has: " + String.concat ", " names
        | RoleCannotRead roles ->
            let list =
                roles |> List.map (fun (name, permission) -> $"'{name}' ({permission})") |> String.concat ", "

            $"Members of {list} can't query data: a role needs the Read, ReadRefresh or Administrator permission"
