/// `tess completion bash|zsh|fish|pwsh`: shell completion scripts, generated from the Argu parser
/// definitions so they list every command, subcommand and option tess has. And the hidden
/// `tess __complete`, which the scripts call for what only the model knows: its tables, measures,
/// columns, roles and perspectives.
///
/// How a command line is read, the same in each script and here: walking the words after `tess`,
/// a subcommand's name enters it; an option that takes a value skips the next word; anything else
/// is the next positional argument. The word being completed is then an option's value, an option
/// (it starts with `-`), or a subcommand or positional argument.
module Tessera.Cli.CompletionCommand

open System
open System.IO
open System.Reflection
open System.Text
open Argu
open Microsoft.FSharp.Reflection
open Tessera
open Tessera.Cli.Common

/// One-line description, shown in `tess --help`.
let summary =
    "print a shell completion script for bash, zsh, fish or pwsh (see the README to install it)."

type Shell =
    | Bash
    | Zsh
    | Fish
    | Pwsh

type Args =
    | [<MainCommand; ExactlyOnce>] Shell of shell: Shell

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Shell _ -> "the shell: bash, zsh, fish or pwsh (PowerShell)."

// ---- The command line, as completion sees it --------------------------------------------------

/// What a word can be completed with.
type Value =
    /// Anything: nothing to offer.
    | Text
    /// A file or folder.
    | Path
    | Directory
    | Choice of string list
    /// References (`measure:Sales/Total`) to objects of the model, of these kinds (none: any).
    | Objects of ObjectKind list
    /// Names of the model's tables, roles or perspectives.
    | Tables
    | Roles
    | Perspectives

module Value =
    /// Completed by asking the model (`tess __complete`).
    let isDynamic =
        function
        | Objects _
        | Tables
        | Roles
        | Perspectives -> true
        | Text
        | Path
        | Directory
        | Choice _ -> false

type Opt = {
    Name: string
    Help: string
    /// None: a flag, which takes no value.
    Value: Value option
}

type Node = {
    /// The words that lead here, e.g. ["bpa"; "rules"; "list"]; [] for tess itself.
    Path: string list
    Help: string
    Options: Opt list
    Children: Node list
    /// The positional arguments, in order.
    Positionals: Value list
    /// Any further positional arguments.
    Rest: Value option
}

module Node =
    let key (n: Node) = String.concat " " n.Path

    let rec all (n: Node) = n :: (n.Children |> List.collect all)

    /// The value of the positional argument at this index (from 0).
    let positional (n: Node) (index: int) =
        match List.tryItem index n.Positionals with
        | Some v -> Some v
        | None -> n.Rest

    let tryChild (n: Node) (word: string) =
        n.Children |> List.tryFind (fun c -> List.last c.Path = word)

    let tryOption (n: Node) (word: string) =
        n.Options |> List.tryFind (fun o -> o.Name = word)

// Values Argu can't know: the option takes a string, but only some strings mean anything. Each list
// comes from the code that parses the option where it exposes one; the rest copy the parser, and
// a test checks each value appears in the option's own help text.
let private severities = Validate.Severity.all |> List.map Validate.Severity.token

/// Every case of a union with no fields (so a new one is offered without a change here).
let private casesOf<'T> () : 'T list =
    FSharpType.GetUnionCases(typeof<'T>)
    |> Array.map (fun c -> FSharpValue.MakeUnion(c, [||]) :?> 'T)
    |> List.ofArray

let private outputFormats =
    casesOf<ModelIO.OutputFormat>() |> List.map ModelIO.OutputFormat.token

let private ruleLayers =
    casesOf<BpaRules.RuleLayer>() |> List.map BpaRules.RuleLayer.token

let private kinds = ObjectKind.all |> List.map ObjectKind.token

/// An option's value, where its name and type don't say enough. Keyed by the command's words and
/// the option's first name.
let private optionOverride (command: string) (option: string) : Value option =
    match command, option with
    | _, "--ci" -> Some(Choice [ "github"; "azdo"; "none" ])
    | ("validate" | "lint" | "data check"), ("--min-severity" | "--fail-on") -> Some(Choice severities)
    | "bpa", "--fail-on" -> Some(Choice severities)
    | "bpa", "--min-severity" -> Some(Choice [ "1"; "2"; "3" ])
    | ("test run" | "test compare"), "--fail-on" -> Some(Choice [ "error"; "warning" ])
    | ("convert" | "init" | "data embed"), "--format" -> Some(Choice outputFormats)
    | "convert", "--eol" -> Some(Choice [ "auto"; "lf"; "crlf" ])
    | "refresh", "--type" -> Some(Choice(Refresh.Kind.all |> List.map Refresh.Kind.token))
    | "refresh", "--progress" -> Some(Choice [ "objects"; "all"; "none" ])
    | "add", "--data-type" -> Some(Choice [ "string"; "int64"; "double"; "decimal"; "datetime"; "boolean"; "binary" ])
    | "add", "--mode" -> Some(Choice [ "import"; "directquery"; "dual" ])
    | "add", "--cross-filter" -> Some(Choice [ "single"; "both" ])
    | "add", "--cardinality" -> Some(Choice [ "many-to-one"; "one-to-one"; "many-to-many" ])
    | "add", "--permission" -> Some(Choice [ "read"; "readrefresh"; "refresh"; "administrator"; "none" ])
    | "bpa rules list", "--layer" -> Some(Choice ruleLayers)
    | "query", "--role" -> Some Roles
    // A property path (`get -p Expression`), not a file.
    | ("get" | "set"), ("--property" | "--unset") -> Some Text
    | _ -> None

/// The positional arguments of a command whose field names don't say: (positionals, rest).
let private positionalOverride (command: string) : (Value list * Value option) option =
    match command with
    | "query" -> Some([ Path ], Some Text)
    // Its objects are on a server (model files are refused), and completion never connects.
    | "refresh" -> Some([ Text ], Some Text)
    | "deploy" -> Some([ Path; Text; Text ], None)
    // The new object's reference: nothing to offer.
    | "add" -> Some([ Path; Text ], None)
    | _ -> None

/// What a field's name says about its value.
let private valueOfName (name: string) =
    match name with
    | "model"
    | "path"
    | "file"
    | "files"
    | "source"
    | "destination"
    | "before"
    | "after"
    | "input"
    | "script"
    | "target" -> Path
    | "directory" -> Directory
    | "object"
    | "objects" -> Objects []
    | "measure" -> Objects [ ObjectKind.Measure ]
    | "column" -> Objects [ ObjectKind.Column ]
    | "table" -> Tables
    | "perspective" -> Perspectives
    | "kinds" -> Choice kinds
    | _ -> Text

/// A field name's words: `model_and_objects` -> model, objects; `input_or_model_and_objects` ->
/// input, objects (an alternative counts once); `new_name` is one word.
let private nameWords (name: string) =
    let rec go words =
        match words with
        | "new" :: "name" :: rest -> "new_name" :: go rest
        | a :: "or" :: _ :: rest -> a :: go rest
        | "and" :: rest -> go rest
        | w :: rest -> w :: go rest
        | [] -> []

    go (name.Split('_') |> List.ofArray)

let private isList (t: Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>>

/// The main command's positional arguments, from its field names: a tuple names each argument; a
/// list's name lists them, its last (plural) word being any number more.
let private positionalsOf (command: string) (fields: PropertyInfo array) =
    match positionalOverride command with
    | Some p -> p
    | None ->
        match fields with
        | [| f |] when isList f.PropertyType ->
            if f.Name = "Item" then
                [ Path ], Some Path
            else
                match List.rev (nameWords f.Name) with
                | last :: before when last.EndsWith "s" && last <> "this" ->
                    (before |> List.rev |> List.map valueOfName), Some(valueOfName last)
                | words -> (words |> List.rev |> List.map valueOfName), None
        | fields ->
            let named =
                fields
                |> List.ofArray
                |> List.collect (fun f ->
                    if f.Name.StartsWith "Item" then
                        []
                    else
                        nameWords f.Name |> List.map valueOfName)

            // A tuple with one label (`model_and_rule: string * string`) names its fields in it.
            let padded =
                if named.Length >= fields.Length then
                    named
                else
                    named @ List.replicate (fields.Length - named.Length) Text

            padded, None

let private optionValueOf (command: string) (case: ArgumentCaseInfo) =
    match case.UnionCaseInfo.GetFields() with
    | [||] -> None
    | fields ->
        match optionOverride command case.CommandLineNames.Value.Head with
        | Some v -> Some v
        | None ->
            let f = fields.[0]
            let t = f.PropertyType

            if t = typeof<string> then
                Some(valueOfName f.Name)
            elif FSharpType.IsUnion t && not (isList t) && not t.IsGenericType then
                Some(
                    Choice(
                        FSharpType.GetUnionCases t
                        |> Array.map (fun c -> c.Name.ToLowerInvariant())
                        |> List.ofArray
                    )
                )
            else
                Some Text

let private isInherited (case: ArgumentCaseInfo) =
    case.UnionCaseInfo.GetCustomAttributes(typeof<InheritAttribute>).Length > 0

let private helpText = "display this list of options."

let rec private build (path: string list) (help: string) (inherited: Opt list) (parser: ArgumentParser) : Node =
    let command = String.concat " " path

    let cases = parser.GetArgumentCases() |> List.filter (fun c -> not c.IsHidden.Value)

    let subparsers = parser.GetSubCommandParsers()

    let optionsOf (case: ArgumentCaseInfo) =
        let value = optionValueOf command case

        case.CommandLineNames.Value
        |> List.map (fun n -> {
            Name = n
            Help = case.Description.Value
            Value = value
        })

    let own = [
        for c in cases do
            if c.ArgumentType <> ArgumentType.SubCommand && not c.IsMainCommand then
                yield! optionsOf c
    ]

    // Inherited options (--json) are only declared on tess itself; Argu takes them anywhere below.
    let inheritedHere =
        inherited
        @ (own
           |> List.filter (fun o ->
               cases
               |> List.exists (fun c -> isInherited c && List.contains o.Name c.CommandLineNames.Value)))

    let ownHere = own |> List.filter (fun o -> not (List.contains o inheritedHere))

    let helpFlags =
        parser.HelpFlags
        |> List.map (fun n -> {
            Name = n
            Help = helpText
            Value = None
        })

    let children = [
        for c in cases do
            if c.ArgumentType = ArgumentType.SubCommand then
                let name = c.CommandLineNames.Value.Head

                let sub =
                    subparsers
                    |> List.find (fun s ->
                        match s.ParentInfo with
                        | Some p -> p.CommandLineNames.Value.Head = name
                        | None -> false)

                build (path @ [ name ]) c.Description.Value inheritedHere sub
    ]

    let positionals, rest =
        match cases |> List.tryFind (fun c -> c.IsMainCommand) with
        | Some main -> positionalsOf command (main.UnionCaseInfo.GetFields())
        | None -> [], None

    {
        Path = path
        Help = help
        Options = ownHere @ inheritedHere @ helpFlags
        Children = children
        Positionals = positionals
        Rest = rest
    }

/// The command line tess accepts, from its Argu parser.
let spec (parser: ArgumentParser) : Node =
    build [] "a command line for Power BI / Analysis Services semantic models." [] parser

// ---- Reading a command line -------------------------------------------------------------------

/// A word as a POSIX shell would read it (bash passes words as typed): quotes and backslashes
/// removed, a leading ~ expanded. An unfinished quote runs to the end of the word.
let unquote (home: string option) (word: string) =
    let sb = StringBuilder()
    let mutable i = 0
    let mutable quote = None

    match home with
    | Some h when word = "~" || word.StartsWith "~/" ->
        sb.Append(h) |> ignore
        i <- 1
    | _ -> ()

    while i < word.Length do
        let c = word.[i]

        match quote, c with
        | None, '\'' -> quote <- Some '\''
        | None, '"' -> quote <- Some '"'
        | None, '\\' when i + 1 < word.Length ->
            i <- i + 1
            sb.Append(word.[i]) |> ignore
        | Some '\'', '\'' -> quote <- None
        | Some '"', '"' -> quote <- None
        | Some '"', '\\' when i + 1 < word.Length && "\"\\$`".Contains word.[i + 1] ->
            i <- i + 1
            sb.Append(word.[i]) |> ignore
        | _ -> sb.Append c |> ignore

        i <- i + 1

    sb.ToString()

/// Where a command line leaves the word being completed.
type Position = {
    Node: Node
    /// The positional arguments before it.
    Positionals: string list
    /// What the word is.
    Expect: Expect
}

and Expect =
    /// The value of this option.
    | OptionValue of Value
    /// An option, a subcommand, or the positional argument at this index.
    | Word of index: int

/// Read the words before the one being completed (after `tess`).
let locate (root: Node) (words: string list) : Position =
    let rec go (node: Node) (positionals: string list) (words: string list) =
        match words with
        | [] -> {
            Node = node
            Positionals = List.rev positionals
            Expect = Word positionals.Length
          }
        | w :: rest ->
            match Node.tryChild node w with
            | Some child -> go child [] rest
            | None ->
                match
                    (if w.Length > 1 && w.StartsWith "-" then
                         Node.tryOption node w
                     else
                         None)
                with
                | Some { Value = Some v } ->
                    match rest with
                    | [] -> {
                        Node = node
                        Positionals = List.rev positionals
                        Expect = OptionValue v
                      }
                    | _ :: rest -> go node positionals rest
                | Some _ -> go node positionals rest
                | None when w.Length > 1 && w.StartsWith "-" -> go node positionals rest
                | None -> go node (w :: positionals) rest

    go root [] words

/// What the model offers for a value: references or names, from the model's own objects.
let private fromModel (value: Value) (objects: Inventory.ObjectEntry list) =
    let ofKind k =
        objects |> List.filter (fun e -> e.Ref.Kind = k)

    match value with
    | Objects [] -> objects |> List.map (fun e -> ObjectRef.format e.Ref)
    | Objects ks ->
        objects
        |> List.filter (fun e -> List.contains e.Ref.Kind ks)
        |> List.map (fun e -> ObjectRef.format e.Ref)
    | Tables -> ofKind ObjectKind.Table |> List.map (fun e -> ObjectRef.name e.Ref)
    | Roles -> ofKind ObjectKind.Role |> List.map (fun e -> ObjectRef.name e.Ref)
    | Perspectives -> ofKind ObjectKind.Perspective |> List.map (fun e -> ObjectRef.name e.Ref)
    | Text
    | Path
    | Directory
    | Choice _ -> []

/// The objects of a local model (never a server's: completion makes no connections).
let private localObjects (model: string) =
    if String.IsNullOrWhiteSpace model || XmlaAddress.isRemote model then
        []
    else
        match ModelIO.loadPath model with
        | Ok(_, db) -> Inventory.objects db
        | Error _ -> []

/// The candidates for the word being completed (`current`), given the words before it. Paths are
/// left to the shell. `objects` reads a model's objects (tests supply their own).
let candidatesWith
    (objects: string -> Inventory.ObjectEntry list)
    (root: Node)
    (words: string list)
    (current: string)
    : string list =
    let position = locate root words

    let complete (value: Value) =
        match value with
        | Choice values -> values
        | v when Value.isDynamic v ->
            // The model is the command's first positional argument (as for every command that has one).
            match position.Node.Positionals, position.Positionals with
            | Path :: _, model :: _ -> fromModel v (objects model)
            | _ -> []
        | _ -> []

    let all =
        match position.Expect with
        | OptionValue v -> complete v
        | Word _ when current.StartsWith "-" -> position.Node.Options |> List.map (fun o -> o.Name)
        | Word index ->
            let children =
                if index = 0 then
                    position.Node.Children |> List.map (fun c -> List.last c.Path)
                else
                    []

            children
            @ (Node.positional position.Node index
               |> Option.map complete
               |> Option.defaultValue [])

    all
    |> List.filter (fun c -> c.StartsWith(current, StringComparison.OrdinalIgnoreCase))
    |> List.distinct

/// `tess __complete`'s words travel to it as one argument (joined with U+001F, after a marker), so
/// Argu reads none of them as an option (`--json`, `--help`).
type CompleteArgs =
    | [<MainCommand>] Packed of words: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Packed _ -> "the words."

let private separator = "\u001F"
let private marker = "words:"

/// `__complete a b c` -> `__complete <a b c as one argument>`; any other command line unchanged.
let packWords (argv: string array) =
    if argv.Length > 0 && argv.[0] = "__complete" then
        [| "__complete"; marker + String.Join(separator, argv.[1..]) |]
    else
        argv

let private unpackWords (packed: string) =
    if packed.StartsWith marker then
        match packed.Substring marker.Length with
        | "" -> []
        | words -> words.Split(separator) |> List.ofArray
    else
        []

/// `tess __complete [--quoted] --current=<word> [--] <words before it...>`: candidates, one per
/// line. --quoted: the words are as typed in a POSIX shell (bash). Never fails and never
/// writes to stderr: any problem means no candidates.
let complete (parser: ArgumentParser) (ctx: Ctx) (args: ParseResults<CompleteArgs>) =
    try
        let rec flags (quoted, current) args =
            match args with
            | "--quoted" :: rest -> flags (true, current) rest
            | (a: string) :: rest when a.StartsWith "--current=" -> flags (quoted, a.Substring 10) rest
            | "--" :: rest -> quoted, current, rest
            | rest -> quoted, current, rest

        let quoted, current, words =
            flags (false, "") (unpackWords (args.TryGetResult Packed |> Option.defaultValue ""))

        let home =
            match ctx.GetEnv "HOME" with
            | Some h -> Some h
            | None -> ctx.GetEnv "USERPROFILE"

        let clean (w: string) = if quoted then unquote home w else w
        let words = words |> List.map clean
        let current = clean current

        let objects (model: string) =
            try
                localObjects model
            with _ -> []

        let text =
            candidatesWith objects (spec parser) words current
            |> List.map (fun c -> c + "\n")
            |> String.concat ""

        ctx.Out.Write text
        ctx.Out.Flush()
    with _ ->
        ()

    Done

// ---- The scripts ------------------------------------------------------------------------------

/// How a script handles a value: path, dir, text, choice <values...>, or dyn (ask `tess __complete`).
let private kindOf (value: Value) =
    match value with
    | Text -> "text"
    | Path -> "path"
    | Directory -> "dir"
    | Choice values -> "choice " + String.concat " " values
    | Objects _
    | Tables
    | Roles
    | Perspectives -> "dyn"

/// The first sentence of a help text, on one line.
let private brief (help: string) =
    let oneLine = help.Replace("\r", " ").Replace("\n", " ").Trim()

    // Not at "e.g. " or "i.e. ".
    let sentence =
        match RegularExpressions.Regex.Match(oneLine, @"(?<!\be\.g|\bi\.e)\. ") with
        | m when m.Success -> oneLine.Substring(0, m.Index)
        | _ -> oneLine

    sentence.TrimEnd('.')

/// For a single-quoted string in bash, zsh and fish (POSIX style: close, escaped quote, reopen).
let private posixQuote (s: string) = "'" + s.Replace("'", "'\\''") + "'"

/// For a single-quoted string in fish: only \\ and \' are escapes.
let private fishQuote (s: string) =
    "'" + s.Replace("\\", "\\\\").Replace("'", "\\'") + "'"

/// For a single-quoted string in PowerShell.
let private pwshQuote (s: string) = "'" + s.Replace("'", "''") + "'"

type private Tables = {
    /// (node key, [(name, help)]).
    Children: (string * (string * string) list) list
    Options: (string * (string * string) list) list
    /// (node key, option, kind) for options that take a value.
    Values: (string * string * string) list
    /// (node key, index or None for the rest, kind).
    Positionals: (string * int option * string) list
}

let private tablesOf (root: Node) =
    let nodes = Node.all root

    {
        Children = [
            for n in nodes do
                if not n.Children.IsEmpty then
                    Node.key n, n.Children |> List.map (fun c -> List.last c.Path, brief c.Help)
        ]
        Options = [
            for n in nodes -> Node.key n, n.Options |> List.map (fun o -> o.Name, brief o.Help)
        ]
        Values = [
            for n in nodes do
                for o in n.Options do
                    match o.Value with
                    | Some v -> Node.key n, o.Name, kindOf v
                    | None -> ()
        ]
        Positionals = [
            for n in nodes do
                for i, v in List.indexed n.Positionals do
                    Node.key n, Some i, kindOf v

                match n.Rest with
                | Some v -> Node.key n, None, kindOf v
                | None -> ()
        ]
    }

let private header (shell: string) =
    $"# {shell} completion for tess. Generated by `tess completion {shell}`: run it again after upgrading tess.\n"

/// Lines joined with LF, whatever the platform.
let private lines (ls: string seq) = String.concat "\n" ls + "\n"

let bash (root: Node) =
    let t = tablesOf root

    let caseArms (arms: (string * string) seq) = [
        for pattern, value in arms do
            $"        {pattern}) _tess_r={posixQuote value} ;;"
        "        *) _tess_r='' ;;"
    ]

    lines [
        header "bash"
        "# Install: source it from ~/.bashrc, or save it as ~/.local/share/bash-completion/completions/tess."
        ""
        "_tess_children() {"
        "    case $1 in"
        yield!
            caseArms (
                t.Children
                |> List.map (fun (k, cs) -> posixQuote k, cs |> List.map fst |> String.concat " ")
            )
        "    esac"
        "}"
        ""
        "_tess_options() {"
        "    case $1 in"
        yield!
            caseArms (
                t.Options
                |> List.map (fun (k, os) -> posixQuote k, os |> List.map fst |> String.concat " ")
            )
        "    esac"
        "}"
        ""
        "# The value an option takes (node|option): path, dir, text, choice <values>, dyn; empty for a flag."
        "_tess_value() {"
        "    case $1\\|$2 in"
        yield! caseArms (t.Values |> List.map (fun (k, o, kind) -> posixQuote $"{k}|{o}", kind))
        "    esac"
        "}"
        ""
        "# The positional argument at an index (node|index)."
        "_tess_positional() {"
        "    case $1\\|$2 in"
        yield!
            caseArms (
                t.Positionals
                |> List.map (fun (k, i, kind) ->
                    match i with
                    | Some i -> posixQuote $"{k}|{i}", kind
                    | None -> posixQuote $"{k}|" + "*", kind)
            )
        "    esac"
        "}"
        ""
        "# Add the lines of $1 to COMPREPLY, escaped for the command line unless the word is quoted, and"
        "# less what bash has already split off at ':' or '=' (it replaces only the rest)."
        "_tess_reply() {"
        "    local c strip=${_tess_cur%\"${COMP_WORDS[COMP_CWORD]}\"}"
        "    while IFS= read -r c; do"
        "        [[ -n $c ]] || continue"
        "        if [[ $_tess_cur != [\\'\\\"]* ]]; then printf -v c '%q' \"$c\"; fi"
        "        COMPREPLY+=(\"${c#\"$strip\"}\")"
        "    done <<< \"$1\""
        "}"
        ""
        "_tess() {"
        "    local _tess_r _tess_cur words=() i w line trimmed node='' pos=0 skip='' kind='' children=''"
        "    COMPREPLY=()"
        "    # The words up to the cursor, joining back what bash split at ':' and '=' (measure:Sales/Total)."
        "    line=${COMP_LINE:0:COMP_POINT}"
        "    for ((i = 0; i <= COMP_CWORD; i++)); do"
        "        w=${COMP_WORDS[i]}"
        "        trimmed=${line#\"${line%%[![:space:]]*}\"}"
        "        if ((i > 0)) && [[ ${#trimmed} -eq ${#line} ]] && ((${#words[@]} > 0)); then"
        "            words[${#words[@]}-1]+=$w"
        "        else"
        "            words+=(\"$w\")"
        "        fi"
        "        line=${trimmed#\"$w\"}"
        "    done"
        "    _tess_cur=${words[${#words[@]}-1]}"
        "    for ((i = 1; i < ${#words[@]} - 1; i++)); do"
        "        w=${words[i]}"
        "        if [[ -n $skip ]]; then skip=''; continue; fi"
        "        _tess_children \"$node\""
        "        if [[ \" $_tess_r \" == *\" $w \"* ]]; then node=${node:+$node }$w; pos=0; continue; fi"
        "        if [[ $w == -?* ]]; then"
        "            _tess_value \"$node\" \"$w\""
        "            [[ -n $_tess_r ]] && skip=1"
        "            continue"
        "        fi"
        "        pos=$((pos + 1))"
        "    done"
        "    if [[ -n $skip ]]; then"
        "        _tess_value \"$node\" \"${words[${#words[@]}-2]}\"; kind=$_tess_r"
        "    elif [[ $_tess_cur == -* ]]; then"
        "        _tess_options \"$node\""
        "        COMPREPLY=($(compgen -W \"$_tess_r\" -- \"$_tess_cur\"))"
        "        return"
        "    else"
        "        _tess_positional \"$node\" \"$pos\"; kind=$_tess_r"
        "        if ((pos == 0)); then _tess_children \"$node\"; children=$_tess_r; fi"
        "    fi"
        "    if [[ -n $children ]]; then COMPREPLY=($(compgen -W \"$children\" -- \"$_tess_cur\")); fi"
        "    case $kind in"
        "        choice\\ *) COMPREPLY+=($(compgen -W \"${kind#choice }\" -- \"$_tess_cur\")) ;;"
        "        dyn) _tess_reply \"$(\"${COMP_WORDS[0]}\" __complete --quoted \"--current=$_tess_cur\" -- \"${words[@]:1:${#words[@]}-2}\" 2>/dev/null)\" ;;"
        "        dir) if ((${#COMPREPLY[@]} == 0)) && type compopt &>/dev/null; then compopt -o dirnames; fi ;;"
        "    esac"
        "    # Nothing else to offer for a path: bash completes file names (complete -o default)."
        "}"
        ""
        "complete -o bashdefault -o default -F _tess tess"
    ]

let zsh (root: Node) =
    let t = tablesOf root

    let describedArms (rows: (string * (string * string) list) list) = [
        for k, items in rows do
            let entries =
                items
                |> List.map (fun (name, help) -> posixQuote (name.Replace(":", "\\:") + ":" + help))
                |> String.concat " "

            $"        ({posixQuote k}) reply=({entries}) ;;"
        "        (*) reply=() ;;"
    ]

    let arms (rows: (string * string) seq) = [
        for pattern, value in rows do
            $"        ({pattern}) REPLY={posixQuote value} ;;"
        "        (*) REPLY='' ;;"
    ]

    lines [
        "#compdef tess"
        header "zsh"
        "# Install: save it as _tess in a folder on $fpath (before compinit runs), or source it from ~/.zshrc"
        "# after compinit."
        ""
        "_tess_children() {"
        "    case $1 in"
        yield! describedArms t.Children
        "    esac"
        "}"
        ""
        "_tess_options() {"
        "    case $1 in"
        yield! describedArms t.Options
        "    esac"
        "}"
        ""
        "# The value an option takes (node|option): path, dir, text, choice <values>, dyn; empty for a flag."
        "_tess_value() {"
        "    case \"$1|$2\" in"
        yield! arms (t.Values |> List.map (fun (k, o, kind) -> posixQuote $"{k}|{o}", kind))
        "    esac"
        "}"
        ""
        "# The positional argument at an index (node|index)."
        "_tess_positional() {"
        "    case \"$1|$2\" in"
        yield!
            arms (
                t.Positionals
                |> List.map (fun (k, i, kind) ->
                    match i with
                    | Some i -> posixQuote $"{k}|{i}", kind
                    | None -> posixQuote $"{k}|" + "*", kind)
            )
        "    esac"
        "}"
        ""
        "_tess() {"
        "    local node='' pos=0 skip='' kind='' i w"
        "    local -a reply names candidates"
        "    for ((i = 2; i < CURRENT; i++)); do"
        "        w=${(Q)words[i]}"
        "        if [[ -n $skip ]]; then skip=''; continue; fi"
        "        _tess_children \"$node\"; names=(${reply%%:*})"
        "        if ((${names[(Ie)$w]})); then node=${node:+$node }$w; pos=0; continue; fi"
        "        if [[ $w == -?* ]]; then"
        "            _tess_value \"$node\" \"$w\""
        "            [[ -n $REPLY ]] && skip=1"
        "            continue"
        "        fi"
        "        ((pos++))"
        "    done"
        "    if [[ -n $skip ]]; then"
        "        _tess_value \"$node\" \"${(Q)words[CURRENT-1]}\"; kind=$REPLY"
        "    elif [[ $PREFIX == -* ]]; then"
        "        _tess_options \"$node\"; _describe -t options option reply"
        "        return"
        "    else"
        "        _tess_positional \"$node\" \"$pos\"; kind=$REPLY"
        "        if ((pos == 0)); then _tess_children \"$node\"; ((${#reply})) && _describe -t commands command reply; fi"
        "    fi"
        "    case $kind in"
        "        (path) _files ;;"
        "        (dir) _files -/ ;;"
        "        (choice\\ *) candidates=(${=kind#choice }); compadd -a candidates ;;"
        "        (dyn)"
        "            candidates=(\"${(@f)$(\"${words[1]}\" __complete \"--current=${(Q)PREFIX}\" -- \"${(@Q)words[2,CURRENT-1]}\" 2>/dev/null)}\")"
        "            candidates=(${candidates:#})"
        "            compadd -a candidates ;;"
        "    esac"
        "}"
        ""
        "if [[ $zsh_eval_context[-1] == loadautofunc ]]; then"
        "    _tess \"$@\""
        "else"
        "    compdef _tess tess"
        "fi"
    ]

let fish (root: Node) =
    let t = tablesOf root

    let describedArms (rows: (string * (string * string) list) list) = [
        for k, items in rows do
            $"        case {fishQuote k}"

            for name, help in items do
                $"            printf '%%s\\t%%s\\n' {fishQuote name} {fishQuote help}"
    ]

    let arms (rows: (string * string) seq) = [
        for pattern, value in rows do
            $"        case {pattern}"
            $"            echo {fishQuote value}"
    ]

    lines [
        header "fish"
        "# Install: save it as ~/.config/fish/completions/tess.fish."
        ""
        "function __tess_children"
        "    switch $argv[1]"
        yield! describedArms t.Children
        "    end"
        "end"
        ""
        "function __tess_options"
        "    switch $argv[1]"
        yield! describedArms t.Options
        "    end"
        "end"
        ""
        "# The value an option takes (node|option): path, dir, text, choice <values>, dyn; nothing for a flag."
        "function __tess_value"
        "    switch \"$argv[1]|$argv[2]\""
        yield! arms (t.Values |> List.map (fun (k, o, kind) -> fishQuote $"{k}|{o}", kind))
        "    end"
        "end"
        ""
        "# The positional argument at an index (node|index)."
        "function __tess_positional"
        "    switch \"$argv[1]|$argv[2]\""
        yield!
            arms (
                t.Positionals
                // fish runs the first case that matches: an index before the rest.
                |> List.sortBy (fun (_, i, _) -> i.IsNone)
                |> List.map (fun (k, i, kind) ->
                    match i with
                    | Some i -> fishQuote $"{k}|{i}", kind
                    | None -> fishQuote $"{k}|" + "'*'", kind)
            )
        "    end"
        "end"
        ""
        "function __tess_complete"
        "    set -l tokens (commandline -opc)"
        "    set -l cur (commandline -ct)"
        "    set -l tess $tokens[1]"
        "    set -e tokens[1]"
        "    set -l node ''"
        "    set -l pos 0"
        "    set -l skip 0"
        "    for w in $tokens"
        "        if test $skip = 1"
        "            set skip 0"
        "            continue"
        "        end"
        "        if contains -- $w (__tess_children $node | string replace -r '\\t.*' '')"
        "            set node (string trim -- \"$node $w\")"
        "            set pos 0"
        "            continue"
        "        end"
        "        if string match -qr -- '^-.' $w"
        "            set -l value (__tess_value $node $w)"
        "            if test -n \"$value\""
        "                set skip 1"
        "            end"
        "            continue"
        "        end"
        "        set pos (math $pos + 1)"
        "    end"
        "    set -l kind ''"
        "    if test $skip = 1"
        "        set kind (__tess_value $node $tokens[-1])"
        "    else if string match -qr -- '^-' $cur"
        "        __tess_options $node"
        "        return"
        "    else"
        "        set kind (__tess_positional $node $pos)"
        "        if test $pos = 0"
        "            __tess_children $node"
        "        end"
        "    end"
        "    switch \"$kind\""
        "        case path"
        "            __fish_complete_path $cur"
        "        case dir"
        "            __fish_complete_directories $cur"
        "        case 'choice *'"
        "            string split ' ' -- (string replace 'choice ' '' -- $kind)"
        "        case dyn"
        "            # The words are unescaped already (commandline -o); the current one, as typed."
        "            set -l current (string unescape -- $cur)"
        "            $tess __complete \"--current=$current\" -- $tokens 2>/dev/null"
        "    end"
        "end"
        ""
        "complete -c tess -f -a '(__tess_complete)'"
    ]

let pwsh (root: Node) =
    let t = tablesOf root

    let described (rows: (string * (string * string) list) list) = [
        for k, items in rows do
            let entries =
                items
                |> List.map (fun (name, help) -> $"@{{ n = {pwshQuote name}; h = {pwshQuote help} }}")
                |> String.concat ", "

            $"        {pwshQuote k} = @({entries})"
    ]

    let pairs (rows: (string * string) seq) = [
        for key, value in rows do
            $"        {pwshQuote key} = {pwshQuote value}"
    ]

    lines [
        header "pwsh"
        "# Install: add `tess completion pwsh | Out-String | Invoke-Expression` to your $PROFILE, or save it"
        "# to a file and dot-source that from $PROFILE."
        ""
        "Register-ArgumentCompleter -Native -CommandName tess -ScriptBlock {"
        "    param($wordToComplete, $commandAst, $cursorPosition)"
        "    $children = @{"
        yield! described t.Children
        "    }"
        "    $options = @{"
        yield! described t.Options
        "    }"
        "    # The value an option takes (node|option): path, dir, text, choice <values>, dyn."
        "    $values = @{"
        yield! pairs (t.Values |> List.map (fun (k, o, kind) -> $"{k}|{o}", kind))
        "    }"
        "    # The positional argument at an index (node|index; node|* for the rest)."
        "    $positionals = @{"
        yield!
            pairs (
                t.Positionals
                |> List.map (fun (k, i, kind) ->
                    match i with
                    | Some i -> $"{k}|{i}", kind
                    | None -> $"{k}|*", kind)
            )
        "    }"
        ""
        "    # The word without its quotes (PowerShell closes an open quote in $wordToComplete)."
        "    $current = $wordToComplete -replace '^([''\"])(.*?)\\1?$', '$2'"
        "    $words = @()"
        "    foreach ($e in $commandAst.CommandElements | Select-Object -Skip 1) {"
        "        if ($e.Extent.EndOffset -gt $cursorPosition) { break }"
        "        if ($wordToComplete -ne '' -and $e.Extent.EndOffset -eq $cursorPosition) { break }"
        "        if ($e -is [System.Management.Automation.Language.StringConstantExpressionAst]) { $words += $e.Value }"
        "        else { $words += $e.Extent.Text }"
        "    }"
        "    $node = ''; $pos = 0; $skip = $false"
        "    foreach ($w in $words) {"
        "        if ($skip) { $skip = $false; continue }"
        "        if ($children.ContainsKey($node) -and ($children[$node] | Where-Object { $_.n -ceq $w })) {"
        "            $node = ($node + ' ' + $w).Trim(); $pos = 0; continue"
        "        }"
        "        if ($w -match '^-.') { if ($values.ContainsKey(\"$node|$w\")) { $skip = $true }; continue }"
        "        $pos++"
        "    }"
        ""
        "    $result = [System.Collections.Generic.List[System.Management.Automation.CompletionResult]]::new()"
        "    $add = {"
        "        param($text, $type, $tip)"
        "        if (-not $text.StartsWith($current, [System.StringComparison]::OrdinalIgnoreCase)) { return }"
        "        $insert = if ($text -match '[\\s''\"`$#;,(){}@|&<>]') { \"'\" + $text.Replace(\"'\", \"''\") + \"'\" } else { $text }"
        "        if (-not $tip) { $tip = $text }"
        "        $result.Add([System.Management.Automation.CompletionResult]::new($insert, $text, $type, $tip))"
        "    }"
        "    $kind = ''"
        "    if ($skip) {"
        "        $kind = $values[\"$node|$($words[-1])\"]"
        "    } elseif ($wordToComplete.StartsWith('-')) {"
        "        foreach ($o in $options[$node]) { & $add $o.n 'ParameterName' $o.h }"
        "        return $result"
        "    } else {"
        "        $kind = $positionals[\"$node|$pos\"]"
        "        if (-not $kind) { $kind = $positionals[\"$node|*\"] }"
        "        if ($pos -eq 0 -and $children.ContainsKey($node)) { foreach ($c in $children[$node]) { & $add $c.n 'Command' $c.h } }"
        "    }"
        "    if ($kind -like 'choice *') {"
        "        foreach ($v in $kind.Substring(7).Split(' ')) { & $add $v 'ParameterValue' $null }"
        "    } elseif ($kind -eq 'dyn') {"
        "        $exe = $commandAst.CommandElements[0].Extent.Text"
        "        foreach ($c in (& $exe __complete \"--current=$current\" -- @words 2>$null)) { if ($c) { & $add $c 'ParameterValue' $null } }"
        "    }"
        "    # For a path, no results: PowerShell completes file names itself."
        "    if ($result.Count -gt 0) { return $result }"
        "}"
    ]

/// The script for a shell.
let script (shell: Shell) (root: Node) =
    match shell with
    | Bash -> bash root
    | Zsh -> zsh root
    | Fish -> fish root
    | Pwsh -> pwsh root

let run (parser: ArgumentParser) (ctx: Ctx) (args: ParseResults<Args>) =
    // A script, not a document: --json doesn't apply.
    ctx.Out.Write(script (args.GetResult Args.Shell) (spec parser))
    ctx.Out.Flush()
    Done
