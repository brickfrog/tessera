/// Shared output helpers. Nothing in Tessera.Core writes output itself; commands render here.
module Tessera.Cli.Output

open System.IO
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Nodes
open Tessera

let jsonOptions =
    JsonSerializerOptions(WriteIndented = true, Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

let str (s: string) : JsonNode = JsonValue.Create s
let num (n: int) : JsonNode = JsonValue.Create n
let bool (b: bool) : JsonNode = JsonValue.Create b

let optStr =
    function
    | Some s -> str s
    | None -> null

let obj (props: (string * JsonNode) list) : JsonNode =
    let o = JsonObject()

    for k, v in props do
        o.[k] <- v

    o

let arr (items: JsonNode seq) : JsonNode = JsonArray(Array.ofSeq items)

let writeJson (out: TextWriter) (node: JsonNode) =
    out.WriteLine(node.ToJsonString jsonOptions)

let sourceJson (s: ModelSource) =
    obj [
        "format", str (ModelFormat.token s.Format)
        "path", str s.Path
        "semanticModelFolder", optStr s.SemanticModelFolder
    ]

let errorJson (code: string) (message: string) =
    obj [ "error", obj [ "code", str code; "message", str message ] ]

let versionJson (v: string) = obj [ "version", str v ]

let tessErrorJson (e: TessError) =
    errorJson (TessError.code e) (TessError.message e)

/// "ref" and "kind" fields for an object.
let refFields (e: Inventory.ObjectEntry) = [
    "ref", str (ObjectRef.format e.Ref)
    "kind", str (ObjectKind.token e.Ref.Kind)
]
