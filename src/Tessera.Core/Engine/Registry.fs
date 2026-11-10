namespace Tessera.DaxEngine

open System
open System.Collections.Generic
open Tessera.DaxEngine.Functions

/// Every function the engine implements, by name. To add a family: write
/// `Functions/<Family>.fs` with a `functions: DaxFunction list`, add its `<Compile>` line before
/// this file, and add it to `all`.
module Registry =

    let all: DaxFunction list =
        List.concat [
            Aggregation.functions
            Logical.functions
            Math.functions
            Text.functions
            DateAndTime.functions
            Formatting.functions
            Filter.functions
            TableFunctions.functions
        ]

    let byName: IReadOnlyDictionary<string, DaxFunction> =
        let d = Dictionary<string, DaxFunction>(StringComparer.OrdinalIgnoreCase)

        for f in all do
            if not (d.TryAdd(f.Name, f)) then
                invalidOp $"The DAX function {f.Name} is registered twice"

        d :> IReadOnlyDictionary<string, DaxFunction>

    /// The one evaluator: it holds no state of its own.
    let evaluator: IEval = Evaluator(byName) :> IEval
