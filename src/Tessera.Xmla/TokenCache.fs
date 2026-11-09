/// The opt-in sign-in cache file (TESS_XMLA_TOKEN_CACHE). It holds refresh tokens, so it is created
/// readable and writable by its owner only (0600) on Unix; on Windows it gets the folder's access
/// rules, so keep it under the user profile. Without the opt-in nothing is ever written to disk.
module Tessera.Xmla.TokenCache

open System
open System.IO

/// Read the cache; None if there is none yet.
let read (path: string) : byte array option =
    if File.Exists path then
        Some(File.ReadAllBytes path)
    else
        None

/// Write the cache atomically, owner-only.
let write (path: string) (data: byte array) : unit =
    let full = Path.GetFullPath path

    match Path.GetDirectoryName full with
    | null
    | "" -> ()
    | dir -> Directory.CreateDirectory dir |> ignore

    let temp = full + ".tmp"

    let options =
        FileStreamOptions(Mode = FileMode.Create, Access = FileAccess.Write, Share = FileShare.None)

    if not (OperatingSystem.IsWindows()) then
        options.UnixCreateMode <- UnixFileMode.UserRead ||| UnixFileMode.UserWrite

    if File.Exists temp then
        File.Delete temp

    using (new FileStream(temp, options)) (fun stream -> stream.Write(data, 0, data.Length))

    if not (OperatingSystem.IsWindows()) then
        // The mode above applies only when the file is created; make sure of it either way.
        File.SetUnixFileMode(temp, UnixFileMode.UserRead ||| UnixFileMode.UserWrite)

    File.Move(temp, full, true)
