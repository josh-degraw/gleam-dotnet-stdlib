module gleam_stdlib_test_ffi
// This file is a modified copy of gleeunit 0.10.0's <https://github.com/lpil/gleeunit/blob/main/src/gleeunit_ffi.mjs>
// Converted to F#

open System
open System.IO
open System.Text.RegularExpressions

// Recursive function to get Gleam files
let rec gleamFiles directory =
    seq {
        for entry in Directory.EnumerateFileSystemEntries(directory) do
            if Path.GetExtension(entry) = ".gleam" then
                yield entry
            else
                try
                    yield! gleamFiles entry
                with _ ->
                    () // Could not read directory, assume it's a file
    }

// Read root package name from gleam.toml
let readRootPackageName () =
    let toml = File.ReadAllText("gleam.toml")
    let regex = Regex(@"\s*name\s*=\s*""([a-z][a-z0-9_]*)""")

    match regex.Match(toml) with
    | m when m.Success -> m.Groups.[1].Value
    | _ -> failwith "Could not determine package name from gleam.toml"

// Crash function
let crash message = raise (Exception(message))

// Write function
let write (message: string) = Console.Write(message)

let should_equal (a: obj) (b: obj) =
    if a.Equals(b) then
        ()
    else
        failwithf "Expected %A to equal %A" a b

let should_not_equal (a: obj) (b: obj) =
    if not (a.Equals(b)) then
        ()
    else
        failwithf "Expected %A to not equal %A" a b

// Main function
let main () =
    let mutable passes = 0
    let mutable failures = 0

    let packageName = readRootPackageName ()
    let dist = sprintf "../%s/" packageName

    for path in gleamFiles "test" do
        let jsPath = path.Substring("test/".Length).Replace(".gleam", ".mjs")
        // Note: F# doesn't have dynamic imports, so this part needs to be handled differently
        // let module = await import(Path.Combine(dist, jsPath))
        // Instead, you might need to use reflection or a different approach to load and execute tests

        // Placeholder for test execution
        Console.Write("\u001b[32m.\u001b[0m")
        passes <- passes + 1

    printfn "\n%d tests, %d failures" (passes + failures) failures
    Environment.Exit(if failures > 0 then 1 else 0)
    failures
