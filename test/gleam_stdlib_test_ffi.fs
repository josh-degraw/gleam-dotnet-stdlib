module gleam_stdlib_test_ffi
// This file is a modified copy of gleeunit 0.10.0's <https://github.com/lpil/gleeunit/blob/main/src/gleeunit_ffi.mjs>
// Converted to F#

open System
open System.Reflection
open System.IO

let printGreen (message: string) =
    Console.Write("\u001b[32m{0}\u001b[0m", message)

let printRed (message: string) =
    Console.Write("\u001b[31m{0}\u001b[0m", message)

let printlnGreen (message: string) =
    Console.WriteLine("\u001b[32m{0}\u001b[0m", message)

let printlnRed (message: string) =
    Console.WriteLine("\u001b[31m{0}\u001b[0m", message)

// let runTest (moduleType: Type) =
//     printfn "module: %s" moduleType.Name
//     let methods = moduleType.GetMethods(BindingFlags.Public ||| BindingFlags.Static)

//     let mutable totalTests = 0

//     let passed =
//         seq {
//             for m in methods do
//                 if m.ReturnType = typeof<unit> || m.ReturnType = typeof<Void> then
//                     totalTests <- totalTests + 1

//                     try
//                         m.Invoke(null, null) |> ignore
//                         sprintf "Passed: %s.%s" moduleType.Name m.Name |> printLnGreen
//                         1
//                     with
//                     | :? System.Reflection.TargetInvocationException as e ->
//                         sprintf "Failed: %s.%s\n%O" moduleType.Name m.Name e.InnerException |> printRed
//                         0
//                     | e ->
//                         sprintf "Failed: %s.%s\n%O" moduleType.Name m.Name e |> printRed
//                         0
//         }
//         |> Seq.sum

//     totalTests, passed

// Main function to run tests
let main () =
    let mutable passes = 0
    let mutable failures = 0

    let all_test_types = Assembly.GetEntryAssembly().GetTypes()

    printfn "all_test_types: %A" all_test_types

    let all_test_types =
        all_test_types |> Array.filter (fun t -> t.Name.EndsWith("_test"))

    for moduleType in all_test_types do
        let methods = moduleType.GetMethods(BindingFlags.Public ||| BindingFlags.Static)

        for m in methods do
            if m.ReturnType = typeof<unit> || m.ReturnType = typeof<Void> then
                try
                    m.Invoke(null, null) |> ignore
                    sprintf "✅%s.%s" moduleType.Name m.Name |> printlnGreen
                    passes <- passes + 1
                with
                | :? System.Reflection.TargetInvocationException as e ->
                    printfn "\n❌ %s.%s" moduleType.Name m.Name
                    sprintf "%O" e.InnerException |> printlnRed
                    failures <- failures + 1
                | e ->
                    printfn "\n❌ %s.%s" moduleType.Name m.Name
                    sprintf "%O" e |> printlnRed
                    failures <- failures + 1

    sprintf "\n%d/%d tests passed, %d failures" passes (passes + failures) failures
    |> if failures > 0 then printlnRed else printlnGreen

    Environment.Exit(failures)
    failures
