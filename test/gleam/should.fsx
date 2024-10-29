// This is only loaded here to aid during development of the standard library itself
// where changing the file to an fsx file yields better type information
// #if INTERACTIVE
#r "nuget: Xunit, 2.5.2"
// #endif
namespace Gleam

open System.Text
open System
open Xunit

module Should =
    open System.Collections

    open System.Reflection
    open System.Collections.Generic

    let bindingFlags = BindingFlags.Public ||| BindingFlags.NonPublic

    let (|UnionType|_|) (value: obj) =
        FSharp.Reflection.FSharpType.IsUnion(value.GetType(), bindingFlags)
        |> function
            | true -> Some(FSharp.Reflection.FSharpValue.GetUnionFields(value, value.GetType(), bindingFlags))
            | false -> None

    let inline assertThat fn (actual: 'a) (expected: 'a) =
        if fn actual expected |> not then
            match box actual, box expected with
            | :? StringBuilder as actual, (:? StringBuilder as expected) ->
                Xunit.Assert.Equal(string expected, string actual)
            | :? string as actual, (:? string as expected) -> Xunit.Assert.Equal(expected, actual)
            | UnionType(actualCase, actualFields), UnionType(expectedCase, expectedFields) ->
                Xunit.Assert.Equal(expectedCase.Name, actualCase.Name)
                Xunit.Assert.Equal<obj array>(expectedFields, actualFields)
            | _ -> Xunit.Assert.Equal<'a>(expected, actual)

    let inline failIf fn (actual: 'a) (expected: 'a) =
        if fn actual expected then
            match box actual, box expected with
            | :? StringBuilder as actual, (:? StringBuilder as expected) ->
                Xunit.Assert.NotEqual<string>(string expected, string actual)
            | :? string as actual, (:? string as expected) -> Xunit.Assert.NotEqual<string>(expected, actual)
            | _ -> Xunit.Assert.NotEqual<'a>(expected, actual)

    let objectEquality (a: obj) (b: obj) =
        match a, b with
        | null, null -> true
        | null, _
        | _, null -> false
        | a', b' -> EqualityComparer.Default.Equals(a', b')

    let rec performCheck (assertionFn: ('a -> 'a -> bool) -> obj -> obj -> unit) (a: obj) (b: obj) =
        match box a, box b with
        | null, null -> ()
        | null, _
        | _, null -> assertionFn (fun _ _ -> false) a b
        | :? bool as a', (:? bool as b') -> assertionFn (=) a' b'
        | :? float as a', (:? float as b') -> assertionFn (=) a' b'
        | :? int as a', (:? int as b') -> assertionFn (=) a' b'
        | :? int64 as a', (:? int64 as b') -> assertionFn (=) a' b'
        | :? StringBuilder as a', (:? StringBuilder as b') -> assertionFn (=) (string a') (string b')
        | :? string as a', (:? string as b') -> assertionFn (=) a' b'
        | UnionType(aCase, aFields), UnionType(bCase, bFields) ->

            let aStr = sprintf "%s%A" aCase.Name (List.ofArray aFields)
            let bStr = sprintf "%s%A" bCase.Name (List.ofArray bFields)

            assertionFn (=) aStr bStr

            for i in 0 .. aFields.Length - 1 do
                let a' = aFields[i]
                let b' = bFields[i]

                performCheck assertionFn a' b'

        | :? IStructuralEquatable as a', (:? IStructuralEquatable as b') ->
            assertionFn (fun _ _ -> a'.Equals(b', StructuralComparisons.StructuralEqualityComparer)) a' b'

        //| :? IEquatable<'a> as a', (:? IEquatable<'a> as b') -> assertEquals a' b'
        | :? IEnumerable as a', (:? IEnumerable as b') ->
            let a' = a'.GetEnumerator()
            let b' = b'.GetEnumerator()

            let rec loop () =
                let aHasNext = a'.MoveNext()
                let bHasNext = b'.MoveNext()

                if aHasNext && bHasNext then
                    performCheck assertionFn a'.Current b'.Current
                    loop ()

            loop ()

        | _, _ -> assertionFn objectEquality a b

    let equal (a: obj) (b: obj) = performCheck assertThat a b

    let not_equal (a: 'a) (b: 'a) = performCheck failIf a b

    let be_ok<'a, 'b> (a: Result<'a, 'b>) : 'a =
        match a with
        | Ok(value) -> value
        | Error(err) ->
            Xunit.Assert.Fail $"\n%A{err}\nshould be ok"
            failwith "Impossible to reach this line"

    let be_error<'a, 'b> (a: Result<'a, 'b>) =
        match a with
        | Error(_) -> ()
        | Ok(_) -> Xunit.Assert.Fail $"\n%A{a}\nshould be error"

    let be_true (actual: bool) = Xunit.Assert.True actual

    let be_false (actual: bool) = Xunit.Assert.False actual

    let fail () = Xunit.Assert.Fail "Expected failure"
