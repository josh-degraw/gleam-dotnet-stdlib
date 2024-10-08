// This is only loaded here to aid during development of the standard library itself
// where changing the file to an fsx file yields better type information
#if INTERACTIVE
#load "../../../compiler-core/src/fsharp/prelude.fs"
#endif
namespace Gleam

open gleam
open gleam.Prelude
open System

module Util =
    let option_of_string (value: string) =
        if String.IsNullOrEmpty value then None else Some(value)

    let result_of_option (value: 'a option) =
        match value with
        | Some(v) -> Ok(v)
        | None -> Error()

module Map =
    let size = Map.count
    let is_empty = Map.isEmpty
    let to_list = Map.toList
    let from_list = Map.ofList
    let has_key (key: 'a) (map: Map<'a, 'b>) = Map.containsKey key map
    let empty () = Map.empty

    let get map key =
        Map.tryFind key map |> Util.result_of_option

    let insert (key: 'a) (value: 'b) (map: Map<'a, 'b>) = Map.add key value map

    let map_values f dict = Map.map f dict

    let keys (dict: Map<'a, 'b>) = Map.keys dict |> List.ofSeq

    let values (dict: Map<'a, 'b>) = Map.values dict |> List.ofSeq

    let filter (predicate: 'a -> 'b -> bool) (dict: Map<'a, 'b>) = Map.filter predicate dict

    let each (list: Map<'a, 'b>) (f: 'a -> 'b -> 'c) : unit =
        Map.iter (fun a b -> f a b |> ignore) list

    let remove (key: 'a) (dict: Map<'a, 'b>) = Map.remove key dict

module Set =
    let init () = Set.empty
    let size set = Set.count set |> int64
    let is_empty set = Set.isEmpty set
    let insert set key = Set.add key set
    let has_key set key = Set.contains key set
    let delete set key = Set.remove key set
    let to_list set = Set.toList set
    let from_list set = Set.ofList set
    let fold set acc reducer = Set.fold reducer acc set
    let filter set predicate = Set.filter predicate set
    let map set f = Set.map f set
    let drop set keys = Set.difference set (Set.ofList keys)
    let take set keys = Set.intersect set (Set.ofList keys)

    let order first second =
        if Set.count first > Set.count second then
            (first, second)
        else
            (second, first)

    let union first second = Set.union first second
    let intersection first second = Set.intersect first second
    let difference first second = Set.difference first second


module Float =

    let numberFormatInfo = System.Globalization.NumberFormatInfo.CurrentInfo

    /// For some reason gleam float parsing expects string values must _always_ be floating point
    /// (include a decimal point)
    let parse_float (a: string) =
        if not (a.Contains numberFormatInfo.NumberDecimalSeparator) then
            Error()
        else
            match System.Double.TryParse(a) with
            | (true, v) -> Ok v
            | (false, _) -> Error()

    let floor (a: float) = floor (a)

    let ceiling (a: float) = ceil (a)

    // For some reason gleam rounding doesn't produce the same output as .NET rounding so we need to customize a bit
    let round (a: float) =
        if a >= 0 then
            if (a - floor (a)) < 0.49999 then round (a) else ceiling (a)
            |> int64
        else
            if (a - ceiling (a)) < 0.49999 then round (a) else floor (a)
            |> int64

    let inline to_float (a) = float (a)

    let power (base': float) (exponent: float) = base' ** exponent

    let random () = System.Random().NextDouble()

    let sqrt (a: float) = sqrt (a)

    let truncate (a: float) = int64 (a)

    let to_string (a: float) = a.ToString("F1")

module Int =

    let parse (a: string) =
        try
            Ok(System.Convert.ToInt64(a))
        with _ ->
            Error()

    let base_parse (a: string) (b: int64) =
        try
            Ok(System.Convert.ToInt64(a, int b))
        with _ ->
            Error()

    let to_string (a: int64) = a.ToString()

    let to_base_string (a: int64) (b: int64) = System.Convert.ToString(a, int b)

    let inline to_float a = float (a)

    let bitwise_and (x: int64) (y: int64) = x &&& y

    let bitwise_not (x: int64) = ~~~x

    let bitwise_or (x: int64) (y: int64) = x ||| y

    let bitwise_exclusive_or (x: int64) (y: int64) = x ^^^ y

    let bitwise_shift_left (x: int64) (y: int64) = x <<< int y

    let bitwise_shift_right (x: int64) (y: int64) = x >>> int y


module StringBuilder =

    open System.Text


    let init () = StringBuilder()


    let append (a: StringBuilder) (b: StringBuilder) = a.Append(b)

    let from_strings (strings: string list) =
        let sb = StringBuilder()

        for s in strings do
            sb.Append(s) |> ignore

        sb

    let concat (builders: StringBuilder list) =
        let sb = StringBuilder()

        for b in builders do
            sb.Append(b) |> ignore

        sb

    let from_string (a: string) = StringBuilder(a)

    let to_string (a: StringBuilder) = a.ToString()

    let byte_size (a: StringBuilder) = a.Length |> int64

    let lowercase (a: StringBuilder) =
        for i in 0 .. a.Length do
            a[i] <- System.Char.ToLower(a[i])

        a

    let uppercase (a: StringBuilder) =
        for i in 0 .. a.Length do
            a[i] <- System.Char.ToUpper(a[i])

        a

    let reverse (a: StringBuilder) =
        a.ToString().ToCharArray()
        |> Array.rev
        |> fun a -> new string (a)
        |> StringBuilder

    let split (a: StringBuilder) (b: string) =
        a.ToString().Split(b) |> Array.map StringBuilder |> Array.toList

    let replace (builder: StringBuilder) (pattern: string) (substitute: string) = builder.Replace(pattern, substitute)

    let is_equal (a: StringBuilder) (b: StringBuilder) = a.ToString() = b.ToString()

    let is_empty (a: StringBuilder) = a.Length = 0

    let inspect (term: obj) : StringBuilder =
        let builder = StringBuilder()
        // TODO: This may not be safe for AOT
        Printf.bprintf builder "%A" term
        builder

module String =

    let ofChars (chars: char list) = new string (List.toArray chars)

    let length (s: string) = s.Length |> int64

    let lowercase (s: string) = s.ToLower()

    let uppercase (s: string) = s.ToUpper()

    let less_than (a: string) (b: string) = a < b

    let slice (s: string) (start: int64) (length: int64) = s.Substring(int start, int length)

    let crop (s: string) (before: string) =
        let index = s.IndexOf(before)
        if index = -1 then s else s.Substring(index + before.Length)

    let contains (haystack: string) (needle: string) = haystack.Contains(needle)

    let starts_with (string: string) (prefix: string) = string.StartsWith(prefix)

    let ends_with (string: string) (suffix: string) = string.EndsWith(suffix)

    let split (x: string) (substring: string) = x.Split(substring) |> Array.toList

    let split_once (x: string) (substring: string) =
        let index = x.IndexOf(substring)

        if index = -1 then
            Error()
        else
            Ok(x.Substring(0, index), x.Substring(index + substring.Length))

    let join (strings: string list) (separator: string) = String.concat separator strings

    let trim (s: string) = s.Trim()

    let trim_left (s: string) = s.TrimStart()

    let trim_right (s: string) = s.TrimEnd()

    let pop_grapheme (s: string) =
        let index = s.IndexOf(s[0])

        if index = -1 then
            Error()
        else
            Ok(s[0] |> string, s.Substring(1))

    let to_graphemes (s: string) =
        s.ToCharArray() |> Array.map string |> Array.toList

    let unsafe_int_to_utf_codepoint (a: int64) = UtfCodepoint(a)

    let to_utf_codepoints (s: string) =
        s.ToCharArray()
        |> Array.map (fun c -> int64 c)
        |> Array.map UtfCodepoint
        |> Array.toList

    let from_utf_codepoints (codepoints: UtfCodepoint list) =
        codepoints |> List.map (fun (UtfCodepoint c) -> char (c)) |> ofChars

    let utf_codepoint_to_int (UtfCodepoint(value)) = value

    let inspect (term: obj) =
        // TODO: This may not be safe for AOT
        sprintf "%A" term

    let byte_size (s: string) = s.Length

module BitArray =

    let from_string (s: string) : BitArray =
        raise (NotImplementedException("BitArray.from_string not yet implemented"))

    let byte_size (arr: BitArray) : int64 =
        raise (NotImplementedException("BitArray.byte_size not yet implemented"))

    let concat (bit_arrays: BitArray list) : BitArray =
        raise (NotImplementedException("BitArray.concat not yet implemented"))

    let slice (arr: BitArray) (start: int64) (length: int64) : Result<BitArray, unit> =
        raise (NotImplementedException("BitArray.slice not yet implemented"))

    let is_utf8 (arr: BitArray) : bool =
        raise (NotImplementedException("BitArray.is_utf8 not yet implemented"))

    let to_string (arr: BitArray) : Result<string, unit> =
        raise (NotImplementedException("BitArray.to_string not yet implemented"))

    let base64_encode (input: BitArray) (padding: bool) : string =
        raise (NotImplementedException("BitArray.base64_encode not yet implemented"))

    let base64_decode (encoded: string) : Result<BitArray, unit> =
        raise (NotImplementedException("BitArray.base64_decode not yet implemented"))

    let base16_encode (input: BitArray) : string =
        raise (NotImplementedException("BitArray.base16_encode not yet implemented"))

    let base16_decode (input: string) : Result<BitArray, unit> =
        raise (NotImplementedException("BitArray.base16_decode not yet implemented"))

    let inspect (arr: BitArray) : string =
        raise (NotImplementedException("BitArray.inspect not yet implemented"))

    let do_inspect (arr: BitArray) (accumulator: string) : string =
        raise (NotImplementedException("BitArray.do_inspect not yet implemented"))

    let compare (a: BitArray) (b: BitArray) : Order =
        raise (NotImplementedException("BitArray.compare not yet implemented"))

    let to_int_and_size (arr: BitArray) : (int64 * int64) =
        raise (NotImplementedException("BitArray.to_int_and_size not yet implemented"))


module Dynamic =
    type DecodeErrors = gleam.DecodeErrors
    type Dynamic = gleam.Dynamic

    let from (a: obj) = Dynamic(a)

    let decode_bit_array (Dynamic(data)) =
        raise (NotImplementedException("Dynamic.bit_array not yet implemented"))

    let classify (Dynamic(data)) =
        match data with
        | :? string -> "String"
        | :? int64 -> "Int"
        | :? float -> "Float"
        | :? bool -> "Bool"
        | _ ->
            if data.GetType().IsGenericType then
                if data.GetType().GetGenericTypeDefinition() = typedefof<Result<_, _>> then
                    "Result"
                elif data.GetType().GetGenericTypeDefinition() = typedefof<list<_>> then
                    "List"
                elif data.GetType().GetGenericTypeDefinition() = typedefof<Option<_>> then
                    "Option"
                elif data.GetType().GetGenericTypeDefinition() = typedefof<Map<_, _>> then
                    "Map"
                elif data.GetType().GetGenericTypeDefinition().Name.StartsWith("Tuple`") then
                    "Tuple"
                else
                    data.GetType().Name
            else
                data.GetType().Name


    let decode_int (Dynamic(data) as dyn) : Result<int64, DecodeErrors> =
        match (classify dyn) with
        | "Int" -> Ok(Convert.ToInt64 data)
        | found ->
            Error
                [ { expected = "Int"
                    found = found
                    path = [] } ]

    let decode_float (Dynamic(data) as dyn) : Result<float, DecodeErrors> =
        match (classify dyn) with
        | "Float" -> Ok(Convert.ToDouble data)
        | found ->
            Error
                [ { expected = "Float"
                    found = found
                    path = [] } ]

    let decode_bool (Dynamic(data) as dyn) : Result<bool, DecodeErrors> =
        match (classify dyn) with
        | "Bool" -> Ok(Convert.ToBoolean data)
        | found ->
            Error
                [ { expected = "Bool"
                    found = found
                    path = [] } ]

    let decode_string (Dynamic(data) as dyn) : Result<string, DecodeErrors> =
        match (classify dyn) with
        | "String" -> Ok(Convert.ToString data)
        | found ->
            Error
                [ { expected = "String"
                    found = found
                    path = [] } ]

    let decode_list (Dynamic(data) as dyn) : Result<list<Dynamic>, DecodeErrors> =
        match (classify dyn) with
        | "List" ->
            match data with
            | :? list<obj> as data -> Ok(List.map Dynamic data)
            | _ ->
                Error
                    [ { expected = "List"
                        found = "Unknown"
                        path = [] } ]
        | found ->
            Error
                [ { expected = "List"
                    found = found
                    path = [] } ]

    let decode_result (Dynamic(data) as dyn) : Result<Result<'a, 'b>, DecodeErrors> =
        match classify dyn with
        | "Result" ->
            match data with
            | :? Result<'a, 'b> as data -> Ok(data)
            | _ ->
                Error
                    [ { expected = "Result"
                        found = "Unknown"
                        path = [] } ]
        | found ->
            Error
                [ { expected = "Result"
                    found = found
                    path = [] } ]

    type Decoder<'t> = Dynamic -> Result<'t, DecodeErrors>

    let decode_option (Dynamic(data) as dyn) (decoder: Decoder<'a>) : Result<Option<'a>, DecodeErrors> =
        if isNull data || data = box None then
            Ok(None)
        else
            match decoder dyn with
            | Ok value -> Ok(Some value)
            | Error errors -> Error errors


    let decode_map (Dynamic(data) as dyn) : Result<Dict<Dynamic, Dynamic>, DecodeErrors> =
        match classify dyn with
        | "Map" ->
            match data with
            | :? Map<Dynamic, obj> as data -> Ok(Map.map (fun k v -> Dynamic(v)) data)
            | _ ->
                Error
                    [ { expected = "Map"
                        found = "Unknown"
                        path = [] } ]
        | found ->
            Error
                [ { expected = "Map"
                    found = found
                    path = [] } ]

    let decode_field (Dynamic(data) as dyn) (name: 'name) : Result<Option<Dynamic>, DecodeErrors> =
        match classify dyn with
        | "Map" ->
            match data with
            | :? Map<'name, obj> as data -> Map.tryFind name data |> Option.map Dynamic |> Ok
            | _ ->
                Error
                    [ { expected = "Map"
                        found = "Unknown"
                        path = [] } ]
        | found ->
            Error
                [ { expected = "Map"
                    found = found
                    path = [] } ]



    let decode_tuple (Dynamic(data) as dyn) : Result<UnknownTuple, DecodeErrors> =
        match classify dyn with
        | "Tuple" ->
            let tupleType = data.GetType()

            let values =
                tupleType.GetGenericArguments()
                |> Array.mapi (fun i t -> t.GetProperty("Item" + string (i + 1)).GetValue(data))
                |> Array.map Dynamic
                |> Array.toList

            Ok(UnknownTuple(values))
        | found ->
            Error
                [ { expected = "Tuple"
                    found = found
                    path = [] } ]

    let decode_tuple2 (Dynamic(data) as dyn) : Result<(Dynamic * Dynamic), DecodeErrors> =
        match classify dyn with
        | "Tuple" ->
            match data with
            | :? Tuple<obj, obj> as (a, b) -> Ok((Dynamic(a), Dynamic(b)))
            | _ ->
                Error
                    [ { expected = "Tuple"
                        found = "Unknown"
                        path = [] } ]
        | found ->
            Error
                [ { expected = "Tuple"
                    found = found
                    path = [] } ]

    let decode_tuple3 (Dynamic(data) as dyn) : Result<(Dynamic * Dynamic * Dynamic), DecodeErrors> =
        match classify dyn with
        | "Tuple" ->
            match data with
            | :? Tuple<obj, obj, obj> as (a, b, c) -> Ok((Dynamic(a), Dynamic(b), Dynamic(c)))
            | _ ->
                Error
                    [ { expected = "Tuple"
                        found = "Unknown"
                        path = [] } ]
        | found ->
            Error
                [ { expected = "Tuple"
                    found = found
                    path = [] } ]

    let decode_tuple4 (Dynamic(data) as dyn) : Result<(Dynamic * Dynamic * Dynamic * Dynamic), DecodeErrors> =
        match classify dyn with
        | "Tuple" ->
            match data with
            | :? Tuple<obj, obj, obj, obj> as (a, b, c, d) -> Ok((Dynamic(a), Dynamic(b), Dynamic(c), Dynamic(d)))
            | _ ->
                Error
                    [ { expected = "Tuple"
                        found = "Unknown"
                        path = [] } ]
        | found ->
            Error
                [ { expected = "Tuple"
                    found = found
                    path = [] } ]

    let decode_tuple5 (Dynamic(data) as dyn) : Result<(Dynamic * Dynamic * Dynamic * Dynamic * Dynamic), DecodeErrors> =
        match classify dyn with
        | "Tuple" ->
            match data with
            | :? Tuple<obj, obj, obj, obj, obj> as (a, b, c, d, e) ->
                Ok((Dynamic(a), Dynamic(b), Dynamic(c), Dynamic(d), Dynamic(e)))
            | _ ->
                Error
                    [ { expected = "Tuple"
                        found = "Unknown"
                        path = [] } ]
        | found ->
            Error
                [ { expected = "Tuple"
                    found = found
                    path = [] } ]

    let decode_tuple6
        (Dynamic(data) as dyn)
        : Result<(Dynamic * Dynamic * Dynamic * Dynamic * Dynamic * Dynamic), DecodeErrors> =
        match classify dyn with
        | "Tuple" ->
            match data with
            | :? Tuple<obj, obj, obj, obj, obj, obj> as (a, b, c, d, e, f) ->
                Ok((Dynamic(a), Dynamic(b), Dynamic(c), Dynamic(d), Dynamic(e), Dynamic(f)))
            | _ ->
                Error
                    [ { expected = "Tuple"
                        found = "Unknown"
                        path = [] } ]
        | found ->
            Error
                [ { expected = "Tuple"
                    found = found
                    path = [] } ]

    let tuple_get (UnknownTuple(values) as tuple) (index: int64) : Result<Dynamic, DecodeErrors> =
        match List.tryItem (int index) values with
        | Some(value) -> Ok(value)
        | None ->
            Error
                [ { expected = "Tuple"
                    found = "Unknown"
                    path = [ string index ] } ]

    let tuple_size (UnknownTuple(values)) = List.length values |> int64

module IO =
    let print (string: string) = Console.Write(string)

    let println (string: string) = Console.WriteLine(string)

    let print_error (string: string) = Console.Error.Write(string)

    let println_error (string: string) = Console.Error.WriteLine(string)

    /// Writes a value to standard error (stderr) yielding Gleam syntax.
    let debug (term: 'a) = eprintfn "%A" term


module Regex =
    type Match = gleam.Match
    type RegexOptions = System.Text.RegularExpressions.RegexOptions
    type Regex = System.Text.RegularExpressions.Regex

    let compile (pattern: string) (reg_options: gleam.RegexOptions) = // (case_insensitive: bool) (multi_line: bool) =

        let mutable options = RegexOptions.Compiled

        if reg_options.case_insensitive then
            options <- options ||| RegexOptions.IgnoreCase

        if reg_options.multi_line then
            options <- options ||| RegexOptions.Multiline

        try
            new Regex(pattern, options) |> Ok
        with ex ->
            Error { error = ex.Message; byte_index = 0 }

    let check (regex: Regex) (content: string) = regex.IsMatch(content)

    let split (regex: Regex) (content: string) = regex.Split(content) |> Array.toList

    let scan (regex: Regex) (content: string) : list<Match> =
        let matches = regex.Matches(content)

        matches
        |> Seq.cast<System.Text.RegularExpressions.Match>
        |> Seq.map (fun m ->
            { content = m.Value
              submatches =
                m.Groups
                |> Seq.cast<System.Text.RegularExpressions.Group>
                |> Seq.skip 1 // Skip the first match, which is the whole match
                |> Seq.map (fun g -> if g.Success then Some(g.Value) else None)
                |> Seq.toList })
        |> Seq.toList

    let replace (regex: Regex) (content: string) (substitute: string) : string = regex.Replace(content, substitute)

module Uri =
    type NativeUri = System.Uri
    type Uri = gleam.Uri

    let percent_encode (value: string) = NativeUri.EscapeDataString(value)

    let percent_decode (value: string) =
        try
            NativeUri.UnescapeDataString(value) |> Ok
        with ex ->
            Error()

    let parse (uri_string: string) : Result<Uri, unit> =
        try
            let uri = new NativeUri(uri_string)

            Ok
                { scheme = uri.Scheme |> Util.option_of_string
                  userinfo = uri.UserInfo |> Util.option_of_string
                  host = uri.Host |> Util.option_of_string
                  port = uri.Port |> int64 |> Some
                  path = uri.AbsolutePath
                  query = uri.Query |> Util.option_of_string
                  fragment = uri.Fragment |> Util.option_of_string }
        with ex ->
            Error()

    let parse_query (query: string) : Result<list<(string * string)>, unit> =
        try
            let uri = new NativeUri(query)

            uri.Query.Split '&'
            |> Array.map (fun pair -> pair.Split '=')
            |> Array.map (fun pair -> NativeUri.UnescapeDataString(pair.[0]), NativeUri.UnescapeDataString(pair.[1]))
            |> Array.toList
            |> Ok
        with ex ->
            Error()

module List =
    let length (list: list<'a>) = List.length list |> int64

    let reverse (list: list<'a>) = List.rev list

    let contains (list: list<'a>) (elem: 'a) = List.contains elem list

    let append (first: list<'a>) (second: list<'a>) = List.append first second

    let concat (lists: list<list<'a>>) = List.concat lists

    let each (list: list<'a>) (f: 'a -> 'b) : unit = List.iter (f >> ignore) list

module Should =
    let equal (a: obj) (b: obj) =
        if a.Equals(b) then
            ()
        else
            failwithf "Expected %A to equal %A" a b

    let not_equal (a: obj) (b: obj) =
        if not (a.Equals(b)) then
            ()
        else
            failwithf "Expected %A to not equal %A" a b

    let be_ok (a: Result<'a, 'b>) =
        match a with
        | Ok(value) -> value
        | Error(_) -> failwithf "\n%A\nshould be ok" a

    let be_error (a: Result<'a, 'b>) =
        match a with
        | Error(_) -> ()
        | Ok(_) -> failwithf "\n%A\nshould be error" a

    let be_true (actual: bool) = equal actual true

    let be_false (actual: bool) = equal actual false

    let fail () = failwith "Expected failure"

// module rec Iterator =
//     type Iterator<'a> = System.Collections.Generic.IEnumerable<'a>

//     type Step<'element, 'accumulator> =
//         | Next of element: 'element * accumulator: 'accumulator
//         | Done
//     type Action<'element> =
//         // Dedicated to Electric Six
//         // https://youtu.be/_30t2dzEgiw?t=162
//         | Stop
//         | Continue of 'element * (unit -> Action<'element>)

//     let stop<'a> () = Seq.empty<'a>

//     // let rec do_unfold
//     //     (initial: 'acc)
//     //     (f: 'acc -> Step<'element, 'acc>)
//     //     =
//     //     fun () ->

//     //         match f(initial) with
//     //         | Next(x, acc) ->
//     //             let next = do_unfold(acc, f)
//     //             Continue(x, next)
//     //         | Done -> Stop

//     let private do_unfold (initial: 'acc) (f: 'acc -> Step<'element, 'acc>): unit -> Action<'element> = begin
//         fun() -> begin
//             match f initial with
//             | (Next(x, acc)) ->
//                 Continue(x, (do_unfold acc f))
//             | (Done) ->
//                 Stop
//         end
//     end

//     [<TailCall>]
//     let unfold (initial: 'acc) (f: 'acc -> Step<'element, 'acc>) : Iterator<'element> =
//         let rec loop acc =
//             match f (acc) with
//             | Done -> Seq.empty
//             | Next(element, newAcc) ->
//                 seq {
//                     yield element
//                     yield! loop newAcc
//                 }
