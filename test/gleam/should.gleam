//// A module for testing your Gleam code. The functions found here are
//// compatible with the Erlang eunit test framework.
////
//// More information on running eunit can be found in [the rebar3
//// documentation](https://rebar3.org/docs/testing/eunit/).

import gleam/string

@external(erlang, "gleam_stdlib_test_ffi", "should_equal")
@external(fsharp, "./should.fsx", "Gleam.Should.equal")
pub fn equal(a: a, b: a) -> Nil {
  case a == b {
    True -> Nil
    _ ->
      panic as {
        string.concat([
          "\n\t",
          string.inspect(a),
          "\n\tshould equal \n\t",
          string.inspect(b),
        ])
      }
  }
}

@external(erlang, "gleam_stdlib_test_ffi", "should_not_equal")
@external(fsharp, "./should.fsx", "Gleam.Should.not_equal")
pub fn not_equal(a: a, b: a) -> Nil {
  case a != b {
    True -> Nil
    _ ->
      panic as {
        string.concat([
          "\n",
          string.inspect(a),
          "\nshould not equal \n",
          string.inspect(b),
        ])
      }
  }
}

@external(fsharp, "./should.fsx", "Gleam.Should.be_ok")
pub fn be_ok(a: Result(a, e)) -> a {
  case a {
    Ok(e) -> e
    _ -> panic as { string.concat(["\n", string.inspect(a), "\nshould be ok"]) }
  }
}

@external(fsharp, "./should.fsx", "Gleam.Should.be_error")
pub fn be_error(a: Result(a, e)) -> Nil {
  case a {
    Error(_) -> Nil
    _ ->
      panic as { string.concat(["\n", string.inspect(a), "\nshould be error"]) }
  }
}

pub fn be_true(actual: Bool) -> Nil {
  actual
  |> equal(True)
}

pub fn be_false(actual: Bool) -> Nil {
  actual
  |> equal(False)
}

@external(fsharp, "./should.fsx", "Gleam.Should.fail")
pub fn fail() -> Nil {
  be_true(False)
}
