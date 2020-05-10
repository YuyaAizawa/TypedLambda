module AstTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Ast exposing (parseType, Ty(..))

suite : Test
suite =
  describe "all"
  [ describe "Type"
    [ test "A" <|
      \_ ->
        parseType "A"
          |> Expect.equal (Ok (Atomic "A"))

    , test "A->A" <|
      \_ ->
        parseType "A->A"
          |> Expect.equal (Ok (Arrow (Atomic "A") (Atomic "A")))

    , test "A->A->A" <|
      \_ ->
        parseType "A->A->A"
          |> Expect.equal (Ok (Arrow (Atomic "A") (Arrow (Atomic "A") (Atomic "A"))))

    , test "A->(A->A)" <|
      \_ ->
        parseType "A->(A->A)"
          |> Expect.equal (Ok (Arrow (Atomic "A") (Arrow (Atomic "A") (Atomic "A"))))

    , test "(A->A)->A" <|
      \_ ->
        parseType "(A->A)->A"
          |> Expect.equal (Ok (Arrow (Arrow (Atomic "A") (Atomic "A")) (Atomic "A")))
    ]
  ]