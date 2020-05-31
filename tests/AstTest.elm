module AstTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Ast exposing (parse, parseType, Ast(..), Ty(..))

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

  , describe "Ast"
    [ test "1" <|
      \_ ->
        parse "1"
          |> Expect.equal (Ok (TmInt 1))
    , test "apply" <|
      \_ ->
        parse "nade mofu funi nyan"
          |> Expect.equal (Ok (App (App (App (Id "nade")(Id "mofu"))(Id "funi"))(Id "nyan")))
    , test "not" <|
      \_ ->
        parse "λb:Bool.if b then False else True"
          |> Expect.equal (Ok (Abs "b" (Atomic "Bool")(If (Id "b") TmFalse TmTrue)))

    , test "factorial" <|
      \_ ->
        parse "letrec f:I32->I32 = λn:I32.if eq n 0 then 1 else mul n (f(sub n 1)) in f"
         |> Expect.equal (Ok (
          Letrec "f" (Arrow (Atomic "I32") (Atomic "I32"))
          (Abs "n" (Atomic "I32") (If
            (App (App (Id "eq") (Id "n")) (TmInt 0))
            (TmInt 1)
            (App (App (Id "mul") (Id "n")) (App (Id "f") (App (App (Id "sub") (Id "n")) (TmInt 1))))))
          (Id "f")))
    ]
  ]