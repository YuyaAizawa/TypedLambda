module LambdaTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import TypedLambda exposing (..)

tyA = TyAtomic "A"
tyB = TyAtomic "B"
tyC = TyAtomic "C"
tyAA = TyFun tyA tyA
tyNat = TyFun tyAA (TyFun tyA tyA)
tyAB = TyFun tyA tyB
tyABC = TyFun tyA (TyFun tyB tyC)

s = Abs "x" tyABC (Abs "y" tyAB (Abs "z" tyA (App (App (Id 2) (Id 0)) (App (Id 1 )(Id 0)))))
k = Abs "x" tyA (Abs "y" tyB (Id 1))
i = Abs "x" tyA (Id 0)
-- y = Abs "f" tyA (App (Abs "x" tyA (App (Id 1) (App (Id 0) (Id 0)))) (Abs "x" tyA (App (Id 1) (App (Id 0) (Id 0)))))

succ = Abs "n" tyNat (Abs "f" tyAA (Abs "x" tyA (App (Id 1) (App (App (Id 2) (Id 1)) (Id 0)))))
-- pred = Abs "n" (TyFun tyAA tyA) (Abs "f" tyAA (Abs "x" tyA (App (App (App (Id 0) (Abs "g" tyA (Abs "h" tyA (App (Id 4) (App (Id 3) (Id 1)))))) (Abs "u" tyA (Id 2))) (Abs "u" tyA (Id 3)))))
zero = Abs "f" tyAA (Abs "x" tyA (Id 0))
one = Abs "f" tyAA (Abs "x" tyA (App (Id 1) (Id 0)))
not = Abs "b" TyBool (TmIf (Id 0) TmFalse TmTrue)

suite : Test
suite =
  describe "all"
  [ describe "toString"
      [ test "S" <| \_ ->
          s
            |> toString
            |> Expect.equal "λx:A->B->C y:A->B z:A.x z(y z)"

      , test "K" <| \_ ->
          k
            |> toString
            |> Expect.equal "λx:A y:B.x"

      , test "I" <| \_ ->
          i
            |> toString
            |> Expect.equal "λx:A.x"

      --, test "Y" <| \_ ->
      --    y
      --      |> toString
      --      |> Expect.equal "λf.(λx.f(x x))(λx.f(x x))"
      , test "ZERO" <| \_ ->
          zero
              |> toString
              |> Expect.equal "λf:A->A x:A.x"

      , test "SUCC" <| \_ ->
          succ
            |> toString
            |> Expect.equal "λn:(A->A)->A->A f:A->A x:A.f(n f x)"

      , test "NOT" <| \_ ->
          not
            |> toString
            |> Expect.equal "λb:Bool.if b then False else True"
      ]

  , describe "parse"
    [ test "S" <| \_ ->
        fromString "λx:A->B->C y:A->B z:A.x z(y z)"
          |> Expect.equal (Ok s)

    , test "K" <| \_ ->
        fromString "λx:A y:B.x"
          |> Expect.equal (Ok k)

    , test "I" <| \_ ->
        fromString "λx:A.x"
          |> Expect.equal (Ok i)

    --, test "Y" <| \_ ->
    --    fromString "λf.(λx.f(x x))(λx.f(x x))"
    --      |> Expect.equal (Ok y)

    , test "ZERO" <| \_ ->
      fromString "λf:A->A x:A.x"
        |> Expect.equal (Ok zero)

    , test "SUCC" <| \_ ->
        fromString "λn:(A->A)->A->A f:A->A x:A.f(n f x)"
          |> Expect.equal (Ok succ)

    , test "NOT" <| \_ ->
        fromString "λb:Bool.if b then False else True"
          |> Expect.equal (Ok not)
    ]

  , describe "eval"
    [ test "IS" <| \_ ->
      App i s
        |> eval
        |> Expect.equal s

    , test "IK" <| \_ ->
      App i k
        |> eval
        |> Expect.equal k

    , test "II" <| \_ ->
      App i i
        |> eval
        |> Expect.equal i

    , test "not True" <| \_ ->
        App not TmTrue
          |> eval
          |> Expect.equal TmFalse
    ]

  , describe "typeOf"
    [ test "S" <| \_ ->
      s
        |> typeOf
        |> Expect.equal (Ok (TyFun tyABC <| TyFun tyAB <| TyFun tyA tyC))

    , test "SUCC" <| \_ ->
      succ
        |> typeOf
        |> Expect.equal (Ok (TyFun tyNat tyNat))

    ]

  , describe "let"
    [ test "typeOf SUCC ZERO" <| \_ ->
      fromString "let zero = λf:A->A x:A.x in let succ = λn:(A->A)->A->A f:A->A x:A.f(n f x) in succ zero"
        |> Result.andThen typeOf
        |> Expect.equal (Ok tyNat)
    ]
  ]