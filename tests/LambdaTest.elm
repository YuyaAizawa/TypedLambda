module LambdaTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import TypedLambda exposing (..)

s = Abs "x" (Abs "y" (Abs "z" (App (App (Id 2) (Id 0)) (App (Id 1 )(Id 0)))))
k = Abs "x" (Abs "y" (Id 1))
i = Abs "x" (Id 0)
y = Abs "f" (App (Abs "x" (App (Id 1) (App (Id 0) (Id 0)))) (Abs "x" (App (Id 1) (App (Id 0) (Id 0)))))

succ = Abs "n" (Abs "f" (Abs "x" (App (Id 1) (App (App (Id 2) (Id 1)) (Id 0)))))
pred = Abs "n" (Abs "f" (Abs "x" (App (App (App (Id 0) (Abs "g" (Abs "h" (App (Id 4) (App (Id 3) (Id 1)))))) (Abs "u" (Id 2))) (Abs "u" (Id 3)))))
zero = Abs "f" (Abs "x" (Id 0))
one = Abs "f" (Abs "x" (App (Id 1) (Id 0)))
not = Abs "b" (TmIf (Id 0) TmFalse TmTrue)

suite : Test
suite =
  describe "all"
  [ describe "toString"
      [ test "S" <| \_ ->
          s
            |> toString
            |> Expect.equal "λx y z.x z(y z)"

      , test "K" <| \_ ->
          k
            |> toString
            |> Expect.equal "λx y.x"

      , test "I" <| \_ ->
          i
            |> toString
            |> Expect.equal "λx.x"

      , test "Y" <| \_ ->
          y
            |> toString
            |> Expect.equal "λf.(λx.f(x x))(λx.f(x x))"

      , test "SUCC" <| \_ ->
          succ
            |> toString
            |> Expect.equal "λn f x.f(n f x)"

      , test "NOT" <| \_ ->
          not
            |> toString
            |> Expect.equal "λb.if b then False else True"
      ]

  , describe "parse"
    [ test "S" <| \_ ->
        fromString "λx y z.x z(y z)"
          |> Expect.equal (Ok s)

    , test "K" <| \_ ->
        fromString "λx y.x"
          |> Expect.equal (Ok k)

    , test "I" <| \_ ->
        fromString "λx.x"
          |> Expect.equal (Ok i)

    , test "Y" <| \_ ->
        fromString "λf.(λx.f(x x))(λx.f(x x))"
          |> Expect.equal (Ok y)

    , test "SUCC" <| \_ ->
        fromString "λn f x.f(n f x)"
          |> Expect.equal (Ok succ)

    , test "NOT" <| \_ ->
        fromString "λb.if b then False else True"
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
  ]