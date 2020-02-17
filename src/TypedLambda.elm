module TypedLambda exposing
  ( Term(..)
  , alphaEq
  , eval
  , toString
  , fromString
  )

import Ast exposing (Ast)

type Term
  = Id Int -- de Bruijn index
  | Abs String Term -- variable name hint, content
  | App Term Term -- lhs, rhs
  | TmTrue
  | TmFalse
  | TmIf Term Term Term

alphaEq : Term -> Term -> Bool
alphaEq lhs rhs =
  case ( lhs, rhs ) of
    ( Id l, Id r ) -> l == r
    ( Abs _ l, Abs _ r ) -> alphaEq l r
    ( App ll lr, App rl rr ) -> alphaEq ll rl && alphaEq lr rr
    ( TmTrue, TmTrue ) -> True
    ( TmFalse, TmFalse ) -> True
    ( TmIf lc lt lf , TmIf rc rt rf ) -> alphaEq lc rc && alphaEq lt rt && alphaEq lf rf
    _ -> False

type alias NamingContext = List String

getId : String -> NamingContext -> Maybe Int
getId name ctx =
  let
    help n ctx_ =
      case ctx_ of
        [] ->
          Nothing
        hd :: tl ->
          if hd == name
          then Just n
          else help (n+1) tl
  in
    help 0 ctx

--type Ty
--  = TyFun Ty Ty
--  | TyBool

eval : Term -> Term
eval exp =
  case evalStep exp of
    Just exp_ -> eval exp_
    Nothing -> exp

{- One step of evaluation.

Call-by-value strategy evaluation
-}
evalStep : Term -> Maybe Term
evalStep exp =
    case exp of
      App ((Abs _ t) as abs) v ->
        if v |> isValue
        then
          shift (-1) 0 (substitute 0 (shift 1 0 v) t)
            |> Just
        else
          evalStep v
            |> Maybe.map (\v_ -> App abs v_)

      App t1 t2 ->
        if t1 |> isValue
        then
          evalStep t2
            |> Maybe.map (\t2_ -> App t1 t2_)
        else
          evalStep t1
            |> Maybe.map (\t1_ -> App t1_ t2)

      TmIf TmTrue t _ ->
        Just t

      TmIf TmFalse _ t ->
        Just t

      TmIf c t f ->
        evalStep c
          |> Maybe.map (\c_ -> TmIf c_ t f)

      _ -> Nothing

isValue exp =
  case exp of
    Abs _ _ -> True
    TmTrue -> True
    TmFalse -> True
    _ -> False


{-| Shift.

Add d the index greater than or equal c in term t.
```
shift d c t
```
-}
shift : Int -> Int -> Term -> Term
shift d =
  mapOnId (\c k -> if k < c then Id k else Id (k + d))


{-| Substitute.

Return the result of substitution term s to index j in term t.
```
substitute j s t
```
-}
substitute : Int -> Term -> Term -> Term
substitute j s =
  mapOnId (\c k -> if k == c then shift c 0 s else Id k) j


mapOnId : (Int -> Int -> Term) -> Int -> Term -> Term
mapOnId onId c t =
  let
    walk c_ t_ = case t_ of
      Id k ->
        onId c_ k

      Abs name t1 ->
        Abs name (walk (c_ + 1) t1)

      App t1 t2 ->
        App (walk c_ t1) (walk c_ t2)

      TmIf cnd et ef ->
        TmIf (walk c_ cnd) (walk c_ et) (walk c_ ef)

      others ->
        others
  in
    walk c t

toString : Term -> String
toString =
  let
    withContext : NamingContext -> Term -> String
    withContext ctx term =
      case term of
        Id k ->
          indexToName ctx k

        Abs _ _ ->
          let ( absList, inner ) = absHelp [] ctx term in
          "λ" ++ (absList |> List.reverse |> String.join " ") ++ "." ++ inner

        App lhs rhs ->
          let
            lhsInner = withContext ctx lhs
            lhsParren =
              case lhs of
                Abs _ _ -> True
                _ -> False
            lhsStr =
              if lhsParren
              then "(" ++ lhsInner ++ ")"
              else lhsInner

            rhsInner = withContext ctx rhs
            rhsParen =
              case rhs of
                Abs _ _ -> True
                App _ _ -> True
                _ -> False
            rhsStr =
              if rhsParen
              then "(" ++ rhsInner ++ ")"
              else rhsInner

            interLR =
              if lhsParren || rhsParen
              then ""
              else " "
          in
            lhsStr ++ interLR ++ rhsStr

        TmTrue ->
          "True"

        TmFalse ->
          "False"

        TmIf c t f ->
          "if "++withContext ctx c++
          " then "++withContext ctx t++
          " else "++withContext ctx f

    absHelp : List String -> NamingContext -> Term -> ( List String, String )
    absHelp absList ctx exp =
      case exp of
        Abs nameHint t ->
          let ( ctx_, varName ) = pickFreshName ctx nameHint in
          absHelp (varName :: absList) ctx_ t

        _ -> ( absList, withContext ctx exp )
  in
    withContext []

pickFreshName : NamingContext -> String -> ( NamingContext, String )
pickFreshName ctx nameHint =
  if List.member nameHint ctx
  then pickFreshName ctx (nameHint ++ "'")
  else ( nameHint :: ctx, nameHint )

indexToName ctx idx =
  case ( ctx, idx ) of
    ( varName :: _, 0 ) -> varName
    ( _ :: tl, n ) -> indexToName tl (n-1)
    ( [], n ) -> String.fromInt n

fromString : String -> Result String Term
fromString src =
  let
    withContext ctx ast_ =
      case ast_ of
        Ast.Id name ->
          case getId name ctx of
            Nothing ->
              Err <| "Missing identifier: " ++ name
            Just id ->
              Ok <| Id id

        Ast.Abs var exp ->
          let
            inner = withContext (var::ctx) exp
          in
            Result.map (Abs var) inner

        Ast.App lAst rAst ->
          let
            lExp = withContext ctx lAst
            rExp = withContext ctx rAst
          in
            Result.map2 App lExp rExp

        Ast.TmTrue ->
          Ok TmTrue

        Ast.TmFalse ->
          Ok TmFalse

        Ast.If c t v ->
          let
            c_ = withContext ctx c
            t_ = withContext ctx t
            v_ = withContext ctx v
          in
            Result.map3 TmIf c_ t_ v_
  in
    Ast.parse src
      |> Result.andThen (withContext [])
