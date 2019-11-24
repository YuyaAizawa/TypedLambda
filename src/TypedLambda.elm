module TypedLambda exposing
  ( Term(..)
  , eval
  , toString
  )

type Term
  = Id Int -- de Bruijn index
  | Abs String Term -- variable name hint, content
  | App Term Term -- lhs, rhs

type alias Context = List (String, Binding)

type Binding
  = NameBind


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

      _ -> Nothing


isValue exp =
  case exp of
    Abs _ _ -> True
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
  in
    walk c t

toString : Term -> String
toString =
  toStringWithContext []

toStringWithContext : Context -> Term -> String
toStringWithContext ctx term =
  case term of
    Id k ->
      indexToName ctx k

    Abs _ _ ->
      let ( absList, inner ) = toStringAbsHelp "" ctx term in
      "λ" ++ absList ++ "." ++ inner

    App lhs rhs ->
      let
        lhsInner = toStringWithContext ctx lhs
        lhsStr =
          case lhs of
            Abs _ _ -> "(" ++ lhsInner ++ ")"
            _ -> lhsInner

        rhsInner = toStringWithContext ctx rhs
        rhsStr =
          case rhs of
            Abs _ _ -> "(" ++ rhsInner ++ ")"
            App _ _ -> "(" ++ rhsInner ++ ")"
            _ -> rhsInner
      in
        lhsStr ++ rhsStr

toStringAbsHelp : String -> Context -> Term -> ( String, String )
toStringAbsHelp absList ctx exp =
  case exp of
    Abs nameHint t ->
      let ( ctx_, varName ) = pickFreshName ctx nameHint in
      toStringAbsHelp (absList ++ varName) ctx_ t

    _ -> ( absList, toStringWithContext ctx exp )

pickFreshName : Context -> String -> ( Context, String )
pickFreshName ctx nameHint =
  if List.member ( nameHint, NameBind ) ctx
  then pickFreshName ctx (nameHint ++ "'")
  else ( ( nameHint , NameBind ) :: ctx, nameHint )

indexToName ctx idx =
  case ( ctx, idx ) of
    ( ( varName, NameBind ) :: _, 0 ) -> varName
    ( _ :: tl, n ) -> indexToName tl (n-1)
    ( [], n ) -> String.fromInt n
