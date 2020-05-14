module TypedLambda exposing
  ( Term(..)
  , Ty(..)
  , alphaEq
  , typeOf
  , eval
  , toString
  , fromString
  )

import Ast exposing (Ast)

type Ty
  = TyFun Ty Ty
  | TyBool
  | TyAtomic String

type Term
  = Id Int -- de Bruijn index
  | Abs String Ty Term -- variable name hint, type, content
  | App Term Term -- lhs, rhs
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  | Fix Term

type alias NamingContext = List String
type alias TypingContext = List ( String, Ty )

namingContextToString : NamingContext -> String
namingContextToString ctx =
  "[" ++ String.join ", " ctx ++ "]"
typingContextToString : TypingContext -> String
typingContextToString ctx =
  let
    ctx_ =
      ctx
        |> List.map (\(name, ty) -> name ++ ":" ++ typeToString ty)
  in
    "[" ++ String.join ", " ctx_ ++ "]"

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

alphaEq : Term -> Term -> Bool
alphaEq lhs rhs =
  case ( lhs, rhs ) of
    ( Id l, Id r ) -> l == r
    ( Abs _ _ l, Abs _ _ r ) -> alphaEq l r
    ( App ll lr, App rl rr ) -> alphaEq ll rl && alphaEq lr rr
    ( TmTrue, TmTrue ) -> True
    ( TmFalse, TmFalse ) -> True
    ( TmIf lc lt lf , TmIf rc rt rf ) -> alphaEq lc rc && alphaEq lt rt && alphaEq lf rf
    _ -> False

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
      App ((Abs _ _ t) as abs) v ->
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

      Fix ((Abs _ _ t) as abs) ->
        shift (-1) 0 (substitute 0 (shift 1 0 abs) t)
          |> Just

      Fix t ->
        evalStep t
          |> Maybe.map Fix

      _ -> Nothing

isValue exp =
  case exp of
    Abs _ _ _ -> True
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

      Abs name ty t1 ->
        Abs name ty (walk (c_ + 1) t1)

      App t1 t2 ->
        App (walk c_ t1) (walk c_ t2)

      TmIf cnd et ef ->
        TmIf (walk c_ cnd) (walk c_ et) (walk c_ ef)

      others ->
        others
  in
    walk c t

typeOf : Term -> Result String Ty
typeOf t =
  let
    helper ctx t_ =
      case t_ of
        Id i ->
          case ctx |> indexToType i of
            Just ( _, ty ) ->
              Ok ty

            Nothing ->
              Err (String.fromInt i ++ " on " ++ typingContextToString ctx)

        Abs nameHint ty1 t2 ->
          let
            ctx_ = (nameHint, ty1) :: ctx
            ty2r = helper ctx_ t2
          in
            ty2r
              |> Result.map (\ty2 -> TyFun ty1 ty2)

        App t1 t2 ->
          let
            ty1r = helper ctx t1
            ty2r = helper ctx t2
          in
            ty1r |> Result.andThen (\ty1 ->
              ty2r |> Result.andThen (\ty2 ->
                case ty1 of
                  TyFun ty11 ty12 ->
                    if ty11 == ty2
                    then Ok <| ty12
                    else Err
                      <| "type mismatch, param: "
                      ++ typeToString ty1
                      ++ ", arg: "
                      ++ typeToString ty2

                  _ ->
                    Err
                      <| "function type expected: "
                      ++ toString t1
              )
            )

        TmTrue ->
          Ok TyBool

        TmFalse ->
          Ok TyBool

        TmIf t1 t2 t3 ->
          let
            ty1r = helper ctx t1
            ty2r = helper ctx t2
            ty3r = helper ctx t3
          in
            ty1r |> Result.andThen (\ty1 ->
              if ty1 == TyBool
              then
                ty2r |> Result.andThen (\ty2 ->
                  ty3r |> Result.andThen (\ty3 ->
                    if ty2 == ty3
                    then Ok ty2
                    else Err
                      <| "type mismatch, then: "
                      ++ typeToString ty2
                      ++ ", else: "
                      ++ typeToString ty3
                  )
                )
              else
                Err "conditional not a boolean"
            )

        Fix t1 ->
          helper ctx t1
            |> Result.andThen (\ty ->
              case ty of
                TyFun ty1 ty2 ->
                  if ty1 == ty2
                  then Ok ty1
                  else Err <| "cannot generate fixpoint: " ++ typeToString ty

                _ ->
                  Err <| "cannot generate fixpoint: " ++ typeToString ty
            )
  in
    helper [] t

typeToString : Ty -> String
typeToString ty =
  case ty of
    TyFun ty1 ty2 ->
      let
        str1 = typeToString ty1
        str2 = typeToString ty2
      in
        case ty1 of
          TyFun _ _ ->
            "(" ++ str1 ++ ")->" ++ str2

          _ ->
            str1 ++ "->" ++ str2

    TyBool ->
      "Bool"

    TyAtomic name ->
      name

toString : Term -> String
toString =
  let
    withContext : NamingContext -> Term -> String
    withContext ctx term =
      case term of
        Id k ->
          ctx |> indexToName k

        Abs _ _ _ ->
          let ( absList, inner ) = absHelp [] ctx term in
          "λ" ++ (absList |> List.reverse |> String.join " ") ++ "." ++ inner

        App lhs rhs ->
          let
            lhsInner = withContext ctx lhs
            lhsParren =
              case lhs of
                Abs _ _ _ -> True
                _ -> False
            lhsStr =
              if lhsParren
              then "(" ++ lhsInner ++ ")"
              else lhsInner

            rhsInner = withContext ctx rhs
            rhsParen =
              case rhs of
                Abs _ _ _ -> True
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

        Fix t ->
          "fix "++withContext ctx t

    absHelp : List String -> NamingContext -> Term -> ( List String, String )
    absHelp absList ctx exp =
      case exp of
        Abs nameHint ty t ->
          let
            ( ctx_, varName ) = pickFreshName ctx nameHint
            tyName = typeToString ty
          in
            absHelp ((varName ++ ":" ++ tyName) :: absList) ctx_ t

        _ -> ( absList, withContext ctx exp )
  in
    withContext []

pickFreshName : NamingContext -> String -> ( NamingContext, String )
pickFreshName ctx hint =
  if List.member hint ctx
  then pickFreshName ctx (hint ++ "'")
  else ( hint :: ctx, hint )

listGet : Int -> List a -> Maybe a
listGet idx binds =
    case ( idx, binds ) of
      ( _, []) -> Nothing
      ( 0, a :: _) -> Just a
      ( n, _ :: tl) -> listGet (n-1) tl

indexToName : Int -> NamingContext -> String
indexToName idx ctx =
  listGet idx ctx
    |> Maybe.withDefault ("index out of bounds: " ++ String.fromInt idx)

indexToType : Int -> TypingContext -> Maybe ( String, Ty )
indexToType =
  listGet

fromString : String -> Result String Term
fromString src =
  let
    tyFromAst ty =
      case ty of
        Ast.Arrow aTy1 aTy2 ->
          let
            ty1 = aTy1 |> tyFromAst
            ty2 = aTy2 |> tyFromAst
          in
            TyFun ty1 ty2

        Ast.Atomic str ->
          if str == "Bool"
          then TyBool
          else TyAtomic str

    withContext ctx ast_ =
      case ast_ of
        Ast.Id name ->
          case getId name ctx of
            Nothing ->
              Err <| "Missing identifier: " ++ name
            Just id ->
              Ok <| Id id

        Ast.Abs var ty exp ->
          let
            inner = withContext (var::ctx) exp
            ty_ = tyFromAst ty
          in
            Result.map (Abs var ty_) inner

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

        Ast.If c t f ->
          let
            c_ = withContext ctx c
            t_ = withContext ctx t
            f_ = withContext ctx f
          in
            Result.map3 TmIf c_ t_ f_

        Ast.Let v d t ->
          let
            d_ = withContext ctx d
            t_ = withContext (v::ctx) t
            dty = d_ |> Result.andThen typeOf
          in
            Result.map3
            (\d__ t__ dty_ -> App (Abs v dty_ t__) d__)
            d_ t_ dty

        Ast.Letrec v t1 t2 ->
          let
            t1_ = withContext ctx t1
            t2_ = withContext (v::ctx) t2
            ty1 = t1_ |> Result.andThen typeOf
          in
            Result.map3
            (\t1__ t2__ ty1_ -> App (Abs v ty1_ t2__) (Fix (Abs v ty1_ t1__)))
            t1_ t2_ ty1
  in
    Ast.parse src
      |> Result.andThen (withContext [])
