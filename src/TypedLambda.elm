module TypedLambda exposing
  ( Term(..)
  , Ty(..)
  , Bbo(..)
  , alphaEq
  , typeOf
  , eval
  , evalStep
  , toString
  , fromString
  )

import Ast exposing (Ast)

type Ty
  = TyFun Ty Ty
  | TyBool
  | TyI32
  | TyAtomic String

type Bbo
  = AddI32
  | SubI32
  | MulI32
  | EqI32

bboToTy bbo =
  case bbo of
    AddI32 -> TyFun TyI32 (TyFun TyI32 TyI32)
    SubI32 -> TyFun TyI32 (TyFun TyI32 TyI32)
    MulI32 -> TyFun TyI32 (TyFun TyI32 TyI32)
    EqI32 -> TyFun TyI32 (TyFun TyI32 TyBool)

bboToString bbo =
  case bbo of
    AddI32 -> "add"
    SubI32 -> "sub"
    MulI32 -> "mul"
    EqI32  -> "eq"

bboFromString name =
  case name of
    "add" -> Just AddI32
    "sub" -> Just SubI32
    "mul" -> Just MulI32
    "eq"  -> Just EqI32
    _ -> Nothing

type Term
  = Id Int -- de Bruijn index
  | Abs String Ty Term -- variable name hint, type, content
  | App Term Term -- lhs, rhs
  | TmTrue
  | TmFalse
  | TmI32 Int
  | TmIf Term Term Term
  | Fix Term
  | BuiltinBinOp Bbo Term Term

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
        shift (-1) 0 (substitute 0 (shift 1 0 exp) t)
          |> Just

      BuiltinBinOp bbo t1 t2 ->
              if t1 |> isValue |> not
              then
                evalStep t1
                  |> Maybe.map (\t1_ -> BuiltinBinOp bbo t1_ t2)
              else if t2 |> isValue |> not
              then
                evalStep t2
                  |> Maybe.map (\t2_ -> BuiltinBinOp bbo t1 t2_)
              else
                case ( bbo, t1, t2 ) of
                  ( AddI32, TmI32 i, TmI32 j ) -> TmI32 (i + j) |> Just
                  ( SubI32, TmI32 i, TmI32 j ) -> TmI32 (i - j) |> Just
                  ( MulI32, TmI32 i, TmI32 j ) -> TmI32 (i * j) |> Just
                  (  EqI32, TmI32 i, TmI32 j ) ->
                    if i == j
                    then TmTrue |> Just
                    else TmFalse |> Just

                  _ -> Nothing

      _ -> Nothing

isValue exp =
  case exp of
    Abs _ _ _ -> True
    TmTrue -> True
    TmFalse -> True
    TmI32 _ -> True
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

      Fix f ->
        Fix (walk c_ f)

      BuiltinBinOp bbo t1 t2 ->
        BuiltinBinOp bbo (walk c_ t1) (walk c_ t2)

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

        TmI32 _ ->
          Ok TyI32

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

        BuiltinBinOp bbo t1 t2 ->
          case (typeOf t1, typeOf t2) of
            ( Ok TyI32, Ok TyI32 ) ->
              case bbo |> bboToTy of
                TyFun _ (TyFun _ r) -> Ok r
                _ -> Err <| "never happen"
            _ -> Err <| "operand type mismatch"
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

    TyI32 ->
      "I32"

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

        TmI32 i ->
          i |> String.fromInt

        TmIf c t f ->
          "if "++withContext ctx c++
          " then "++withContext ctx t++
          " else "++withContext ctx f

        Fix t ->
          "(fix "++withContext ctx t++")"

        BuiltinBinOp bbo t1 t2 ->
          "("++bboToString bbo
          ++" "++withContext ctx t1
          ++" "++withContext ctx t2
          ++")"

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
          case str of
            "Bool" -> TyBool
            "I32"  -> TyI32
            others -> TyAtomic others

    appHelp ctx lAst rAst =
      let
        lExp = withContext ctx lAst
        rExp = withContext ctx rAst
      in
        Result.map2 App lExp rExp

    withContext : NamingContext -> Ast.Ast -> Result String Term
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

        Ast.App ((Ast.App (Ast.Id name) t1) as t) t2 ->
          case name |> bboFromString of
            Nothing ->
              appHelp ctx t t2

            Just bbo ->
              let
                t1_ = withContext ctx t1
                t2_ = withContext ctx t2
              in
                Result.map2 (BuiltinBinOp bbo) t1_ t2_


        Ast.App lAst rAst ->
          appHelp ctx lAst rAst

        Ast.TmTrue ->
          Ok TmTrue

        Ast.TmFalse ->
          Ok TmFalse

        Ast.TmInt i ->
          Ok <| TmI32 i

        Ast.If c t f ->
          let
            c_ = withContext ctx c
            t_ = withContext ctx t
            f_ = withContext ctx f
          in
            Result.map3 TmIf c_ t_ f_

        Ast.Let x m n ->
          -- let x = M in N
          -- (λx:typeof M.N) M
          let
            m_ = withContext ctx m
            n_ = withContext (x::ctx) n
            tyM = m_ |> Result.andThen typeOf
          in
            Result.map3
            (\m__ n__ tyM_ -> App (Abs x tyM_ n__) m__)
            m_ n_ tyM

        Ast.Letrec f ty m n ->
          -- letrec f = M in N
          -- let f = Fix (λf:typeof f.M) in N
          -- (λf:typeof f.N)(Fix (λf:typeof f.M))
          let
            lm =
              Ast.Abs f ty m
                |> withContext ctx
            ln =
              Ast.Abs f ty n
                |> withContext ctx
          in
            Result.map2
            (\ln_ lm_ -> App ln_ (Fix lm_))
            ln lm
  in
    Ast.parse src
      |> Result.andThen (withContext [])
