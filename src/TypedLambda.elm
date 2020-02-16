module TypedLambda exposing
  ( Term(..)
  , alphaEq
  , eval
  , toString
  , parse
  )

import Peg.Parser as Peg exposing (Parser)

type Term
  = Id Int -- de Bruijn index
  | Abs String Term -- variable name hint, content
  | App Term Term -- lhs, rhs
  --| TmTrue
  --| TmFalse
  --| TmIf Term Term Term

alphaEq : Term -> Term -> Bool
alphaEq lhs rhs =
  case ( lhs, rhs ) of
    ( Id l, Id r ) -> l == r
    ( Abs _ l, Abs _ r ) -> alphaEq l r
    ( App ll lr, App rl rr ) -> alphaEq ll rl && alphaEq lr rr
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

toStringWithContext : NamingContext -> Term -> String
toStringWithContext ctx term =
  case term of
    Id k ->
      indexToName ctx k

    Abs _ _ ->
      let ( absList, inner ) = toStringAbsHelp [] ctx term in
      "λ" ++ (absList |> List.reverse |> String.join " ") ++ "." ++ inner

    App lhs rhs ->
      let
        lhsInner = toStringWithContext ctx lhs
        lhsParren =
          case lhs of
            Abs _ _ -> True
            _ -> False
        lhsStr =
          if lhsParren
          then "(" ++ lhsInner ++ ")"
          else lhsInner

        rhsInner = toStringWithContext ctx rhs
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

toStringAbsHelp : List String -> NamingContext -> Term -> ( List String, String )
toStringAbsHelp absList ctx exp =
  case exp of
    Abs nameHint t ->
      let ( ctx_, varName ) = pickFreshName ctx nameHint in
      toStringAbsHelp (varName :: absList) ctx_ t

    _ -> ( absList, toStringWithContext ctx exp )

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

parse : String -> Result String Term
parse src =
  case Peg.parse src parser of
    Nothing -> Err "Parse Error"
    Just ast -> ast |> termFromAst

termFromAst : Ast -> Result String Term
termFromAst ast =
  let
    withContext ctx ast_ =
      case ast_ of
        AstId name ->
          case getId name ctx of
            Nothing ->
              Err <| "Missing identifier: " ++ name
            Just id ->
              Ok <| Id id

        AstAbs var exp ->
          let
            inner = withContext (var::ctx) exp
          in
            Result.map (Abs var) inner

        AstApp lAst rAst ->
          let
            lExp = withContext ctx lAst
            rExp = withContext ctx rAst
          in
            Result.map2 App lExp rExp
  in
    withContext [] ast


type Ast
  = AstId String
  | AstAbs String Ast
  | AstApp Ast Ast

parser : Parser Ast
parser =
  pAst

pSp =
  Peg.char (\c -> List.member c spChars)
    |> Peg.oneOrMore
spChars =
  [' ', '\t']

pOpSp = Peg.option pSp

pLambda = Peg.match "λ"
pDot = Peg.match "."
pLParen = Peg.match "("
pRParen = Peg.match ")"

pName =
  Peg.seq2
  pNameHead pNameTail
  (\hd tl -> String.fromList (hd :: tl))
pNameHead =
  Peg.char Char.isAlpha
pNameTail =
  Peg.char (\c -> Char.isAlphaNum c || c == '_')
    |> Peg.zeroOrMore

pId =
  pName |> Peg.map AstId

pAbs =
  Peg.seq4
  pLambda (Peg.join pSp pName) pDot pAst
  (\_ vars _ inner ->
    List.foldr
    (\var inner_ -> AstAbs var inner_)
    inner
    vars
  )

pApp =
  let
    pParen =
      Peg.intersperseSeq3 pOpSp
      pLParen pAst pRParen
      (\_ inner _ -> inner)

    pAExpHd =
      Peg.choice
      [ \_ -> pId
      , \_ -> pParen
      ]

    pAExpTl =
      Peg.choice
      [ \_ -> Peg.seq2 pSp pId (\_ p -> p)
      , \_ -> Peg.seq2 pOpSp pParen (\_ p -> p)
      ]
  in
    Peg.seq3
    pAExpHd pAExpTl (Peg.zeroOrMore pAExpTl)
    (\first second rest ->
      List.foldl
      (\exp app -> AstApp app exp)
      (AstApp first second)
      rest
    )

pAst =
  Peg.choice
  [ \_ -> pApp
  , \_ -> pId
  , \_ -> pAbs
  ]