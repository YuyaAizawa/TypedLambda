module Ast exposing
  ( Ast(..)
  , parse
  )

import Peg.Parser as Peg exposing (Parser)

type Ast
  = Id String
  | Abs String Ast
  | App Ast Ast
  | TmTrue
  | TmFalse
  | If Ast Ast Ast

parse : String -> Result String Ast
parse src =
  case Peg.parse src pAst of
    Nothing -> Err "Parse Error"
    Just ast -> Ok ast

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
pIf = Peg.match "if"
pThen = Peg.match "then"
pElse = Peg.match "else"

keyword =
  [ "True"
  , "False"
  , "if"
  , "then"
  , "else"
  ]

pName =
  Peg.seq2
  pNameHead pNameTail
  (\hd tl -> String.fromList (hd :: tl))
    |> Peg.andThen (\name ->
      if List.member name keyword
      then Peg.fail
      else Peg.return name
    )

pNameHead =
  Peg.char Char.isAlpha
pNameTail =
  Peg.char (\c -> Char.isAlphaNum c || c == '_')
    |> Peg.zeroOrMore

pId =
  pName |> Peg.map Id

pAbs =
  Peg.seq4
  pLambda (Peg.join pSp pName) pDot pAst
  (\_ vars _ inner ->
    List.foldr
    (\var inner_ -> Abs var inner_)
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
      , \_ -> pTrue
      , \_ -> pFalse
      ]

    pAExpTl =
      Peg.choice
      [ \_ -> Peg.seq2 pSp pId (\_ p -> p)
      , \_ -> Peg.seq2 pOpSp pParen (\_ p -> p)
      , \_ -> Peg.seq2 pSp pTrue (\_ p -> p)
      , \_ -> Peg.seq2 pSp pFalse (\_ p -> p)
      ]
  in
    Peg.seq3
    pAExpHd pAExpTl (Peg.zeroOrMore pAExpTl)
    (\first second rest ->
      List.foldl
      (\exp app -> App app exp)
      (App first second)
      rest
    )

pTrue =
  Peg.match "True"
    |> Peg.map (always TmTrue)

pFalse =
  Peg.match "False"
    |> Peg.map (always TmFalse)

pIfExp =
  Peg.intersperseSeq6 pSp
  pIf pAst pThen pAst pElse pAst
  (\_ c _ t _ f -> If c t f)

pAst =
  Peg.choice
  [ \_ -> pApp
  , \_ -> pTrue
  , \_ -> pFalse
  , \_ -> pIfExp
  , \_ -> pId
  , \_ -> pAbs
  ]