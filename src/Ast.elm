module Ast exposing
  ( Ast(..)
  , parse
  )

import Peg.Parser as Peg exposing (Parser)

type Ast
  = Id String
  | Abs String Ast
  | App Ast Ast

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
      (\exp app -> App app exp)
      (App first second)
      rest
    )

pAst =
  Peg.choice
  [ \_ -> pApp
  , \_ -> pId
  , \_ -> pAbs
  ]