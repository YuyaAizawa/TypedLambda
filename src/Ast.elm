module Ast exposing
  ( Ast(..)
  , Ty(..)
  , parse
  , parseType
  )

import Peg.Parser as Peg exposing (Parser)

type Ast
  = Id String
  | Abs String Ty Ast
  | App Ast Ast
  | TmTrue
  | TmFalse
  | TmInt Int
  | If Ast Ast Ast
  | Let String Ast Ast
  | Letrec String Ty Ast Ast

type Ty
  = Arrow Ty Ty
  | Atomic String

parse : String -> Result String Ast
parse src =
  case Peg.parse src pAst of
    Nothing -> Err "Parse Error"
    Just ast -> Ok ast

parseType : String -> Result String Ty
parseType src =
  case Peg.parse src pType of
    Nothing -> Err "Parse Error"
    Just ty -> Ok ty

pSp =
  Peg.char (\c -> List.member c spChars)
    |> Peg.oneOrMore
spChars =
  [' ', '\t']

pOpSp = Peg.option pSp

pLambda = Peg.match "λ"
pDot    = Peg.match "."
pLParen = Peg.match "("
pRParen = Peg.match ")"
pColon  = Peg.match ":"
pEq     = Peg.match "="
pArrow  = Peg.match "->"
pIf     = Peg.match "if"
pThen   = Peg.match "then"
pElse   = Peg.match "else"
pLet    = Peg.match "let"
pLetrec = Peg.match "letrec"
pIn     = Peg.match "in"

keyword =
  [ "True"
  , "False"
  , "if"
  , "then"
  , "else"
  , "let"
  , "letrec"
  , "in"
  ]

pDigit =
  Peg.char Char.isDigit
pLower =
  Peg.char Char.isAlpha
pUpper =
  Peg.char Char.isUpper
pNameTail =
  Peg.char (\c -> Char.isAlphaNum c || c == '_')
    |> Peg.zeroOrMore

nameHelper phead ptail =
  Peg.seq2
  phead ptail
  (\hd tl -> String.fromList (hd :: tl))
    |> Peg.andThen (\name ->
      if List.member name keyword
      then Peg.fail
      else Peg.return name
    )

pVarName =
  nameHelper pLower pNameTail
pTypeName =
  nameHelper pUpper pNameTail

pTypeExcludeArrow =
  Peg.choice
  [ \_ -> Peg.seq3 pLParen pType pRParen (\_ ty _ -> ty)
  , \_ -> (pTypeName |> Peg.map (\name -> Atomic name))
  ]
pType =
  Peg.choice
  [ \_ -> Peg.seq3 pTypeExcludeArrow pArrow pType (\l _ r -> Arrow l r)
  , \_ -> pTypeExcludeArrow
  ]

pVarDecl =
  Peg.seq3
  pVarName pColon pType
  (\var _ ty -> ( var, ty ))

pId =
  pVarName |> Peg.map Id

pAbs =
  Peg.seq4
  pLambda (Peg.join pSp pVarDecl) pDot pAst
  (\_ vars _ inner ->
    List.foldr
    (\( name, ty ) inner_ -> Abs name ty inner_)
    inner
    vars
  )

pApp =
  let
    pParen =
      Peg.intersperseSeq3 pOpSp
      pLParen pAst pRParen
      (\_ inner _ -> inner)

    pTermExceptParen =
      Peg.choice
      [ \_ -> pId
      , \_ -> pTrue
      , \_ -> pFalse
      , \_ -> pInt
      , \_ -> pParen
      ]

    pAExpHd =
      Peg.choice
      [ \_ -> pTermExceptParen
      , \_ -> pParen
      ]

    pAExpTl =
      Peg.choice
      [ \_ -> Peg.seq2 pSp pTermExceptParen (\_ p -> p)
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

pTrue =
  Peg.match "True"
    |> Peg.map (always TmTrue)

pFalse =
  Peg.match "False"
    |> Peg.map (always TmFalse)

pInt =
  pDigit
    |> Peg.oneOrMore
    |> Peg.andThen (\chars ->
      case chars |> String.fromList |> String.toInt of
        Just i -> Peg.return <| TmInt i
        Nothing -> Peg.fail
    )

pIfExp =
  Peg.intersperseSeq6 pSp
  pIf pAst pThen pAst pElse pAst
  (\_ c _ t _ f -> If c t f)

pLetExp =
  let
    pVarDef =
      Peg.intersperseSeq3 pOpSp pVarName pEq pAst
      (\var _ def -> ( var, def ))
  in
    Peg.intersperseSeq4 pSp pLet pVarDef pIn pAst
    (\_ ( var, def ) _ t -> Let var def t)

pLetrecExp =
  let
    pVarDef =
      Peg.intersperseSeq3 pOpSp pVarDecl pEq pAst
      (\( var, ty ) _ def -> ( var, ty, def ))
  in
    Peg.intersperseSeq4 pSp pLetrec pVarDef pIn pAst
    (\_ ( var, ty, def ) _ t -> Letrec var ty def t)


pAst =
  Peg.choice
  [ \_ -> pApp
  , \_ -> pIfExp
  , \_ -> pLetExp
  , \_ -> pLetrecExp
  , \_ -> pTrue
  , \_ -> pFalse
  , \_ -> pInt
  , \_ -> pId
  , \_ -> pAbs
  ]