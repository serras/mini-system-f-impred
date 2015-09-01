{-# LANGUAGE LambdaCase #-}
module Parser where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Unbound.Generics.LocallyNameless

import Syntax

parseExpr :: Parsec String s Expr
parseExpr = do e <- parseExpr_
               rest e
            where rest x = do reservedOp "@"
                              t <- parseTy
                              rest (AppTy x t)
                       <|> do y <- parseExpr_
                              rest (App x y)
                       <|> return x

parseExpr_ :: Parsec String s Expr
parseExpr_ = parens parseExpr
         <|> try ((\(x, t) e -> AnnLam (bind (s2n x, embed t) e))
                        <$  reservedOp "\\"
                        <*> parens ((,) <$> identifier
                                        <*  reservedOp "::"
                                        <*> parseTy)
                        <*  reservedOp "->"
                        <*> parseExpr)
         <|> (\x e -> Lam (bind (s2n x) e))
                        <$  reservedOp "\\"
                        <*> identifier
                        <*  reservedOp "->"
                        <*> parseExpr
         <|> Var <$> (s2n <$> identifier)

parseTy :: Parsec String s Ty
parseTy = foldr1 tyArrow <$> parseTy_ `sepBy1` reservedOp "->"

parseTy_ :: Parsec String s Ty
parseTy_ = parens parseTy
       <|> (\x t -> TyForAll (bind (s2n x) t))
                      <$> braces identifier
                      <*> parseTy
       <|> TyCon <$  char '\''
                 <*> identifier
                 <*> many parseTy
       <|> TyVar <$> (s2n <$> identifier)

parseDecl :: Parsec String s Decl
parseDecl = Import <$  reserved "import"
                   <*> (s2n <$> identifier)
                   <*  reservedOp "::"
                   <*> parseTy
                   <*  reservedOp ";"
        <|> try (Typed <$> (s2n <$> identifier)
                       <*  reservedOp "::"
                       <*> parseTy
                       <*  reservedOp "="
                       <*> parseExpr
                       <*  reservedOp ";")
        <|> Untyped <$> (s2n <$> identifier)
                    <*  reservedOp "="
                    <*> parseExpr
                    <*  reservedOp ";"

parseProgram :: Parsec String s Program
parseProgram = many parseDecl

--- The lexer ---
lexer :: T.TokenParser t
lexer = T.makeTokenParser haskellDef
parens :: Parsec String s a -> Parsec String s a
parens = T.parens lexer
braces :: Parsec String s a -> Parsec String s a
braces = T.braces lexer
brackets :: Parsec String s a -> Parsec String s a
brackets = T.brackets lexer
comma :: Parsec String s String
comma = T.comma lexer
commaSep :: Parsec String s a -> Parsec String s [a]
commaSep = T.commaSep lexer
commaSep1 :: Parsec String s a -> Parsec String s [a]
commaSep1 = T.commaSep1 lexer
identifier :: Parsec String s String
identifier = T.identifier lexer
reserved :: String -> Parsec String s ()
reserved = T.reservedOp lexer
reservedOp :: String -> Parsec String s ()
reservedOp = T.reservedOp lexer
integer :: Parsec String s Integer
integer = T.integer lexer
stringLiteral :: Parsec String s String
stringLiteral = T.stringLiteral lexer
