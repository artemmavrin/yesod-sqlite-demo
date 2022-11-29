{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Expr
  ( Expr(Lit, Var, Neg, Add, Sub, Mul, Div)
  , evaluateExpr
  , isValidName
  , parseExpr
  , variablesOf
  ) where

import Control.Applicative ((<|>))
import Data.Char (isAlpha)
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec
  ( ParseError
  , ParsecT
  , Stream
  , between
  , chainl1
  , digit
  , eof
  , letter
  , many
  , many1
  , option
  , parse
  , try
  )
import Text.Parsec.Char (char, spaces)

data Expr
  = Lit Double
  | Var Text
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show)

isValidName :: Text -> Bool
isValidName = Text.all isAlpha

variablesOf :: Expr -> Set Text
variablesOf = go Set.empty
  where
    go s (Lit _) = s
    go s (Var x) = Set.insert x s
    go s (Neg a) = go s a
    go s (Add a b) = go (go s a) b
    go s (Sub a b) = go (go s a) b
    go s (Mul a b) = go (go s a) b
    go s (Div a b) = go (go s a) b

evaluateExpr :: Map Text Double -> Expr -> Maybe Double
evaluateExpr _ (Lit d) = Just d
evaluateExpr env (Var x) = Map.lookup x env
evaluateExpr env (Neg a) = fmap negate (evaluateExpr env a)
evaluateExpr env (Add a b) = (+) <$> evaluateExpr env a <*> evaluateExpr env b
evaluateExpr env (Sub a b) = (-) <$> evaluateExpr env a <*> evaluateExpr env b
evaluateExpr env (Mul a b) = (*) <$> evaluateExpr env a <*> evaluateExpr env b
evaluateExpr env (Div a b) = (/) <$> evaluateExpr env a <*> evaluateExpr env b

parseExpr :: Stream s Identity Char => s -> Either ParseError Expr
parseExpr = parse (expr <* eof) ""

expr :: Stream s m Char => ParsecT s u m Expr
expr = pad addOrSub

addOrSub :: Stream s m Char => ParsecT s u m Expr
addOrSub = chainl1 mulOrDiv (pad (try (Add <$ char '+') <|> Sub <$ char '-'))

mulOrDiv :: Stream s m Char => ParsecT s u m Expr
mulOrDiv = chainl1 negative (pad (try (Mul <$ char '*') <|> Div <$ char '/'))

negative :: Stream s m Char => ParsecT s u m Expr
negative = spaces *> (foldl (.) id <$> many (Neg <$ char '-') <*> parensOrAtom)

parensOrAtom :: Stream s m Char => ParsecT s u m Expr
parensOrAtom = try (parens expr) <|> try literal <|> variable

literal :: Stream s m Char => ParsecT s u m Expr
literal = fmap (Lit . read) double
  where
    double = (++) <$> natural <*> decimal
    natural = many1 digit
    decimal = option "" ((:) <$> char '.' <*> natural)

variable :: Stream s m Char => ParsecT s u m Expr
variable = Var . Text.pack <$> many1 letter

pad :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
pad = between spaces spaces

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = between (char '(') (char ')')
