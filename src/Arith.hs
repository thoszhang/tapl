{-# LANGUAGE OverloadedStrings #-}

module Arith (Term, eval, evalBigStep, printTerm, genTerm, isVal, termEof, parse) where

import           Control.Applicative        (liftA3)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen,
                                             NonZero, oneof, sized)
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving (Show, Eq)

instance Arbitrary Term where
    arbitrary = genTerm

genTerm :: Gen Term
genTerm = sized (\n -> oneof [genBool n, genNum n])

genBool :: Int -> Gen Term
genBool 0 = oneof $ return <$> [TmTrue, TmFalse]
genBool n = oneof [genBool 0, node] where
    node = let gb = genBool (n - 1)
               gn = genNum (n - 1)
        in oneof [TmIsZero <$> gn, liftA3 TmIf gb gb gb]

genNum :: Int -> Gen Term
genNum 0 = return TmZero
genNum n = oneof [genNum 0, node] where
    node = let gb = genBool (n - 1)
               gn = genNum (n - 1)
        in oneof [TmSucc <$> gn, TmPred <$> gn, liftA3 TmIf gb gn gn]

printTerm :: Term -> String
printTerm t = case t of
    TmTrue         -> "true"
    TmFalse        -> "false"
    TmIf  t1 t2 t3 -> unwords ["if", printTerm t1, "then", printTerm t2, "else", printTerm t3]
    TmZero         -> "0"
    TmSucc  t1     -> unwords ["succ", printTerm t1]
    TmPred  t1     -> unwords ["pred", printTerm t1]
    TmIsZero  t1   -> unwords ["iszero", printTerm t1]


isNumericVal :: Term -> Bool
isNumericVal t = case t of
    TmZero                       -> True
    TmSucc  t1 | isNumericVal t1 -> True
    _                            -> False

isVal :: Term -> Bool
isVal t = case t of
    TmTrue             -> True
    TmFalse            -> True
    t | isNumericVal t -> True
    _                  -> False

eval1 :: Term -> Maybe Term
eval1 t = case t of
    TmIf TmTrue t2 _                         -> Just t2
    TmIf TmFalse _ t3                        -> Just t3
    TmIf t1 t2 t3                            -> let t1' = eval1 t1 in TmIf <$> t1' <*> Just t2 <*> Just t3
    TmSucc t1                                -> let t1' = eval1 t1 in TmSucc <$> t1'
    TmPred TmZero                            -> Just TmZero
    TmPred (TmSucc nv1) | isNumericVal nv1   -> Just nv1
    TmPred t1                                -> let t1' = eval1 t1 in TmPred <$> t1'
    TmIsZero TmZero                          -> Just TmTrue
    TmIsZero (TmSucc nv1) | isNumericVal nv1 ->  Just TmFalse
    TmIsZero t1                              -> let t1' = eval1 t1 in TmIsZero <$> t1'
    _                                        -> Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)

evalBigStep :: Term -> Term
evalBigStep t = case t of
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmZero -> TmZero
    TmIf t1 t2 t3 -> case evalBigStep t1 of
        TmTrue  -> evalBigStep t2
        TmFalse -> evalBigStep t3
        _       -> t
    TmSucc t1 -> let nv1 = evalBigStep t1 in if isNumericVal nv1 then TmSucc  nv1 else t
    TmPred t1 -> case evalBigStep t1 of
        TmZero                        -> TmZero
        TmSucc nv1 | isNumericVal nv1 -> nv1
        _                             -> t
    TmIsZero t1 -> case evalBigStep t1 of
        TmZero                        -> TmTrue
        TmSucc nv1 | isNumericVal nv1 -> TmFalse
        _                             -> t

type Parser = M.Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 M.empty M.empty

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

term :: Parser Term
term = M.choice
    [ TmTrue <$ symbol "true"
    , TmFalse <$ symbol "false"
    , TmZero <$ symbol "0"
    , symbol "pred" *> (TmPred <$> term)
    , symbol "succ" *> (TmSucc <$> term)
    , symbol "iszero" *> (TmIsZero <$> term)
    , do
        symbol "if"
        cond <- term
        symbol "then"
        thenBranch <- term
        symbol "else"
        TmIf cond thenBranch <$> term
    ]

termEof :: Parser Term
termEof = term <* M.eof

parse :: String -> Maybe Term
parse s = case M.runParser termEof "" s of
    Left _  -> Nothing
    Right t -> Just t
