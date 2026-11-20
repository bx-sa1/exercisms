{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError (..),
    ForthState,
    evalText,
    toList,
    emptyState,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Functor
import Data.Maybe
import Data.Text as T
import Data.Text.Read

data ForthError
  = DivisionByZero
  | StackUnderflow
  | InvalidWord
  | UnknownWord T.Text
  deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int],
    userWords :: [(Text, [ForthSymbol])]
  }
  deriving (Show, Eq)

data ForthSymbol
  = TokNum Int
  | TokWord Text
  | TokWordDef Text [ForthSymbol]
  deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState {stack = [], userWords = []}

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText text state = do
  syms <- parse text
  eval syms state

parse :: T.Text -> Either ForthError [ForthSymbol]
parse text
  | T.null text = Right []
  | c == ':' = userWord cs
  | isDigit c = number c cs
  | isAlpha c || c `T.elem` "+-*/" = word c cs
  | isSpace c = parse cs
  | otherwise = Left InvalidWord
  where
    c = T.head text
    cs = T.tail text

userWord :: T.Text -> Either ForthError [ForthSymbol]
userWord cs =
  let (name, cs') = T.break isSpace (T.stripStart cs)
      (def, cs'') = T.break (== ';') cs'
   in if isDigit $ T.head name
      then Left InvalidWord
      else do
        pdef <- parse def
        rest <- parse (T.drop 1 cs'')
        return (TokWordDef (T.toLower name) pdef : rest)

word :: Char -> T.Text -> Either ForthError [ForthSymbol]
word c cs =
  let (str, cs') = T.break isSpace cs
      w = T.toLower $ T.cons c str
   in do
        rest <- parse cs'
        return (TokWord w : rest)

number :: Char -> T.Text -> Either ForthError [ForthSymbol]
number c cs =
  let (digs, cs') = T.span isDigit cs
      num = T.cons c digs
   in do
        dec <- first (UnknownWord . T.pack) $ signed decimal num
        rest <- parse cs'
        return (TokNum (fst dec) : rest)

eval :: [ForthSymbol] -> ForthState -> Either ForthError ForthState
eval [] state = Right state
eval (TokNum n : ts) state = pushStack n state >>= eval ts
eval (TokWord w : ts) state = evalWord w state >>= eval ts
eval (TokWordDef n d : ts) state = addUserWord n d state >>= eval ts

evalWord :: Text -> ForthState -> Either ForthError ForthState
evalWord t state
  | isJust $ getUserWord t state = evalUserWord t state
  | t == "dup" =
      popStack state
        >>= \(a, s) -> pushStack a s >>= pushStack a
  | t == "drop" = do
      (_, s1) <- popStack state
      return s1
  | t == "swap" = do
      (a, s1) <- popStack state
      (b, s2) <- popStack s1
      s3 <- pushStack a s2
      pushStack b s3
  | t == "over" = do
      (a, s1) <- popStack state
      (b, s2) <- popStack s1
      s3 <- pushStack b s2
      s4 <- pushStack a s3
      pushStack b s4
  | T.head t `T.elem` "+-*/" = do
      (b, s1) <- popStack state
      (a, s2) <- popStack s1
      case t of
        "+" -> pushStack (a + b) s2
        "-" -> pushStack (a - b) s2
        "*" -> pushStack (a * b) s2
        "/" ->
          if b /= 0
            then pushStack (a `div` b) s2
            else Left DivisionByZero
  | otherwise = Left $ UnknownWord t

pushStack :: Int -> ForthState -> Either ForthError ForthState
pushStack num state = Right $ state {stack = num : stack state}

popStack :: ForthState -> Either ForthError (Int, ForthState)
popStack state
  | Prelude.null $ stack state = Left StackUnderflow
  | otherwise =
      let (x : xs) = stack state
       in Right (x, state {stack = xs})

addUserWord :: Text -> [ForthSymbol] -> ForthState -> Either ForthError ForthState
addUserWord name def state = Right $ state {userWords = (name, def) : userWords state}

evalUserWord :: Text -> ForthState -> Either ForthError ForthState
evalUserWord name state = do
  let wordDef = getUserWord name state
      rewindState = state { userWords = Prelude.drop 1 $ Prelude.dropWhile ((/= name) . fst) $ userWords state }
  newState <- maybe (Left $ UnknownWord name) (`eval` rewindState) wordDef
  return state { stack = stack newState }

getUserWord :: Text -> ForthState -> Maybe [ForthSymbol]
getUserWord name state = lookup name $ userWords state

toList :: ForthState -> [Int]
toList = Prelude.reverse . stack
