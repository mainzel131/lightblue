module DTS.Alligator.AlexHappy.Syntax where

data Expr
  = Sout String
  | File String String
  | PreNum Int
  | Formula String String String String
  deriving (Eq,Show)