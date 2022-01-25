module Zen where

data Color  = Red | Green | Blue
              deriving (Eq,Ord,Show,Read)

data Shape    = Circle | Triangle | Square
              deriving (Eq,Ord,Show,Read)

data ObjectProps color shape = Object { color :: color, shape :: shape }
  deriving (Show, Eq, Ord)

data Polarity = Yes | No
  deriving (Eq,Show)

data Constraint a = Is Polarity a
                  | Unconstrained
  deriving Show

data Constraint2 a = Same | Different | Unspecified
  deriving Show


type Object   = ObjectProps Color Shape
type Prop     = ObjectProps (Constraint Color) (Constraint Shape)
type Prop2    = ObjectProps (Constraint2 Color) (Constraint2 Shape)

data Op      = Eq | Lt | Leq
  deriving Show

data PosRule =
    Exist Prop
  | ExistAdjacent Prop Prop Prop2
  | ExistBefore Prop Prop Prop2
  | Compare Op Term Term
    deriving Show

data Term =
    Const Int
  | Count Prop
    deriving Show

data Rule = Rule Polarity PosRule
  deriving Show

class Neg a where
  neg :: a -> a

instance Neg Polarity where
  neg p =
    case p of
      Yes -> No
      No  -> Yes

instance Neg Rule where
  neg (Rule p pr) = Rule (neg p) pr

data Thing = Empty | Full Object
  deriving (Eq,Ord)

type Model  = [ Thing ]

data Command  = Guess Rule
              | Check Model Polarity
              | Giveup



--------------------------------------------------------------------------------

data Token = T String | I Int
  deriving Show

lexer :: String -> [Token]
lexer = concatMap toToken . words
  where
  toToken x
    | (a,_: (b@(_ : _))) <- break (== '-') x = [ T (a ++ "-"), T b ]
    | [(a,"")] <- reads x                    = [ I a ]
    | otherwise                              = [ T x ]
