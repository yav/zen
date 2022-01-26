module ModelUI (ModelUI(..), initModelUI, nextModelUI) where

import Zen

data ModelUI =
  ModelUI
    { cur :: Object
    , model :: [ Thing ] -- reversed
    , len :: Int
    , prev :: ModelUI
    }

initModelUI :: ModelUI
initModelUI =
  ModelUI
    { cur   = Object Red Circle
    , model = []
    , len   = 0
    , prev  = initModelUI
    }

nextModelUI :: Char -> ModelUI -> Either ModelUI Model
nextModelUI c s =
  case c of
    '\DEL' -> Left (prev s)
    '\n'   -> Right (reverse (model s))
    _ | len s == 5 -> Left s

    ' ' -> Left
           case model s of
             Full _ : _ -> s { model = Empty : model s
                             , len = len s + 1
                             , prev = s }
             _ -> s

    ',' -> Left s { model = Full (cur s) : model s, len = len s + 1, prev = s }

    _ | Just col <- isColor c -> Left s { cur = (cur s) { color = col } }
      | Just sh  <- isShape c -> Left s { cur = (cur s) { shape = sh  } }
      | otherwise -> Left s

isColor :: Char -> Maybe Color
isColor c =
  case c of
    'r' -> Just Red
    'g' -> Just Green
    'b' -> Just Blue
    _   -> Nothing

isShape :: Char -> Maybe Shape
isShape c =
  case c of
    'c' -> Just Circle
    't' -> Just Triangle
    's' -> Just Square
    _   -> Nothing
