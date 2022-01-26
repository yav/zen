module ModelUI (ModelUI(..), initModelUI, nextModelUI) where

import Zen

data ModelUI =
  ModelUI
    { cur   :: Object
    , model :: [ Thing ] -- reversed
    , len   :: Int
    , prev  :: Maybe ModelUI
    }

initModelUI :: ModelUI
initModelUI =
  ModelUI
    { cur   = Object Red Circle
    , model = []
    , len   = 0
    , prev  = Nothing
    }

nextModelUI :: Char -> ModelUI -> Either ModelUI (Maybe Model)
nextModelUI c s =
  case c of
    '\DEL' -> case prev s of
                Nothing -> Right Nothing
                Just s1 -> Left s1
    '\n'   -> Right (Just (take 5 (reverse (model s) ++ [Full (cur s)])))
    _ | len s == 5 -> Left s

    ' ' -> Left
           case model s of
             Full _ : _ -> s { model = Empty : model s
                             , len = len s + 1
                             , prev = Just s }
             _ -> s

    ',' -> Left s { model = Full (cur s) : model s, len = len s + 1
                  , prev = Just s }

    _ | Just col <- isColor c -> Left s { cur = (cur s) { color = col }
                                        , prev = Just s }
      | Just sh  <- isShape c -> Left s { cur = (cur s) { shape = sh  }
                                        , prev = Just s }
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
