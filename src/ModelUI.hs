module ModelUI (ModelUI(..), initModelUI, nextModelUI) where

import Zen

data ModelUI =
  ModelUI
    { cur   :: Thing
    , model :: [ Thing ] -- reversed
    , len   :: Int
    , prev  :: Maybe ModelUI
    }

initModelUI :: ModelUI
initModelUI =
  ModelUI
    { cur   = Empty
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
    '\n'   -> Right (Just (take 5 (reverse (model s) ++ [cur s])))
    _ | len s == 5 -> Left s

    ' ' -> Left
           case model s of
             [] -> s { cur = Empty, prev = Just s }
             Full _ : _ -> s { model = Empty : model s
                             , len = len s + 1
                             , prev = Just s }
             _ -> s

    ',' -> case (model s, cur s) of
             ([], Empty) -> Left s
             _ -> Left s { model = cur s : model s, len = len s + 1
                         , prev = Just s }

    _ | Just col <- isColor c -> Left s { cur = Full (doGetCur { color = col })
                                        , prev = Just s }
      | Just sh  <- isShape c -> Left s { cur = Full (doGetCur { shape = sh })
                                        , prev = Just s }
      | otherwise -> Left s

  where
  doGetCur = case cur s of
               Empty -> Object { color = Red, shape = Circle }
               Full o -> o

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
