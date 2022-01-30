module Main where

import qualified Data.Map as Map
import Data.List(groupBy,intercalate,foldl')
import Text.Read(readMaybe)
import Control.Monad(unless,when,forM_,msum)
import Control.Monad.IO.Class(liftIO)
import Control.Exception(finally)
import qualified SimpleSMT as SMT
import SimpleSMT(SExpr)
import System.Random.TF(newTFGen)
import System.Random.TF.Gen
import System.Random.TF.Instances
import System.Console.Haskeline
import System.IO
import System.Console.ANSI hiding (Color(..))
import qualified System.Console.ANSI as ANSI

import Zen
import ModelUI
import Options
import Parser

class ToSMT a where
  toSMT :: a -> SExpr
  fromSMT :: SMT.Value -> Maybe a

fromSExpr :: ToSMT a => SMT.SExpr -> Maybe a
fromSExpr = fromSMT . SMT.Other

fromSMTRead :: Read a => SMT.Value -> Maybe a
fromSMTRead v =
  case v of
    SMT.Other (SMT.Atom r) -> readMaybe r
    _ -> Nothing

fromSMTCon :: String -> SMT.Value -> Maybe [SMT.SExpr]
fromSMTCon x v =
  case v of
    SMT.Other (SMT.List (SMT.Atom c:cs)) | x == c -> Just cs
    _ -> Nothing


fromSMTCon0 :: String -> a -> SMT.Value -> Maybe a
fromSMTCon0 x a v =
  case v of
    SMT.Other (SMT.Atom y) | x == y -> pure a
    _ -> Nothing

fromSMTCon1 :: ToSMT a => String -> (a -> b) -> SMT.Value -> Maybe b
fromSMTCon1 con f v =
  do [x] <- fromSMTCon con v
     f <$> fromSExpr x

fromSMTCon2 :: (ToSMT a, ToSMT b) =>
  String -> (a -> b -> c) -> SMT.Value -> Maybe c
fromSMTCon2 con f v =
  do [x,y] <- fromSMTCon con v
     f <$> fromSExpr x <*> fromSExpr y

fromSMTCon3 :: (ToSMT a, ToSMT b, ToSMT c) =>
  String -> (a -> b -> c -> d) -> SMT.Value -> Maybe d
fromSMTCon3 con f v =
  do [x,y,z] <- fromSMTCon con v
     f <$> fromSExpr x <*> fromSExpr y <*> fromSExpr z

class ToSMT a => TypeName a where
  typeName :: f a -> SExpr

instance ToSMT Color where
  toSMT = SMT.const . show
  fromSMT = fromSMTRead

instance TypeName Color where
  typeName _ = SMT.const "Color"

instance ToSMT Shape where
  toSMT = SMT.const . show
  fromSMT = fromSMTRead

instance TypeName Shape where
  typeName _ = SMT.const "Shape"


instance (ToSMT a, ToSMT b) => ToSMT (ObjectProps a b) where
  toSMT o = SMT.fun "Object" [ toSMT (color o), toSMT (shape o) ]
  fromSMT = fromSMTCon2 "Object" Object

instance ToSMT Polarity where
  toSMT = SMT.const . show
  fromSMT = fromSMTRead

instance (TypeName a) => ToSMT (Constraint a) where
  toSMT c =
    case c of
      Is pol a      -> SMT.fun "Is" [ toSMT pol, toSMT a ]
      Unconstrained -> SMT.fun "as" [ SMT.const "Unconstrained"
                                    , SMT.fun "Constraint" [typeName c ] ]

  fromSMT v =
    msum
      [ fromSMTCon2 "Is" Is v
      , do [SMT.Atom "Unconstrained",_] <- fromSMTCon "as" v
           pure Unconstrained
      ]


instance ToSMT (Constraint2 a) where
  toSMT = SMT.const . show
  fromSMT = fromSMTRead

instance ToSMT Op where
  toSMT = SMT.const . show
  fromSMT = fromSMTRead

instance ToSMT Int where
  toSMT = SMT.int . toInteger
  fromSMT v =
    case v of
      SMT.Int i -> Just (fromIntegral i) -- assuming fits in Int
      SMT.Other (SMT.Atom x) -> readMaybe x
      SMT.Other (SMT.List [SMT.Atom "-", SMT.Atom x]) -> negate <$> readMaybe x
      _         -> Nothing

instance ToSMT Term where
  toSMT t =
    case t of
      Const n -> SMT.fun "Const" [ SMT.int (toInteger n) ]
      Count p -> SMT.fun "Count" [ toSMT p ]

  fromSMT v =
    msum
      [ fromSMTCon1 "Const" Const v
      , fromSMTCon1 "Count" Count v
      ]

instance ToSMT PosRule where
  toSMT pr =
    case pr of
      Exist p ->
        SMT.fun "Exist" [ toSMT p ]
      ExistAdjacent p q c ->
        SMT.fun "ExistAdjacent" [ toSMT p, toSMT q, toSMT c ]
      ExistBefore p q c   ->
        SMT.fun "ExistBefore" [ toSMT p, toSMT q, toSMT c ]
      Compare op t1 t2    ->
        SMT.fun "Compare" [ toSMT op, toSMT t1, toSMT t2 ]

  fromSMT v =
    msum
      [ fromSMTCon1 "Exist" Exist v
      , fromSMTCon3 "ExistAdjacent" ExistAdjacent v
      , fromSMTCon3 "ExistBefore" ExistBefore v
      , fromSMTCon3 "Compare" Compare v
      ]

instance ToSMT Rule where
  toSMT (Rule p pr) = SMT.fun "Rule" [ toSMT p, toSMT pr ]
  fromSMT v =
    case v of
      SMT.Other e -> fromSMTCon2 "Rule" Rule (SMT.Other (expandLet e))
      _ -> Nothing

instance ToSMT Thing where
  toSMT t =
    case t of
      Empty  -> SMT.const "Empty"
      Full x -> SMT.fun "Full" [ toSMT x ]
  fromSMT v =
    msum [ fromSMTCon0 "Empty" Empty v
         , fromSMTCon1 "Full" Full v
         ]

modelToSMT :: Model -> SMT.SExpr
modelToSMT ms = SMT.fun "Model" [ toSMT x | x <- ms ]

modelFromSMT :: SMT.Value -> Maybe Model
modelFromSMT v =
  do as <- fromSMTCon "Model" v
     mapM fromSExpr as

expandLet :: SMT.SExpr -> SMT.SExpr
expandLet = go Map.empty
  where
  go env e =
    case e of
      SMT.Atom x -> Map.findWithDefault e x env
      SMT.List [ SMT.Atom "let", SMT.List defs, e1 ]
        | Just ds <- mapM isDef defs ->
          go (foldl' (\m (x,v) -> Map.insert x v m) env ds) e1
      SMT.List xs -> SMT.List (map (go env) xs)

    where isDef d =
             case d of
               SMT.List [ SMT.Atom x, v ] -> Just (x,go env v)
               _                          -> Nothing

--------------------------------------------------------------------------------
-- Models


printModel :: Model -> IO ()
printModel = mapM_ (\x -> putStr " " >> printThing x)

printThing :: Thing -> IO ()
printThing t =
  case t of
    Empty  -> putStr "⏠"
    Full o -> printObject o

printObject :: Object -> IO ()
printObject (Object c s) =
  do setSGR [ SetColor Foreground Dull col ]
     putStr sym
     setSGR [ SetDefaultColor Foreground ]
  where
  col = case c of
          Red   -> ANSI.Red
          Green -> ANSI.Green
          Blue  -> ANSI.Blue

  sym = case s of
          Circle   -> "●"
          Triangle -> "▲"
          Square   -> "■"


printState :: State -> IO ()
printState s =
  do when (clear s)
      do clearScreen
         setCursorPosition 0 0

     unless (null (message s)) (putStrLn (message s))

     let heading x = putStrLn (bold x)
     heading ("Guess points: " ++ show (points s))

     let modelGrid ms = case ms of
                          [] -> pure ()
                          _  -> do let (as,bs) = splitAt 5 ms
                                   forM_ as \m ->
                                      do printModel m >> putStr "   "
                                   putStrLn ""
                                   modelGrid bs
     unless (null (posExamples s))
       do heading "Valid:"
          modelGrid (posExamples s)

     unless (null (negExamples s))
       do heading "Invalid:"
          modelGrid (negExamples s)

     unless (null (badGuesses s))
        do heading "Guesses:"
           forM_ (badGuesses s) \(r,p,m) ->
             do putStr (pp r ++ " (")
                putStr (case p of
                          Yes -> "accepts "
                          No  -> "rejects ")
                printModel m
                putStrLn " )"

     let key x = "[" ++ bold x ++ "]"

     case status s of
       Ready ->
         do putStrLn ""
            putStrLn $ key "v" ++ " enter valid model"
            putStrLn $ key "i" ++ " enter invalid model"
            unless (points s < 1) (putStrLn $ key "g" ++ " guess rule")
            putStrLn $ key "h" ++ " print rule grammar"
            putStrLn $ key "s" ++ " suggest a rule"
            putStrLn $ key "q" ++ " quit and show rule"

       Solved ->
         do putStrLn "Solved!  The rule is:"
            putStrLn (pp (theRule s))

       EnteringModel pol uis ->
         do putStr case pol of
                     Yes -> "Valid: "
                     No  -> "Invalid: "
            printModel (reverse (model uis))
            unless (len uis == 5)
               do putStr " ["
                  printThing (cur uis)
                  putStr "]"
                  printModel (replicate (5 - 1 - len uis) Empty)
            putStrLn ""
            putStrLn $ concat [ key "r", " red   ", key "c", " circle   ", key "Space", " empty" ]
            putStrLn $ concat [ key "g", " green ", key "t", " triangle ", key ","    , "     next position" ]
            putStrLn $ concat [ key "b", " blue  ", key "s", " square   ", key "Enter", " submit model" ]
            putStrLn $ concat [ "                       ", key "Back", "  undo" ]

ruleHelp :: String
ruleHelp = unlines
  [ "RULE  := " ++ sequ [ opt (term "no"), "PROP" ]
  , "       | " ++ sequ [ opt (term "no"), "PROP", "REL", "PROP", opt "QUAL" ]
  , "       | " ++ sequ [ term "count", "PROP", "OP", "TERM" ]
  , ""
  , "PROP  := " ++ alts [ term "thing", "COLOR", "SHAPE", sequ [ "COLOR", "SHAPE" ] ]
  , ""
  , "COLOR := " ++ opt (term "non-") ++ paren (terms [ "red", "green", "blue" ])
  , "SHAPE := " ++ opt (term "non-") ++ paren (terms [ "circle", "triangle", "square" ])
  , ""
  , "REL   := " ++ terms [ "touches", "before" ]
  , "OP    := " ++ terms [ "=", "/=", "<", ">", "<=", ">=" ]
  , ""
  , "QUAL  := " ++ sequ [ term "of", "CTR", opt (sequ [ term "and", "CTR" ]) ]
  , "CTR   := " ++ sequ [ paren (terms ["same", "different"]), paren (terms ["shape", "color"]) ]
  , ""
  , "TERM  := " ++ alts [ sequ [ term "count", "PROP" ], "NUMBER" ]
  ]
  where
  term  = bold
  opt x = "[" ++ x ++ "]"
  alts  = intercalate " | "
  terms = alts . map term
  sequ  = unwords
  paren x = "(" ++ x ++ ")"

bold :: String -> String
bold x = setSGRCode [ SetConsoleIntensity BoldIntensity ] ++ x ++
         setSGRCode [ SetConsoleIntensity NormalIntensity ]

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Pretty printing

class PP a where
  pp :: a -> String

instance PP Color where
  pp c =
    case c of
      Red   -> "red"
      Green -> "green"
      Blue  -> "blue"

instance PP Shape where
  pp s =
    case s of
      Circle    -> "circle"
      Triangle  -> "triangle"
      Square    -> "square"

class PPThing a where
  ppThing :: f a -> String

instance PPThing Color where
  ppThing _ = "color"

instance PPThing Shape where
  ppThing _ = "shape"

class PPEmpty a where
  ppEmpty :: a -> String

instance PPEmpty (Constraint a) where
  ppEmpty _ = "thing"

instance PPEmpty (Constraint2 a) where
  ppEmpty _ = ""

instance PP a => PP (Constraint a) where
  pp c =
    case c of
      Is p a ->
        case p of
          Yes -> pp a
          No  -> "non-" ++ pp a
      Unconstrained -> ""

instance (PPEmpty a, PP a, PP b) => PP (ObjectProps a b) where
  pp o =
    let as = pp (color o)
        bs = pp (shape o)
    in if null as
          then if null bs then ppEmpty (color o) else bs
          else if null bs then as else as ++ " " ++ bs

instance PP Term where
  pp t =
    case t of
      Const i -> show i
      Count p -> "count " ++ pp p

instance PP PosRule where
  pp r =
    case r of
      Exist p           -> pp p
      ExistAdjacent p q c -> pp p ++ " touches " ++ pp q ++ " " ++ ppProp2 c
      ExistBefore p q c  -> pp p ++ " before " ++ pp q ++ " " ++ ppProp2 c
      Compare op t1 t2  -> pp t1 ++ ops ++ pp t2
          where ops = case op of
                        Eq  -> " = "
                        Lt  -> " < "
                        Leq -> " <= "

ppProp2 :: Prop2 -> String
ppProp2 p =
  case (color p, shape p) of
    (Unspecified, Unspecified) -> ""
    (Unspecified, x)  -> "of " ++ pp x
    (x,Unspecified)   -> "of " ++ pp x
    (x,y)             -> "of " ++ pp x ++ " and " ++ pp y

instance PPThing a => PP (Constraint2 a) where
  pp c =
    case c of
      Unspecified -> ""
      Same        -> "same " ++ ppThing c
      Different   -> "different " ++ ppThing c

instance PP Rule where
  pp (Rule p pr) =
    case p of
      Yes -> pp pr
      No  -> case pr of
               Exist {}         -> "no " ++ pp pr
               ExistAdjacent {} -> "no " ++ pp pr
               ExistBefore {}   -> "no " ++ pp pr
               Compare op t1 t2 -> pp t1 ++ ops ++ pp t2
                 where ops = case op of
                               Eq  ->  " /= "
                               Lt  ->  " >= "
                               Leq ->  " > "


--------------------------------------------------------------------------------
-- Random

class Rand a where
  rand :: RandomGen g => g -> (a, g)

instance Rand Color where
  rand g = ([Red,Green,Blue] !! i, g1)
    where (i,g1) = randomR (0,2) g

instance Rand Shape where
  rand g = ([Circle,Triangle,Square] !! i, g1)
    where (i,g1) = randomR (0,2) g

instance Rand Op where
  rand g = ([Eq,Lt,Leq] !! i, g1)
    where (i,g1) = randomR (0,2) g

instance Rand Polarity where
  rand g = ([Yes,No] !! i, g1)
    where (i,g1) = randomR (0,1) g

instance Rand (Constraint2 a) where
  rand g = ([Same,Different,Unspecified] !! i, g1)
    where (i,g1) = randomR (0,2) g

instance Rand a => Rand (Constraint a) where
  rand g
    | i = (Unconstrained, g1)
    | let (pol,g2)  = rand g1
    , let (prop,g3) = rand g2
    = (Is pol prop, g3)
    where (i,g1) = random g

instance Rand Prop where
  rand g = (Object col sh,g2)
    where
    (col,g1) = rand g
    (sh, g2) = rand g1

instance Rand Prop2 where
  rand g = (Object a b, g2)
    where (a,g1) = rand g
          (b,g2) = rand g1

instance Rand Term where
  rand g
    | i =
      let (n,g2) = randomR (0,5) g1
      in (Const n, g2)
    | otherwise =
      let (p,g2) = rand g1
      in (Count p, g2)
    where (i,g1) = random g

instance Rand PosRule where
  rand g =
    case i :: Int of
      0 -> let (p,g2) = rand g1
           in (Exist p, g2)
      1 -> let (p,g2) = rand g1
               (q,g3) = rand g2
               (c,g4) = rand g3
           in (ExistAdjacent p q c, g4)
      2 -> let (p,g2) = rand g1
               (q,g3) = rand g2
               (c,g4) = rand g3
           in (ExistBefore p q c, g4)
      _ -> let (p,g2) = rand g1
               (r,g3) = rand g2
               (op,g4) = rand g3
           in (Compare op (Count p) r,g4)
    where
    (i,g1) = randomR (0,3) g

instance Rand Rule where
  rand g = (Rule p r, g2)
    where
    (p,g1) = rand g
    (r,g2) = rand g1






--------------------------------------------------------------------------------
-- Solver interactoins

assertRule :: SMT.Solver -> Rule -> IO ()
assertRule s r =
  SMT.assert s (SMT.fun "checkRule" [ SMT.const "theModel", toSMT r ])

assertModel :: SMT.Solver -> Model -> IO ()
assertModel s m =
  SMT.assert s (SMT.eq (SMT.const "theModel") (modelToSMT m))

assertRuleModel :: SMT.Solver -> Polarity -> Model -> IO ()
assertRuleModel s p m = SMT.assert s (SMT.fun "semPol" [ toSMT p, sm ])
  where
  sm = SMT.fun "checkRule" [  modelToSMT m, SMT.const "theRule" ]


getModel :: SMT.Solver -> IO Model
getModel s =
  do x <- SMT.getConst s "theModel"
     case modelFromSMT x of
       Just m -> pure m
       Nothing -> error "Invalid model"

-- | Check and get model if any
getModelMaybe :: SMT.Solver -> IO (Maybe Model)
getModelMaybe s =
  do res <- SMT.check s
     case res of
       SMT.Unsat    -> pure Nothing
       SMT.Sat      -> Just <$> getModel s
       SMT.Unknown  -> error "Unknown"


-- Assumes rule is asserted
checkModel :: SMT.Solver -> Model -> IO Bool
checkModel s m =
  SMT.inNewScope s
  do assertModel s m
     res <- SMT.check s
     pure case res of
            SMT.Unsat   -> False
            SMT.Sat     -> True
            SMT.Unknown -> error "Unknown"


data RuleCmp = Equivalent
             | LeftYesRightNo Model
             | LeftNoRightYes Model

-- Assumes no rule is asserted
checkRules :: SMT.Solver -> Rule -> Rule -> IO RuleCmp
checkRules s r1 r2 =
  do mb1 <- cmp r1 (neg r2)
     case mb1 of
       Nothing ->
         do mb2 <- cmp (neg r1) r2
            case mb2 of
              Nothing -> pure Equivalent
              Just m  -> pure (LeftNoRightYes m)
       Just m -> pure (LeftYesRightNo m)
  where
  cmp a b =
    do SMT.inNewScope s
        do assertRule s a
           assertRule s b
           getModelMaybe s


getSuggestion :: SMT.Solver -> [Model] -> [Model] -> IO Rule
getSuggestion s yes no =
  SMT.inNewScope s
  do mapM_ (assertRuleModel s Yes) yes
     mapM_ (assertRuleModel s No) no
     res <- SMT.check s
     case res of
       SMT.Sat -> do r <- SMT.getConst s "theRule"
                     case fromSMT r of
                       Just r1 -> pure r1
                       Nothing -> error ("Failed to parse rule: " ++ show r)
       SMT.Unsat -> error "Can't find rule"
       SMT.Unknown -> error "Unknown"


--------------------------------------------------------------------------------

data Status = Solved | Ready | EnteringModel Polarity ModelUI

data State = State
  { solver      :: SMT.Solver
  , theRule     :: Rule
  , posExamples :: [Model]
  , negExamples :: [Model]
  , badGuesses  :: [(Rule,Polarity,Model)]
  , message     :: String
  , points      :: Int
  , status      :: Status
  , clear       :: Bool
  }

blankState :: SMT.Solver -> Rule -> State
blankState s r = State
  { solver      = s
  , theRule     = r
  , posExamples = []
  , negExamples = []
  , badGuesses  = []
  , message     = ""
  , points      = 0
  , status      = Ready
  , clear       = True
  }

-- Remove adjacnet empty spaces, and also empty spaces at the beginning
normalizeModel :: Model -> Model
normalizeModel =
  pad . dropWhile isEmpty . concatMap oneEmpty . groupBy bothEmpty
  where
  pad xs = take 5 (xs ++ repeat Empty)

  isEmpty e = case e of
                Empty -> True
                _     -> False

  oneEmpty es =
    case es of
      Empty : _ -> [Empty]
      _         -> es

  bothEmpty a b = isEmpty a && isEmpty b


tryAddPosExample :: State -> IO State
tryAddPosExample s =
  SMT.inNewScope (solver s)
  do assertRule (solver s) (theRule s)
     mb <- getModelMaybe (solver s)
     pure case mb of
            Nothing -> s
            Just m  -> s { posExamples = m : posExamples s }

tryAddNegExample :: State -> IO State
tryAddNegExample s =
  SMT.inNewScope (solver s)
  do assertRule (solver s) (neg (theRule s))
     mb <- getModelMaybe (solver s)
     pure case mb of
            Nothing -> s
            Just m  -> s { negExamples = m : negExamples s }

checkExperiment :: Model -> Polarity -> State -> IO State
checkExperiment m expect s
  | m `elem` posExamples s || m `elem` negExamples s =
    pure s { message = "This is an aleady known example" }
  | otherwise =
  SMT.inNewScope (solver s)
  do assertRule (solver s) (theRule s)
     yes <- checkModel (solver s) m
     let s1 = if yes && expect == Yes || not yes && expect == No
                then s { points = points s + 1 } else s
     pure if yes then s1 { posExamples = m : posExamples s1 }
                 else s1 { negExamples = m : negExamples s1 }

checkGuess :: Bool -> Rule -> State -> IO State
checkGuess limit r s
  | limit && points s < 1 =
    pure s { message = "No guess points, do some experiments." }
  | otherwise =
  do res <- checkRules (solver s) (theRule s) r
     pure case res of
            Equivalent -> s { status = Solved }
            LeftYesRightNo m -> s { badGuesses = (r,No,m) : badGuesses s
                                  , posExamples = m : posExamples s
                                  , points = max 0 (points s - 1)
                                  }
            LeftNoRightYes m -> s { badGuesses = (r,Yes,m) : badGuesses s
                                  , negExamples = m : negExamples s
                                  , points = max 0 (points s - 1)
                                  }

main :: IO ()
main =
 do opts <- getArgs
    if optShowHelp opts then showHelp else doMain opts
  `finally` typingMode

doMain :: Options -> IO ()
doMain opts =
 do l <- SMT.newLogger (if optVerboseSMT opts then 0 else 5)
    s <- SMT.newSolver "z3" ["-smt2", "-in"] (Just l)
    SMT.loadFile s "src/Zen.z3"
    r <- case optUseRule opts of
           Just r -> pure r
           Nothing ->
             do rng <- newTFGen
                pure (fst (rand rng))
    s0 <- tryAddNegExample =<< tryAddPosExample (blankState s r)
    let s1 = s0 { points = optGuesses opts
                , clear = not (optVerboseSMT opts)
                }
    buttonMode
    runInputT defaultSettings (play s1)
    typingMode

buttonMode :: IO ()
buttonMode =
  do hSetBuffering stdin NoBuffering
     hSetEcho stdin False
     hideCursor

typingMode :: IO ()
typingMode =
  do hSetBuffering stdin LineBuffering
     hSetEcho stdin True
     showCursor

play :: State -> InputT IO ()
play s0 =
  do liftIO (printState s0)
     let s = s0 { message = "" }
     case status s of
       Solved -> pure ()

       EnteringModel pol uis ->
         do c <- liftIO getChar
            play =<<
              case nextModelUI c uis of
                Left uis1 -> pure s { status = EnteringModel pol uis1 }
                Right mb  ->
                  case mb of
                    Just m -> liftIO
                                (checkExperiment (normalizeModel m) pol
                                                  s { status = Ready })
                    Nothing -> pure s { status = Ready }
       Ready ->
         do c <- liftIO getChar
            play =<<
              case c of
                'i' -> pure s { status = EnteringModel No initModelUI }
                'v' -> pure s { status = EnteringModel Yes initModelUI }
                'q' -> pure s { status = Solved }
                'h' -> pure s { message = ruleHelp }
                'g'
                  | points s < 1 -> pure s { message = "No guess points" }
                  | otherwise ->
                  do liftIO typingMode
                     txt <- getInputLine "rule: "
                     liftIO buttonMode
                     case txt of
                       Nothing -> pure s
                       Just l ->
                         case parseRule l of
                           Left err -> pure s { message = err }
                           Right r -> liftIO (checkGuess True r s)
                's' -> liftIO do r <- getSuggestion (solver s)
                                                    (posExamples s)
                                                    (negExamples s)
                                 s1 <- checkGuess False r s
                                 pure s1 { message = "Guessing: " ++ pp r }
                _ -> pure s



