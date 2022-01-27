module Main(main) where

import Data.List(groupBy,intercalate)
import Control.Monad(unless,when,forM_)
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

class ToSMT a => TypeName a where
  typeName :: f a -> SExpr

instance ToSMT Color where
  toSMT = SMT.const . show

instance TypeName Color where
  typeName _ = SMT.const "Color"

instance ToSMT Shape where
  toSMT = SMT.const . show

instance TypeName Shape where
  typeName _ = SMT.const "Shape"


instance (ToSMT a, ToSMT b) => ToSMT (ObjectProps a b) where
  toSMT o = SMT.fun "Object" [ toSMT (color o), toSMT (shape o) ]

instance ToSMT Polarity where
  toSMT p =
    case p of
      Yes -> SMT.const "Yes"
      No  -> SMT.const "No"

instance (TypeName a) => ToSMT (Constraint a) where
  toSMT c =
    case c of
      Is pol a      -> SMT.fun "Is" [ toSMT pol, toSMT a ]
      Unconstrained -> SMT.fun "as" [ SMT.const "Unconstrained"
                                    , SMT.fun "Constraint" [typeName c ] ]


instance ToSMT (Constraint2 a) where
  toSMT c =
    case c of
      Same        -> SMT.const "Same"
      Different   -> SMT.const "Different"
      Unspecified -> SMT.const "Unspecified"

instance ToSMT Term where
  toSMT t =
    case t of
      Const n -> SMT.int (toInteger n)
      Count p -> SMT.fun "count" [ toSMT p ]

instance ToSMT PosRule where
  toSMT pr =
    case pr of
      Exist p             -> SMT.fun "existsOne" [ toSMT p ]
      ExistAdjacent p q c ->
        SMT.fun "existsAdjacent" [ toSMT p, toSMT q, toSMT c ]
      ExistBefore p q c   ->
        SMT.fun "existsBefore" [ toSMT p, toSMT q, toSMT c ]
      Compare op t1 t2    -> doOp op (toSMT t1) (toSMT t2)

doOp :: Op -> SExpr -> SExpr -> SExpr
doOp op =
  case op of
    Lt  -> SMT.lt
    Leq -> SMT.leq
    Eq  -> SMT.eq

instance ToSMT Rule where
  toSMT (Rule p pr) =
    case p of
      Yes -> toSMT pr
      No  -> SMT.not (toSMT pr)

instance ToSMT Thing where
  toSMT t =
    case t of
      Empty  -> SMT.const "Empty"
      Full x -> SMT.fun "Full" [ toSMT x ]


getObject :: SExpr -> Object
getObject x =
  case x of
    SMT.List [ SMT.Atom "Object", SMT.Atom c, SMT.Atom s ] ->
      Object (read c) (read s)
    _ -> error "Invalid object"

getThing :: SMT.Value -> Thing
getThing x =
  case x of
    SMT.Other e ->
      case e of
        SMT.Atom "Empty" -> Empty
        SMT.List [ SMT.Atom "Full", y ] -> Full (getObject y)
        _ -> error "Invalid thing"
    _ -> error "Invalid thing"


--------------------------------------------------------------------------------
-- Models


printModel :: Model -> IO ()
printModel = mapM_ printThing

printThing :: Thing -> IO ()
printThing t =
  case t of
    Empty  -> putStr "_ "
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
          Circle   -> "● "
          Triangle -> "▲ "
          Square   -> "■ "


printState :: State -> IO ()
printState s =
  do when (clear s)
      do clearScreen
         setCursorPosition 0 0

     unless (null (message s)) (putStrLn (message s))

     let heading x = putStrLn (bold x)
     heading ("Guess points: " ++ show (points s))
     unless (null (posExamples s))
       do heading "Valid:"
          forM_ (posExamples s) \m -> printModel m >> putStrLn ""

     unless (null (negExamples s))
       do heading "Invalid:"
          forM_ (negExamples s) \m -> printModel m >> putStrLn ""

     unless (null (badGuesses s))
        do heading "Guesses:"
           forM_ (badGuesses s) \(r,p,m) ->
             do putStr (pp r ++ " (")
                putStr (case p of
                          Yes -> "accepts "
                          No  -> "rejects ")
                printModel m
                putStrLn ")"

     let key x = "[" ++ bold x ++ "]"

     case status s of
       Ready ->
         do putStrLn ""
            putStrLn $ key "v" ++ " enter valid model"
            putStrLn $ key "i" ++ " enter invalid model"
            unless (points s < 1) (putStrLn $ key "g" ++ " guess rule")
            putStrLn $ key "h" ++ " print rule grammar"
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
                  printObject (cur uis)
                  putStr "]"
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
                               Eq  ->  "/= "
                               Lt  ->  ">= "
                               Leq ->  "> "


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


-- Assumes the rule is validated
assertRule :: SMT.Solver -> Rule -> IO ()
assertRule s r = SMT.assert s (toSMT r)

assertPol :: SMT.Solver -> Polarity -> SExpr -> IO ()
assertPol s p e = SMT.assert s
                  case p of
                    Yes -> e
                    No  -> SMT.not e

assertModel :: SMT.Solver -> Polarity -> Model -> IO ()
assertModel s p m = assertPol s p modelE
  where
  modelE     = SMT.andMany [ thingE i x | (i,x) <- zip [ 1 .. ] m ]
  thingE i x = SMT.eq (SMT.const ("place_" ++ show (i::Int))) (toSMT x)

getModel :: SMT.Solver -> IO Model
getModel s = imp <$> SMT.getConsts s [ "place_" ++ show i | i <- [ 1..5 :: Int]]
  where imp = normalizeModel . map (getThing . snd)

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
  do assertModel s Yes m
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

checkGuess :: Rule -> State -> IO State
checkGuess r s
  | points s < 1 = pure s { message = "No guess points, do some experiments." }
  | otherwise =
  do res <- checkRules (solver s) (theRule s) r
     pure case res of
            Equivalent -> s { status = Solved }
            LeftYesRightNo m -> s { badGuesses = (r,No,m) : badGuesses s
                                  , posExamples = m : posExamples s
                                  , points = points s - 1
                                  }
            LeftNoRightYes m -> s { badGuesses = (r,Yes,m) : badGuesses s
                                  , negExamples = m : negExamples s
                                  , points = points s - 1
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
                           Right r -> liftIO (checkGuess r s)
                _ -> pure s



