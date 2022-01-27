module Options where

import SimpleGetOpt
import Text.Read(readMaybe)
import Zen
import Parser

data Options = Options
  { optVerboseSMT   :: Bool
  , optGuesses      :: Int
  , optShowHelp     :: Bool
  , optUseRule      :: Maybe Rule
  }

options :: OptSpec Options
options = OptSpec
  { progDefaults =
    Options { optVerboseSMT = False
            , optShowHelp   = False
            , optGuesses    = 0
            , optUseRule    = Nothing
            }

  , progOptions =
    [ Option ['v'] ["verbose"]
      "Show solver interactions"
      $ NoArg $ \o -> Right o { optVerboseSMT = True }

    , Option ['g'] ["guess"]
      "Start with this many guesses (default: 0)"
      $ ReqArg "NUMBER" $ \s o ->
        case readMaybe s of
          Just n | n > 0 -> Right o { optGuesses = n }
          _ -> Left "Invalid number of guesses"

    , Option ['h'] ["help"]
      "Show this help"
      $ NoArg $ \o -> Right o { optShowHelp = True }
    ]

  , progParamDocs = [("RULE", "Use this rule")]
  , progParams = \x o -> do r <- parseRule x
                            pure o { optUseRule = Just r }
  }

getArgs :: IO Options
getArgs = getOpts options

showHelp :: IO ()
showHelp = dumpUsage options
