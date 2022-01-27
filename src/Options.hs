module Options where

import SimpleGetOpt
import Zen
import Parser

data Options = Options
  { optVerboseSMT :: Bool
  , optShowHelp   :: Bool
  , optUseRule    :: Maybe Rule
  }

options :: OptSpec Options
options = OptSpec
  { progDefaults =
    Options { optVerboseSMT = False
            , optShowHelp   = False
            , optUseRule    = Nothing
            }

  , progOptions =
    [ Option ['v'] ["verbose"]
      "Show solver interactions"
      $ NoArg $ \o -> Right o { optVerboseSMT = True }

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
