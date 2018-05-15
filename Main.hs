import Control.Monad
import Options.Applicative
import System.Process

main :: IO ()
main =
  join (customExecParser parserPrefs parserInfo)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

parserPrefs :: ParserPrefs
parserPrefs =
  prefs (showHelpOnError <> showHelpOnEmpty)

parserInfo :: ParserInfo (IO ())
parserInfo =
  info parser infoMod

infoMod :: InfoMod (IO ())
infoMod =
  mempty

parser :: Parser (IO ())
parser =
  hsubparser
    (mconcat
      [ subcommand "pid" pidParser pidInfo
      ])

subcommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
subcommand n p m =
  command n (info p m)

--------------------------------------------------------------------------------
-- pid
--------------------------------------------------------------------------------

pidParser :: Parser (IO ())
pidParser =
  (\s -> callCommand ("pidof " ++ s))
    <$> strArgument (metavar "NAME")

pidInfo :: InfoMod (IO ())
pidInfo =
  mempty
