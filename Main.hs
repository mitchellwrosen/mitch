{-# language LambdaCase #-}

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
      [ subcommand "docker" dockerParser dockerInfo
      , subcommand "pid" pidParser pidInfo
      ])

subcommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
subcommand n p m =
  command n (info p m)

--------------------------------------------------------------------------------
-- docker
--------------------------------------------------------------------------------

dockerParser :: Parser (IO ())
dockerParser =
  hsubparser
    (mconcat
      [ subcommand "gc" dockerGcParser dockerGcInfo
      ])

dockerInfo :: InfoMod (IO ())
dockerInfo =
  progDesc "Docker porcelain"

dockerGcParser :: Parser (IO ())
dockerGcParser =
  pure $ do
    putStrLn "# Removing exited containers"
    readCreateProcess (shell "docker ps -aq --filter=status=exited") "" >>= \case
      "" ->
        pure ()
      ids ->
        callCommand ("docker rm " ++ unwords (lines ids))

    putStrLn "# Removing dangling images"
    readCreateProcess (shell "docker images -q --filter=dangling=true") "" >>= \case
      "" ->
        pure ()
      ids ->
        callCommand ("docker rmi " ++ unwords (lines ids))

dockerGcInfo :: InfoMod (IO ())
dockerGcInfo =
  progDesc "Garbage collect stopped containers and dangling images"

--------------------------------------------------------------------------------
-- pid
--------------------------------------------------------------------------------

pidParser :: Parser (IO ())
pidParser =
  (\s -> callCommand ("pidof " ++ s))
    <$> strArgument (metavar "NAME")

pidInfo :: InfoMod (IO ())
pidInfo =
  progDesc "Find the pid of a running process"
