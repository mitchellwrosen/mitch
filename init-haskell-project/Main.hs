\(name : Text) ->

''
module Main
  ( main
  , sendSIGUSR1
  ) where

import Mitchell

import Exception (bracket_)
import File (FilePath, removePathForcibly)
import File.Text (readFile, writeFile)
import Process (getProcessID)
import System.Posix.Signals (sigUSR1, signalProcess)
import System.Posix.Types (CPid)
import Read (readMaybe)
import Text (pack, unpack)

main :: IO ()
main =
  bracket_
    (writeFile pidfile . pack . show =<< getProcessID)
    (removePathForcibly pidfile)
    main'

main' :: IO ()
main' =
  pure ()

--------------------------------------------------------------------------------
-- Send SIGUSR1 to the running process, if any
--------------------------------------------------------------------------------

pidfile :: FilePath
pidfile =
  ".${name}.pid"

sendSIGUSR1 :: IO ()
sendSIGUSR1 =
  go <|> pure ()
 where
  go :: IO ()
  go = do
    bytes :: Text <-
      readFile pidfile
    Just pid :: Maybe CPid <-
      pure (readMaybe (unpack bytes))
    signalProcess sigUSR1 pid
''
