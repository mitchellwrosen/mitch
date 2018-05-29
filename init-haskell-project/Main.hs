\(name : Text) ->

''
module Main
  ( main
  , sendSIGINT
  ) where

import Mitchell

import Exception (bracket_)
import File (FilePath, removePathForcibly)
import File.Text (readFile, writeFile)
import Process (getProcessID)
import System.Posix.Signals (sigINT, signalProcess)
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
-- Send SIGINT to the running process, if any
--------------------------------------------------------------------------------

pidfile :: FilePath
pidfile =
  ".${name}.pid"

sendSIGINT :: IO ()
sendSIGINT =
  go <|> pure ()
 where
  go :: IO ()
  go = do
    bytes :: Text <-
      readFile pidfile
    Just pid :: Maybe CPid <-
      pure (readMaybe (unpack bytes))
    signalProcess sigINT pid
''
