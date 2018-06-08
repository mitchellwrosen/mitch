\(name : Text) ->

''
{-# language NoOverloadedStrings #-}

import Mitchell

import Environment (getEnvironment)
import Exception (ExitCode(..))
import Development.Shake
import Process (executeFile, proc, runProcess, runProcess_, shell)
import System.Posix.Signals (sigUSR1)

import qualified List.Partial as List

main :: IO ()
main =
  shakeArgs shakeOptions { shakeChange = ChangeModtimeAndDigestInput } $ do
    phony "run" $ do
      runAfter $
        runProcess (shell "cabal new-run exe:${name}") >>= \case
          ExitSuccess ->
            pure ()
          ExitFailure code ->
            if code == fromIntegral (-sigUSR1)
              then do
                env :: [([Char], [Char])] <-
                  getEnvironment
                executeFile "./Shakefile" False ["run"] (Just env)
              else
                throwIO (ExitFailure code)

    phony "dev" $ do
      runAfter $
        runProcess_
          (proc
            "ghcid"
            [ "-c", "cabal new-repl exe:${name}"
            , "--restart", "cabal.project"
            , "--restart", "${name}.cabal"
            , "-T", "sendSIGUSR1"
            , "-W"
            ])

    want [".shake/shake", "bin/${name}"]

    ".shake/shake" %> \out -> do
      need ["cabal.project", "shakefile/${name}-shakefile.cabal", "shakefile/Main.hs"]
      cmd_ "cabal new-build shakefile"
      Stdout binary <- cmd "cabal-plan list-bin ${name}-shakefile"
      runAfter $ do
        env :: [([Char], [Char])] <-
          getEnvironment
        executeFile
          "sh"
          True
          [ "-c"
          , "cp " ++ List.init binary ++ " " ++ out ++ " && ./Shakefile"
          ]
          (Just env)

    "bin/${name}" %> \out -> do
      srcfiles <- getDirectoryFiles "" ["src//*.hs"]
      appfiles <- getDirectoryFiles "" ["app/*.hs"]
      need ("cabal.project" : "${name}.cabal" : appfiles ++ srcfiles)
      cmd_ "cabal new-build"
      Stdout (binary :: [Char]) <- cmd "cabal-plan list-bin ${name}"
      cmd_ "cp" (List.init binary) out
''
