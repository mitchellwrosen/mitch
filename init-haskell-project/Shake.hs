\(name : Text) ->

''
import Development.Shake
import System.Process (callCommand)

main :: IO ()
main =
  shakeArgs shakeOptions $ do
    phony "run" $ do
      need ["${name}.cabal"]
      runAfter (callCommand "cabal new-run exe:${name}")

    phony "dev" $ do
      need ["${name}.cabal"]
      runAfter $
        callCommand
          "ghcid -c 'cabal new-repl exe:${name}' --restart cabal.project --restart ${name}.cabal -T sendSIGINT -W"

    want ["bin/${name}"]

    "bin/${name}" %> \out -> do
      srcfiles <- getDirectoryFiles "" ["src//*.hs"]
      appfiles <- getDirectoryFiles "" ["app/*.hs"]
      need ("cabal.project" : "${name}.cabal" : appfiles ++ srcfiles)
      cmd_ "cabal new-build"
      Stdout binary <- cmd "cabal-plan list-bin ${name}"
      cmd_ "cp" (binary :: String) out

    "${name}.cabal" %> \out -> do
      need ["${name}.cabal.dhall"]
      cmd_
        (FileStdin "${name}.cabal.dhall")
        (FileStdout out)
        "dhall-to-text"
''
