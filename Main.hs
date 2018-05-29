{-# language TemplateHaskell #-}

import Mitchell

import Cidr (Cidr(..), parseCidr)

import Bool (bool)
import Control.Lens ((^..), to)
import Data.Bits.Lens
import Data.FileEmbed (embedDir)
import Exception (exitFailure)
import File
import File.Text (writeFile)
import List (break, lookup)
import Optparse
import Process (runProcess_, setWorkingDir, shell)
import Read (readMaybe)
import String (String)
import Text (pack)

import qualified Dhall
import qualified Text
import qualified Text.Lazy
import qualified Text.Lazy as Lazy (Text)

data SshTunnelNode
  = Local String Int
  | Remote String String Int
  deriving Show

main :: IO ()
main =
  join
    (customExecParser
      (prefs (showHelpOnError <> showHelpOnEmpty))
      (info parser mempty))

parser :: Parser (IO ())
parser =
  commands
    [ ( "arch-linux"
      , commands
          [ ( "update-packages"
            , pure (runProcess_ (shell "pacman -Syu"))
            , progDesc "Update all installed packages"
            )
          , ( "which-package-owns"
            , (\package -> runProcess_ (shell ("pacman -Qo " ++ package)))
                <$> strArgument (metavar "PACKAGE")
            , progDesc "Which package owns this file?"
            )
          ]
      , progDesc "Arch linux porcelain"
      )

    , ( "display-cidr"
      , argument
          (maybeReader
            (\s -> do
              let handle :: Cidr -> IO ()
                  handle Cidr{netmask, address = (a, b, c, d)} = do
                    let showBits :: Word8 -> Text
                        showBits w =
                          pack (w ^.. bits . to (bool '0' '1'))
                    putStrLn . pack $
                           show a ++ replicate (8 - length (show a)) ' '
                        ++ show b ++ replicate (8 - length (show b)) ' '
                        ++ show c ++ replicate (8 - length (show c)) ' '
                        ++ show d ++ replicate (8 - length (show d)) ' '
                    putStrLn
                      (showBits a <> showBits b <> showBits c <> showBits d)
                    case netmask of
                      Just netmask' | netmask' > 0 ->
                        putStrLn (pack (replicate netmask' '^'))
                      _ ->
                        pure ()
              handle <$> parseCidr s))
          (metavar "IP")
      , progDesc "Display the bits of an IPv4 address given in CIDR notation"
      )

    , ( "docker"
      , commands
          [ ( "gc"
            , pure (runProcess_ (shell "docker system prune -f"))
            , progDesc "Prune stopped containers, dangling images, unused volumes, and unused networks"
            )
          ]
      , progDesc "Docker porcelain"
      )

    , ( "find-file"
      , (\patt ->
          runProcess_ (shell ("find . -type f -iname '" ++ patt ++ "'")))
          <$> strArgument (metavar "PATTERN")
      , progDesc "Find a file by regular expression"
      )

    , ( "gpg"
      , commands
         [ ( "restart-agent"
           , pure (runProcess_ (shell "gpgconf --kill all"))
           , progDesc "Restart the GPG agent"
           )
         ]
      , progDesc "GPG porcelain"
      )

     , ( "nix"
       , commands
           [ ( "install"
             , (\name -> runProcess_ (shell ("nix-env -i " ++ name)))
                 <$> strArgument (metavar "PACKAGE")
             , progDesc "Install a package"
             )
           ]
       , progDesc "Nix porcelain"
       )

    , ( "init-haskell-project"
      , initHaskellProject
          <$> strArgument (metavar "NAME")
      , progDesc "Initialize a Haskell project"
      )

    , ( "pid"
      , (\s -> runProcess_ (shell ("pidof " ++ s)))
          <$> strArgument (metavar "NAME")
      , progDesc "Find the pid of a running process"
      )

    , ( "ssh-tunnel"
      , let
          nodeParser :: Parser SshTunnelNode
          nodeParser =
            argument
              (maybeReader readLocalNode <|> maybeReader readRemoteNode)
              mempty

          readLocalNode :: String -> Maybe SshTunnelNode
          readLocalNode s = do
            [addr, readMaybe -> Just port] <-
              pure (splitOnComma s)
            pure (Local addr port)

          readRemoteNode :: String -> Maybe SshTunnelNode
          readRemoteNode s = do
            [host, addr, readMaybe -> Just port] <-
              pure (splitOnComma s)
            pure (Remote host addr port)

          splitOnComma :: String -> [String]
          splitOnComma s =
            case break (== ':') s of
              (t, []) -> [t]
              (t, _:u) -> t : splitOnComma u
        in
          (\case
            (Local laddr lport, Remote host raddr rport) -> do
              let cmd :: String
                  cmd =
                    "ssh -N -L " ++ laddr ++ ":" ++ show lport ++ ":" ++ raddr ++ ":"
                      ++ show rport ++ " " ++ host
              putStrLn (pack cmd)
              runProcess_ (shell cmd)
            (Remote host raddr rport, Local laddr lport) -> do
              let cmd :: String
                  cmd =
                    "ssh -N -R " ++ raddr ++ ":" ++ show rport ++ ":" ++ laddr ++ ":"
                      ++ show lport ++ " " ++ host
              putStrLn (pack cmd)
              runProcess_ (shell cmd)
            _ -> do
              hPutStrLn stderr "Must provide one local node, one remote node"
              exitFailure)
            <$> (liftA2 (,) nodeParser nodeParser)

      , progDesc "Forward or reverse SSH tunnel"
      )
    ]

commands :: [(String, Parser a, InfoMod a)] -> Parser a
commands =
  map (\(n, p, m) -> command n (info p m))
    >>> mconcat
    >>> hsubparser

initHaskellProject :: String -> IO ()
initHaskellProject name = do
  createDirectory name
  createDirectory (name ++ "/app")
  createDirectory (name ++ "/shakefile")
  createDirectory (name ++ "/src")

  let run :: String -> IO ()
      run s =
        runProcess_ (setWorkingDir name (shell s))

  -- Initialize git repo and add submodules
  run "git init"
  run "git submodule add https://github.com/dhall-lang/Prelude deps/dhall"
  run "git submodule add https://github.com/mitchellwrosen/mitchell-stdlib deps/mitchell-stdlib"

  let dhall :: FilePath -> FilePath -> IO ()
      dhall src dst = do
        let bytes :: Text
            bytes =
              Text.decodeUtf8 (fromJust (lookup src initHaskellProjectDir))
        render :: Lazy.Text -> Text <-
          Dhall.input Dhall.auto (Text.Lazy.fromStrict bytes)
        writeFile (name ++ "/" ++ dst) (render (Text.Lazy.pack name))

  let copy :: FilePath -> FilePath -> IO ()
      copy src dst =
        writeFile
          (name ++ "/" ++ dst)
          (Text.decodeUtf8 (fromJust (lookup src initHaskellProjectDir)))

  dhall "cabal.dhall" (name ++ ".cabal.dhall")
  dhall "Main.hs" "app/Main.hs"
  dhall "Shake.hs" "shakefile/Main.hs"
  dhall "Shakefile" "Shakefile"
  dhall "shakefile.cabal" ("shakefile/" ++ name ++ "-shakefile.cabal")

  copy "CHANGELOG.md" "CHANGELOG.md"
  copy "cabal.project" "cabal.project"
  copy "ghci" ".ghci"
  copy "gitignore" ".gitignore"
  copy "LICENSE" "LICENSE"
  copy "travis.yml" ".travis.yml"

  run "chmod +x ./Shakefile"
  run "./Shakefile"
  run "stack init --resolver lts"
  run "sed -i -e 's/^#.*//' -e '/^$/d' stack.yaml"
  run "git add --all"

initHaskellProjectDir :: [(FilePath, ByteString)]
initHaskellProjectDir =
  $(embedDir "init-haskell-project")

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"
