{-# language LambdaCase     #-}
{-# language NamedFieldPuns #-}
{-# language ViewPatterns   #-}

import Cidr (Cidr(..))

import qualified Cidr

import Control.Arrow ((>>>))
import Control.Lens ((^..), to)
import Control.Monad
import Data.Bits.Lens
import Data.Bool (bool)
import Data.Word
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process
import Text.Read (readMaybe)

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
            , pure (callCommand "pacman -Syu")
            , progDesc "Update all installed packages"
            )
          , ( "which-package-owns"
            , (\package -> callCommand ("pacman -Qo " ++ package))
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
                    let showBits :: Word8 -> String
                        showBits w =
                          w ^.. bits . to (bool '0' '1')
                    putStrLn $
                           show a ++ replicate (8 - length (show a)) ' '
                        ++ show b ++ replicate (8 - length (show b)) ' '
                        ++ show c ++ replicate (8 - length (show c)) ' '
                        ++ show d ++ replicate (8 - length (show d)) ' '
                    putStrLn (showBits a ++ showBits b ++ showBits c ++ showBits d)
                    case netmask of
                      Just netmask'
                        | netmask' > 0 -> putStrLn (replicate netmask' '^')
                      _ -> pure ()
              handle <$> Cidr.parse s))
          (metavar "IP")
      , progDesc "Display the bits of an IPv4 address given in CIDR notation"
      )

    , ( "docker"
      , commands
          [ ( "gc"
            , pure (callCommand "docker system prune -f")
            , progDesc "Prune stopped containers, dangling images, unused volumes, and unused networks"
            )
          ]
      , progDesc "Docker porcelain"
      )

    , ( "find-file"
      , (\patt -> callCommand ("find . -type f -iname '" ++ patt ++ "'"))
          <$> strArgument (metavar "PATTERN")
      , progDesc "Find a file by regular expression"
      )

    , ( "gpg"
      , commands
         [ ( "restart-agent"
           , pure (callCommand "gpgconf --kill all")
           , progDesc "Restart the GPG agent"
           )
         ]
      , progDesc "GPG porcelain"
      )

     , ( "nix"
       , commands
           [ ( "install"
             , (\name -> callCommand ("nix-env -i " ++ name))
                 <$> strArgument (metavar "PACKAGE")
             , progDesc "Install a package"
             )
           ]
       , progDesc "Nix porcelain"
       )

    , ( "pid"
      , (\s -> callCommand ("pidof " ++ s))
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
              putStrLn cmd
              callCommand cmd
            (Remote host raddr rport, Local laddr lport) -> do
              let cmd :: String
                  cmd =
                    "ssh -N -R " ++ raddr ++ ":" ++ show rport ++ ":" ++ laddr ++ ":"
                      ++ show lport ++ " " ++ host
              putStrLn cmd
              callCommand cmd
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
