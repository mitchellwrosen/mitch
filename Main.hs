{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

import Mitchell.Prelude

import Cidr (Cidr(..), parseCidr)

import Bool                 (bool)
import Char
import Data.Bits.Lens
import Exception            (exitFailure)
import List                 (break)
import Monad                (join)
import Optic.Fold           ((^..))
import Optic.Getter         (to)
import Parser.Cli
import Printf               (printf)
import Process              (runProcess_, shell)
import Read                 (readMaybe)
import System.Posix.Signals
import Text                 (pack)

import qualified ByteString
import qualified File.Binary

data SshTunnelNode
  = Local [Char] Int
  | Remote [Char] [Char] Int
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
            , pure $ do
                runProcess_ (shell "pacman -S archlinux-keyring")
                runProcess_ (shell "pacman-key --refresh-keys")
                runProcess_ (shell "pacman -Syu")
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

    , ( "bin2hex"
      , pure (File.Binary.getContents >>= File.Binary.putStr . ByteString.asHexadecimal)
      , progDesc ""
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
            , pure $ do
                runProcess_ (shell "docker system prune -f")
                runProcess_ (shell "docker volume prune -f")
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

    , ( "pid"
      , (\s -> runProcess_ (shell ("pidof " ++ s)))
          <$> strArgument (metavar "NAME")
      , progDesc "Find the pid of a running process"
      )

    , ( "signal"
      , (\arg1 arg2 ->
          let
            readPid :: [Char] -> Maybe Int
            readPid =
              readMaybe

            readSig :: [Char] -> Maybe Int
            readSig =
              map toLower >>> \case
                "abrt" -> pure (fromIntegral sigABRT)
                "alrm" -> pure (fromIntegral sigALRM)
                "bus" -> pure (fromIntegral sigBUS)
                "chld" -> pure (fromIntegral sigCHLD)
                "cont" -> pure (fromIntegral sigCONT)
                "fpe" -> pure (fromIntegral sigFPE)
                "hup" -> pure (fromIntegral sigHUP)
                "ill" -> pure (fromIntegral sigILL)
                "int" -> pure (fromIntegral sigINT)
                "kill" -> pure (fromIntegral sigKILL)
                "pipe" -> pure (fromIntegral sigPIPE)
                "poll" -> pure (fromIntegral sigPOLL)
                "prof" -> pure (fromIntegral sigPROF)
                "quit" -> pure (fromIntegral sigQUIT)
                "segv" -> pure (fromIntegral sigSEGV)
                "stop" -> pure (fromIntegral sigSTOP)
                "sys" -> pure (fromIntegral sigSYS)
                "term" -> pure (fromIntegral sigTERM)
                "trap" -> pure (fromIntegral sigTRAP)
                "tstp" -> pure (fromIntegral sigTSTP)
                "ttin" -> pure (fromIntegral sigTTIN)
                "ttou" -> pure (fromIntegral sigTTOU)
                "urg" -> pure (fromIntegral sigURG)
                "usr1" -> pure (fromIntegral sigUSR1)
                "usr2" -> pure (fromIntegral sigUSR2)
                "vtalrm" -> pure (fromIntegral sigVTALRM)
                "xcpu" -> pure (fromIntegral sigXCPU)
                "xfsz" -> pure (fromIntegral sigXFSZ)
                _ -> Nothing
          in
            case (readPid arg1, readSig arg2) of
              (Just pid, Just sig) ->
                runProcess_ (shell (printf "kill -%d %d" sig pid))
              _ ->
                case (readSig arg1, readPid arg2) of
                  (Just sig, Just pid) ->
                    runProcess_ (shell (printf "kill -%d %d" sig pid))
                  _ ->
                    putStrLn "Parse error"
          )
          <$> strArgument (metavar "SIG|PID")
          <*> strArgument (metavar "SIG|PID")
      , progDesc "Signal a process"
      )

    , ( "ssh-tunnel"
      , let
          nodeParser :: Parser SshTunnelNode
          nodeParser =
            argument
              (maybeReader readLocalNode <|> maybeReader readRemoteNode)
              mempty

          readLocalNode :: [Char] -> Maybe SshTunnelNode
          readLocalNode s = do
            [addr, readMaybe -> Just port] <-
              pure (splitOnComma s)
            pure (Local addr port)

          readRemoteNode :: [Char] -> Maybe SshTunnelNode
          readRemoteNode s = do
            [host, addr, readMaybe -> Just port] <-
              pure (splitOnComma s)
            pure (Remote host addr port)

          splitOnComma :: [Char] -> [[Char]]
          splitOnComma s =
            case break (== ':') s of
              (t, [])  -> [t]
              (t, _:u) -> t : splitOnComma u
        in
          (\case
            (Local laddr lport, Remote host raddr rport) -> do
              let cmd :: [Char]
                  cmd =
                    "ssh -N -L " ++ laddr ++ ":" ++ show lport ++ ":" ++ raddr ++ ":"
                      ++ show rport ++ " " ++ host
              putStrLn (pack cmd)
              runProcess_ (shell cmd)
            (Remote host raddr rport, Local laddr lport) -> do
              let cmd :: [Char]
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

commands :: [([Char], Parser a, InfoMod a)] -> Parser a
commands =
  map (\(n, p, m) -> command n (info p m))
    >>> mconcat
    >>> hsubparser

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "fromJust: Nothing"
