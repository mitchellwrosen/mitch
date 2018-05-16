{-# language LambdaCase     #-}
{-# language NamedFieldPuns #-}

import Cidr (Cidr(..))

import qualified Cidr

import Control.Arrow ((>>>))
import Control.Lens ((^..), to)
import Control.Monad
import Data.Bits.Lens
import Data.Bool (bool)
import Data.Word
import Options.Applicative
import System.Process

main :: IO ()
main =
  join
    (customExecParser
      (prefs (showHelpOnError <> showHelpOnEmpty))
      (info parser mempty))

parser :: Parser (IO ())
parser =
  commands
    [ ( "display-cidr"
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

   ,  ( "docker"
      , commands
          [ ( "gc"
            , pure (callCommand "docker system prune -f")
            , progDesc "Garbage collect stopped containers and dangling images"
            )
          ]
      , progDesc "Docker porcelain"
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

    ]

commands :: [(String, Parser a, InfoMod a)] -> Parser a
commands =
  map (\(n, p, m) -> command n (info p m))
    >>> mconcat
    >>> hsubparser
