{-# language LambdaCase #-}

import Control.Arrow ((>>>))
import Control.Monad
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
    [ ( "docker"
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
