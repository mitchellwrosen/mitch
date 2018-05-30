\(name : Text) ->

''
'''
-- This file was generated by dhall-to-text.
-- Do not modify directly!

cabal-version: 2.0

author: Mitchell Rosen
bug-reports: https://github.com/mitchellwrosen/${name}/issues
build-type: Simple
-- category:
copyright: (C) 2018 Mitchell Rosen
-- description:
extra-source-files:
  CHANGELOG.md
homepage: https://github.com/mitchellwrosen/${name}
license-file: LICENSE
license: BSD3
maintainer: Mitchell Rosen <mitchellwrosen@gmail.com>
name: ${name}
-- synopsis:
tested-with: GHC == 8.4.2
version: 0.1.0

-- library
--   build-depends:
--       mitchell-stdlib
--   default-extensions: ''${./config/default-extensions .v8-2-2}
--   if impl(ghc >= 8.4.2)
--     default-extensions: ''${./config/default-extensions .v8-4-2}
--   default-language: Haskell2010
--   -- exposed-modules:
--   ghc-options: ''${./config/ghc-options .v8-2-2}
--   if impl(ghc >= 8.4.2)
--     ghc-options: ''${./config/ghc-options .v8-4-2}
--   hs-source-dirs: src

executable ${name}
  build-depends:
      mitchell-stdlib
    , unix ^>= 2.7
  default-extensions: ''${./config/default-extensions .v8-2-2}
  if impl(ghc >= 8.4.2)
    default-extensions: ''${./config/default-extensions .v8-4-2}
  default-language: Haskell2010
  ghc-options: ''${./config/ghc-options .v8-2-2} -threaded -rtsopts
  if impl(ghc >= 8.4.2)
    ghc-options: ''${./config/ghc-options .v8-4-2}
  hs-source-dirs: app
  main-is: Main.hs
'''
''
