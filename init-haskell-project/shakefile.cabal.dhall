\(name : Text) ->

''
'''
build-type: Simple
cabal-version: >= 1.10
name: ${name}-shakefile
version: 0

executable ${name}-shakefile
  build-depends: mitchell-stdlib, shake, unix
  default-extensions: ''${../config/default-extensions .v8-2-2}
  if impl(ghc >= 8.4.2)
    default-extensions: ''${../config/default-extensions .v8-4-2}
  default-language: Haskell2010
  ghc-options: ''${../config/ghc-options .v8-2-2} -threaded -rtsopts
  if impl(ghc >= 8.4.2)
    ghc-options: ''${../config/ghc-options .v8-4-2}
  main-is: Main.hs
'''
''
