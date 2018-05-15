#!/bin/sh

set -ex

cabal new-build
cp $(find dist-newstyle -name mitch -type f -executable) ~/.local/bin/
