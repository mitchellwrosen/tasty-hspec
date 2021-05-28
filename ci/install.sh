#!/bin/sh

set -e

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ghcup install ghc 9.0.1
ghcup install ghc 8.10.4
ghcup install ghc 8.8.4
ghcup install ghc 8.6.5
