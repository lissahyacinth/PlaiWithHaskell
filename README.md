# HaskellInterpreter

A simple interpreter in Haskell, following the [PLAI](https://www.plai.org/) book.

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) (>= 8.10)
- [Cabal](https://www.haskell.org/cabal/) (>= 3.0)

## Quick Start

```sh
cabal build        # build everything
cabal run plai     # run the interpreter
cabal test         # run tests
cabal repl         # interactive REPL with the library loaded
```

## Development

For fast error feedback while editing, use [ghcid](https://github.com/ndmitchell/ghcid):

```sh
cabal install ghcid
ghcid --command="cabal repl"
```

It watches your files and re-checks on every save.

## Project Structure

```
src/Interp.hs    -- interpreter library
app/Main.hs      -- executable entry point
test/Main.hs     -- HUnit tests
```
