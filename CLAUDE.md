# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

A PLAI (Programming Languages: Application and Interpretation) interpreter written in Haskell. The AST is constructed directly via Haskell data constructors — there is no lexer/parser stage.

Two variants exist:
- **HaskellInterpreter** (main) — numbers, booleans, lambdas, closures, conditionals, let-bindings
- **EvenSmallerInterpreter** — minimal subset (numbers, booleans, addition, variables only)

## Build Commands

```sh
cabal build          # build both projects
cabal run plai       # run main interpreter
cabal test           # run HSpec test suite
cabal repl           # interactive REPL with library loaded
ghcid --command="cabal repl"  # auto-recheck on save
```

## Architecture

The interpreter pipeline is: `Expr` (AST) → `interp :: Expr -> Environment -> Value` → `Value`.

**Key types in `src/Interp.hs`:**
- `Expr` — AST nodes: `NumE`, `BoolE`, `VarE`, `PlusE`, `LamE`, `AppE`, `CondE`, `Let1E`
- `Value` — results: `NumV Float`, `BoolV Bool`, `FunctionV Symbol Expr Environment` (closures)
- `Environment` — `Map Symbol Value` for variable bindings

Functions use lexical scoping — `LamE` captures the environment at definition time, creating closures.

## Workspace Layout

`cabal.project` defines a multi-package workspace. Both `HaskellInterpreter.cabal` and `EvenSmallerInterpreter/EvenSmallerInterpreter.cabal` are included.

GHC options: `-Wall` is enabled for all targets.
