
# Features

## Program Cycle
- Lexing ✅
- Parsing
- Semantic Analysis
- Type Checking
- Optimisation Loop
- IR Generation / Interpreter Setup
- Optim Loop 2  / JIT
- Execution

# Todo
## CLI

- Basic parser ✅
- Interpret a single file ✅
- REPL ✅

## Lexing

All done for now ✅

## Parsing

- Have parsing fail if the input is incorrect

### Expressions
- Unit {=} ✅
- Unary "." ✅
- Allow any valid operator
- Still allow custom precedence, based on the operator's name
- Cones ✅ & Cocones ✅

### Type Expressions
- Unit {:} ✅
- Type arrow ✅
- Functor application
- Cones ✅ & Cocones ✅

### Declarations
- Object declarations ✅
- Arrow declarations ✅
- Currying

## Semantic Analysis

- Identifiers out of scope ✅
- Invalid arrow constructs ✅

## Type Checking

- Define Category
- Define Base ✅
  - Actually defined a set of built-ins ✅

## Optimisation Loop

Nothing yet

## IR Generation (JS)

Nothing yet

## Interpreter Setup

- "Scope" ✅, TypeContexts ✅
- Thunk creation

## Optim Loop (JS)

Nothing yet

## JIT ?

Nothing yet

## Execution (interp)

- Composition ✅
- Arithmetic expression evaluation ✅
- Boolean logic ✅
- Base Equality ✅ / Comparaison
- Cones ✅
- Cocones ✅
- . operators and @ operator ✅
- Functor ✅
- `$` and `'` operators ✅
- `:` operator ✅
- Currying
- Pretty printing results (lists and stuff) ✅

## REPL
- Arrow keys ✅
- :load
- :r

# General

- Debate whether Rina need currying

- Refactor [(t, v)] with maps ✅
- Refactor `.` and `@` to use `single`✅
