## EXP - Elixir Proofreader (!!! Proof of concept !!!)

EXP is an ultimate type helper for your Elixir development environment.

### Expected features
- Gradual *dependent* typing
- Algebraic effects inference
- Code holes (thing Smalltalk, Idris) with `~h<hole_name>`
- Inexhaustive case/function head warnings

### Editor JSON interface
- Case splits over inferred domain and zero elements
- Type-at-point query
- Function argument types / returned type query

### Researched features
- Inifnite result function range inference (think `x * x == -1` type errors)
- Function totality checks
- Internal proof consulting language based on [Elchemy](http://github.com/wende/elchemy)
