# hisp

A `lisp` REPL interpreter made in `Haskell`.

This project is heavly inspired by:

- (Rust Lisp REPL interpreter)[https://m.stopa.io/risp-lisp-in-rust-90a0dad5b116]
- (Python Lisp REPL interpreter)[https://norvig.com/lispy.html]

## How to run

### Prerequisites

You will need to have `ghc` (`Haskell` compiler) installed.

Also you will need to have `cabal`, and install these packages:

- `MissingH`
- `split`

```shell
cabal install MissingH split
```

### Running

This repository has a shell script (already with `chmod +x`, to run like a binary) called `compile_and_run.sh`. Just run it like this:


```shell
./compile_and_run.sh
```
