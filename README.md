
<p align="center">
  <img src="https://user-images.githubusercontent.com/15306309/57203245-c734c000-6f84-11e9-878d-985be42faf8f.png" alt="hisp-logo" />
  <br />
  <br />
  <b>A <code>lisp</code> REPL interpreter made in <code>Haskell</code>.</b>
</p>

<br />

## Language Features

- [x] Numbers (`Float`)
- [x] Addition (`+`)
- [x] Subtraction (`-`)
- [x] Booleans (`true`, `false`)
- [x] Comparison operators (`=`, `>`, `<`, `>=`, `<=`)
- [x] def
- [x] if
- [x] lambdas

## How to run

### With Docker

Just run:
```
docker build . -t hisp
docker run -it hisp
```

> Observation: the build tools of Haskell themselves seem to be very heavy, I've got a 1GB Docker image on my computer just with a 3 line Dockerfile :fearful:

### Without Docker

`hisp` can be compiled/executed using either using `stack` or just by using `ghc`.

#### Using Stack

You will need `stack` installed on your computer, which is a tool for developing `Haskell` projects.

To compile and run:

```shell
stack run
```

#### Using just GHC

You will need `ghc` installed on your computer, which is the `Haskell` compiler.

This repository has a shell script (already with `chmod +x`, to run like a binary) called `compile_and_run.sh`. Just run it like this:


```shell
./compile_and_run.sh
```

<p align="center">
  <img src="https://user-images.githubusercontent.com/15306309/57204202-9062a800-6f8c-11e9-8f89-ac4e07c51395.png" alt="hisp-terminal-repl-example" />
</p>

## Project structure

Since `hisp` is built with `stack`, the folders follow the standard of it.

```
hisp
│   README.md
│   ...
└─── app
│   └─── Main.hs
└─── src
│   └─── ...
└─── test
    └─── ...
```

- The `app` folder contains the `main` function that starts the REPL.
- The `src` folder contains the code that is consumed by the `main` function, so it is much like a library folder.
- The `test` folder, well, as the name suggests, it has the code for the tests.

## Tests

Tests are run using `stack`, you should install it first. To run them, just use:

```shell
stack test
```
## How it works

Well it has 3 main steps, like many interpreters:

### 1. Tokenize

Considering someone wants to see the result of the following `lisp` code:

<p align="center">
  <img src="https://user-images.githubusercontent.com/15306309/57578966-a6c8a200-746b-11e9-8df3-30c343433b19.png" alt="hisp-input-diagram" />
</p>

First we take it as raw input (`String`) and divide it into tokens, which is a list of values (`[String]`):

<p align="center">
  <img src="https://user-images.githubusercontent.com/15306309/57578967-a6c8a200-746b-11e9-9a22-9340a3b1e2d2.png" alt="hisp-tokenize-diagram" />
</p>

### 2. Parse

Now that we have those `tokens`, we can convert them to something meaningful to us, that we can interpret.
```rust
enum Expression {
  Symbol(String),
  Number(Float),
  List([Expression])
  Function(([Expression]) -> Expression),
}
```

The code above is a pseudocode with a `Rust`-like syntax. The idea is that you can have a value that is **either** a `Symbol` (holding a String), a `Number` (holding a Float), a `List` (holding a list of `Expression`) **or** a `Function` (holding a function that maps a list of `Expression` to a single `Expression`).

```rust
let four = Expression::Number(4.0);
let plus_sign = Expression::Symbol("+");
```
<p align="center">
  <img src="https://user-images.githubusercontent.com/15306309/57578968-a6c8a200-746b-11e9-96ab-187a60fc5902.png" alt="hisp-parse-diagram" />
</p>

### 3. Evaluate

After we have the values that represent the code we need to execute, we will interpret it!

<p align="center">
  <img src="https://user-images.githubusercontent.com/15306309/57578965-a6300b80-746b-11e9-8b14-33b899d5c9a1.png" alt="hisp-evaluate-diagram" />
</p>

To actually get to that `5.0` in the end, we must have somewhere defined that `+` receives a bunch of `Number`s and then sum them.
We could have an `Environment` that contains the standard functions/operators.
```rust
struct Environment {
  data: HashMap<String, Expression>// map of keys and values
}
```
Imagine this like a `class` that has a map of things like `+`, `-`, `=`, ... to values like `Expression::Function(implementation)`.

```
function sum_implementation (values) {
  return values.reduce((acc, curr) => acc + curr, 0)
}

environment = {
  "+": sum_implementation,
  "-": subtract_implementation,
  ...
}
```
Above there is a JSON/JavaScript-like visualization.

That is it! It was just an overview, but you can see all of this on this repository, just a bit more complex to make things like `def`s, `if`s and `lambda`s available to the language.

If you really want to understand more, you can follow some tutorials that are below :slightly_smiling_face:

## Notes

This project is heavly inspired by:

- [Rust Lisp REPL interpreter](https://m.stopa.io/risp-lisp-in-rust-90a0dad5b116)
- [Python Lisp REPL interpreter](https://norvig.com/lispy.html)

Also, I don't really know `Haskell` well, so the code probably could be a lot better. I did my best with what I know about the language at the time.
