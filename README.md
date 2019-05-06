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
- [ ] Booleans (`true`, `false`)
- [ ] Comparison operators (`=`, `>`, `<`, `>=`, `<=`)
- [ ] def
- [ ] if
- [ ] lambdas

## How to run

### With Docker

Just run:
```
docker build . -t hisp
docker run -it hisp
```

> Observation: the build tools themselves are very heavy, I've got a 1GB Docker image on my computer just by running those commands :fearful:

### Without Docker

This repository has a shell script (already with `chmod +x`, to run like a binary) called `compile_and_run.sh`. Just run it like this:


```shell
./compile_and_run.sh
```

<p align="center">
  <img src="https://user-images.githubusercontent.com/15306309/57204202-9062a800-6f8c-11e9-8f89-ac4e07c51395.png" alt="hisp-terminal-repl-example" />
</p>

## Notes

This project is heavly inspired by:

- [Rust Lisp REPL interpreter](https://m.stopa.io/risp-lisp-in-rust-90a0dad5b116)
- [Python Lisp REPL interpreter](https://norvig.com/lispy.html)

Also, I don't really know `Haskell` well, so the code probably could be simpler, for now, it just works :slightly_smiling_face:
