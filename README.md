# Instant

Compile using `stack build` or `stack install`, alternatively use `make` which runs `stack build`
and copies executables to the toplevel directory.

## Dependencies

Outlined in package.yaml, they are all standard Haskell packages from Hackage.
Obvious dependency on BNFC that was used to compile the `instant.cf` grammar.

## Project stucture

Pretty much standard Stack project.

- `app` contains sources for executables
- `src` contains source code for the compiler
  - `Jvm`/`Llvm` contains specific compiler code for the relevant platform
- `lib` contains Jasmin `jar` and the Instant runtime
- `test` contains example inputs and outputs


