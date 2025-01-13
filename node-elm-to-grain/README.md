Transpile all elm modules in the current project
(source-directories + dependencies)
into a bundled `src/elm.gr` file that exposes every value/function declaration
(e.g. `main_runOnString(a, b)` for `Main.runOnString a b`)


```bash
npm install
```

You can run it once:

```bash
npm run start
```

or build it into a standalone executable `dist/elm-to-grain`:
```bash
npm run build
```

To do something with the grain file, like compiling to wasm or executing it,
please [build the grain compiler from source](https://grain-lang.org/docs/getting_grain#Building-Grain-from-Source).
The default, packaged compiler
- uses an old version of the standard library elm-syntax-to-grain does not target
- is multiple orders of magnitude slower (yes, really)

Here's the typical command you'd use to turn grain code into wasm
```bash
grain compile --release --elide-type-info src/elm.gr
```

For specifics, read [the grain CLI guide](https://grain-lang.org/docs/tooling/grain_cli).
