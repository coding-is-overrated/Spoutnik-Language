# Project Spoutnik
Programming language allowing basic types to be parametrized with arbitrary units.
The language simplifies the units used in calculations and ensures a stricter verification upon type checking.
For instance, if two integers don't have the same units, they can't be added together, and the language warns of a unit error.

### Usage
The interpreter is called spoutnik.
The `-d` or `--debug` flags can be used to print the AST before executing a statement.

### To run in interactive mode

```
dune exec spoutnik
```

or

```
dune build
./_build/default/bin/main.exe
```

### To execute a file

```
dune exec spoutnik -- test.sptk
```

or

```
dune build
./_build/default/bin/main.exe test.sptk
```

### To get the doc

```
dune build @doc
```

Then open in a web browser `./_build/default/_doc/_html/index.html`

### Recommendations

Use the `rlwrap` command to allow for line history and editing in interactive mode.
