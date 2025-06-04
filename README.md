# XORM (⊕M)

XORM is a tiny DSL with exactly two 8‑bit registers `R1` and `R0` and a
single runtime instruction `⊕` (`xor`).  Everything else is built using
macros that expand to that primitive instruction.  Running a program
produces a list of the final values of `R0` and `R1`.

`XORM` (`⊕M`) is just xor, macros and 2 abstract 8-bit registers: `R1` and `R0`.
Macros are the only abstraction allowed.
XOR(⊕) is the only runtime instruction available, i.e. R0 ← R0 ⊕ R1

To use the DSL in another Racket file:

```racket
(require "xorm.rkt")

(module+ main
  (do (set-r0 42))
  (do (← 13))
  (displayln (run-xorm xorm-program)))
```



## Setup

1. Install [Racket](https://racket-lang.org/) (version 8 or newer).
2. Clone this repository and enter the directory.

```
$ git clone <repo-url>
$ cd XORM
```

No additional packages are required – all files run with the default
Racket distribution.

## Example usage

### `xorm.rkt`

This file defines the XORM DSL and includes a small sample program at the
bottom.  Running the file will execute that sample and print the resulting
program and register values.

```
$ racket xorm.rkt
```

You can modify the sequence of `(do ...)` forms at the end of the file to
experiment with the macros.  Each macro emits primitive instructions that
are stored in `xorm-program` and executed by `run-xorm`.

### `mrox.rkt`

`mrox.rkt` is a very small "decompiler" that attempts to turn a sequence
of primitive instructions back into the higher level macros.  Running the
file will print the example program and the decompiled form:

```
$ racket mrox.rkt
```

## Running the tests

Both `xorm.rkt` and `mrox.rkt` contain example programs at the bottom of
their files.  Running them with `racket` acts as a simple test that the
language and decompiler behave as expected.  Execute:

```
$ racket xorm.rkt
$ racket mrox.rkt
```

The output should display the generated instruction list and the final
register state for `xorm.rkt`, and a pretty‑printed decompilation for
`mrox.rkt`.
