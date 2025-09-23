# XORM (⊕M)

XORM is a tiny DSL with two 8‑bit registers (`R1` and `R0`).  Programs are
written in terms of macros that expand to a small set of primitive
instructions executed by `run-xorm`.  Originally only XOR was available;
the machine now also exposes helper primitives for computing carries,
bitwise logic and addition so that `add-r0-r1` can perform genuine
8‑bit arithmetic.  Running a program produces a list of the final values
of `R0` and `R1`.

`XORM` (`⊕M`) is just xor, macros and two abstract 8-bit registers: `R1`
and `R0`.  Macros are the only abstraction allowed.  The runtime supports
`⊕` along with helper instructions for addition (`ADD` plus carry control)
and basic bitwise logic.

To use the DSL in another Racket file:

```racket
(require "xorm.rkt")

(module+ main
  (do (set-r0 42))
  (do (← 13))
  (displayln (run-xorm xorm-program)))
```



## Setup

1. Install [Racket](https://racket-lang.org/) (version 8 or newer).  The
   `raco` command from this installation is used to run the test suite.
2. Clone this repository and enter the directory.

```
$ git clone <repo-url>
$ cd XORM
```

No additional packages are required – all files run with the default
Racket distribution.

## Macros

The language is built entirely from macros that expand to the primitive
`xor` instruction.  The most important forms are:

- `xor` – perform `R0 ← R0 ⊕ R1`.
- `← c` – set `R1` to the constant or register `c`.
- `set-r0 c` – load the constant `c` into `R0`.
- `do` – evaluate a sequence of operations.
- `swap` – exchange the values of `R0` and `R1`.
- `clear-r0` / `clear-r1` – set the respective register to zero.
- `inc-r0` / `dec-r0` – increment or decrement `R0`.
- `copy-to-r1` – copy the current value of `R0` into `R1`.
- `not-r0` – bitwise complement of `R0`.
- `and-r0-r1` / `or-r0-r1` – bitwise logic with the result placed in `R0`.
- `add-r0-r1` – add `R1` to `R0` (8‑bit arithmetic with wrap-around).
- `set-carry` / `clear-carry` – control the carry flag used by `ADD`.
- `store-carry-in-r1` – expose the carry flag to software.
- `shift-left-r0` / `shift-right-r0` – shift `R0` one bit left or right.
- `<<` / `>>` – compile‑time helpers that shift numeric constants.

Some operations remain *placeholders*.  Increment/decrement and the shift
macros are still implemented purely in terms of XOR and constant loads and
do **not** behave like real arithmetic shifts.  See the implementation in
[`xorm.rkt`](xorm.rkt) for details.

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

An automated test suite lives in the `tests` directory.  After installing
Racket you can execute all tests with:

```
$ raco test tests
```

Running `xorm.rkt` and `mrox.rkt` directly is still useful for quick
experimentation:

```
$ racket xorm.rkt
$ racket mrox.rkt
```

The first command prints the generated instruction list and final register
state; the second shows a decompilation of that program.
