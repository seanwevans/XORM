# XORM (⊕M)
<img width="256" alt="\oplus" src="https://github.com/user-attachments/assets/acf232dd-e236-4637-8126-728d10831198" />

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
  (seq (set-r0 42))
  (seq (← 13))
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

## Program representation

`xorm-program` holds the compiled program as a list of primitive instructions
in **emission order**: the first instruction emitted is the first executed.
Every consumer reads it in that same order — `run-xorm` executes it, and
`decompile-xorm` (in `mrox.rkt`) reads it back. Nothing needs to reverse it.

```racket
(reset-program!)
(seq (set-r0 3) (inc-r0))
xorm-program              ; => ((← R0) ⊕ (← 3) ⊕ (← 1) (set-carry 0) ADD)
(run-xorm xorm-program)   ; => (4 1)
```

## Validity

`emit` and `run-xorm` share one definition of what a XORM instruction is, so a
program that can be built can be run and vice versa.  Anything else is
rejected, with the whole program checked before any of it executes.

Constants written as literals are checked during macro expansion, so the error
points at the offending source rather than surfacing when the module runs:

```
> (seq (← 300))
←: constant 300 is out of range for an 8-bit register (0..255)
  at: 300
```

Values only known at run time are still checked by `emit` when the instruction
is appended.

## Macros

The language is built entirely from macros that expand to the primitive
`xor` instruction.  The most important forms are:

- `xor` – perform `R0 ← R0 ⊕ R1`.
- `← c` – set `R1` to the constant or register `c`.
- `set-r0 c` – load the constant `c` into `R0` **and overwrite `R1` with `c`**.  Also accepts a register: `(set-r0 'R1)` moves `R1` into `R0`, and `(set-r0 'R0)` is a no-op.  The register forms leave `R1` alone.
- `seq` – evaluate a sequence of operations.  (This was called `do`, which shadowed Racket's own iteration form for anyone requiring the module.)
- `swap` – exchange the values of `R0` and `R1`.
- `clear-r0` / `clear-r1` – set the respective register to zero (`clear-r0` also leaves `R1 = 0` because it expands through `set-r0`).
- `inc-r0` / `dec-r0` – add or subtract 1 from `R0` with 8‑bit wrap-around.  Both clobber `R1`.
- `copy-to-r1` – copy the current value of `R0` into `R1`.
- `not-r0` – bitwise complement of `R0`.
- `and-r0-r1` / `or-r0-r1` – bitwise logic with the result placed in `R0`.
- `add-r0-r1` – add `R1` to `R0` (8‑bit arithmetic with wrap-around).
- `set-carry` / `clear-carry` – control the carry flag used by `ADD`.
- `store-carry-in-r1` – expose the carry flag to software.
- `shift-left-r0` / `shift-right-r0` – shift `R0` one bit left or right.  The bit shifted out lands in the carry.  `shift-left-r0` clobbers `R1`; `shift-right-r0` does not.
- `<<` / `>>` – compile‑time helpers that shift numeric constants.

### How the arithmetic macros are built

Three of these are derived rather than primitive, which is the point of the
language — the machine stays small and the macros do the work:

| Macro | Expands to | Why it works |
| --- | --- | --- |
| `inc-r0` | `(← 1)` `ADD` | adding the constant 1 |
| `dec-r0` | `(← 255)` `ADD` | subtracting 1 mod 256 *is* adding 255 |
| `shift-left-r0` | `(← R0)` `ADD` | a left shift is a doubling, and doubling is `x + x` |

`shift-right-r0` is the exception: it emits a genuine `SHR` primitive.  Halving
is not expressible from XOR, ADD and constant loads, because the machine has no
conditional to test a bit with and no operation that moves information toward
the low end of the register.

Carry follows the usual convention throughout: `ADD` sets it on overflow,
`SHR` sets it to the bit shifted out, and `dec-r0` sets it whenever the
subtraction does *not* borrow (that is, for every starting value except 0).
Read it with `store-carry-in-r1`.

## Example usage

### `xorm.rkt`

This file defines the XORM DSL and includes a small sample program at the
bottom.  Running the file will execute that sample and print the resulting
program and register values.

```
$ racket xorm.rkt
```

You can modify the sequence of `(seq ...)` forms at the end of the file to
experiment with the macros.  Each macro emits primitive instructions that
are stored in `xorm-program` and executed by `run-xorm`.

### `mrox.rkt`

`mrox.rkt` is a very small "decompiler" that turns a sequence of primitive
instructions back into the higher level macros, operands included — its output
recompiles to the instructions it came from, which the test suite checks
directly.  Running the file prints the example program and the decompiled
form:

```
$ racket mrox.rkt
```

## Running the tests

An automated test suite lives in the `tests` directory.  After installing
Racket you can execute all tests with:

```
$ raco test tests
```

Alongside the hand-written cases, `tests/exhaustive-tests.rkt` checks every
macro against an independent reference model at **every input it can be
given**.  The machine's whole architectural state is two 8‑bit registers and a
carry flag, so the space is small enough to enumerate outright rather than
sample — each macro is run from all 65536 `(R0, R1)` starting states and its
result and carry compared against a Racket-level model.  The whole sweep takes
about a second.

This matters because the failure it catches is the one that actually shipped:
`inc-r0` and `dec-r0` were implemented as `xor 1`, which is correct for exactly
the inputs the hand-written tests happened to use and wrong everywhere else.
No single example distinguishes those; 65536 of them do.

Running `xorm.rkt` and `mrox.rkt` directly is still useful for quick
experimentation:

```
$ racket xorm.rkt
$ racket mrox.rkt
```

The first command prints the generated instruction list and final register
state; the second shows a decompilation of that program.
