# BCPL Instructions Implementation for AArch64

This document describes the implementation of BCPL instructions for the AArch64 architecture. Each instruction is implemented to maintain BCPL's semantics while utilizing AArch64's capabilities efficiently.

## Register Conventions

- `X0`: A register (accumulator, first argument, return value)
- `X1`: B register (second argument)
- `X2`: C register (third argument)
- `X25`: MW register
- `X28`: Global pointer (G)
- `X29`: Frame pointer (FP)
- `X30`: Link register (LR)
- `SP/XZR`: Stack pointer/Zero register (31)

## Basic Load Instructions

### Immediate Loading
- `Ln` (0 ≤ n ≤ 10): `B := A; A := n`
- `LM1`: `B := A; A := -1`
- `L b` (0 ≤ b ≤ 255): `B := A; A := b`
- `LM b` (0 ≤ b ≤ 255): `B := A; A := -b`
- `LH h` (0 ≤ h ≤ 65535): `B := A; A := h`
- `LMH h` (0 ≤ h ≤ 65535): `B := A; A := -h`
- `LW w`: `B := A; A := w` (32-bit value)
- `MW w`: `MW := w` (loads to X25)

### Stack and Global Access
- `LPn` (3 ≤ n ≤ 16): `B := A; A := P!n`
- `LP b`: `B := A; A := P!b`
- `LPH h`: `B := A; A := P!h`
- `LPW w`: `B := A; A := P!w`
- `LG b`: `B := A; A := G!b`
- `LG1 b`: `B := A; A := G!(b + 256)`
- `LGH h`: `B := A; A := G!h`

## Indirect Load Instructions

- `GBYT`: `A := B%A` (byte load)
- `RV`: `A := A!0`
- `RVn` (1 ≤ n ≤ 6): `A := A!n`
- `RVPn` (3 ≤ n ≤ 7): `A := P!n!A`
- `L0Pn` (3 ≤ n ≤ 12): `B := A; A := P!n!0`
- `L1Pn` (3 ≤ n ≤ 6): `B := A; A := P!n!1`

## Expression Operators

### Arithmetic
- `NEG`: `A := -A`
- `NOT`: `A := ~A`
- `MUL`: `A := B * A`
- `DIV`: `A := B / A`
- `MOD`: `A := B MOD A`
- `ADD`: `A := B + A`
- `SUB`: `A := B - A`

### Bitwise
- `LSH`: `A := B << A`
- `RSH`: `A := B >> A`
- `AND`: `A := B & A`
- `OR`: `A := B | A`
- `XOR`: `A := B XOR A`

### Immediate Arithmetic
- `An` (1 ≤ n ≤ 5): `A := A + n`
- `Sn` (1 ≤ n ≤ 4): `A := A - n`
- `A b`: `A := A + b`
- `AH h`: `A := A + h`
- `AW w`: `A := A + w`

## Store Instructions

### Simple Store
- `SPn` (3 ≤ n ≤ 16): `P!n := A`
- `SP b`: `P!b := A`
- `SPH h`: `P!h := A`
- `SPW w`: `P!w := A`
- `SG b`: `G!b := A`
- `SG1 b`: `G!(b+256) := A`
- `SGH h`: `G!h := A`

### Indirect Store
- `PBYT`: `B%A := C`
- `XPBYT`: `A%B := C`
- `ST`: `A!0 := B`
- `STn` (1 ≤ n ≤ 3): `A!n := B`

## Function Calls

- `Kn` (3 ≤ n ≤ 11): Call A with stack increment n
- `K b`, `KH h`, `KW w`: Call A with variable stack increment
- `KnG b`: Call G!b with stack increment n
- `RTN`: Return from function

## Control Flow

- `J Ln`: Unconditional jump
- `Jrel Ln`: Conditional jump based on comparison
- `GOTO`: Jump to address in A
- `FHOP`: False hop (for conditional expressions)

## Switch Instructions

- `SWL`: Label vector switch
- `SWB`: Binary chop switch

## Floating-Point Operations

FLTOP instructions for floating-point operations:
- `FLTOP 1 b`: Convert to float and multiply by 10^b
- `FLTOP 3`: Integer to float conversion
- `FLTOP 4`: Float to integer conversion
- `FLTOP 5`: Absolute value
- `FLTOP 6-9`: Basic arithmetic (#*, #/, #+, #-)
- `FLTOP 10-11`: Unary operations (#+, #-)
- `FLTOP 12-17`: Comparisons (#=, #~=, #<, #>, #<=, #>=)

## System and Miscellaneous

- `XCH`: Exchange A and B
- `ATB`: Copy A to B
- `ATC`: Copy A to C
- `BTC`: Copy B to C
- `NOP`: No operation
- `SYS`: System call
- `BRK`: Debugger breakpoint
- `CHGCO`: Coroutine context switch

## Implementation Notes

1. All memory accesses are 64-bit aligned (8-byte boundaries)
2. Stack grows downward
3. Function calls preserve caller-saved registers
4. Floating-point operations use AArch64's NEON/FP unit
5. Switch tables use either direct jumps or binary search depending on density
6. System calls follow AArch64 ABI conventions

## Error Handling

- Division by zero raises exception 5
- Invalid memory access causes segmentation fault
- Stack overflow checking is implementation-dependent