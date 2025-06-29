# ARM64 Cintcode Specification for macOS

This document outlines the specification for an ARM64-specific implementation of the BCPL Cintcode, tailored for macOS. This adaptation assumes a 64-bit architecture where integers, floating-point numbers, and pointers are all 64-bit.

## 1. Cintcode Virtual Registers and ARM64 Register Mapping

[cite_start]The standard Cintcode machine has nine virtual registers. These are mapped to ARM64 registers following the AArch64 Procedure Call Standard (AAPCS64) for macOS, prioritizing efficiency and adherence to standard calling conventions.

| Cintcode Register | ARM64 Register | Description                                                          | Volatility (Caller/Callee-saved) |
| :---------------- | :------------- | :------------------------------------------------------------------- | :------------------------------- |
| `A` (Accumulator) | `x0`           | Main accumulator, function first argument, and result register.      | Caller-saved                     |
| `B` (Auxiliary)   | `x1`           | Second accumulator, used in dyadic operations.                       | Caller-saved                     |
| `C` (Byte/Work)   | `x2`           | Used for byte subscription (`PBYT`, `XPBYT`) and temporary work.     | Caller-saved                     |
| `P` (Stack Frame) | `x29` (FP)     | Frame Pointer: Points to the base of the current stack frame.        | Callee-saved                     |
| `G` (Global)      | `x28`          | Global Pointer: Points to the base of the Global Vector.             | Callee-saved                     |
| `ST` (Status)     | `x27`          | Status Register: Used in Cintpos for system status.                  | Callee-saved                     |
| `PC` (Program Counter) | `x30` (LR)     | Link Register: Holds the return address for function calls. Implicitly managed by branch instructions. | Callee-saved                     |
| `Count`           | `x26`          | Debugging register for instruction counting.                         | Callee-saved                     |
| `MW` (Memory Word) | `x25`          | Used in 64-bit Cintcode for senior half of 64-bit results.           | Callee-saved                     |

**Notes on Register Usage:**
* **`x0` to `x7`**: Used for parameter passing and return values in C ABI. `A`, `B`, `C` are mapped to `x0`, `x1`, `x2` respectively, aligning with AAPCS64.
* **`x9` to `x15`**: Volatile scratch registers, suitable for intermediate calculations.
* **`x19` to `x28`**: Callee-saved registers. `P`, `G`, `ST`, `Count`, and `MW` are mapped to callee-saved registers to ensure their values are preserved across function calls.
* **`x29` (FP) and `x30` (LR)**: Standard frame pointer and link register, managed by the calling convention. `P` is explicitly mapped to `FP` for stack frame management.
* **`sp`**: Stack Pointer, managed automatically by stack operations.

## 2. Data Types and Sizes (64-bit Architecture)

On ARM64 macOS, all fundamental data types for Cintcode are 64-bit:
* **Integers**: All integer values (including constants and results of arithmetic operations) are 64-bit.
* **Floating-Point Numbers**: Floating-point values (e.g., those with the `FLT` tag) are represented using 64-bit IEEE 754 double-precision format.
* **Pointers**: All pointers (memory addresses) are 64-bit. This affects how addresses are calculated and stored.

## 3. Function and Routine Calls (C ABI Compliance)

Function and routine calls in ARM64 Cintcode will adhere to the AAPCS64, enabling seamless interoperability with C functions.

* **Argument Passing**: The first eight arguments are passed in registers `x0` through `x7`. Subsequent arguments are passed on the stack. For floating-point arguments, `v0` through `v7` are used.
* **Return Values**:
    * **64-bit Integer/Pointer**: Returned in `x0`.
    * **64-bit Floating-point**: Returned in `v0`.
* **Stack Frame Management**:
    * The `P` register (`x29`/FP) will always point to the base of the current stack frame.
    * The stack grows downwards.
    * Save/restore of callee-saved registers (`x19`-`x28`, `x29`, `x30`) will occur in the function prologue and epilogue as per AAPCS64.
* **`Kn`, `K b`, `KH h`, `KW w` (Function/Routine Calls)**: These instructions will generate code that:
    1.  Saves necessary caller-saved registers (`x0` to `x15`).
    2.  Sets up arguments in `x0` to `x7` (and/or `v0` to `v7`).
    3.  Pushes additional arguments onto the stack if needed.
    4.  Calls the target function using `BL` (Branch with Link) instruction.
    5.  Restores caller-saved registers.
    6.  The `P` register will be updated using the frame pointer mechanism to manage stack frames.
* **`RTN` (Return)**: This instruction will generate code to:
    1.  Restore the saved frame pointer (`x29`).
    2.  Restore the link register (`x30`).
    3.  Deallocate stack space.
    4.  Return to the caller using `RET`.

## 4. Memory Model

* **Little-Endian**: macOS ARM64 systems are typically little-endian. Cintcode will generate code optimized for little-endian byte ordering.
* **Memory Addressing**: All memory accesses will be 64-bit aligned for optimal performance where appropriate.
* **`MW` Register Usage**: In 64-bit mode, `MW` (`x25`) will be used to extend 32-bit immediate operands to 64-bit values during specific load instructions (e.g., `KW`, `LLPW`, `LW`, `LPW`, `SPW`, `APW`, `AW`). [cite_start]The 32-bit value will be sign-extended, and `MW`'s value will be added to the upper 32 bits if needed, then `MW` will be reset to zero.

## 5. Floating-Point Operations

* **IEEE 754 Double-Precision**: All floating-point numbers are 64-bit double-precision.
* **`FLTOP` Instruction**: This instruction will be translated to a series of ARM64 floating-point instructions (e.g., `FADDD`, `FSUBD`, `FMULD`, `FDIVD`, `FCMPE`, `FCVTZS`, etc.) operating on `D` registers (`v0` to `v31`).
* **Register Usage for Floats**: `v0` to `v7` for arguments and results, `v8` to `v15` for callee-saved, and `v16` to `v31` for caller-saved. The Cintcode `A` and `B` registers might need to be explicitly moved to/from floating-point registers for `FLTOP` operations.

## 6. Integer Arithmetic

* All arithmetic operations will operate on 64-bit integer values.
* Instructions like `MUL`, `DIV`, `MOD` will use 64-bit multiplication and division instructions (e.g., `MUL`, `SDIV`, `UDIV`, `MSUB`, `UMSUBL`). [cite_start]Exception handling for division by zero will be implemented as per Cintcode's specification.

## 7. Conditional Jumps and Relations

* Conditional jump instructions (`JEQ`, `JNE`, `JLS`, etc.) will utilize ARM64's conditional branch instructions (`B.EQ`, `B.NE`, `B.LT`, etc.) based on the status flags set by preceding comparison or arithmetic operations.
* The `FHOP` instruction will manipulate results in `x0` (A register) to `TRUE` or `FALSE` (-1 or 0 for 64-bit integers) based on the preceding comparison.

## 8. Development and Debugging

* **Assembly Output**: The `bcpl2sial` and `sial-sasm` commands can be used to generate human-readable Sial and ARM64 assembly output, aiding in understanding the compilation process and debugging.
* **Tracing**: The `mcK(mc_debug, ...)` directives can be used to enable various levels of tracing during dynamic code generation, including comments, MC instructions, target instructions, and compiled binary code. This will be invaluable for verifying the correct translation to ARM64.
* **Native Debuggers**: Standard ARM64 debuggers (e.g., LLDB) can be used to debug the generated native code.

## 9. Installation and Building

The process would generally involve:
1.  Obtaining the BCPL distribution.
2.  Configuring the build system (e.g., Makefiles) to target ARM64 macOS. This would involve selecting appropriate C compilers (e.g., `clang`) and linking against macOS system libraries.
3.  Implementing the ARM64 Sial translator (`sial-arm` equivalent) that converts Sial intermediate code into ARM64 assembly.
4.  Compiling the C interpreter and runtime components with ARM64 flags.
5.  Compiling the BCPL source code (including the compiler itself and standard libraries) using the new `bcpl2sial` and `sial-arm` tools.

This specification aims to provide a robust foundation for running Cintcode programs efficiently on ARM64 macOS, leveraging the native architecture and adhering to standard conventions.
