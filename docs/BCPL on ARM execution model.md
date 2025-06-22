# **BCPL on ARM64 Execution Model Summary for Compiler Writers (32-bit Characters)**

This document outlines the execution model for a BCPL compiler targeting ARM64 on macOS, specifically for a 64-bit Cintcode implementation where individual characters (and "byte" operations) are treated as **32-bit words**. It is intended to guide a code generator by detailing data representation, register usage, memory access, and control flow mechanisms, aligning with the AArch64 Procedure Call Standard (AAPCS64).

### **1\. Data Representation**

* **Word Size:** All fundamental data types—integers, pointers, and floating-point numbers—are **64-bit**.  
* **Characters/Words (for "byte" operations):** When operations akin to PBYT are performed, they operate on **32-bit words**. A "character" in this context is a 32-bit value.  
* **Integers:** Represented as 64-bit signed integers.  
* **Pointers:** 64-bit memory addresses.  
* **Floating-Point Numbers:** 64-bit IEEE 754 double-precision format.

### **2\. Register Usage and Mapping**

The Cintcode virtual machine's registers are mapped directly to ARM64 registers, prioritizing compliance with AAPCS64 for seamless interoperability with C code.

| Cintcode Register | ARM64 Register | Description | Volatility (AAPCS64) |
| :---- | :---- | :---- | :---- |
| A (Accumulator) | x0 | Primary accumulator, first function argument, function return value (int/ptr). | Caller-saved |
| B (Auxiliary) | x1 | Second accumulator, second function argument. | Caller-saved |
| C (Char/Work) | x2 | Third function argument. Holds 32-bit words for PBYT/XPBYT operations. | Caller-saved |
| P (Stack Frame) | x29 (FP) | Frame Pointer: Base of the current stack frame. | Callee-saved |
| G (Global) | x28 | Global Pointer: Base of the Global Vector. | Callee-saved |
| ST (Status) | x27 | System/debug status register (Cintpos context). | Callee-saved |
| PC (Program Counter) | x30 (LR) | Link Register: Holds return address. Implicitly managed by BL/RET. | Callee-saved |
| Count | x26 | Debugging register for instruction counts. | Callee-saved |
| MW (Memory Word) | x25 | Used for extending 32-bit w operands to 64-bit. | Callee-saved |

**Floating-Point Register Usage:**

* For floating-point operations, the A and B values will typically be moved to d0 and d1 (double-precision) respectively.  
* d0 is used for floating-point return values.  
* d0 \- d7 are caller-saved and used for passing floating-point arguments.  
* d8 \- d15 are callee-saved.  
* d16 \- d31 are caller-saved.

**General-Purpose Register Usage (x3 \- x7):** These registers are available for passing additional arguments in function calls, following AAPCS64. They are caller-saved. Temporary values can also be stored in other caller-saved registers (x9 \- x15) as needed.

### **3\. Memory Model and Addressing**

* **Endianness:** macOS ARM64 uses **little-endian** byte ordering. Code generation must account for this.  
* **Memory Access:**  
  * All *word-sized* memory accesses (loads/stores of 64-bit values) should be 8-byte aligned for optimal performance.  
  * LDR (Load Register) and STR (Store Register) instructions are used for 64-bit word operations.  
  * Offsets for local (P\!n) and global (G\!n) variables are expressed as byte offsets relative to x29 (FP) and x28 (G), respectively. A word offset k translates to a byte offset k \* 8\.  
  * Indirect memory access (e.g., A\!n) uses the value in x0 as the base address, with n\*8 as the byte offset.  
* **32-bit Word Operations (GBYT, PBYT, XPBYT):**  
  * These instructions now operate on 32-bit quantities.  
  * LDR Wt, \[Xn, Xm\] (Load 32-bit word from Xn \+ Xm) is used for GBYT. Wt will be the 32-bit part of x0.  
  * STR Wt, \[Xn, Xm\] (Store 32-bit word from Wt to Xn \+ Xm) is used for PBYT and XPBYT. Wt will be the 32-bit part of x2 (C register).  
  * Addresses for these operations are byte-level addresses. ARM64 handles unaligned 32-bit loads/stores, but performance is best with 4-byte alignment.  
* **Stack:** The stack grows downwards. Stack frames are managed via x29 (FP) and sp (Stack Pointer).

### **4\. Instruction Translation Principles**

* **Immediate Values:**  
  * Small immediates (up to 4095 for arithmetic/logical, or specific ranges for MOVZ/MOVN) can be encoded directly.  
  * Larger 64-bit immediates will require MOVZ/MOVK pairs or loading from a literal pool (ADRP/ADD/LDR).  
  * The MW (x25) register is used to extend 32-bit w operands (e.g., in LPW, AW) to their full 64-bit value by adding its contents to the sign-extended 32-bit w. MW must then be reset to zero.  
* **Arithmetic and Logical Operations:**  
  * Direct mapping to ARM64 instructions (e.g., ADD, SUB, MUL, SDIV, UDIV, LSL, LSR, AND, ORR, EOR, NEG, MVN).  
  * DIV/MOD require careful handling for division by zero (raise exception 5).  
* **Comparisons and Conditional Jumps:**  
  * Comparisons (CMP) set condition flags.  
  * Conditional branches (B.EQ, B.NE, B.LT, B.GT, B.LE, B.GE) are used based on the desired relational operator.  
  * FHOP usually translates to a CMP followed by a conditional move (CSET) or a specific sequence to set x0 to 0 (false) or \-1 (true).  
* **Bitfield Operations (SELLD, SELST):** Translated to ARM64 bitfield instructions like UBFX (unsigned bitfield extract), SBFX (signed bitfield extract), BFI (bitfield insert). SELST operations require careful extraction, modification, and re-insertion of the bitfield.

### **5\. Function/Routine Calling Convention (AAPCS64)**

* **Arguments:**  
  * First eight integer/pointer arguments are passed in x0 through x7.  
  * First eight floating-point arguments are passed in d0 through d7.  
  * Additional arguments (beyond 8\) are passed on the stack, pushed in reverse order.  
* **Return Values:**  
  * 64-bit integer/pointer results returned in x0.  
  * 64-bit floating-point results returned in d0.  
* **Stack Frame Management (Kn, K b/h/w, RTN):**  
  * **Function Prologue:**  
    1. Push FP (x29) and LR (x30) onto the stack (paired store STP FP, LR, \[SP, \#-16\]\!).  
    2. Move SP to FP (MOV FP, SP).  
    3. Allocate space for local variables and spill registers by decrementing SP (SUB SP, SP, \#frame\_size).  
    4. Save any callee-saved registers (x19-x28, d8-d15) if they are used by the function.  
  * **Function Epilogue:**  
    1. Restore callee-saved registers.  
    2. Deallocate stack space (MOV SP, FP).  
    3. Restore FP and LR (LDP FP, LR, \[SP\], \#16).  
    4. Return to caller (RET).  
* **Function Pointers:** Functions can be called via address in a register (BLR \<reg\>).

### **6\. Special Considerations**

* **MDIV (Multi-Divide):** This complex instruction, involving 128-bit products, should be delegated to an external C helper function, or implemented as a sequence of ARM64 instructions that handle 64x64-bit multiplication and division to produce 128-bit results and remainders.  
* **SYS (System Call):** Directs control to an underlying C dosys function which handles specific system functionalities. Compiler should generate a call to this C function, passing parameters in x0, x1 etc.  
* **CHGCO (Change Coroutine):** This instruction performs a context switch between coroutines. This is highly architecture-specific and typically implemented as a C function that saves and restores necessary registers (including FP, LR, and all callee-saved registers) for the active and target coroutines.  
* **Debugging (BRK):** Maps to BRK \#0x1 for debugger interaction.  
* **Compiler-Generated Code:** The code generator must manage jump targets and literal pool generation for addresses and large immediates, especially for instructions like LL, LF, SWL, and SWB.

By adhering to this model, a compiler writer can effectively translate BCPL Cintcode into optimized and ABI-compliant ARM64 assembly for macOS, supporting the notion of 32-bit characters.