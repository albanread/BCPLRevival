# **Project Introduction: BCPL JIT Compiler**

## **1\. Project Overview**

Welcome to the BCPL JIT (Just-In-Time) Compiler project. The goal is to build a modern, performant, and safe runtime environment for the BCPL programming language. This is achieved by compiling BCPL source code directly to native AArch64 machine code at runtime.  
The project is implemented in **Rust**, chosen for its high performance, excellent C interoperability, and compile-time memory safety guarantees, which are critical for building a stable and secure system.

### **Core Philosophies:**

* **Unix Philosophy**: Leverage the host OS (Linux, macOS) for fundamental services like I/O and memory management instead of recreating them.  
* **JIT-Oriented**: The entire runtime is designed to support the JIT compilation model.  
* **Safety & Simplicity**: Use OS features and Rust's safety model to create a secure environment while keeping the runtime layer thin and understandable.

## **2\. Architecture**

The compiler is architected as a modular Rust library crate consumed by a thin binary executable. This enforces a clean separation of concerns between the different stages of the compilation pipeline.  
The logical data flow is a classic pipeline:  
BCPL Source String \-\> Vec\<Token\> \-\> Abstract Syntax Tree (AST) \-\> AArch64 Machine Code  
The library is organized into the following key modules:

* runtime.rs: Manages the execution environment, including the symbol table and C library interfaces.  
* lexer.rs: Converts source code into a stream of tokens.  
* parser.rs: Consumes tokens to build an Abstract Syntax Tree (AST).  
* codegen/: The "back-end" parent module responsible for generating AArch64 machine code from the AST.  
  * aarch64.rs: The primary code generator.  
  * regalloc.rs: Manages CPU register allocation.  
  * instruction.rs: Helpers for encoding AArch64 instructions.

## **3\. The BCPL Language Dialect**

This compiler implements a modernized version of BCPL. While it supports the classic syntax, it includes several extensions for modern programming.

### **3.1. Core Syntax**

The compiler parses standard BCPL syntax for lexical elements, expressions, commands, and declarations as described in historical manuals. This includes features like:

* **Expressions**: V\!E (vector subscript), @E (address-of), \!E (indirection).  
* **Commands**: IF, WHILE, FOR, SWITCHON.  
* **Declarations**: LET, MANIFEST, STATIC, VEC.

### **3.2. Modern Extensions**

* **Character and String Model**:  
  * Characters are **32-bit unsigned integers** to support Unicode.  
  * Strings are pointers to a sequence of 32-bit characters, terminated by a 32-bit zero. This makes them directly compatible with C libraries.  
  * A new operator, %, is introduced for 32-bit character indirection (S % E).  
* **Floating-Point Model**:  
  * Floats are **64-bit IEEE 754** values.  
  * A new set of "dotted" operators is introduced for float arithmetic (+., \-., \*., /., \=. etc.).  
  * A dotted vector indirection operator, .%, is used for accessing arrays of floats.  
  * Intrinsic functions FLOAT(n) and TRUNC(f) are provided for efficient integer-float conversions.

## **4\. Runtime and Execution Model**

The runtime acts as a host for the JIT-compiled code, managing memory, linking, and interaction with the operating system.

### **4.1. JIT Process**

1. **Memory Allocation**: The runtime allocates a memory region using mmap with PROT\_READ | PROT\_WRITE permissions.  
2. **Compilation**: The built-in compiler parses the BCPL source and emits native AArch64 machine code directly into the allocated memory.  
3. **Linking**: A runtime symbol table is used to resolve references between functions, mimicking a traditional linker but in memory.  
4. **Memory Protection**: The memory is then re-mapped to PROT\_READ | PROT\_EXEC as a security measure (W^X).  
5. **Execution**: The runtime finds the START function in the symbol table, casts its address to a function pointer, and calls it.

### **4.2. Memory Management**

* **Stack**: The native machine stack is used directly for function calls, local variables (LET), and argument passing. This is efficient and automatically supports recursion.  
* **Vectors**: LET V \= VEC K is compiled into a call to the C malloc function. Memory is reclaimed via free when variables go out of scope.  
* **Global Vector**: The traditional Global Vector is replaced by the JIT's internal **Symbol Table**. GLOBAL declarations register names in this table for linking.

### **4.3. Standard Library**

Standard library functions (rdch, wrch, findinput, etc.) are implemented as thin wrappers over the host's C standard library (fgetc, fputc, fopen).

## **5\. Target: AArch64 ABI**

All generated code must strictly conform to the **AArch64 Procedure Call Standard (AAPCS64)** to ensure compatibility with the OS and C libraries.

### **5.1. Register Usage**

The JIT maps BCPL's conceptual registers to physical AArch64 registers.  
| Role | Register(s) | Preservation | Description |  
| :--- | :--- | :--- | :--- |  
| Argument/Return | x0 \- x7 | Caller-Saved | Used for passing arguments and returning results. x0 is the primary accumulator. |  
| Scratch/Temp | x8 \- x15 | Caller-Saved | Freely available for intermediate calculations. |  
| BCPL Context | x19 | Callee-Saved | Dedicated to hold a pointer to the BCPL runtime context (symbol table, C function pointers, etc.). Must be preserved. |  
| LET Variables | x20 \- x28 | Callee-Saved | Available for LET variables that must be preserved across calls. Must be saved/restored by any function that uses them. |  
| Frame Pointer | x29 (FP) | Callee-Saved | Standard frame pointer, points to the base of the current stack frame. |  
| Link Register | x30 (LR) | Caller-Saved | Stores the return address for function calls. |  
| Global Pointer | x28 (G) | Callee-Saved | Points to the base of the Global Vector. |

### **5.2. Stack Frame Layout**

The JIT generates standard AArch64 stack frames. A typical function prologue involves saving the caller's Frame Pointer (x29) and Link Register (x30) and allocating space for local variables. The epilogue restores these registers and deallocates the stack space before returning.  
; Function Prologue  
stp     fp, lr, \[sp, \#-16\]\! ; Push fp and lr  
mov     fp, sp              ; Set up new frame pointer  
sub     sp, sp, \#32         ; Allocate space for locals & saved registers  
stp     x20, x21, \[sp, \#16\] ; Save callee-saved registers x20, x21

; ... function body ...

; Function Epilogue  
ldp     x20, x21, \[sp, \#16\] ; Restore callee-saved registers  
mov     sp, fp              ; Deallocate locals  
ldp     fp, lr, \[sp\], \#16   ; Pop fp and lr  
ret                         ; Return to caller

*An example of a function prologue and epilogue.*

## **6\. Getting Started**

To resume work, a new developer should:

1. **Review the ADRs**: Start with ADR-001.md and ADR-002.md for the high-level technical decisions.  
2. **Understand the AST**: The AST.h file provides a C++ representation of the Abstract Syntax Tree. This is a good guide for understanding the structure of the Rust AST that the parser (parser.rs) should produce.  
3. **Examine the codegen module**: This is where the AST is transformed into machine code. The ABI.md and BCPL on ARM execution model.md documents are essential references for how BCPL constructs map to AArch64 instructions.  
4. **Trace the Pipeline**: Follow the flow of data from the main executable, through the lexer, parser, and finally into the codegen to see how a simple BCPL program is compiled and executed.