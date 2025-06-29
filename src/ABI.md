# **BCPL JIT ABI for AArch64 (macOS & Linux)**

## **1\. Introduction**

This document defines the Application Binary Interface (ABI) for the modern BCPL Just-In-Time (JIT) compiler targeting the AArch64 (arm64) architecture on Unix-like systems, specifically macOS and Linux.  
The primary goal is to establish a clear convention for register usage and stack management that:

1. **Adheres to the standard C ABI:** This is essential for calling external C library functions (e.g., malloc, fopen, printf) seamlessly.  
2. **Defines a stable environment for JIT-compiled code:** By assigning specific roles to certain registers, we can simplify the JIT's design and potentially improve the performance of the generated code.

All JIT-compiled code must strictly follow these conventions.

## **2\. AArch64 C ABI Summary (AAPCS64)**

Our JIT will conform to the "Procedure Call Standard for the ARM 64-bit Architecture" (AAPCS64). The following table summarizes the register usage defined by this standard.

| Register(s) | Role | Preservation | Description |
| :---- | :---- | :---- | :---- |
| x0 \- x7 | Argument / Return | Caller-Saved | Used to pass the first 8 integer or pointer arguments to a function. x0 is also used to return an integer or pointer result. |
| x8 \- x15 | Scratch / Temp | Caller-Saved | General-purpose scratch registers. Can be used for temporary storage within a function without needing to be saved. |
| x16 (IP0) | Scratch / Temp | Caller-Saved | Intra-Procedure call scratch register. Used by the linker for veneers; best avoided by the JIT. |
| x17 (IP1) | Scratch / Temp | Caller-Saved | Intra-Procedure call scratch register. Used by the linker; best avoided by the JIT. |
| x18 | Platform Register | Reserved | Reserved for platform-specific use (e.g., on macOS, points to the thread information block). Should not be used by the JIT. |
| x19 \- x28 | General Purpose | **Callee-Saved** | A function must preserve the values of these registers. If it modifies them, it must save them on entry and restore them on exit. |
| x29 (FP) | Frame Pointer | **Callee-Saved** | Points to the base of the current function's stack frame. |
| x30 (LR) | Link Register | Caller-Saved | Stores the return address when a function call (BL) is made. |
| sp | Stack Pointer | N/A | Points to the top of the stack. Must be 16-byte aligned. |
| xzr | Zero Register | N/A | Hard-wired to the value 0\. |

## **3\. BCPL JIT Register Usage**

To create an efficient execution environment for BCPL, we will assign specific roles to some of the available registers.

### **3.1. Dedicated Registers**

We will dedicate one of the callee-saved registers to hold a pointer to our runtime context. This avoids the overhead of passing a context pointer as an argument to every JIT-compiled function.

* x19: **BCPL Runtime Context Pointer**.  
  * This register will hold a pointer to a global struct containing pointers to key runtime facilities.  
  * This includes pointers to the C functions we use for I/O (fopen, fgetc, etc.), memory management (malloc, free), and the internal symbol table.  
  * All JIT-compiled BCPL code can assume that x19 is valid and points to this context. It must be preserved across all external C function calls.

### **3.2. General-Purpose Register Conventions**

* **Callee-Saved Registers (x20 \- x28, x29):**  
  * These registers are available for the JIT to allocate for BCPL LET variables that need to have their values preserved across function calls.  
  * If a JIT-compiled function uses any of these registers, it **must** save its original value in its prologue and restore it in its epilogue.  
  * x29 will be used as the standard frame pointer.  
* **Caller-Saved / Scratch Registers (x0 \- x15):**  
  * This block of registers is freely available to the JIT for all intermediate calculations when evaluating expressions.  
  * The JIT can use these registers to hold temporary values (e.g., the right-hand side of an assignment, intermediate arithmetic results) without needing to save them.  
  * x0 \- x7 will be used to pass arguments to both other BCPL functions and external C functions, conforming to AAPCS64.  
  * x0 will be used to receive the result of any function call.

## **4\. Stack Frame Layout**

The JIT will generate standard AArch64 stack frames. A BCPL function's stack frame will be organized as follows (from higher to lower memory addresses):  
\+---------------------------+  
| Arguments for functions   |  \<-- (If more than 8 args)  
| called by this function   |  
\+---------------------------+  
| Local variables (LET)     |  \<-- Stack-allocated space for locals  
\+---------------------------+  
| Saved Callee-Saved Regs   |  \<-- (e.g., x20, x21 if used)  
\+---------------------------+  
| Saved x19 (Context Ptr)   |  \<-- If making an external C call  
\+---------------------------+  
| Saved x29 (Caller's FP)   |  \<-- sp upon entry \+ offset  
| Saved x30 (Caller's LR)   |  
\+---------------------------+   \<-- x29 (FP) points here  
| Return Address to Caller  |  \<-- sp upon entry  
\+---------------------------+

### **Function Prologue Example**

A typical function prologue generated by the JIT will look like this:  
; stp: store pair of registers  
; fp \= x29, lr \= x30  
stp     fp, lr, \[sp, \#-16\]\! ; Push frame pointer and link register, pre-decrement sp  
mov     fp, sp              ; Set up our new frame pointer

; Allocate space for locals and save any callee-saved registers we will use  
; e.g., save x20 and x21  
sub     sp, sp, \#32         ; Allocate 32 bytes on stack  
stp     x20, x21, \[sp, \#16\] ; Store x20, x21

### **Function Epilogue Example**

A typical function epilogue will unwind the actions of the prologue:  
; Restore any callee-saved registers we used  
; e.g., restore x20 and x21  
ldp     x20, x21, \[sp, \#16\] ; Load x20, x21

; Deallocate local space, restore caller's frame, and return  
mov     sp, fp              ; Deallocate locals by resetting sp to fp  
ldp     fp, lr, \[sp\], \#16   ; Pop frame pointer and link register, post-increment sp  
ret                         ; Return to caller (jumps to address in lr)

This ABI provides a solid foundation for the JIT, balancing the need for compatibility with the host system and the desire for an optimized, consistent environment for executing BCPL code.