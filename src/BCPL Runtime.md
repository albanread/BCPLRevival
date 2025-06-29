# **Notes on a Modernised BCPL Runtime for Unix**

## **1\. Introduction and Philosophy**

This document outlines a design for a modern BCPL runtime system, specifically targeting Unix-like operating systems (Linux, macOS, BSD). The primary goals are:

1. **Unix Philosophy:** Embrace standard Unix mechanisms for memory management, I/O, and program loading rather than recreating low-level facilities.  
2. **JIT-Oriented:** Design the runtime to support a Just-In-Time (JIT) compiler that translates BCPL source directly into executable machine code in memory.  
3. **Safety and Portability:** Leverage the host OS to provide a more stable and secure environment than the original, bare-metal-adjacent runtimes.  
4. **Simplicity:** Retain the spirit of BCPL by keeping the runtime layer thin and understandable.

The runtime itself would be an executable (e.g., bcpl\_jit) that hosts the compiler and manages the execution environment for the compiled code.

## **2\. Memory Model**

The original BCPL memory model consisted of the Global Vector, the Stack, and dynamically allocated vectors. We can modernize these concepts to map cleanly onto a modern OS.

### **2.1. The Stack**

In a JIT model, the native machine stack can and should be used directly. This is the most efficient approach.

* **Function Calls:** A BCPL function or routine call (F(A, B)) would be JIT-compiled into a standard native CALL instruction.  
* **Stack Frames:** The JIT would generate conventional function prologues and epilogues. A typical stack frame for a BCPL function would contain:  
  * The return address (pushed by the CALL instruction).  
  * The saved frame pointer of the caller.  
  * Space for its local variables (LET declarations).  
  * Arguments passed to any functions it calls.  
* **Arguments:** Arguments are passed by value, as per the BCPL specification. The JIT would place arguments in registers or on the stack according to the native C ABI (Application Binary Interface) of the target architecture (e.g., System V ABI for x86-64). This makes calling external C functions seamless.  
* **Recursion:** Using the native stack automatically provides support for recursion. Stack overflow becomes a standard OS-level exception.

### **2.2. Dynamic Memory (LET V \= VEC K)**

The original runtime managed a heap for vector allocation. This can be directly replaced by the standard C library allocator, which is highly optimized and integrated with OS virtual memory.

* **Allocation:** A VEC declaration would be JIT-compiled into a call to the C malloc function. The runtime would link against the C standard library.  
  // BCPL: LET MyVec \= VEC 100  
  // JIT compiles to code equivalent to:  
  void\* MyVec \= malloc( (100 \+ 1\) \* sizeof(word) );

* **Deallocation:** BCPL has no free command; memory is reclaimed when a block is exited. The runtime must track VEC allocations made within a block. When the block exits, the JIT-compiled epilogue would call free on each vector allocated within that block's scope.

### **2.3. Modernising the Global Vector**

The Global Vector was BCPL's primary mechanism for linking separately compiled modules. It was a shared, raw block of memory where modules could access data and functions via known integer offsets. This concept is obsolete in a modern OS with dynamic linking.

* **Proposed Replacement:** A hash table or a similar map structure within the runtime can serve as a **Symbol Table** for the JIT.  
* **GLOBAL Declaration:** GLOBAL $( F:100, V:101 $) would not allocate from a vector. Instead, it would register the names F and V in the runtime's symbol table. These names are marked as "external".  
* **JIT Linking:** When the JIT compiles a function or static variable, it places the name and its in-memory machine-code address into the symbol table. When it encounters a call to an external name, it records a placeholder.  
* **Final Linking Step:** After all code is JIT-compiled, the runtime performs a "linking" pass. It iterates through all placeholder calls and patches them with the final addresses looked up from the symbol table. This mimics what a traditional linker does and is extremely fast.

This approach provides the same functionality as the Global Vector but with the benefits of symbolic naming and without the rigidity of fixed offsets.

## **3\. Program Execution and the JIT Process**

The bcpl\_jit runtime would orchestrate the entire lifecycle of a program run.

1. **Invocation:**  
   ./bcpl\_jit my\_program.b

2. **Bootstrap:** The runtime starts up, initializes its internal state (like the symbol table), and opens my\_program.b.  
3. **Memory Allocation:** The runtime allocates a region of memory using mmap on Unix. This memory must be marked as writable and readable (PROT\_WRITE | PROT\_READ).  
4. **JIT Compilation:** The runtime's built-in BCPL compiler reads the source file, parses it, and emits native machine code directly into the mmap-ed region. As it compiles functions and routines, it populates the symbol table with their names and addresses.  
5. **Memory Protection:** After compilation is complete, the runtime uses mprotect to change the memory region's permissions to readable and executable (PROT\_READ | PROT\_EXEC). This is a critical security step (W^X).  
6. **Finding the Entry Point:** The runtime looks up the START symbol in its table to find the address of the main routine.  
7. **Execution:** The runtime casts the address of the START routine to a C function pointer and calls it, passing the program's command-line arguments in a format the BCPL program can understand (e.g., as a vector of strings).  
8. **Termination:** When the START routine returns or calls FINISH, the runtime calls exit() with the appropriate status code.

## **4\. Modernising the Standard Library**

The BCPL library functions can be implemented as thin wrappers over the host's C standard library. The JIT must know the C ABI to call them correctly.

| BCPL Function | Modern Unix/C Implementation | Notes |
| :---- | :---- | :---- |
| findinput(S) | fopen(S, "r") | Returns a FILE\* pointer, which is just a word. |
| findoutput(S) | fopen(S, "w") | Returns a FILE\* pointer. |
| selectinput(F) | Set a global "current input" to F | The runtime maintains FILE\* current\_in. |
| rdch() | fgetc(current\_in) | Maps directly. ENDSTREAMCH becomes EOF. |
| wrch(CH) | fputc(CH, current\_out) | Maps directly. |
| endread() | fclose(current\_in) | Maps directly. |
| stop(N) / finish | exit(N) | The standard process termination call. |
| time() | clock() or clock\_gettime() | Provides process execution time. |
| readn(), writen(N) | Implemented in the runtime using fscanf/fprintf. |  |

## **5\. Error Handling**

* **Compile-Time Errors:** Handled by the JIT compiler and reported to the user before execution begins.  
* **Runtime Errors:** The original ABORT routine was called on faults like illegal memory access. In a modern system, this is handled by the OS.  
  * The runtime should install **signal handlers** for SIGSEGV (segmentation fault), SIGFPE (floating-point/integer error), etc.  
  * When a signal is caught, the handler can provide a rich **stack trace**. The JIT would need to have generated a map of instruction address ranges to BCPL function names, allowing the signal handler to print a meaningful, symbolic backtrace rather than raw hex addresses.