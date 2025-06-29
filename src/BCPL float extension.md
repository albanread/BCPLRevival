# **BCPL Floating-Point Model**

## **1\. Overview**

This document specifies the model for handling 64-bit floating-point numbers in the modern BCPL JIT compiler. The design adheres to the typeless nature of BCPL by introducing a new set of operators for floating-point arithmetic, rather than adding a distinct floating-point type.

## **2\. Representation**

A floating-point number is stored in a standard 64-bit machine word and conforms to the **IEEE 754 double-precision** binary format. The BCPL runtime and JIT-compiled code do not distinguish between integer and floating-point values at the memory level; the distinction is made only by the operators used in expressions.

## **3\. Floating-Point Literals**

The syntax is extended to support floating-point literals. A number literal containing a decimal point (.) or the letter e (for scientific notation) will be parsed as a 64-bit floating-point value.

* **Examples:** 3.14159, 0.0, 1e-5, \-123.456e+10

The JIT compiler will store the IEEE 754 representation of these literals in a read-only data segment.

## **4\. Floating-Point Operators**

A new set of "dotted" operators is introduced for floating-point arithmetic. These operators instruct the JIT to use floating-point hardware instructions.

| Operator | Description | AArch64 Instruction |
| :---- | :---- | :---- |
| \+. | Floating-Point Addition | FADD |
| \-. | Floating-Point Subtraction | FSUB |
| \*. | Floating-Point Multiplication | FMUL |
| /. | Floating-Point Division | FDIV |
| .% | Floating-Point Vector Indirection | LDR (scaled) |

These operators have the same precedence as their integer counterparts.

### **4.1. Floating-Point Relational Operators**

A corresponding set of dotted relational operators is also introduced. These operators perform a floating-point comparison.

* \=., \~=., \<., \<=., \>., \>=.

### **4.2. Floating-Point Vector Indirection (.%)**

To support efficient access to arrays of floating-point numbers (stored in standard BCPL vectors), the .% operator is introduced.

* **Syntax:** V .% E  
* **Description:** Yields the 64-bit floating-point value at index E of the vector pointed to by V.  
* **JIT Implementation:** The JIT compiler must translate this into a scaled memory-load instruction that fetches a 64-bit double-precision value. Given a vector pointer V in a base register (e.g., x1) and an integer index E in another register (e.g., x2), the AArch64 implementation is:  
  ; BCPL: LET F \= V .% E  
  ; x1 contains the address of the vector V  
  ; x2 contains the index E

  ; Load the 64-bit double-precision float (D-register) from the address  
  ; \[base \+ index\*8\]. LSL \#3 performs a logical shift left by 3, which  
  ; is equivalent to multiplying by 8\.  
  LDR D0, \[X1, X2, LSL \#3\]

  ; D0 now holds the floating-point value.

## **5\. Conversion Functions**

To explicitly convert between integer and floating-point interpretations of data, two essential library functions will be provided by the runtime.

### **FLOAT(n)**

* **Description:** Converts a 64-bit integer n into its closest 64-bit floating-point representation.  
* **JIT Implementation:** This will be compiled to a single AArch64 instruction: SCVTF D0, X0 (Signed integer Convert to Floating-point).

### **TRUNC(f)**

* **Description:** Converts a 64-bit floating-point number f into a 64-bit integer by truncating towards zero.  
* **JIT Implementation:** This will be compiled to a single AArch64 instruction: FCVTZS X0, D0 (Floating-point Convert to Signed integer, rounding toward Zero).

These intrinsic functions provide a clear and efficient mechanism for type interpretation without breaking the fundamental typelessness of the language.