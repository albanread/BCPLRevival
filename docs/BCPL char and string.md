# **BCPL Character and String Implementation**

## **1\. Overview**

This document specifies the representation and handling of characters and strings for the modern BCPL JIT compiler. The design is driven by two primary goals:

1. **Unicode Support:** To handle modern text, characters must support the full Unicode character set.  
2. **C ABI Compatibility:** To interface seamlessly with the host operating system's C standard library, the string format must be directly compatible.

## **2\. Character Model**

### **2.1. Representation**

A BCPL character is a **32-bit unsigned integer** representing a Unicode code point. This allows for the representation of all characters in the Basic Multilingual Plane (BMP) and supplementary planes.

### **2.2. Character Constants**

A character constant, written as 'c', is parsed by the compiler and represented as its corresponding 32-bit integer value in the generated machine code. For example, 'A' becomes the integer value 65\.

## **3\. String Model**

### **3.1. Representation**

A BCPL string is a **pointer to a sequence of one or more 32-bit characters, terminated by a 32-bit zero value (NULL terminator)**.  
This zero-terminated format is chosen for direct compatibility with C library functions (e.g., printf, fopen), which eliminates the need for costly conversions at the boundary between BCPL code and system libraries.

### **3.2. String Literals**

When the JIT compiler encounters a string literal like "ABC", it will create a corresponding constant in a read-only data segment of the program. The memory layout will be as follows:

| Address Offset | Hex Value (32-bit) | Character |
| :---- | :---- | :---- |
| \+0 | 0x00000041 | 'A' |
| \+4 | 0x00000042 | 'B' |
| \+8 | 0x00000043 | 'C' |
| \+12 | 0x00000000 | NULL Terminator |

The value of the string expression in the code will be the starting address of this data.

## **4\. Character Indirection Operator: %**

To provide ergonomic access to 32-bit characters within a string, a new dyadic operator, %, is introduced.

* **Syntax:** S % E  
* **Description:** Yields the 32-bit character at index E of the string pointed to by S.  
* **Precedence:** It has the same left-to-right precedence as the vector subscripting operator (\!).

### **4.1. JIT Implementation**

The JIT compiler must translate the % operator into a scaled memory-load instruction. Given a string pointer S in a base register (e.g., x1) and an integer index E in another register (e.g., x2), the AArch64 implementation is:  
; BCPL: LET CHAR \= S % E  
; x1 contains the address of the string S  
; x2 contains the index E

; Load the 32-bit word (W-register) from the address \[base \+ index\*4\]  
; LSL \#2 performs a logical shift left by 2, which is equivalent to multiplying by 4\.  
LDR W0, \[X1, X2, LSL \#2\]

; W0 now holds the character value.

This single instruction efficiently performs the required address calculation and memory access.