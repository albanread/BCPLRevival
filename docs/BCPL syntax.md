# **BCPL Language Syntax Reference**

This document provides a comprehensive overview of the BCPL (Basic Combined Programming Language) syntax, drawing from the 1969 TX-2 BCPL Reference Manual and the 1974 BCPL Programming Manual by M. Richards.

## **1\. Lexical Elements**

### **1.1. Identifiers**

An identifier is a sequence of letters and digits, starting with a letter. In some implementations, the underscore \_ is also permitted.

### **1.2. Numbers**

BCPL supports several number formats:

* **Decimal:** A sequence of decimal digits (e.g., 123).  
* **Octal:** The symbol \# followed by a sequence of octal digits (e.g., \#777).  
* **Hexadecimal:** The characters \#X followed by a sequence of hexadecimal digits (e.g., \#X1A).

### **1.3. String Constants**

A string constant is a sequence of up to 255 characters enclosed in double quotes ("). Special characters can be represented using an escape sequence starting with \*:

* \*n or \*N: Newline  
* \*t or \*T: Tab  
* \*s or \*S: Space  
* \*b or \*B: Backspace  
* \*p or \*P: Newpage  
* \*c or \*C: Carriage Return  
* \*": Double quote  
* \*\*: Asterisk

### **1.4. Character Constants**

A character constant is a single character enclosed in single quotes ('). The same escape sequences as string constants apply.

### **1.5. Truth Values**

* TRUE: Represents the integer value 1 (or all ones in some implementations).  
* FALSE: Represents the integer value 0\.

### **1.6. Comments**

* //: A single-line comment, extending to the end of the line.  
* /\* ... \*/: A multi-line comment.

## **2\. Expressions**

### **2.1. Operator Precedence**

| Precedence | Operator(s) | Associativity | Description |
| :---- | :---- | :---- | :---- |
| Highest | () | \- | Function Call |
|  | \!, OF | Left-to-right | Subscripting, Field |
|  | @, \! | Right-to-left | Address-of, Indirection |
|  | \*, /, REM | Left-to-right | Multiplication, Division |
|  | \+, \- | Left-to-right | Addition, Subtraction |
|  | \<\<, \>\> | Left-to-right | Shift Operators |
|  | \=, \~=, \<, \<=, \>, \>= | Left-to-right | Relational Operators |
|  | & | Left-to-right | Logical AND |
|  | \` | , NEQV, EQV\` | Left-to-right |
|  | \-\> | Right-to-left | Conditional Expression |
|  | TABLE | \- | Table Constructor |
| Lowest | VALOF | \- | Value-of Block |

### **2.2. Addressing Operators**

* **@E (Address of):** Returns the memory address (Lvalue) of E.  
* **\!E (Indirection):** Returns the contents (Rvalue) of the memory location pointed to by E.  
* **V\!E (Vector Subscript):** Accesses the E-th element of vector V. V\!E is equivalent to \!(V+E).

### **2.3. Arithmetic Operators**

* \+ (Addition), \- (Subtraction)  
* \* (Multiplication), / (Division)  
* REM (Remainder)

### **2.4. Relational Operators**

* \= (Equal), \~= (Not Equal)  
* \< (Less than), \<= (Less than or equal)  
* \> (Greater than), \>= (Greater than or equal)

### **2.5. Logical Operators**

* \~E (Logical NOT)  
* E1 & E2 (Logical AND)  
* E1 | E2 (Logical OR)  
* E1 EQV E2 (Equivalence)  
* E1 NEQV E2 (Not Equivalent, XOR)

### **2.6. Shift Operators**

* E1 \<\< E2 (Left shift)  
* E1 \>\> E2 (Right shift)

### **2.7. Conditional Expression**

E1 \-\> E2, E3  
If E1 is TRUE, the result is E2; otherwise, the result is E3.

### **2.8. VALOF Expression**

VALOF C  
Executes the command C until a RESULTIS E command is encountered. The value of the VALOF expression is the value of E.

## **3\. Commands**

### **3.1. Assignment**

\<LHS\> := E  
Assigns the value of expression E to the left-hand side LHS. The LHS can be a variable, a vector element (V\!i), or an indirection (\!addr).  
Multiple assignments are also possible:  
L1, L2 := E1, E2

### **3.2. Routine Call**

R(E1, E2, ...)  
Calls the routine R with the given expressions as arguments.

### **3.3. Conditional Commands**

* IF E THEN C  
* UNLESS E THEN C  
* TEST E THEN C1 OR C2

### **3.4. Repetitive Commands**

* WHILE E DO C  
* UNTIL E DO C  
* C REPEAT  
* C REPEATWHILE E  
* C REPEATUNTIL E  
* FOR N \= E1 TO E2 BY K DO C

### **3.5. SWITCHON Command**

SWITCHON E INTO $(  
  CASE K1: C1  
  CASE K2: C2  
  ...  
  DEFAULT: CD  
$)

Transfers control to the command C associated with the case constant K that matches the value of E. If no case matches, control goes to the DEFAULT command.

### **3.6. Control Transfer**

* GOTO E: Unconditional jump to a label.  
* RETURN: Returns from a routine.  
* FINISH: Terminates the program.  
* BREAK: Exits the innermost loop.  
* LOOP: Restarts the innermost loop.  
* ENDCASE: Exits a SWITCHON block.  
* RESULTIS E: Returns a value from a VALOF block.

## **4\. Declarations**

### **4.1. LET**

LET N1, N2, ... \= E1, E2, ...  
Declares one or more dynamic variables and initializes them.

### **4.2. MANIFEST**

MANIFEST $( N1 \= K1; N2 \= K2 ... $)  
Declares compile-time constants.

### **4.3. STATIC**

STATIC $( N1 \= K1; N2 \= K2 ... $)  
Declares static variables with initial values.

### **4.4. GLOBAL**

GLOBAL $( N1: K1; N2: K2 ... $)  
Declares global variables at specific offsets in the global vector.

### **4.5. VEC**

LET V \= VEC K  
Declares a dynamic vector V with K+1 elements (indices 0 to K).

### **4.6. Function and Routine Declarations**

* **Function:** LET F(P1, P2) \= E  
* **Routine:** LET R(P1, P2) BE C

### **4.7. AND**

LET F1() \= E1 AND F2() \= E2  
Allows for simultaneous declarations, typically used for mutually recursive functions.

### **4.8. Label**

L: C  
Declares a label L for the command C.

## **5\. Program Structure**

### **5.1. Section Brackets ($( and $))**

Used to group declarations and commands into blocks. They can be tagged for matching, e.g., $(LOOP ... $)LOOP.  
In some early implementations, such as for the TX-2 computer, curly braces ({ and }) were used as direct hardware representations for $( and $) respectively.

### **5.2. Compound Command**

A sequence of commands enclosed in section brackets:  
$( C1; C2; ... $)

### **5.3. Block**

A sequence of declarations followed by commands, enclosed in section brackets:  
$( D1; D2; ...; C1; C2; ... $)

### **5.4. GET Directive**

GET "filename"  
Includes the content of another source file.