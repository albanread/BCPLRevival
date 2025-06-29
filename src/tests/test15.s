=== BCPL Compiler ===
Source file: ./tests/test15.b

Parsing...
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: WRITES
parseExpressionStatement: WRITES
parseExpression: WRITES
parsePrimaryExpression: WRITES
parseExpression: Testing built-in functions

parsePrimaryExpression: Testing built-in functions

parseStatement: LET
parseExpression: 123
parsePrimaryExpression: 123
parseStatement: WRITEF
parseExpressionStatement: WRITEF
parseExpression: WRITEF
parsePrimaryExpression: WRITEF
parseExpression: The value of x is %i

parsePrimaryExpression: The value of x is %i

parseExpression: x
parsePrimaryExpression: x
parseStatement: NEWLINE
parseExpressionStatement: NEWLINE
parseExpression: NEWLINE
parsePrimaryExpression: NEWLINE
parseStatement: WRITES
parseExpressionStatement: WRITES
parseExpression: WRITES
parsePrimaryExpression: WRITES
parseExpression: This is the end of the test.

parsePrimaryExpression: This is the end of the test.

parseStatement: FINISH
parseExpressionStatement: FINISH
parseExpression: FINISH
parsePrimaryExpression: FINISH
parseStatement: WRITES
parseExpressionStatement: WRITES
parseExpression: WRITES
parsePrimaryExpression: WRITES
parseExpression: This should not be printed.

parsePrimaryExpression: This should not be printed.

parseStatement: RESULTIS
parseExpression: 0
parsePrimaryExpression: 0
Parsing complete.

Generating code...
Visiting function declaration: START
Generated return label: return_0
Code generation complete.

=== Generated Assembly ===

;------------ Generated ARM64 Assembly ------------

.arch armv8-a
.text
.align 4

.text
.align 4

START:
    stp x29, x30, [sp, #-16]       // Save FP/LR
    mov x29, sp                    // Set up frame pointer
    sub sp, sp, #32                // Allocate stack frame (placeholder)
    adr x0, .L.str0                // Load string literal address
    bl writes                      // Call writes
    movz x0, #0x123                // Load number literal
    ldr x27, [x29, #-8]            // Load variable x into x27
    mov x27, x0                    // Initialize local x in x27
    sub sp, sp, x16                // Allocate space for WRITEF arguments
    mov x0, x27                    // Move x from x27 to X0
    str x0, [sp]                   // Store WRITEF argument 1
    adr x0, .L.str1                // Load string literal address
    str x0, [sp, #8]               // Store WRITEF argument 0
    ldr x0, [sp, #8]               // Load format string for WRITEF
    ldr x1, [sp]                   // Load first data arg for WRITEF
    bl writef                      // Call writef
    add sp, sp, #16                // Deallocate WRITEF arguments
    bl newline                     // Call newline
    adr x0, .L.str2                // Load string literal address
    bl writes                      // Call writes
    bl finish                      // Call finish
    adr x0, .L.str3                // Load string literal address
    bl writes                      // Call writes
    movz x0, #0x0                  // Load number literal
    b return_0                     // Branch to function return
return_0:
    str x27, [x29, #-8]            // Spill x from x27 to stack
    add sp, sp, #32                // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function

.data
.L.str0:
    .string "Testing built-in functions
"
.L.str1:
    .string "The value of x is %i
"
.L.str2:
    .string "This is the end of the test.
"
.L.str3:
    .string "This should not be printed.
"

;------------ End of Assembly ------------


Compilation successful.
