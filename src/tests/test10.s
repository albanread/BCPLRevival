=== BCPL Compiler ===
Source file: ./tests/test10.b

Parsing...
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: LET
parseExpression: 10
parsePrimaryExpression: 10
parseStatement: REPEAT
parseStatement: $(
parseStatement: x
parseExpressionStatement: x
parseExpression: x
parsePrimaryExpression: x
parseExpression: x
parsePrimaryExpression: x
parseExpression: 1
parsePrimaryExpression: 1
parseExpression: x
parsePrimaryExpression: x
parseExpression: 0
parsePrimaryExpression: 0
parseStatement: RESULTIS
parseExpression: x
parsePrimaryExpression: x
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
    movz x0, #0x10                 // Load number literal
    ldr x27, [x29, #-8]            // Load variable x into x27
    mov x27, x0                    // Initialize local x in x27
repeat_2:
    movz x0, #0x1                  // Load number literal
    mov x15, x0                   
    mov x0, x27                    // Move x from x27 to X0
    sub x0, x0, x15                // Subtract
    mov x27, x0                    // Assign value to x in x27
    movz x0, #0x0                  // Load number literal
    mov x15, x0                   
    mov x0, x27                    // Move x from x27 to X0
    cmp x0, x15                   
    cset x0, eq                   
    neg x0, x0                    
    b.eq repeat_2                 
loop_end_1:
    mov x0, x27                    // Move x from x27 to X0
    b return_0                     // Branch to function return
return_0:
    str x27, [x29, #-8]            // Spill x from x27 to stack
    add sp, sp, #32                // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function

;------------ End of Assembly ------------


Compilation successful.
