=== BCPL Compiler ===
Source file: ./tests/test6.bcpl

Parsing...
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: LET
parseExpression: 10
parsePrimaryExpression: 10
parseStatement: LET
parseExpression: 5
parsePrimaryExpression: 5
parseStatement: LET
parseExpression: 3
parsePrimaryExpression: 3
parseStatement: LET
parseExpression: 8
parsePrimaryExpression: 8
parseStatement: LET
parseExpression: a
parsePrimaryExpression: a
parseExpression: (
parsePrimaryExpression: (
parseExpression: b
parsePrimaryExpression: b
parseExpression: c
parsePrimaryExpression: c
parseExpression: d
parsePrimaryExpression: d
parseExpression: 2
parsePrimaryExpression: 2
parseStatement: RESULTIS
parseExpression: result
parsePrimaryExpression: result
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
    sub sp, sp, #64                // Allocate stack frame (placeholder)
    movz x0, #0x10                 // Load number literal
    ldr x27, [x29, #-8]            // Load variable a into x27
    mov x27, x0                    // Initialize local a in x27
    movz x0, #0x5                  // Load number literal
    ldr x26, [x29, #-16]           // Load variable b into x26
    mov x26, x0                    // Initialize local b in x26
    movz x0, #0x3                  // Load number literal
    ldr x25, [x29, #-24]           // Load variable c into x25
    mov x25, x0                    // Initialize local c in x25
    movz x0, #0x8                  // Load number literal
    ldr x24, [x29, #-32]           // Load variable d into x24
    mov x24, x0                    // Initialize local d in x24
    movz x0, #0x2                  // Load number literal
    mov x15, x0                   
    mov x0, x24                    // Move d from x24 to X0
    sdiv x0, x0, x15               // Divide
    mov x15, x0                   
    mov x0, x25                    // Move c from x25 to X0
    mov x14, x0                   
    mov x0, x26                    // Move b from x26 to X0
    add x0, x0, x14                // Add
    mov x14, x0                   
    mov x0, x27                    // Move a from x27 to X0
    mul x0, x0, x14                // Multiply
    sub x0, x0, x15                // Subtract
    ldr x23, [x29, #-40]           // Load variable result into x23
    mov x23, x0                    // Initialize local result in x23
    mov x0, x23                    // Move result from x23 to X0
    b return_0                     // Branch to function return
return_0:
    str x23, [x29, #-40]           // Spill result from x23 to stack
    str x24, [x29, #-32]           // Spill d from x24 to stack
    str x25, [x29, #-24]           // Spill c from x25 to stack
    str x26, [x29, #-16]           // Spill b from x26 to stack
    str x27, [x29, #-8]            // Spill a from x27 to stack
    add sp, sp, #64                // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function

;------------ End of Assembly ------------


Compilation successful.
