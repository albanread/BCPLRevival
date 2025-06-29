=== BCPL Compiler ===
Source file: ./tests/test16.b

Parsing...
parseExpression: A
parsePrimaryExpression: A
parseExpression: B
parsePrimaryExpression: B
parseExpression: C
parsePrimaryExpression: C
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: LET
parseExpression: ADD
parsePrimaryExpression: ADD
parseExpression: 10
parsePrimaryExpression: 10
parseExpression: 20
parsePrimaryExpression: 20
parseExpression: 30
parsePrimaryExpression: 30
parseStatement: RESULTIS
parseExpression: x
parsePrimaryExpression: x
Parsing complete.

Generating code...
Visiting function declaration: ADD
Generated return label: return_0
Visiting function declaration: START
Generated return label: return_1
Code generation complete.

=== Generated Assembly ===

;------------ Generated ARM64 Assembly ------------

.arch armv8-a
.text
.align 4

.text
.align 4

ADD:
    stp x29, x30, [sp, #-16]       // Save FP/LR
    mov x29, sp                    // Set up frame pointer
    sub sp, sp, #48                // Allocate stack frame (placeholder)
    str x0, [x29, #-8]             // Store parameter A to stack home
    ldr x27, [x29, #-8]            // Load variable A into x27
    str x1, [x29, #-16]            // Store parameter B to stack home
    ldr x26, [x29, #-16]           // Load variable B into x26
    str x2, [x29, #-24]            // Store parameter C to stack home
    ldr x25, [x29, #-24]           // Load variable C into x25
    mov x0, x25                    // Move C from x25 to X0
    mov x15, x0                   
    mov x0, x26                    // Move B from x26 to X0
    mov x14, x0                   
    mov x0, x27                    // Move A from x27 to X0
    add x0, x0, x14                // Add
    add x0, x0, x15                // Add
return_0:
    str x25, [x29, #-24]           // Spill C from x25 to stack
    str x26, [x29, #-16]           // Spill B from x26 to stack
    str x27, [x29, #-8]            // Spill A from x27 to stack
    add sp, sp, #48                // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function
START:
    stp x29, x30, [sp, #-16]       // Save FP/LR
    mov x29, sp                    // Set up frame pointer
    str x25, [x29, #-32]           // Save callee-saved register x25
    str x26, [x29, #-40]           // Save callee-saved register x26
    str x27, [x29, #-48]           // Save callee-saved register x27
    sub sp, sp, #128               // Allocate stack frame (placeholder)
    str x25, [x29, #-56]           // Save caller-saved register x25
    str x26, [x29, #-64]           // Save caller-saved register x26
    str x27, [x29, #-72]           // Save caller-saved register x27
    sub sp, sp, x24                // Allocate space for outgoing arguments
    movz x0, #0x10                 // Load number literal
    str x0, [sp]                   // Store argument 0
    movz x0, #0x20                 // Load number literal
    str x0, [sp, #8]               // Store argument 1
    movz x0, #0x30                 // Load number literal
    str x0, [sp, #16]              // Store argument 2
    ldr x0, [sp]                   // Load parameter into register
    ldr x1, [sp, #8]               // Load parameter into register
    ldr x2, [sp, #16]              // Load parameter into register
    bl ADD                         // Call ADD
    add sp, sp, #24                // Deallocate outgoing arguments
    ldr x27, [x29, #-72]           // Restore caller-saved register x27
    ldr x26, [x29, #-64]           // Restore caller-saved register x26
    ldr x25, [x29, #-56]           // Restore caller-saved register x25
    ldr x24, [x29, #-56]           // Load variable x into x24
    mov x24, x0                    // Initialize local x in x24
    mov x0, x24                    // Move x from x24 to X0
    b return_1                     // Branch to function return
return_1:
    str x24, [x29, #-56]           // Spill x from x24 to stack
    ldr x27, [x29, #-48]           // Restore callee-saved register x27
    ldr x26, [x29, #-40]           // Restore callee-saved register x26
    ldr x25, [x29, #-32]           // Restore callee-saved register x25
    add sp, sp, #128               // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function

;------------ End of Assembly ------------


Compilation successful.
