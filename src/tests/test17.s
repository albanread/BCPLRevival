=== BCPL Compiler ===
Source file: ./tests/test17.b

Parsing...
parseStatement: $(
parseStatement: g_val
parseExpressionStatement: g_val
parseExpression: g_val
parsePrimaryExpression: g_val
parseExpression: g_val
parsePrimaryExpression: g_val
parseExpression: val
parsePrimaryExpression: val
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: g_val
parseExpressionStatement: g_val
parseExpression: g_val
parsePrimaryExpression: g_val
parseExpression: 10
parsePrimaryExpression: 10
parseStatement: ADD_TO_G
parseExpressionStatement: ADD_TO_G
parseExpression: ADD_TO_G
parsePrimaryExpression: ADD_TO_G
parseExpression: 5
parsePrimaryExpression: 5
parseStatement: RESULTIS
parseExpression: g_val
parsePrimaryExpression: g_val
Parsing complete.

Generating code...
Visiting function declaration: ADD_TO_G
Generated return label: return_0
Visiting function declaration: START
Generated return label: return_1
Code generation complete.

=== Generated Assembly ===

;------------ Generated ARM64 Assembly ------------

.arch armv8-a
.text
.align 4

.data
.global g_val
g_val:
    .space 8

.text
.align 4

ADD_TO_G:
    stp x29, x30, [sp, #-16]       // Save FP/LR
    mov x29, sp                    // Set up frame pointer
    sub sp, sp, #32                // Allocate stack frame (placeholder)
    str x0, [x29, #-8]             // Store parameter val to stack home
    ldr x27, [x29, #-8]            // Load variable val into x27
    mov x0, x27                    // Move val from x27 to X0
    mov x15, x0                   
    ldr x0, [x28]                  // Load global g_val
    add x0, x0, x15                // Add
    str x0, [x28]                  // Store to global g_val
return_0:
    str x27, [x29, #-8]            // Spill val from x27 to stack
    add sp, sp, #32                // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function
START:
    stp x29, x30, [sp, #-16]       // Save FP/LR
    mov x29, sp                    // Set up frame pointer
    str x27, [x29, #-16]           // Save callee-saved register x27
    sub sp, sp, #48                // Allocate stack frame (placeholder)
    movz x0, #0x10                 // Load number literal
    str x0, [x28]                  // Store to global g_val
    str x27, [x29, #-24]           // Save caller-saved register x27
    sub sp, sp, x8                 // Allocate space for outgoing arguments
    movz x0, #0x5                  // Load number literal
    str x0, [sp]                   // Store argument 0
    ldr x0, [sp]                   // Load parameter into register
    bl ADD_TO_G                    // Call routine ADD_TO_G
    add sp, sp, #8                 // Deallocate outgoing arguments
    ldr x27, [x29, #-24]           // Restore caller-saved register x27
    ldr x0, [x28]                  // Load global g_val
    b return_1                     // Branch to function return
return_1:
    ldr x27, [x29, #-16]           // Restore callee-saved register x27
    add sp, sp, #48                // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function

;------------ End of Assembly ------------


Compilation successful.
