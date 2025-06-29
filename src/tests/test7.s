=== BCPL Compiler ===
Source file: ./tests/test7.b

Parsing...
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: LET
parseExpression: 0
parsePrimaryExpression: 0
parseStatement: FOR
parseExpression: 1
parsePrimaryExpression: 1
parseExpression: 10
parsePrimaryExpression: 10
parseStatement: $(
parseStatement: total
parseExpressionStatement: total
parseExpression: total
parsePrimaryExpression: total
parseExpression: total
parsePrimaryExpression: total
parseExpression: i
parsePrimaryExpression: i
parseStatement: RESULTIS
parseExpression: total
parsePrimaryExpression: total
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
    sub sp, sp, #48                // Allocate stack frame (placeholder)
    movz x0, #0x0                  // Load number literal
    ldr x27, [x29, #-8]            // Load variable total into x27
    mov x27, x0                    // Initialize local total in x27
    movz x0, #0x1                  // Load number literal
    str x0, [x29, #-16]            // Store initial value
    movz x0, #0x10                 // Load number literal
    str x0, [x29, #-24]            // Store end value
    movz x0, #0x1                  // Loading 1 into x0
    str x0, [x29, #-32]            // Store step value
repeat_2:
    ldr x0, [x29, #-16]           
    ldr x1, [x29, #-24]           
    cmp x0, x1                    
    b.gt loop_end_1               
    ldr x26, [x29, #-16]           // Load variable i into x26
    mov x0, x26                    // Load i from stack to x26 and then to X0
    mov x15, x0                   
    mov x0, x27                    // Move total from x27 to X0
    add x0, x0, x15                // Add
    mov x27, x0                    // Assign value to total in x27
    ldr x0, [x29, #-16]           
    ldr x1, [x29, #-32]           
    add x0, x0, x1                
    str x0, [x29, #-16]           
    b repeat_2                    
loop_end_1:
    mov x0, x27                    // Move total from x27 to X0
    b return_0                     // Branch to function return
return_0:
    str x27, [x29, #-8]            // Spill total from x27 to stack
    add sp, sp, #48                // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function

;------------ End of Assembly ------------


Compilation successful.
