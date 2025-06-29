=== BCPL Compiler ===
Source file: ./tests/test20.b

Parsing...
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: LET
parseExpression: a
parsePrimaryExpression: a
parseExpression: b
parsePrimaryExpression: b
parseStatement: RESULTIS
parseExpression: c
parsePrimaryExpression: c
parseExpression: 2
parsePrimaryExpression: 2
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: LET
parseExpression: x
parsePrimaryExpression: x
parseExpression: y
parsePrimaryExpression: y
parseExpression: z
parsePrimaryExpression: z
parseStatement: LET
parseExpression: funcB
parsePrimaryExpression: funcB
parseExpression: local_a
parsePrimaryExpression: local_a
parseExpression: z
parsePrimaryExpression: z
parseExpression: 5
parsePrimaryExpression: 5
parseStatement: RESULTIS
parseExpression: local_a
parsePrimaryExpression: local_a
parseExpression: local_b
parsePrimaryExpression: local_b
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: LET
parseExpression: 10
parsePrimaryExpression: 10
parseStatement: LET
parseExpression: 5
parsePrimaryExpression: 5
parseStatement: WRITES
parseExpressionStatement: WRITES
parseExpression: WRITES
parsePrimaryExpression: WRITES
parseExpression: Calling funcA...

parsePrimaryExpression: Calling funcA...

parseStatement: LET
parseExpression: funcA
parsePrimaryExpression: funcA
parseExpression: val1
parsePrimaryExpression: val1
parseExpression: 2
parsePrimaryExpression: 2
parseExpression: val2
parsePrimaryExpression: val2
parseExpression: 3
parsePrimaryExpression: 3
parseExpression: 4
parsePrimaryExpression: 4
parseStatement: WRITEF
parseExpressionStatement: WRITEF
parseExpression: WRITEF
parsePrimaryExpression: WRITEF
parseExpression: Result from funcA is %i

parsePrimaryExpression: Result from funcA is %i

parseExpression: result
parsePrimaryExpression: result
parseStatement: RESULTIS
parseExpression: 0
parsePrimaryExpression: 0
Parsing complete.

Generating code...
Visiting function declaration: funcB
Generated return label: return_0
Visiting function declaration: funcA
Generated return label: return_1
Visiting function declaration: START
Generated return label: return_2
Code generation complete.

=== Generated Assembly ===

;------------ Generated ARM64 Assembly ------------

.arch armv8-a
.text
.align 4

.text
.align 4

funcB:
    stp x29, x30, [sp, #-16]       // Save FP/LR
    mov x29, sp                    // Set up frame pointer
    sub sp, sp, #48                // Allocate stack frame (placeholder)
    str x0, [x29, #-8]             // Store parameter a to stack home
    ldr x27, [x29, #-8]            // Load variable a into x27
    str x1, [x29, #-16]            // Store parameter b to stack home
    ldr x26, [x29, #-16]           // Load variable b into x26
    mov x0, x26                    // Move b from x26 to X0
    mov x15, x0                   
    mov x0, x27                    // Move a from x27 to X0
    sub x0, x0, x15                // Subtract
    ldr x25, [x29, #-24]           // Load variable c into x25
    mov x25, x0                    // Initialize local c in x25
    movz x0, #0x2                  // Load number literal
    mov x15, x0                   
    mov x0, x25                    // Move c from x25 to X0
    mul x0, x0, x15                // Multiply
    b return_0                     // Branch to function return
return_0:
    str x25, [x29, #-24]           // Spill c from x25 to stack
    str x26, [x29, #-16]           // Spill b from x26 to stack
    str x27, [x29, #-8]            // Spill a from x27 to stack
    add sp, sp, #48                // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function
funcA:
    stp x29, x30, [sp, #-16]       // Save FP/LR
    mov x29, sp                    // Set up frame pointer
    str x25, [x29, #-32]           // Save callee-saved register x25
    str x26, [x29, #-40]           // Save callee-saved register x26
    str x27, [x29, #-48]           // Save callee-saved register x27
    sub sp, sp, #144               // Allocate stack frame (placeholder)
    str x0, [x29, #-56]            // Store parameter x to stack home
    ldr x24, [x29, #-56]           // Load variable x into x24
    str x1, [x29, #-64]            // Store parameter y to stack home
    ldr x23, [x29, #-64]           // Load variable y into x23
    str x2, [x29, #-72]            // Store parameter z to stack home
    ldr x22, [x29, #-72]           // Load variable z into x22
    mov x0, x22                    // Move z from x22 to X0
    mov x15, x0                   
    mov x0, x23                    // Move y from x23 to X0
    mov x14, x0                   
    mov x0, x24                    // Move x from x24 to X0
    mul x0, x0, x14                // Multiply
    add x0, x0, x15                // Add
    ldr x21, [x29, #-80]           // Load variable local_a into x21
    mov x21, x0                    // Initialize local local_a in x21
    str x21, [x29, #-80]           // Spill local_a from x21 to stack
    str x22, [x29, #-72]           // Spill z from x22 to stack
    str x23, [x29, #-64]           // Spill y from x23 to stack
    str x24, [x29, #-56]           // Spill x from x24 to stack
    str x21, [x29, #-88]           // Save caller-saved register x21
    str x22, [x29, #-96]           // Save caller-saved register x22
    str x23, [x29, #-104]          // Save caller-saved register x23
    str x24, [x29, #-112]          // Save caller-saved register x24
    str x25, [x29, #-120]          // Save caller-saved register x25
    str x26, [x29, #-128]          // Save caller-saved register x26
    str x27, [x29, #-136]          // Save caller-saved register x27
    sub sp, sp, x16                // Allocate space for outgoing arguments
    ldr x20, [x29, #-80]           // Load variable local_a into x20
    mov x0, x20                    // Load local_a from stack to x20 and then to X0
    str x0, [sp]                   // Store argument 0
    movz x0, #0x5                  // Load number literal
    mov x15, x0                   
    ldr x19, [x29, #-72]           // Load variable z into x19
    mov x0, x19                    // Load z from stack to x19 and then to X0
    add x0, x0, x15                // Add
    str x0, [sp, #8]               // Store argument 1
    ldr x0, [sp]                   // Load parameter into register
    ldr x1, [sp, #8]               // Load parameter into register
    bl funcB                       // Call funcB
    add sp, sp, #16                // Deallocate outgoing arguments
    ldr x27, [x29, #-136]          // Restore caller-saved register x27
    ldr x26, [x29, #-128]          // Restore caller-saved register x26
    ldr x25, [x29, #-120]          // Restore caller-saved register x25
    ldr x24, [x29, #-112]          // Restore caller-saved register x24
    ldr x23, [x29, #-104]          // Restore caller-saved register x23
    ldr x22, [x29, #-96]           // Restore caller-saved register x22
    ldr x21, [x29, #-88]           // Restore caller-saved register x21
    ldr x20, [x29, #-88]           // Load variable local_b into x20
    mov x20, x0                    // Initialize local local_b in x20
    mov x0, x20                    // Move local_b from x20 to X0
    mov x15, x0                   
    mov x0, x21                    // Move local_a from x21 to X0
    add x0, x0, x15                // Add
    b return_1                     // Branch to function return
return_1:
    str x20, [x29, #-88]           // Spill local_b from x20 to stack
    ldr x27, [x29, #-48]           // Restore callee-saved register x27
    ldr x26, [x29, #-40]           // Restore callee-saved register x26
    ldr x25, [x29, #-32]           // Restore callee-saved register x25
    add sp, sp, #144               // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function
START:
    stp x29, x30, [sp, #-16]       // Save FP/LR
    mov x29, sp                    // Set up frame pointer
    str x20, [x29, #-72]           // Save callee-saved register x20
    str x21, [x29, #-80]           // Save callee-saved register x21
    str x22, [x29, #-88]           // Save callee-saved register x22
    str x23, [x29, #-96]           // Save callee-saved register x23
    str x24, [x29, #-104]          // Save callee-saved register x24
    str x25, [x29, #-112]          // Save callee-saved register x25
    str x26, [x29, #-120]          // Save callee-saved register x26
    str x27, [x29, #-128]          // Save callee-saved register x27
    sub sp, sp, #256               // Allocate stack frame (placeholder)
    movz x0, #0x10                 // Load number literal
    ldr x19, [x29, #-136]          // Load variable val1 into x19
    mov x19, x0                    // Initialize local val1 in x19
    movz x0, #0x5                  // Load number literal
    ldr x8, [x29, #-144]           // Load variable val2 into x8
    mov x8, x0                     // Initialize local val2 in x8
    adr x0, .L.str0                // Load string literal address
    bl writes                      // Call writes
    str x8, [x29, #-144]           // Spill val2 from x8 to stack
    str x19, [x29, #-136]          // Spill val1 from x19 to stack
    str x8, [x29, #-152]           // Save caller-saved register x8
    str x19, [x29, #-160]          // Save caller-saved register x19
    str x20, [x29, #-168]          // Save caller-saved register x20
    str x21, [x29, #-176]          // Save caller-saved register x21
    str x22, [x29, #-184]          // Save caller-saved register x22
    str x23, [x29, #-192]          // Save caller-saved register x23
    str x24, [x29, #-200]          // Save caller-saved register x24
    str x25, [x29, #-208]          // Save caller-saved register x25
    str x26, [x29, #-216]          // Save caller-saved register x26
    str x27, [x29, #-224]          // Save caller-saved register x27
    sub sp, sp, x24                // Allocate space for outgoing arguments
    movz x0, #0x2                  // Load number literal
    mov x15, x0                   
    ldr x7, [x29, #-136]           // Load variable val1 into x7
    mov x0, x7                     // Load val1 from stack to x7 and then to X0
    add x0, x0, x15                // Add
    str x0, [sp]                   // Store argument 0
    movz x0, #0x3                  // Load number literal
    mov x15, x0                   
    ldr x6, [x29, #-144]           // Load variable val2 into x6
    mov x0, x6                     // Load val2 from stack to x6 and then to X0
    mul x0, x0, x15                // Multiply
    str x0, [sp, #8]               // Store argument 1
    movz x0, #0x4                  // Load number literal
    str x0, [sp, #16]              // Store argument 2
    ldr x0, [sp]                   // Load parameter into register
    ldr x1, [sp, #8]               // Load parameter into register
    ldr x2, [sp, #16]              // Load parameter into register
    bl funcA                       // Call funcA
    add sp, sp, #24                // Deallocate outgoing arguments
    ldr x27, [x29, #-224]          // Restore caller-saved register x27
    ldr x26, [x29, #-216]          // Restore caller-saved register x26
    ldr x25, [x29, #-208]          // Restore caller-saved register x25
    ldr x24, [x29, #-200]          // Restore caller-saved register x24
    ldr x23, [x29, #-192]          // Restore caller-saved register x23
    ldr x22, [x29, #-184]          // Restore caller-saved register x22
    ldr x21, [x29, #-176]          // Restore caller-saved register x21
    ldr x20, [x29, #-168]          // Restore caller-saved register x20
    ldr x19, [x29, #-160]          // Restore caller-saved register x19
    ldr x8, [x29, #-152]           // Restore caller-saved register x8
    ldr x6, [x29, #-152]           // Load variable result into x6
    mov x6, x0                     // Initialize local result in x6
    sub sp, sp, x16                // Allocate space for WRITEF arguments
    mov x0, x6                     // Move result from x6 to X0
    str x0, [sp]                   // Store WRITEF argument 1
    adr x0, .L.str1                // Load string literal address
    str x0, [sp, #8]               // Store WRITEF argument 0
    ldr x0, [sp, #8]               // Load format string for WRITEF
    ldr x1, [sp]                   // Load first data arg for WRITEF
    bl writef                      // Call writef
    add sp, sp, #16                // Deallocate WRITEF arguments
    movz x0, #0x0                  // Load number literal
    b return_2                     // Branch to function return
return_2:
    str x6, [x29, #-152]           // Spill result from x6 to stack
    ldr x27, [x29, #-128]          // Restore callee-saved register x27
    ldr x26, [x29, #-120]          // Restore callee-saved register x26
    ldr x25, [x29, #-112]          // Restore callee-saved register x25
    ldr x24, [x29, #-104]          // Restore callee-saved register x24
    ldr x23, [x29, #-96]           // Restore callee-saved register x23
    ldr x22, [x29, #-88]           // Restore callee-saved register x22
    ldr x21, [x29, #-80]           // Restore callee-saved register x21
    ldr x20, [x29, #-72]           // Restore callee-saved register x20
    add sp, sp, #256               // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function

.data
.L.str0:
    .string "Calling funcA...
"
.L.str1:
    .string "Result from funcA is %i
"

;------------ End of Assembly ------------


Compilation successful.
