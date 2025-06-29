=== BCPL Compiler ===
Source file: ./tests/test21.b

Parsing...
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: LET
parseExpression: h
parsePrimaryExpression: h
parseExpression: i
parsePrimaryExpression: i
parseStatement: LET
parseExpression: g
parsePrimaryExpression: g
parseExpression: temp1
parsePrimaryExpression: temp1
parseStatement: LET
parseExpression: f
parsePrimaryExpression: f
parseExpression: temp2
parsePrimaryExpression: temp2
parseStatement: LET
parseExpression: e
parsePrimaryExpression: e
parseExpression: temp3
parsePrimaryExpression: temp3
parseStatement: LET
parseExpression: d
parsePrimaryExpression: d
parseExpression: temp4
parsePrimaryExpression: temp4
parseStatement: LET
parseExpression: c
parsePrimaryExpression: c
parseExpression: temp5
parsePrimaryExpression: temp5
parseStatement: LET
parseExpression: b
parsePrimaryExpression: b
parseExpression: temp6
parsePrimaryExpression: temp6
parseStatement: LET
parseExpression: a
parsePrimaryExpression: a
parseExpression: temp7
parsePrimaryExpression: temp7
parseStatement: RESULTIS
parseExpression: result
parsePrimaryExpression: result
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: WRITES
parseExpressionStatement: WRITES
parseExpression: WRITES
parsePrimaryExpression: WRITES
parseExpression: Testing scratch register allocation spilling into callee-saved registers...

parsePrimaryExpression: Testing scratch register allocation spilling into callee-saved registers...

parseStatement: LET
parseExpression: complex_calculation
parsePrimaryExpression: complex_calculation
parseExpression: 100
parsePrimaryExpression: 100
parseExpression: 90
parsePrimaryExpression: 90
parseExpression: 8
parsePrimaryExpression: 8
parseExpression: 700
parsePrimaryExpression: 700
parseExpression: 6
parsePrimaryExpression: 6
parseExpression: 50
parsePrimaryExpression: 50
parseExpression: 4
parsePrimaryExpression: 4
parseExpression: 30
parsePrimaryExpression: 30
parseExpression: 3
parsePrimaryExpression: 3
parseStatement: WRITEF
parseExpressionStatement: WRITEF
parseExpression: WRITEF
parsePrimaryExpression: WRITEF
parseExpression: Result of complex calculation is %i

parsePrimaryExpression: Result of complex calculation is %i

parseExpression: r
parsePrimaryExpression: r
parseStatement: RESULTIS
parseExpression: 0
parsePrimaryExpression: 0
Parsing complete.

Generating code...
Visiting function declaration: complex_calculation
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

complex_calculation:
    stp x29, x30, [sp, #-16]       // Save FP/LR
    mov x29, sp                    // Set up frame pointer
    sub sp, sp, #160               // Allocate stack frame (placeholder)
    str x0, [x29, #-8]             // Store parameter a to stack home
    ldr x27, [x29, #-8]            // Load variable a into x27
    str x1, [x29, #-16]            // Store parameter b to stack home
    ldr x26, [x29, #-16]           // Load variable b into x26
    str x2, [x29, #-24]            // Store parameter c to stack home
    ldr x25, [x29, #-24]           // Load variable c into x25
    str x3, [x29, #-32]            // Store parameter d to stack home
    ldr x24, [x29, #-32]           // Load variable d into x24
    str x4, [x29, #-40]            // Store parameter e to stack home
    ldr x23, [x29, #-40]           // Load variable e into x23
    str x5, [x29, #-48]            // Store parameter f to stack home
    ldr x22, [x29, #-48]           // Load variable f into x22
    str x6, [x29, #-56]            // Store parameter g to stack home
    ldr x21, [x29, #-56]           // Load variable g into x21
    str x7, [x29, #-64]            // Store parameter h to stack home
    ldr x20, [x29, #-64]           // Load variable h into x20
    str x8, [x29, #-72]            // Store parameter i to stack home
    ldr x19, [x29, #-72]           // Load variable i into x19
    mov x0, x19                    // Move i from x19 to X0
    mov x15, x0                   
    mov x0, x20                    // Move h from x20 to X0
    sdiv x0, x0, x15               // Divide
    ldr x8, [x29, #-80]            // Load variable temp1 into x8
    mov x8, x0                     // Initialize local temp1 in x8
    mov x0, x8                     // Move temp1 from x8 to X0
    mov x15, x0                   
    mov x0, x21                    // Move g from x21 to X0
    mul x0, x0, x15                // Multiply
    ldr x7, [x29, #-88]            // Load variable temp2 into x7
    mov x7, x0                     // Initialize local temp2 in x7
    mov x0, x7                     // Move temp2 from x7 to X0
    mov x15, x0                   
    mov x0, x22                    // Move f from x22 to X0
    sub x0, x0, x15                // Subtract
    ldr x6, [x29, #-96]            // Load variable temp3 into x6
    mov x6, x0                     // Initialize local temp3 in x6
    mov x0, x6                     // Move temp3 from x6 to X0
    mov x15, x0                   
    mov x0, x23                    // Move e from x23 to X0
    add x0, x0, x15                // Add
    ldr x5, [x29, #-104]           // Load variable temp4 into x5
    mov x5, x0                     // Initialize local temp4 in x5
    mov x0, x5                     // Move temp4 from x5 to X0
    mov x15, x0                   
    mov x0, x24                    // Move d from x24 to X0
    sdiv x0, x0, x15               // Divide
    ldr x4, [x29, #-112]           // Load variable temp5 into x4
    mov x4, x0                     // Initialize local temp5 in x4
    mov x0, x4                     // Move temp5 from x4 to X0
    mov x15, x0                   
    mov x0, x25                    // Move c from x25 to X0
    mul x0, x0, x15                // Multiply
    ldr x3, [x29, #-120]           // Load variable temp6 into x3
    mov x3, x0                     // Initialize local temp6 in x3
    mov x0, x3                     // Move temp6 from x3 to X0
    mov x15, x0                   
    mov x0, x26                    // Move b from x26 to X0
    sub x0, x0, x15                // Subtract
    str x3, [x29, #-120]           // Spill temp6 from x3 to stack
    ldr x3, [x29, #-128]           // Load variable temp7 into x3
    mov x3, x0                     // Initialize local temp7 in x3
    mov x0, x3                     // Move temp7 from x3 to X0
    mov x15, x0                   
    mov x0, x27                    // Move a from x27 to X0
    add x0, x0, x15                // Add
    str x3, [x29, #-128]           // Spill temp7 from x3 to stack
    ldr x3, [x29, #-136]           // Load variable result into x3
    mov x3, x0                     // Initialize local result in x3
    mov x0, x3                     // Move result from x3 to X0
    b return_0                     // Branch to function return
return_0:
    str x3, [x29, #-136]           // Spill result from x3 to stack
    str x4, [x29, #-112]           // Spill temp5 from x4 to stack
    str x5, [x29, #-104]           // Spill temp4 from x5 to stack
    str x6, [x29, #-96]            // Spill temp3 from x6 to stack
    str x7, [x29, #-88]            // Spill temp2 from x7 to stack
    str x8, [x29, #-80]            // Spill temp1 from x8 to stack
    str x19, [x29, #-72]           // Spill i from x19 to stack
    str x20, [x29, #-64]           // Spill h from x20 to stack
    str x21, [x29, #-56]           // Spill g from x21 to stack
    str x22, [x29, #-48]           // Spill f from x22 to stack
    str x23, [x29, #-40]           // Spill e from x23 to stack
    str x24, [x29, #-32]           // Spill d from x24 to stack
    str x25, [x29, #-24]           // Spill c from x25 to stack
    str x26, [x29, #-16]           // Spill b from x26 to stack
    str x27, [x29, #-8]            // Spill a from x27 to stack
    add sp, sp, #160               // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function
START:
    stp x29, x30, [sp, #-16]       // Save FP/LR
    mov x29, sp                    // Set up frame pointer
    str x19, [x29, #-144]          // Save callee-saved register x19
    str x20, [x29, #-152]          // Save callee-saved register x20
    str x21, [x29, #-160]          // Save callee-saved register x21
    str x22, [x29, #-168]          // Save callee-saved register x22
    str x23, [x29, #-176]          // Save callee-saved register x23
    str x24, [x29, #-184]          // Save callee-saved register x24
    str x25, [x29, #-192]          // Save callee-saved register x25
    str x26, [x29, #-200]          // Save callee-saved register x26
    str x27, [x29, #-208]          // Save callee-saved register x27
    sub sp, sp, #384               // Allocate stack frame (placeholder)
    adr x0, .L.str0                // Load string literal address
    bl writes                      // Call writes
    str x8, [x29, #-216]           // Save caller-saved register x8
    str x19, [x29, #-224]          // Save caller-saved register x19
    str x20, [x29, #-232]          // Save caller-saved register x20
    str x21, [x29, #-240]          // Save caller-saved register x21
    str x22, [x29, #-248]          // Save caller-saved register x22
    str x23, [x29, #-256]          // Save caller-saved register x23
    str x24, [x29, #-264]          // Save caller-saved register x24
    str x25, [x29, #-272]          // Save caller-saved register x25
    str x26, [x29, #-280]          // Save caller-saved register x26
    str x27, [x29, #-288]          // Save caller-saved register x27
    sub sp, sp, unknown            // Allocate space for outgoing arguments
    movz x0, #0x100                // Load number literal
    str x0, [sp]                   // Store argument 0
    movz x0, #0x90                 // Load number literal
    str x0, [sp, #8]               // Store argument 1
    movz x0, #0x8                  // Load number literal
    str x0, [sp, #16]              // Store argument 2
    movz x0, #0x700                // Load number literal
    str x0, [sp, #24]              // Store argument 3
    movz x0, #0x6                  // Load number literal
    str x0, [sp, #32]              // Store argument 4
    movz x0, #0x50                 // Load number literal
    str x0, [sp, #40]              // Store argument 5
    movz x0, #0x4                  // Load number literal
    str x0, [sp, #48]              // Store argument 6
    movz x0, #0x30                 // Load number literal
    str x0, [sp, #56]              // Store argument 7
    movz x0, #0x3                  // Load number literal
    str x0, [sp, #64]              // Store argument 8
    ldr x0, [sp]                   // Load parameter into register
    ldr x1, [sp, #8]               // Load parameter into register
    ldr x2, [sp, #16]              // Load parameter into register
    ldr x3, [sp, #24]              // Load parameter into register
    ldr x4, [sp, #32]              // Load parameter into register
    ldr x5, [sp, #40]              // Load parameter into register
    ldr x6, [sp, #48]              // Load parameter into register
    ldr x7, [sp, #56]              // Load parameter into register
    bl complex_calculation         // Call complex_calculation
    add sp, sp, #72                // Deallocate outgoing arguments
    ldr x27, [x29, #-288]          // Restore caller-saved register x27
    ldr x26, [x29, #-280]          // Restore caller-saved register x26
    ldr x25, [x29, #-272]          // Restore caller-saved register x25
    ldr x24, [x29, #-264]          // Restore caller-saved register x24
    ldr x23, [x29, #-256]          // Restore caller-saved register x23
    ldr x22, [x29, #-248]          // Restore caller-saved register x22
    ldr x21, [x29, #-240]          // Restore caller-saved register x21
    ldr x20, [x29, #-232]          // Restore caller-saved register x20
    ldr x19, [x29, #-224]          // Restore caller-saved register x19
    ldr x8, [x29, #-216]           // Restore caller-saved register x8
    str x3, [x29, #-136]           // Spill result from x3 to stack
    ldr x3, [x29, #-216]           // Load variable r into x3
    mov x3, x0                     // Initialize local r in x3
    sub sp, sp, x16                // Allocate space for WRITEF arguments
    mov x0, x3                     // Move r from x3 to X0
    str x0, [sp]                   // Store WRITEF argument 1
    adr x0, .L.str1                // Load string literal address
    str x0, [sp, #8]               // Store WRITEF argument 0
    ldr x0, [sp, #8]               // Load format string for WRITEF
    ldr x1, [sp]                   // Load first data arg for WRITEF
    bl writef                      // Call writef
    add sp, sp, #16                // Deallocate WRITEF arguments
    movz x0, #0x0                  // Load number literal
    b return_1                     // Branch to function return
return_1:
    str x3, [x29, #-216]           // Spill r from x3 to stack
    ldr x27, [x29, #-208]          // Restore callee-saved register x27
    ldr x26, [x29, #-200]          // Restore callee-saved register x26
    ldr x25, [x29, #-192]          // Restore callee-saved register x25
    ldr x24, [x29, #-184]          // Restore callee-saved register x24
    ldr x23, [x29, #-176]          // Restore callee-saved register x23
    ldr x22, [x29, #-168]          // Restore callee-saved register x22
    ldr x21, [x29, #-160]          // Restore callee-saved register x21
    ldr x20, [x29, #-152]          // Restore callee-saved register x20
    ldr x19, [x29, #-144]          // Restore callee-saved register x19
    add sp, sp, #384               // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function

.data
.L.str0:
    .string "Testing scratch register allocation spilling into callee-saved registers...
"
.L.str1:
    .string "Result of complex calculation is %i
"

;------------ End of Assembly ------------


Compilation successful.
