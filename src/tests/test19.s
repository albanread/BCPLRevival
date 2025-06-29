=== BCPL Compiler ===
Source file: ./tests/test19.b

Parsing...
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: LET
parseExpression: 10
parsePrimaryExpression: 10
parseStatement: LET
parseExpression: 20
parsePrimaryExpression: 20
parseStatement: LET
parseExpression: 10
parsePrimaryExpression: 10
parseStatement: IF
parseExpression: x
parsePrimaryExpression: x
parseExpression: y
parsePrimaryExpression: y
parseStatement: WRITES
parseExpressionStatement: WRITES
parseExpression: WRITES
parsePrimaryExpression: WRITES
parseExpression: x < y

parsePrimaryExpression: x < y

parseStatement: IF
parseExpression: x
parsePrimaryExpression: x
parseExpression: y
parsePrimaryExpression: y
parseStatement: WRITES
parseExpressionStatement: WRITES
parseExpression: WRITES
parsePrimaryExpression: WRITES
parseExpression: x > y

parsePrimaryExpression: x > y

parseStatement: IF
parseExpression: x
parsePrimaryExpression: x
parseExpression: z
parsePrimaryExpression: z
parseStatement: WRITES
parseExpressionStatement: WRITES
parseExpression: WRITES
parsePrimaryExpression: WRITES
parseExpression: x = z

parsePrimaryExpression: x = z

parseStatement: IF
parseExpression: x
parsePrimaryExpression: x
parseExpression: z
parsePrimaryExpression: z
parseStatement: WRITES
parseExpressionStatement: WRITES
parseExpression: WRITES
parsePrimaryExpression: WRITES
parseExpression: x ~= z

parsePrimaryExpression: x ~= z

parseStatement: IF
parseExpression: x
parsePrimaryExpression: x
parseExpression: z
parsePrimaryExpression: z
parseStatement: WRITES
parseExpressionStatement: WRITES
parseExpression: WRITES
parsePrimaryExpression: WRITES
parseExpression: x <= z

parsePrimaryExpression: x <= z

parseStatement: IF
parseExpression: x
parsePrimaryExpression: x
parseExpression: z
parsePrimaryExpression: z
parseStatement: WRITES
parseExpressionStatement: WRITES
parseExpression: WRITES
parsePrimaryExpression: WRITES
parseExpression: x >= z

parsePrimaryExpression: x >= z

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
    sub sp, sp, #48                // Allocate stack frame (placeholder)
    movz x0, #0x10                 // Load number literal
    ldr x27, [x29, #-8]            // Load variable x into x27
    mov x27, x0                    // Initialize local x in x27
    movz x0, #0x20                 // Load number literal
    ldr x26, [x29, #-16]           // Load variable y into x26
    mov x26, x0                    // Initialize local y in x26
    movz x0, #0x10                 // Load number literal
    ldr x25, [x29, #-24]           // Load variable z into x25
    mov x25, x0                    // Initialize local z in x25
    mov x0, x26                    // Move y from x26 to X0
    mov x15, x0                   
    mov x0, x27                    // Move x from x27 to X0
    cmp x0, x15                   
    cset x0, lt                   
    neg x0, x0                    
    cmp x0, x0                    
    b.eq if_end_1                 
    adr x0, .L.str0                // Load string literal address
    bl writes                      // Call writes
if_end_1:
    mov x0, x26                    // Move y from x26 to X0
    mov x15, x0                   
    mov x0, x27                    // Move x from x27 to X0
    cmp x0, x15                   
    cset x0, gt                   
    neg x0, x0                    
    cmp x0, x0                    
    b.eq if_end_2                 
    adr x0, .L.str1                // Load string literal address
    bl writes                      // Call writes
if_end_2:
    mov x0, x25                    // Move z from x25 to X0
    mov x15, x0                   
    mov x0, x27                    // Move x from x27 to X0
    cmp x0, x15                   
    cset x0, eq                   
    neg x0, x0                    
    cmp x0, x0                    
    b.eq if_end_3                 
    adr x0, .L.str2                // Load string literal address
    bl writes                      // Call writes
if_end_3:
    mov x0, x25                    // Move z from x25 to X0
    mov x15, x0                   
    mov x0, x27                    // Move x from x27 to X0
    cmp x0, x15                   
    cset x0, ne                   
    neg x0, x0                    
    cmp x0, x0                    
    b.eq if_end_4                 
    adr x0, .L.str3                // Load string literal address
    bl writes                      // Call writes
if_end_4:
    mov x0, x25                    // Move z from x25 to X0
    mov x15, x0                   
    mov x0, x27                    // Move x from x27 to X0
    cmp x0, x15                   
    cset x0, le                   
    neg x0, x0                    
    cmp x0, x0                    
    b.eq if_end_5                 
    adr x0, .L.str4                // Load string literal address
    bl writes                      // Call writes
if_end_5:
    mov x0, x25                    // Move z from x25 to X0
    mov x15, x0                   
    mov x0, x27                    // Move x from x27 to X0
    cmp x0, x15                   
    cset x0, ge                   
    neg x0, x0                    
    cmp x0, x0                    
    b.eq if_end_6                 
    adr x0, .L.str5                // Load string literal address
    bl writes                      // Call writes
if_end_6:
    movz x0, #0x0                  // Load number literal
    b return_0                     // Branch to function return
return_0:
    str x25, [x29, #-24]           // Spill z from x25 to stack
    str x26, [x29, #-16]           // Spill y from x26 to stack
    str x27, [x29, #-8]            // Spill x from x27 to stack
    add sp, sp, #48                // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function

.data
.L.str0:
    .string "x < y
"
.L.str1:
    .string "x > y
"
.L.str2:
    .string "x = z
"
.L.str3:
    .string "x ~= z
"
.L.str4:
    .string "x <= z
"
.L.str5:
    .string "x >= z
"

;------------ End of Assembly ------------


Compilation successful.
