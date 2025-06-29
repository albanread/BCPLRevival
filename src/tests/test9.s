=== BCPL Compiler ===
Source file: ./tests/test9.b

Parsing...
parseExpression: VALOF
parsePrimaryExpression: VALOF
parseStatement: $(
parseStatement: LET
parseExpression: 2
parsePrimaryExpression: 2
parseStatement: LET
parseExpression: 0
parsePrimaryExpression: 0
parseStatement: SWITCHON
parseExpression: x
parsePrimaryExpression: x
parseStatement: y
parseExpressionStatement: y
parseExpression: y
parsePrimaryExpression: y
parseExpression: 10
parsePrimaryExpression: 10
parseStatement: y
parseExpressionStatement: y
parseExpression: y
parsePrimaryExpression: y
parseExpression: 20
parsePrimaryExpression: 20
parseStatement: y
parseExpressionStatement: y
parseExpression: y
parsePrimaryExpression: y
parseExpression: 30
parsePrimaryExpression: 30
parseStatement: y
parseExpressionStatement: y
parseExpression: y
parsePrimaryExpression: y
parseExpression: -
parsePrimaryExpression: -
parseExpression: 1
parsePrimaryExpression: 1
parseStatement: RESULTIS
parseExpression: y
parsePrimaryExpression: y
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
    movz x0, #0x2                  // Load number literal
    ldr x27, [x29, #-8]            // Load variable x into x27
    mov x27, x0                    // Initialize local x in x27
    movz x0, #0x0                  // Load number literal
    ldr x26, [x29, #-16]           // Load variable y into x26
    mov x26, x0                    // Initialize local y in x26
    mov x0, x27                    // Move x from x27 to X0
    str x0, [x29, #-24]            // Store switch value
    ldr x0, [x29, #-24]           
    movz x1, #0x2                  // Loading 2 into x1
    cmp x0, x1                    
    b.lt case_lt_4                
    b.gt case_gt_5                
    b case_1                      
case_lt_4:
    movz x1, #0x1                  // Loading 1 into x1
    cmp x0, x1                    
    b.lt case_lt_6                
    b.gt case_gt_7                
    b case_0                      
case_lt_6:
    b switch_default_3            
case_gt_7:
    b switch_default_3            
case_gt_5:
    movz x1, #0x3                  // Loading 3 into x1
    cmp x0, x1                    
    b.lt case_lt_8                
    b.gt case_gt_9                
    b case_2                      
case_lt_8:
    b switch_default_3            
case_gt_9:
    b switch_default_3            
case_0:
    movz x0, #0x10                 // Load number literal
    mov x26, x0                    // Assign value to y in x26
    b switch_end_1                
case_1:
    movz x0, #0x20                 // Load number literal
    mov x26, x0                    // Assign value to y in x26
    b switch_end_1                
case_2:
    movz x0, #0x30                 // Load number literal
    mov x26, x0                    // Assign value to y in x26
    b switch_end_1                
switch_default_3:
    movz x0, #0x1                  // Load number literal
    neg x0, x0                     // Arithmetic negation
    mov x26, x0                    // Assign value to y in x26
switch_end_1:
    mov x0, x26                    // Move y from x26 to X0
    b return_0                     // Branch to function return
return_0:
    str x26, [x29, #-16]           // Spill y from x26 to stack
    str x27, [x29, #-8]            // Spill x from x27 to stack
    add sp, sp, #48                // Deallocate stack frame
    ldp x29, x30, [sp, #16]        // Restore FP/LR
    ret                            // Return from function

;------------ End of Assembly ------------


Compilation successful.
