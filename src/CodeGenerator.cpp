#include "CodeGenerator.h"
#include "AST.h"
#include "StringAccess.h"
#include "VectorAllocationVisitor.h"
#include "JitRuntime.h"
#include <stdexcept>
#include <iostream>
#include <iomanip>
#include <cassert>
#include <algorithm> // For std::min

CodeGenerator::CodeGenerator() : instructions(), labelManager(), scratchAllocator(), registerManager(instructions), currentLocalVarOffset(0), maxOutgoingParamSpace(0), maxCallerSavedRegsSpace(0) {
    // Initialize callee-saved registers (x19-x28)
    for (uint32_t i = 19; i <= 28; ++i) {
        calleeSavedRegs.push_back(i);
    }
}

uintptr_t CodeGenerator::compile(ProgramPtr program) {
    // Reset state for a new compilation
    instructions.clear();
    localVars.clear();
    functions.clear();
    globals.clear();
    currentLocalVarOffset = 0;
    maxOutgoingParamSpace = 0;
    maxCallerSavedRegsSpace = 0;
    savedCalleeRegsInPrologue.clear();
    assemblyListing.str("");
    pendingCases.clear();
    registerManager.clear(); // Clear register manager state

    // Register runtime functions
    for (const auto& symbol : JitRuntime::getInstance().getSymbolTable()) {
        functions[symbol.first] = symbol.second;
    }

    // Generate code
    visitProgram(program.get());

    // Find entry point (START function)
    auto it = functions.find("START");
    if (it == functions.end()) {
        throw std::runtime_error("No START function found");
    }
    return it->second;
}

void CodeGenerator::visitProgram(const Program* node) {
    // First pass: collect all global declarations
    for (const auto& decl : node->declarations) {
        if (auto funcDecl = dynamic_cast<const FunctionDeclaration*>(decl.get())) {
            functions[funcDecl->name] = instructions.getCurrentAddress();
        }
    }

    // Second pass: generate code for declarations
    for (const auto& decl : node->declarations) {
        if (auto funcDecl = dynamic_cast<const FunctionDeclaration*>(decl.get())) {
            visitFunctionDeclaration(funcDecl);
        } else if (auto letDecl = dynamic_cast<const LetDeclaration*>(decl.get())) {
            visitLetDeclaration(letDecl);
        } else if (auto globalDecl = dynamic_cast<const GlobalDeclaration*>(decl.get())) {
            visitGlobalDeclaration(globalDecl);
        } else if (auto manifestDecl = dynamic_cast<const ManifestDeclaration*>(decl.get())) {
            visitManifestDeclaration(manifestDecl);
        } else if (auto valof = dynamic_cast<const Valof*>(decl.get())) {
            visitValof(valof);
        }
    }
}

void CodeGenerator::visitManifestDeclaration(const ManifestDeclaration* node) {
    for (const auto& manifest : node->manifests) {
        manifests[manifest.name] = manifest.value;
    }
}

void CodeGenerator::visitGlobalDeclaration(const GlobalDeclaration* node) {
    for (const auto& global : node->globals) {
        globals[global.name] = globals.size();
    }
}

#include <iostream>

void CodeGenerator::visitFunctionDeclaration(const FunctionDeclaration* node) {
    std::cout << "Visiting function declaration: " << node->name << std::endl;
    labelManager.pushScope(LabelManager::ScopeType::FUNCTION);
    auto returnLabel = labelManager.getCurrentReturnLabel();
    std::cout << "Generated return label: " << returnLabel << std::endl;

    // First pass to collect vector allocations
    VectorAllocationVisitor vecVisitor;
    vecVisitor.visit(node);
    vectorAllocations = vecVisitor.allocations;

    // Generate function label and record position
    instructions.setPendingLabel(node->name);
    labelManager.defineLabel(node->name, instructions.getCurrentAddress());
    addToListing(node->name + ":", "Function entry point");

    // PROLOGUE: Save FP and LR, set up FP, and allocate stack frame (placeholder)
    instructions.stp(X29, X30, SP, -16, "Save FP/LR");
    addToListing("stp x29, x30, [sp, #-16]!", "Save FP/LR");
    
    instructions.mov(X29, SP, "Set up frame pointer");
    addToListing("mov x29, sp", "Set up frame pointer");

    // Save callee-saved registers
    saveCalleeSavedRegisters();

    // Placeholder for stack frame allocation. This instruction will be back-patched.
    size_t prologueSubInstructionIndex = instructions.size();
    instructions.sub(SP, SP, 0, "Allocate stack frame (placeholder)");
    addToListing("sub sp, sp, #0");

    // Allocate space for parameters on the stack and potentially load them into registers
    for (size_t i = 0; i < node->params.size(); i++) {
        int offset = allocateLocal(node->params[i]); // Allocate stack space
        // For now, parameters are always loaded into X0, X1, X2...
        // The RegisterManager will handle keeping them in registers if needed later.
        // We still need to store them to their stack home for consistency and potential spills.
        instructions.str(X0 + i, X29, offset, "Store parameter " + node->params[i] + " to stack home");
        // Also, inform RegisterManager that this variable is now in a register (X0+i)
        // and its home is at 'offset'. Mark it dirty as it's been "initialized" in the register.
        registerManager.acquireRegister(node->params[i], offset);
        registerManager.markDirty(node->params[i]);
    }

    // Visit function body
    if (node->body_expr) {
        if (auto valof = dynamic_cast<const Valof*>(node->body_expr.get())) {
            visitStatement(valof->body.get());
        } else {
            visitExpression(node->body_expr.get());
        }
    } else if (node->body_stmt) {
        visitStatement(node->body_stmt.get());
    }

    // Define the return label here, before the epilogue.
    instructions.setPendingLabel(returnLabel);
    labelManager.defineLabel(returnLabel, instructions.getCurrentAddress());

    // Spill all dirty registers before function exit
    registerManager.spillAllDirtyRegisters();

    // Calculate total additional stack space needed (beyond the initial 16 bytes for FP/LR).
    // currentLocalVarOffset is negative, so -currentLocalVarOffset gives positive size.
    size_t locals_and_caller_saved_space = (-currentLocalVarOffset); 
    
    size_t total_frame_size = 16; // For FP and LR
    total_frame_size += locals_and_caller_saved_space; // Includes locals and caller-saved spills
    total_frame_size += savedCalleeRegsInPrologue.size() * 8; // Space for callee-saved registers
    total_frame_size += maxOutgoingParamSpace; // Max space for outgoing parameters

    // Add space for vector allocations
    for (const auto& vec : vectorAllocations) {
        // This is not quite right, as the size can be an expression.
        // For now, we'll assume it's a number literal.
        if (auto size = dynamic_cast<const NumberLiteral*>(vec->size.get())) {
            total_frame_size += (size->value + 1) * 8;
        }
    }


    // Align to 16 bytes
    size_t aligned_total_frame_size = (total_frame_size + 15) & ~15;

    // Back-patch the prologue with the correct frame size
    if (aligned_total_frame_size > 0) {
        instructions.at(prologueSubInstructionIndex).encoding |= (aligned_total_frame_size << 10);
        instructions.at(prologueSubInstructionIndex).assembly = "sub sp, sp, #" + std::to_string(aligned_total_frame_size);
    } else {
        // If no additional stack space is needed, remove the sub instruction
        instructions.getInstructions().erase(instructions.getInstructions().begin() + prologueSubInstructionIndex);
    }

    // EPILOGUE: Restore stack pointer, FP, LR, and return
    // Restore callee-saved registers
    restoreCalleeSavedRegisters();

    if (aligned_total_frame_size > 0) {
        instructions.add(SP, SP, aligned_total_frame_size, "Deallocate stack frame");
        addToListing("add sp, sp, #" + std::to_string(aligned_total_frame_size));
    }

    instructions.ldp(X29, X30, SP, 16, "Restore FP/LR");
    addToListing("ldp x29, x30, [sp], #16", "Restore FP/LR");

    instructions.ret("Return from function");
    addToListing("ret", "Return from function");

    labelManager.popScope();
    vectorAllocations.clear();
}

void CodeGenerator::resolveLabels() {
    auto fixups = labelManager.getFixups();
    for (const auto& fixup : fixups) {
        size_t targetAddress = labelManager.getLabelAddress(fixup.labelName);
        int32_t offset = static_cast<int32_t>(targetAddress - fixup.instructionAddress);
        instructions.resolveBranch(fixup.instructionAddress / 4, offset);
    }
}

// CodeGenerator.cpp (continued)

void CodeGenerator::visitExpression(const Expression* expr) {
    if (auto* numLit = dynamic_cast<const NumberLiteral*>(expr)) {
        visitNumberLiteral(numLit);
    } else if (auto* strLit = dynamic_cast<const StringLiteral*>(expr)) {
        visitStringLiteral(strLit);
    } else if (auto* charLit = dynamic_cast<const CharLiteral*>(expr)) {
        visitCharLiteral(charLit);
    } else if (auto* var = dynamic_cast<const VariableAccess*>(expr)) {
        visitVariableAccess(var);
    } else if (auto* unary = dynamic_cast<const UnaryOp*>(expr)) {
        visitUnaryOp(unary);
    } else if (auto* binary = dynamic_cast<const BinaryOp*>(expr)) {
        visitBinaryOp(binary);
    } else if (auto* call = dynamic_cast<const FunctionCall*>(expr)) {
        visitFunctionCall(call);
    } else if (auto* cond = dynamic_cast<const ConditionalExpression*>(expr)) {
        visitConditionalExpression(cond);
    } else if (auto* valof = dynamic_cast<const Valof*>(expr)) {
        visitValof(valof);
    } else if (auto* table = dynamic_cast<const TableConstructor*>(expr)) {
        visitTableConstructor(table);
    } else if (auto* vec = dynamic_cast<const VectorConstructor*>(expr)) {
        visitVectorConstructor(vec);
    } else if (auto* charAccess = dynamic_cast<const CharacterAccess*>(expr)) {
        visitCharacterAccess(charAccess);
    }
}

void CodeGenerator::visitNumberLiteral(const NumberLiteral* node) {
    instructions.loadImmediate(X0, node->value, "Load number literal");
}

void CodeGenerator::visitCharLiteral(const CharLiteral* node) {
    instructions.loadImmediate(X0, node->value, "Load char literal");
}

void CodeGenerator::visitStringLiteral(const StringLiteral* node) {
    std::string label = ".L.str" + std::to_string(stringPool.size());
    stringPool.push_back(node->value);
    instructions.adr(X0, label, "Load string literal address");
}

void CodeGenerator::visitVariableAccess(const VariableAccess* node) {
    if (auto it = manifests.find(node->name); it != manifests.end()) {
        instructions.loadImmediate(X0, it->second, "Load manifest constant " + node->name);
    } else if (auto it = globals.find(node->name); it != globals.end()) {
        // Load from global vector
        instructions.ldr(X0, X28, it->second * 8, "Load global " + node->name);
    } else {
        // Local variable: try to get from register, otherwise load from stack
        uint32_t reg = registerManager.getVariableRegister(node->name);
        if (reg != 0xFFFFFFFF) { // Variable is already in a register
            instructions.mov(X0, reg, "Move " + node->name + " from " + instructions.regName(reg) + " to X0");
        } else {
            // Variable not in a register, acquire one and load it
            int offset = getLocalOffset(node->name);
            reg = registerManager.acquireRegister(node->name, offset);
            instructions.mov(X0, reg, "Load " + node->name + " from stack to " + instructions.regName(reg) + " and then to X0");
        }
    }
}

void CodeGenerator::visitUnaryOp(const UnaryOp* node) {
    visitExpression(node->rhs.get());

    switch (node->op) {
        case TokenType::OpLogNot:
            instructions.eor(X0, X0, 1, "Logical NOT");
            break;
        case TokenType::OpMinus:
            instructions.neg(X0, X0, "Arithmetic negation");
            break;
        case TokenType::OpAt:  // @ operator (address-of)
            // For variables, calculate address instead of loading value
            if (auto var = dynamic_cast<const VariableAccess*>(node->rhs.get())) {
                if (auto it = globals.find(var->name); it != globals.end()) {
                    instructions.add(X0, X28, it->second * 8, AArch64Instructions::LSL, 0, "Address of global " + var->name);
                } else {
                    int offset = getLocalOffset(var->name);
                    instructions.add(X0, X29, offset, AArch64Instructions::LSL, 0, "Address of local " + var->name);
                }
            } else {
                throw std::runtime_error("@ operator requires addressable operand");
            }
            break;
        case TokenType::OpBang:  // ! operator (indirection)
            instructions.ldr(X0, X0, 0, "Indirection");
            break;
        default:
            throw std::runtime_error("Unknown unary operator");
    }
}

void CodeGenerator::visitBinaryOp(const BinaryOp* node) {
    // Handle special cases first
    if (node->op == TokenType::OpBang) {  // Vector subscript (V!E)
        // Evaluate index first
        visitExpression(node->right.get());
        // Save index
        uint32_t indexReg = scratchAllocator.acquire();
        instructions.mov(indexReg, X0);

        // Evaluate vector address
        visitExpression(node->left.get());

        // Calculate final address and load
        instructions.add(X0, X0, indexReg, AArch64Instructions::LSL, 3, "Calculate element address (word size)"); // Fixed: LSL 3 for 8-byte words
        instructions.ldr(X0, X0, 0, "Load vector element");
        scratchAllocator.release(indexReg);
        return;
    }

    // Normal binary operators
    visitExpression(node->right.get());
    uint32_t rightReg = scratchAllocator.acquire();
    instructions.mov(rightReg, X0);

    visitExpression(node->left.get());
    
    switch (node->op) {
        case TokenType::OpPlus:
            instructions.add(X0, X0, rightReg, AArch64Instructions::LSL, 0, "Add");
            break;
        case TokenType::OpMinus:
            instructions.sub(X0, X0, rightReg, "Subtract");
            break;
        case TokenType::OpMultiply:
            instructions.mul(X0, X0, rightReg, "Multiply");
            break;
        case TokenType::OpDivide:
            // TODO: Add division by zero check
            instructions.sdiv(X0, X0, rightReg, "Divide");
            break;
        case TokenType::OpRemainder:
            // TODO: Add division by zero check
            instructions.sdiv(X2, X0, rightReg, "Divide for remainder");
            instructions.msub(X0, X2, rightReg, X0, "Calculate remainder");
            break;
        case TokenType::OpEq:
            instructions.cmp(X0, rightReg);
            instructions.cset(X0, AArch64Instructions::EQ);
            instructions.neg(X0, X0);
            break;
        case TokenType::OpNe:
            instructions.cmp(X0, rightReg);
            instructions.cset(X0, AArch64Instructions::NE);
            instructions.neg(X0, X0);
            break;
        case TokenType::OpLt:
            instructions.cmp(X0, rightReg);
            instructions.cset(X0, AArch64Instructions::LT);
            instructions.neg(X0, X0);
            break;
        case TokenType::OpGt:
            instructions.cmp(X0, rightReg);
            instructions.cset(X0, AArch64Instructions::GT);
            instructions.neg(X0, X0);
            break;
        case TokenType::OpLe:
            instructions.cmp(X0, rightReg);
            instructions.cset(X0, AArch64Instructions::LE);
            instructions.neg(X0, X0);
            break;
        case TokenType::OpGe:
            instructions.cmp(X0, rightReg);
            instructions.cset(X0, AArch64Instructions::GE);
            instructions.neg(X0, X0);
            break;
        // Add other operators (AND, OR, etc.)
        default:
            throw std::runtime_error("Unknown binary operator");
    }
    scratchAllocator.release(rightReg);
}

// CodeGenerator.cpp (continued)

void CodeGenerator::visitCompoundStatement(const CompoundStatement* node) {
    // Simply visit each statement in the block.
    for (const auto& stmt : node->statements) {
        visitStatement(stmt.get());
    }
}

void CodeGenerator::visitStatement(const Statement* stmt) {
    if (auto compound = dynamic_cast<const CompoundStatement*>(stmt)) {
        visitCompoundStatement(compound);
    } else if (auto letDecl = dynamic_cast<const LetDeclaration*>(stmt)) {
        visitLetDeclaration(letDecl);
    } else if (auto ifStmt = dynamic_cast<const IfStatement*>(stmt)) {
        visitIfStatement(ifStmt);
    } else if (auto testStmt = dynamic_cast<const TestStatement*>(stmt)) {
        visitTestStatement(testStmt);
    } else if (auto whileStmt = dynamic_cast<const WhileStatement*>(stmt)) {
        visitWhileStatement(whileStmt);
    } else if (auto switchStmt = dynamic_cast<const SwitchonStatement*>(stmt)) {
        visitSwitchonStatement(switchStmt);
    } else if (auto forStmt = dynamic_cast<const ForStatement*>(stmt)) {
        visitForStatement(forStmt);
    } else if (auto gotoStmt = dynamic_cast<const GotoStatement*>(stmt)) {
        visitGotoStatement(gotoStmt);
    } else if (auto labeledStmt = dynamic_cast<const LabeledStatement*>(stmt)) {
        visitLabeledStatement(labeledStmt);
    } else if (auto assign = dynamic_cast<const Assignment*>(stmt)) {
        visitAssignment(assign);
    } else if (auto routine = dynamic_cast<const RoutineCall*>(stmt)) {
        visitRoutineCall(routine);
    } else if (auto resultis = dynamic_cast<const ResultisStatement*>(stmt)) {
        visitResultisStatement(resultis);
    } else if (auto breakStmt = dynamic_cast<const BreakStatement*>(stmt)) {
        visitBreakStatement(breakStmt);
    } else if (auto returnStmt = dynamic_cast<const ReturnStatement*>(stmt)) {
        visitReturnStatement(returnStmt);
    } else if (auto loopStmt = dynamic_cast<const LoopStatement*>(stmt)) {
        visitLoopStatement(loopStmt);
    } else if (auto repeatStmt = dynamic_cast<const RepeatStatement*>(stmt)) {
        visitRepeatStatement(repeatStmt);
    } else if (auto endcaseStmt = dynamic_cast<const EndcaseStatement*>(stmt)) {
        visitEndcaseStatement(endcaseStmt);
    } else if (auto finishStmt = dynamic_cast<const FinishStatement*>(stmt)) {
        visitFinishStatement(finishStmt);
    }
}

void CodeGenerator::visitRepeatStatement(const RepeatStatement* node) {
    labelManager.pushScope(LabelManager::ScopeType::LOOP);
    auto startLabel = labelManager.getCurrentRepeatLabel();
    auto endLabel = labelManager.getCurrentEndLabel(); // Added for correct UNTIL logic

    instructions.setPendingLabel(startLabel);
    labelManager.defineLabel(startLabel, instructions.getCurrentAddress());
    visitStatement(node->body.get());
    visitExpression(node->condition.get());
    // REPEAT UNTIL E means loop while E is FALSE. So, branch to start if E is 0 (false).
    labelManager.requestLabelFixup(startLabel, instructions.getCurrentAddress());
    instructions.beq(startLabel); // Fixed: Branch if E is 0 (false) to repeat

    // Define the end label for BREAK statements or when condition is true
    instructions.setPendingLabel(endLabel);
    labelManager.defineLabel(endLabel, instructions.getCurrentAddress());

    labelManager.popScope();
}

void CodeGenerator::visitIfStatement(const IfStatement* node) {
    auto skipLabel = labelManager.generateLabel("if_end");

    // Evaluate condition
    visitExpression(node->condition.get());
    instructions.cmp(X0, 0); // Compare result with 0 (BCPL false)
    labelManager.requestLabelFixup(skipLabel, instructions.getCurrentAddress());
    instructions.beq(skipLabel); // Branch if condition is 0 (false)

    // Generate then branch
    visitStatement(node->then_statement.get());

    // Add skip label
    instructions.setPendingLabel(skipLabel);
    labelManager.defineLabel(skipLabel, instructions.getCurrentAddress());
}

void CodeGenerator::visitTestStatement(const TestStatement* node) {
    auto elseLabel = labelManager.generateLabel("test_else");
    auto endLabel = labelManager.generateLabel("test_end");

    // Evaluate condition
    visitExpression(node->condition.get());
    instructions.cmp(X0, 0);
    labelManager.requestLabelFixup(elseLabel, instructions.getCurrentAddress());
    instructions.beq(elseLabel);

    // Generate then branch
    visitStatement(node->then_statement.get());
    labelManager.requestLabelFixup(endLabel, instructions.getCurrentAddress());
    instructions.b(endLabel);

    // Generate else branch
    instructions.setPendingLabel(elseLabel);
    labelManager.defineLabel(elseLabel, instructions.getCurrentAddress());
    if (node->else_statement) {
        visitStatement(node->else_statement.get());
    }

    instructions.setPendingLabel(endLabel);
    labelManager.defineLabel(endLabel, instructions.getCurrentAddress());
}

void CodeGenerator::visitWhileStatement(const WhileStatement* node) {
    labelManager.pushScope(LabelManager::ScopeType::LOOP);

    auto startLabel = labelManager.getCurrentRepeatLabel();
    auto endLabel = labelManager.getCurrentEndLabel();

    // Loop start
    instructions.setPendingLabel(startLabel);
    labelManager.defineLabel(startLabel, instructions.getCurrentAddress());

    // Evaluate condition
    visitExpression(node->condition.get());
    instructions.cmp(X0, 0); // Compare result with 0 (BCPL false)
    labelManager.requestLabelFixup(endLabel, instructions.getCurrentAddress());
    instructions.beq(endLabel); // Branch if condition is 0 (false) to end

    // Generate loop body
    visitStatement(node->body.get());
    labelManager.requestLabelFixup(startLabel, instructions.getCurrentAddress());
    instructions.b(startLabel);

    // Loop end
    instructions.setPendingLabel(endLabel);
    labelManager.defineLabel(endLabel, instructions.getCurrentAddress());

    labelManager.popScope();
}

void CodeGenerator::visitForStatement(const ForStatement* node) {
    labelManager.pushScope(LabelManager::ScopeType::LOOP);

    auto startLabel = labelManager.getCurrentRepeatLabel();
    auto endLabel = labelManager.getCurrentEndLabel();

    // Initialize loop variable
    visitExpression(node->from_expr.get());
    int var_offset = allocateLocal(node->var_name);
    instructions.str(X0, X29, var_offset, "Store initial value");

    // Store end value
    visitExpression(node->to_expr.get());
    int end_offset = allocateLocal("_for_end");
    instructions.str(X0, X29, end_offset, "Store end value");

    // Store step value
    int step_offset = allocateLocal("_for_step");
    if (node->by_expr) {
        visitExpression(node->by_expr.get());
    } else {
        instructions.loadImmediate(X0, 1);
    }
    instructions.str(X0, X29, step_offset, "Store step value");

    // Loop start
    instructions.setPendingLabel(startLabel);
    labelManager.defineLabel(startLabel, instructions.getCurrentAddress());

    // Compare current with end value
    instructions.ldr(X0, X29, var_offset);
    instructions.ldr(X1, X29, end_offset);
    instructions.cmp(X0, X1);
    labelManager.requestLabelFixup(endLabel, instructions.getCurrentAddress());
    instructions.bgt(endLabel); // Exit if current > end

    // Generate loop body
    visitStatement(node->body.get());

    // Increment loop variable
    instructions.ldr(X0, X29, var_offset);
    instructions.ldr(X1, X29, step_offset);
    instructions.add(X0, X0, X1, AArch64Instructions::LSL, 0);
    instructions.str(X0, X29, var_offset);
    labelManager.requestLabelFixup(startLabel, instructions.getCurrentAddress());
    instructions.b(startLabel);

    // Loop end
    instructions.setPendingLabel(endLabel);
    labelManager.defineLabel(endLabel, instructions.getCurrentAddress());

    labelManager.popScope();
}

void CodeGenerator::visitGotoStatement(const GotoStatement* node) {
    if (auto label = dynamic_cast<const VariableAccess*>(node->label.get())) {
        labelManager.requestLabelFixup(label->name, instructions.getCurrentAddress());
        instructions.b(label->name);
    } else {
        throw std::runtime_error("GOTO requires a label");
    }
}

void CodeGenerator::visitLabeledStatement(const LabeledStatement* node) {
    instructions.setPendingLabel(node->name);
    labelManager.defineLabel(node->name, instructions.getCurrentAddress());
    visitStatement(node->statement.get());
}

// CodeGenerator.cpp (continued)

void CodeGenerator::visitSwitchonStatement(const SwitchonStatement* node) {
    labelManager.pushScope(LabelManager::ScopeType::SWITCHON);

    auto endLabel = labelManager.getCurrentEndLabel();
    auto defaultLabel = labelManager.generateLabel("switch_default");

    // Evaluate switch expression
    visitExpression(node->expression.get());

    // Save switch value
    int switch_val_offset = allocateLocal("_switch_val");
    instructions.str(X0, X29, switch_val_offset, "Store switch value");

    // Generate jump table if case range is small and dense
    if (node->cases.size() > 3 && isSmallDenseRange(node->cases)) {
        generateJumpTable(node->cases, switch_val_offset, defaultLabel);
    } else {
        // Generate binary search tree for sparse or large ranges
        generateBinarySearchTree(node->cases, switch_val_offset, defaultLabel);
    }

    // Generate case bodies
    for (const auto& caseStmt : node->cases) {
        instructions.setPendingLabel(caseStmt.label);
        labelManager.defineLabel(caseStmt.label, instructions.getCurrentAddress());
        visitStatement(caseStmt.statement.get());
        instructions.b(endLabel);
    }

    // Default case
    instructions.setPendingLabel(defaultLabel);
    labelManager.defineLabel(defaultLabel, instructions.getCurrentAddress());
    if (node->default_case) {
        visitStatement(node->default_case.get());
    }

    // End of switch
    instructions.setPendingLabel(endLabel);
    labelManager.defineLabel(endLabel, instructions.getCurrentAddress());

    labelManager.popScope();
}

void CodeGenerator::generateJumpTable(
    const std::vector<SwitchonStatement::SwitchCase>& cases,
    int switchValueOffset,
    const std::string& defaultLabel) {

    // Load switch value
    instructions.ldr(X0, X29, switchValueOffset);

    // Check bounds
    int minValue = cases.front().value;
    int maxValue = cases.back().value;

    instructions.sub(X1, X0, minValue);  // Normalize to 0-based index
    instructions.loadImmediate(X2, maxValue - minValue);
    instructions.cmp(X1, X2);
    instructions.bgt(defaultLabel);  // Branch if out of range

    // Generate jump table
    auto tableLabel = labelManager.generateLabel("jump_table");
    instructions.adr(X2, tableLabel);  // Load table address
    instructions.add(X2, X2, X1, AArch64Instructions::LSL, 3);  // Calculate entry address (x8 for pointer size) - Fixed: LSL 3
    instructions.ldr(X2, X2, 0);  // Load target address
    instructions.br(X2);  // Branch to target

    // Emit jump table
    instructions.setPendingLabel(tableLabel);
    labelManager.defineLabel(tableLabel, instructions.getCurrentAddress());
    for (int i = minValue; i <= maxValue; i++) {
        auto it = std::find_if(cases.begin(), cases.end(),
            [i](const SwitchonStatement::SwitchCase& c) { return c.value == i; });

        if (it != cases.end()) {
            emitAddress(it->label);
        } else {
            // If a value in the range is not explicitly handled, jump to default
            emitAddress(defaultLabel); // Fixed: Jump to default for unhandled values in dense range
        }
    }
}

void CodeGenerator::generateBinarySearchTree(
    const std::vector<SwitchonStatement::SwitchCase>& cases,
    int switchValueOffset,
    const std::string& defaultLabel) {

    // Load switch value
    instructions.ldr(X0, X29, switchValueOffset);

    // Generate binary search using recursive helper
    generateBinarySearchNode(cases, 0, cases.size() - 1, defaultLabel);
}

void CodeGenerator::generateBinarySearchNode(
    const std::vector<SwitchonStatement::SwitchCase>& cases,
    size_t start,
    size_t end,
    const std::string& defaultLabel) {

    if (start > end) {
        instructions.b(defaultLabel);
        return;
    }

    size_t mid = (start + end) / 2;
    const auto& midCase = cases[mid];

    instructions.loadImmediate(X1, midCase.value);
    instructions.cmp(X0, X1);

    auto ltLabel = labelManager.generateLabel("case_lt");
    auto gtLabel = labelManager.generateLabel("case_gt");

    instructions.blt(ltLabel);
    instructions.bgt(gtLabel);

    // Equal case - branch to case body
    instructions.b(midCase.label);

    // Less than case
    instructions.setPendingLabel(ltLabel);
    labelManager.defineLabel(ltLabel, instructions.getCurrentAddress());
    if (mid > start) {
        generateBinarySearchNode(cases, start, mid - 1, defaultLabel);
    } else {
        instructions.b(defaultLabel);
    }

    // Greater than case
    instructions.setPendingLabel(gtLabel);
    labelManager.defineLabel(gtLabel, instructions.getCurrentAddress());
    if (mid < end) {
        generateBinarySearchNode(cases, mid + 1, end, defaultLabel);
    } else {
        instructions.b(defaultLabel);
    }
}

void CodeGenerator::visitFunctionCall(const FunctionCall* node) {
    // Update maxOutgoingParamSpace *before* pushing arguments
    size_t currentCallParamBytes = node->arguments.size() * 8;
    if (currentCallParamBytes > maxOutgoingParamSpace) {
        maxOutgoingParamSpace = currentCallParamBytes;
    }

    saveCallerSavedRegisters(); // This will save used scratch regs and update currentLocalVarOffset

    // Calculate the total space needed for arguments for this call.
    size_t argsBytes = node->arguments.size() * 8;
    
    // Adjust SP to make space for arguments
    if (argsBytes > 0) {
        instructions.sub(SP, SP, argsBytes, "Allocate space for outgoing arguments");
    }

    // Evaluate arguments and store them on the stack
    for (size_t i = 0; i < node->arguments.size(); ++i) {
        visitExpression(node->arguments[i].get()); // Result in X0
        // Store to the correct offset from the current SP
        instructions.str(X0, SP, i * 8, "Store argument " + std::to_string(i));
    }

    // Load arguments into registers (x0, x1, x2...) from the stack
    // Only load up to 8 arguments into registers as per AArch64 ABI
    for (size_t i = 0; i < std::min((size_t)8, node->arguments.size()); ++i) {
        instructions.ldr(X0 + i, SP, i * 8, "Load parameter into register");
    }

    // Generate call
    if (auto funcVar = dynamic_cast<const VariableAccess*>(node->function.get())) {
        // Direct function call
        if (auto it = functions.find(funcVar->name); it != functions.end()) {
            instructions.bl(funcVar->name, "Call " + funcVar->name);
        } else {
            throw std::runtime_error("Unknown function: " + funcVar->name);
        }
    } else {
        // Indirect function call through expression
        visitExpression(node->function.get());
        instructions.br(X0);
    }

    // Deallocate arguments pushed for this call
    if (argsBytes > 0) {
        instructions.add(SP, SP, argsBytes, "Deallocate outgoing arguments");
    }
    restoreCallerSavedRegisters(); // This will restore saved regs and adjust currentLocalVarOffset
}

void CodeGenerator::visitAssignment(const Assignment* node) {
    // First evaluate the right-hand side expression. Result is in X0.
    visitExpression(node->rhs[0].get());

    // Store result based on the left-hand side type
    if (auto var = dynamic_cast<const VariableAccess*>(node->lhs[0].get())) {
        // Simple variable assignment
        if (auto it = manifests.find(var->name); it != manifests.end()) {
            throw std::runtime_error("Cannot assign to manifest constant: " + var->name);
        } else if (auto it = globals.find(var->name); it != globals.end()) {
            // Store to global vector
            instructions.str(X0, X28, it->second * 8, "Store to global " + var->name);
        } else {
            // Local variable: try to store to its register, otherwise acquire one
            uint32_t reg = registerManager.getVariableRegister(var->name);
            if (reg == 0xFFFFFFFF) { // Not currently in a register
                int offset = getLocalOffset(var->name);
                reg = registerManager.acquireRegister(var->name, offset); // Acquire a register for it
            }
            instructions.mov(reg, X0, "Assign value to " + var->name + " in " + instructions.regName(reg));
            registerManager.markDirty(var->name); // Mark the register as dirty
        }
    } else if (auto deref = dynamic_cast<const DereferenceExpr*>(node->lhs[0].get())) {
        // Pointer dereference assignment
        uint32_t valueReg = scratchAllocator.acquire();
        instructions.mov(valueReg, X0, "Save RHS value for dereference assignment");

        // Evaluate address
        visitExpression(deref->pointer.get()); // Address in X0

        // Store value to computed address
        instructions.str(valueReg, X0, 0, "Store to computed address");
        scratchAllocator.release(valueReg);
    } else if (auto vecAccess = dynamic_cast<const VectorAccess*>(node->lhs[0].get())) {
        // Vector element assignment
        uint32_t valueReg = scratchAllocator.acquire();
        instructions.mov(valueReg, X0, "Save RHS value for vector assignment");

        // Evaluate the index
        visitExpression(vecAccess->index.get()); // Index in X0
        uint32_t indexReg = scratchAllocator.acquire();
        instructions.mov(indexReg, X0, "Save index value");

        // Evaluate the vector's base address
        visitExpression(vecAccess->vector.get()); // Vector base in X0
        uint32_t vectorBaseReg = scratchAllocator.acquire();
        instructions.mov(vectorBaseReg, X0, "Save vector base address");

        // Calculate the final address: vectorBaseReg + (indexReg << 3)
        instructions.add(vectorBaseReg, vectorBaseReg, indexReg, AArch64Instructions::LSL, 3, "Calculate element address");

        // Store value: store valueReg to [calculated_address]
        instructions.str(valueReg, vectorBaseReg, 0, "Store to vector element");
        
        // Release scratch registers
        scratchAllocator.release(vectorBaseReg);
        scratchAllocator.release(indexReg);
        scratchAllocator.release(valueReg);
    } else {
        throw std::runtime_error("Unsupported LHS in assignment.");
    }
}

void CodeGenerator::visitResultisStatement(const ResultisStatement* node) {
    // Evaluate the expression
    visitExpression(node->value.get());

    // If the result is a variable, and this is its last use, release its register without spilling.
    if (auto var_access = dynamic_cast<const VariableAccess*>(node->value.get())) {
        uint32_t reg = registerManager.getVariableRegister(var_access->name);
        if (reg != 0xFFFFFFFF) {
            // This is a heuristic: assume RESULTIS is the last use of the variable.
            // A proper liveness analysis would confirm this.
            registerManager.releaseRegisterWithoutSpill(reg);
        }
    }

    // Branch to function epilogue
    auto returnLabel = labelManager.getCurrentReturnLabel();
    labelManager.requestLabelFixup(returnLabel, instructions.getCurrentAddress());
    instructions.b(returnLabel, "Branch to function return");
}

void CodeGenerator::visitBreakStatement(const BreakStatement* node) {
    auto endLabel = labelManager.getCurrentEndLabel();
    labelManager.requestLabelFixup(endLabel, instructions.getCurrentAddress());
    instructions.b(endLabel, "Break from current construct");
}

// Helper methods

int CodeGenerator::allocateLocal(const std::string& name) {
    currentLocalVarOffset -= 8; // Decrement offset from FP
    localVars[name] = currentLocalVarOffset;
    return currentLocalVarOffset;
}

int CodeGenerator::getLocalOffset(const std::string& name) {
    auto it = localVars.find(name);
    if (it == localVars.end()) {
        throw std::runtime_error("Undefined variable: " + name);
    }
    return it->second;
}

size_t CodeGenerator::allocateGlobal() {
    size_t index = globals.size();
    // We don't assign a name here, just reserve space.
    // The actual name mapping happens when 'let' declarations are processed.
    return index;
}

void CodeGenerator::saveCallerSavedRegisters() {
    // Spill all currently dirty registers to memory before saving caller-saved registers.
    registerManager.spillAllDirtyRegisters();

    // Save caller-saved registers that are currently in use by the RegisterManager.
    // These are pushed onto the stack, and `currentLocalVarOffset` is decremented.
    // The offset for `str` should be relative to `X29` (FP).
    // The `savedCallerRegsAroundCall` stores `(reg, offset_from_FP, var_name)`.
    
    int currentSaveBytes = 0;
    std::vector<std::tuple<uint32_t, int, std::string>> regsToSaveInfo; // Store info for later restoration
    for (uint32_t reg : registerManager.getUsedRegisters()) {
        // Exclude argument registers (X0-X7) as they are handled by ABI
        if (reg >= AArch64Instructions::X0 && reg <= AArch64Instructions::X7) continue; 
        
        // Check if it's a caller-saved register (X9-X15 are typical scratch, X0-X7 args)
        // For simplicity, we'll save all non-argument registers currently in use by RegisterManager.
        // A more precise approach would check if 'reg' is in the caller-saved set.
        // For now, assume any non-argument register managed by RegisterManager needs saving.
        
        currentLocalVarOffset -= 8; // Decrement main stack offset (relative to FP)
        currentSaveBytes += 8;
        instructions.str(reg, X29, currentLocalVarOffset, "Save caller-saved register " + instructions.regName(reg));
        
        // Get the variable name associated with this register from RegisterManager
        std::string varName = registerManager.getVariableName(reg);
        regsToSaveInfo.emplace_back(reg, currentLocalVarOffset, varName);
    }
    // Update maxCallerSavedRegsSpace with the maximum space used by caller-saved registers
    if (currentSaveBytes > maxCallerSavedRegsSpace) {
        maxCallerSavedRegsSpace = currentSaveBytes;
    }

    // Now, remove the variables from the RegisterManager's active set
    // so they are not considered in use during the function call.
    // This must be done *after* iterating through getUsedRegisters()
    // to avoid modifying the set while iterating.
    for (const auto& entry : regsToSaveInfo) {
        registerManager.removeVariableFromRegister(std::get<2>(entry));
    }
    savedCallerRegsAroundCall = std::move(regsToSaveInfo); // Store for restoration
}

void CodeGenerator::restoreCallerSavedRegisters() {
    // Restore in reverse order
    for (auto it = savedCallerRegsAroundCall.rbegin(); it != savedCallerRegsAroundCall.rend(); ++it) {
        uint32_t reg = std::get<0>(*it);
        int offset = std::get<1>(*it);
        std::string varName = std::get<2>(*it);

        instructions.ldr(reg, X29, offset, "Restore caller-saved register " + instructions.regName(reg));
        currentLocalVarOffset += 8; // Increment main stack offset
        
        // Re-establish the variable-to-register mapping in RegisterManager
        if (!varName.empty()) { // Only reassign if it was a variable
            registerManager.reassignRegister(varName, reg, offset);
        }
    }
    // Reset saved registers list
    savedCallerRegsAroundCall.clear();
}

bool CodeGenerator::isRegisterInUse(uint32_t reg) {
    // Check if register is currently being used by the RegisterManager
    return registerManager.getUsedRegisters().count(reg);
}

void CodeGenerator::addToListing(const std::string& instruction, const std::string& comment) {
    assemblyListing << std::setw(40) << std::left << instruction;
    if (!comment.empty()) {
        assemblyListing << "; " << comment;
    }
    assemblyListing << "\n";
}

void CodeGenerator::emitAddress(const std::string& label) {
    // This is a placeholder for emitting a data address (e.g., for jump tables)
    // In a real assembler, this would be a .quad or .word directive
    // For now, we\'ll just add a comment to the assembly listing
    addToListing(".quad " + label, "Address of " + label);
}

bool CodeGenerator::isSmallDenseRange(const std::vector<SwitchonStatement::SwitchCase>& cases) {
    if (cases.empty()) return false;

    int minValue = cases.front().value;
    int maxValue = cases.back().value;

    // Check if range is small enough for jump table
    if (maxValue - minValue > 1000) return false;

    // Check density (at least 50% of values in range should be present)
    int range = maxValue - minValue + 1;
    return cases.size() >= range / 2;
}

void CodeGenerator::saveCalleeSavedRegisters() {
    for (const auto& reg : calleeSavedRegs) {
        if (isRegisterInUse(reg)) {
            currentLocalVarOffset -= 8;
            instructions.str(reg, X29, currentLocalVarOffset, "Save callee-saved register " + instructions.regName(reg));
            savedCalleeRegsInPrologue.push_back({reg, currentLocalVarOffset}); // Store as if it's a caller-saved for now
        }
    }
}

void CodeGenerator::restoreCalleeSavedRegisters() {
    for (auto it = savedCalleeRegsInPrologue.rbegin(); it != savedCalleeRegsInPrologue.rend(); ++it) {
        instructions.ldr(it->first, X29, it->second, "Restore callee-saved register " + instructions.regName(it->first));
        currentLocalVarOffset += 8;
    }
    savedCalleeRegsInPrologue.clear();
}

// Final assembly and optimization
void CodeGenerator::finalizeCode() {
    // Resolve all branch targets
    resolveBranchTargets();

    // Perform peephole optimization
    performPeepholeOptimization();

    // Generate final assembly listing
    generateAssemblyListing();
}

void CodeGenerator::resolveBranchTargets() {
    for (auto& instruction : instructions.getInstructions()) {
        if (instruction.needsLabelResolution) {
            size_t targetAddress = labelManager.getLabelAddress(instruction.targetLabel);
            int64_t offset = static_cast<int64_t>(targetAddress - instruction.address);
            instruction.resolveLabel(offset);
        }
    }
}

void CodeGenerator::performPeepholeOptimization() {
    auto& instrs = instructions.getInstructions();
    for (size_t i = 0; i < instrs.size() - 1; ++i) {
        // Example optimization: combine consecutive loads/stores
        if (instrs[i].isStore() && instrs[i+1].isLoad()) {
            if (canCombineLoadStore(instrs[i], instrs[i+1])) {
                combineLoadStore(instrs[i], instrs[i+1]);
                instrs.erase(instrs.begin() + i + 1);
                --i;
            }
        }
    }
}

bool CodeGenerator::canCombineLoadStore(const AArch64Instructions::Instruction& instr1, const AArch64Instructions::Instruction& instr2) {
    // Placeholder: Implement actual logic to check if two instructions can be combined
    // This would involve checking registers, offsets, and instruction types.
    return false;
}

void CodeGenerator::combineLoadStore(AArch64Instructions::Instruction& instr1, AArch64Instructions::Instruction& instr2) {
    // Placeholder: Implement actual combination logic
    // This would modify instr1 to represent the combined instruction
    // and potentially invalidate instr2.
}

void CodeGenerator::generateAssemblyListing() {
    assemblyListing.clear();
    assemblyListing << ".text\n";
    assemblyListing << ".align 4\n\n";

    for (const auto& instr : instructions.getInstructions()) {
        if (instr.hasLabel) {
            assemblyListing << instr.label << ":\n";
        }
        assemblyListing << "\t" << instr.toString() << "\n";
    }
}

void CodeGenerator::visitLetDeclaration(const LetDeclaration* node) {
    // Handle let declarations
    // For now, we'll just allocate space for the variable if it's a simple declaration
    // and then visit the expression if it's an initialization.
    if (node->initializers.empty()) {
        throw std::runtime_error("LetDeclaration must have at least one initializer.");
    }

    for (const auto& init : node->initializers) {
        if (init.init) {
            // Handle initialization: evaluate expression and store result
            visitExpression(init.init.get());
            // Assuming all let declarations are local for now
            int offset = allocateLocal(init.name);
            // Acquire a register for the new local variable and move the value from X0 into it.
            uint32_t reg = registerManager.acquireRegister(init.name, offset);
            instructions.mov(reg, X0, "Initialize local " + init.name + " in " + instructions.regName(reg));
            registerManager.markDirty(init.name); // Mark as dirty since it's initialized in register
        } else {
            // Just declare, no initialization. Allocate stack space and register with RegisterManager.
            int offset = allocateLocal(init.name);
            // The variable is not initialized, so it's not dirty in the register yet.
            // We still register it so it can be allocated a register when first accessed.
            registerManager.acquireRegister(init.name, offset); // Acquire a register, but don't load/mark dirty yet
        }
    }
}

void CodeGenerator::visitValof(const Valof* node) {
    // This typically involves executing a statement block and returning a value.
    // For now, we'll just visit the statement and assume the result is in X0.
    visitStatement(node->body.get());
}

void CodeGenerator::visitTableConstructor(const TableConstructor* node) {
    // This would involve allocating memory for the table and populating it.
    throw std::runtime_error("Table constructors not yet implemented.");
}



        void CodeGenerator::visitVectorConstructor(const VectorConstructor* node) {
    visitExpression(node->size.get());
    // The size of the vector is now in X0.
    // We need to call the `bcpl_vec` runtime function to allocate the vector.
    instructions.bl("bcpl_vec", "Allocate vector on heap");
    // The result of the allocation (the pointer to the vector) is in X0.
}

void CodeGenerator::visitCharacterAccess(const CharacterAccess* node) {
    // Evaluate index first
    visitExpression(node->index.get());
    // Save index
    uint32_t indexReg = scratchAllocator.acquire();
    instructions.mov(indexReg, X0);

    // Evaluate string address
    visitExpression(node->string.get());

    // Calculate final address and load
    instructions.add(X0, X0, indexReg, AArch64Instructions::LSL, 2, "Calculate character address (4-byte chars)");
    instructions.ldr(X0, X0, 0, "Load character");
    scratchAllocator.release(indexReg);
}

void CodeGenerator::visitConditionalExpression(const ConditionalExpression* node) {
    auto elseLabel = labelManager.generateLabel("cond_else");
    auto endLabel = labelManager.generateLabel("cond_end");

    // Evaluate the condition
    visitExpression(node->condition.get());
    instructions.cmp(X0, 0);
    labelManager.requestLabelFixup(elseLabel, instructions.getCurrentAddress());
    instructions.beq(elseLabel);

    // If true, evaluate the 'then' expression
    visitExpression(node->trueExpr.get());
    labelManager.requestLabelFixup(endLabel, instructions.getCurrentAddress());
    instructions.b(endLabel);

    // If false, evaluate the 'else' expression
    instructions.setPendingLabel(elseLabel);
    labelManager.defineLabel(elseLabel, instructions.getCurrentAddress());
    visitExpression(node->falseExpr.get());

    // Define the end label
    instructions.setPendingLabel(endLabel);
    labelManager.defineLabel(endLabel, instructions.getCurrentAddress());
}

void CodeGenerator::visitRoutineCall(const RoutineCall* node) {
    if (auto funcCall = dynamic_cast<const FunctionCall*>(node->call_expression.get())) {
        if (auto funcVar = dynamic_cast<const VariableAccess*>(funcCall->function.get())) {
            if (funcVar->name == "WRITES") {
                visitExpression(funcCall->arguments[0].get());
                instructions.bl("writes", "Call writes");
            } else if (funcVar->name == "WRITEF") {
                // WRITEF takes format string in X0, first arg in X1, etc.
                // For simplicity, we'll push all args to stack and then load first 2 to X0, X1
                // This is a temporary simplification and not fully ABI compliant for multiple args
                size_t argsBytes = funcCall->arguments.size() * 8;
                if (argsBytes > 0) {
                    instructions.sub(SP, SP, argsBytes, "Allocate space for WRITEF arguments");
                }

                // Evaluate arguments and store them on the stack in reverse order
                for (size_t i = 0; i < funcCall->arguments.size(); ++i) {
                    visitExpression(funcCall->arguments[funcCall->arguments.size() - 1 - i].get()); // Result in X0
                    instructions.str(X0, SP, i * 8, "Store WRITEF argument " + std::to_string(funcCall->arguments.size() - 1 - i));
                }

                // Load format string into X0, first argument into X1
                // Assuming WRITEF takes format string as first arg, and then subsequent args
                // This needs to be aligned with the actual WRITEF runtime signature.
                if (funcCall->arguments.size() > 0) {
                    instructions.ldr(X0, SP, (funcCall->arguments.size() - 1) * 8, "Load format string for WRITEF");
                }
                if (funcCall->arguments.size() > 1) {
                    instructions.ldr(X1, SP, (funcCall->arguments.size() - 2) * 8, "Load first data arg for WRITEF");
                }
                // Other arguments would remain on stack for varargs, but not handled here.

                instructions.bl("writef", "Call writef");

                if (argsBytes > 0) {
                    instructions.add(SP, SP, argsBytes, "Deallocate WRITEF arguments");
                }

            } else if (funcVar->name == "NEWLINE") {
                instructions.bl("newline", "Call newline");
            } else if (funcVar->name == "FINISH") {
                instructions.bl("finish", "Call finish");
            } else {
                // Regular function call
                saveCallerSavedRegisters();

                // Evaluate arguments and push them onto the stack in reverse order
                size_t argsBytes = funcCall->arguments.size() * 8;
                if (argsBytes > 0) {
                    instructions.sub(SP, SP, argsBytes, "Allocate space for outgoing arguments");
                }

                for (size_t i = 0; i < funcCall->arguments.size(); ++i) {
                    visitExpression(funcCall->arguments[i].get()); // Result in X0
                    instructions.str(X0, SP, i * 8, "Store argument " + std::to_string(i));
                }

                // Load arguments into registers (x0, x1, x2...) from the stack
                // Only load up to 8 arguments into registers as per AArch64 ABI
                for (size_t i = 0; i < std::min((size_t)8, funcCall->arguments.size()); ++i) {
                    instructions.ldr(X0 + i, SP, i * 8, "Load parameter into register");
                }

                if (auto it = functions.find(funcVar->name); it != functions.end()) {
                    instructions.bl(funcVar->name, "Call routine " + funcVar->name);
                } else {
                    throw std::runtime_error("Unknown routine: " + funcVar->name);
                }

                // Deallocate arguments pushed for this call
                if (argsBytes > 0) {
                    instructions.add(SP, SP, argsBytes, "Deallocate outgoing arguments");
                }
                restoreCallerSavedRegisters();
            }
        }
    }
}

void CodeGenerator::visitReturnStatement(const ReturnStatement* node) {
    // Branch to function epilogue.
    auto returnLabel = labelManager.getCurrentReturnLabel();
    labelManager.requestLabelFixup(returnLabel, instructions.getCurrentAddress());
    instructions.b(returnLabel, "Return from function");
}

void CodeGenerator::visitLoopStatement(const LoopStatement* node) {
    auto startLabel = labelManager.getCurrentRepeatLabel();
    labelManager.requestLabelFixup(startLabel, instructions.getCurrentAddress());
    instructions.b(startLabel, "Loop back");
}

void CodeGenerator::visitEndcaseStatement(const EndcaseStatement* node) {
    // This typically marks the end of a switch case, so we branch to the end of the switch.
    auto endLabel = labelManager.getCurrentEndLabel();
    labelManager.requestLabelFixup(endLabel, instructions.getCurrentAddress());
    instructions.b(endLabel, "End of case");
}

void CodeGenerator::visitFinishStatement(const FinishStatement* node) {
    // This typically exits the current loop or function. For now, we'll treat it like a break.
    auto endLabel = labelManager.getCurrentEndLabel();
    labelManager.requestLabelFixup(endLabel, instructions.getCurrentAddress());
    instructions.b(endLabel, "Finish current construct");
}

void CodeGenerator::printAsm() const {
    std::cout << "\n;------------ Generated ARM64 Assembly ------------\n\n";

    // Print any necessary assembler directives
    std::cout << ".arch armv8-a\n";
    std::cout << ".text\n";
    std::cout << ".align 4\n\n";

    // Print any global declarations
    if (!globals.empty()) {
        std::cout << ".data\n";
        for (const auto& global : globals) {
            std::cout << ".global " << global.first << "\n";
            std::cout << global.first << ":\n";
            std::cout << "    .space " << 8 * 1 << "\n";
        }
        std::cout << "\n";
    }

    std::cout << ".text\n";
    std::cout << ".align 4\n\n";

    // Print all instructions with their comments
    for (const auto& instr : instructions.getInstructions()) {
        // Print labels if present
        if (instr.hasLabel) {
            std::cout << instr.label << ":\n";
        }

        // Print the instruction with proper formatting
        std::cout << "    " << std::left << std::setw(30) << instr.toString();

        // Add comment if present
        if (!instr.comment.empty()) {
            std::cout << " // " << instr.comment;
        }
        std::cout << "\n";
    }

    // Print string pool
    if (!stringPool.empty()) {
        std::cout << "\n.data\n";
        for (size_t i = 0; i < stringPool.size(); ++i) {
            std::cout << ".L.str" << i << ":\n";
            std::cout << "    .string \"" << stringPool[i] << "\"\n";
        }
    }

    std::cout << "\n;------------ End of Assembly ------------\n\n";
}