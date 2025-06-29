// CodeGenerator.h
#ifndef CODEGENERATOR_H
#define CODEGENERATOR_H

#include "AST.h"
#include "AArch64Instructions.h"
#include "LabelManager.h"
#include "ScratchAllocator.h"
#include "RegisterManager.h" // Include the new RegisterManager
#include <string>
#include <unordered_map>
#include <sstream>
#include <memory>

class StringAccess;

class CodeGenerator {
public:
    CodeGenerator();
    uintptr_t compile(ProgramPtr program);
    void printAsm() const;

private:
    // Core components
    AArch64Instructions instructions;
    LabelManager labelManager;
    ScratchAllocator scratchAllocator;
    RegisterManager registerManager; // New RegisterManager member
    std::stringstream assemblyListing;
    std::vector<std::string> stringPool;

    // State tracking
    // Stack management
    int currentLocalVarOffset; // Tracks offset from FP for local variables
    int maxOutgoingParamSpace; // Max bytes for outgoing parameters in this function
    int maxCallerSavedRegsSpace; // Max bytes for caller-saved register spills in this function
    std::vector<std::tuple<uint32_t, int, std::string>> savedCallerRegsAroundCall; // Stores (reg, offset_from_FP, var_name) for caller-saved registers
    std::vector<std::pair<uint32_t, int>> savedCalleeRegsInPrologue; // Stores (reg, offset_from_FP) for callee-saved registers
    std::vector<uint32_t> calleeSavedRegs; // List of callee-saved registers (x19-x28)
    std::vector<const VectorConstructor*> vectorAllocations;

    std::unordered_map<std::string, int> localVars;
    std::unordered_map<std::string, size_t> globals;
    std::unordered_map<std::string, int> manifests;
    std::unordered_map<std::string, size_t> functions;

    struct PendingCase {
        std::string label;
        const Statement* statement;
    };
    std::vector<PendingCase> pendingCases;

    // Register constants
    const uint32_t X0 = AArch64Instructions::X0;   // Result/A register
    const uint32_t X1 = AArch64Instructions::X1;   // B register
    const uint32_t X2 = AArch64Instructions::X2;   // C register
    const uint32_t X28 = AArch64Instructions::X28; // Global vector
    const uint32_t X29 = AArch64Instructions::X29; // Frame pointer
    const uint32_t X30 = AArch64Instructions::X30; // Link register
    const uint32_t SP = AArch64Instructions::SP;   // Stack pointer
    const uint32_t XZR = AArch64Instructions::XZR; // Zero register

    // AST visitors
    void visitProgram(const Program* node);
    void visitStatement(const Statement* node);
    void visitExpression(const Expression* node);

    // Statement visitors
    void visitFunctionDeclaration(const FunctionDeclaration* node);
    void visitGlobalDeclaration(const GlobalDeclaration* node);
    void visitManifestDeclaration(const ManifestDeclaration* node);
    void visitLetDeclaration(const LetDeclaration* node);
    void visitCompoundStatement(const CompoundStatement* node);
    void visitIfStatement(const IfStatement* node);
    void visitTestStatement(const TestStatement* node);
    void visitWhileStatement(const WhileStatement* node);
    void visitForStatement(const ForStatement* node);
    void visitSwitchonStatement(const SwitchonStatement* node);
    void visitGotoStatement(const GotoStatement* node);
    void visitLabeledStatement(const LabeledStatement* node);
    void visitAssignment(const Assignment* node);
    void visitRoutineCall(const RoutineCall* node);
    void visitReturnStatement(const ReturnStatement* node);
    void visitResultisStatement(const ResultisStatement* node);
    void visitBreakStatement(const BreakStatement* node);
    void visitLoopStatement(const LoopStatement* node);
    void visitRepeatStatement(const RepeatStatement* node);
    void visitEndcaseStatement(const EndcaseStatement* node);
    void visitFinishStatement(const FinishStatement* node);

    // Expression visitors
    void visitNumberLiteral(const NumberLiteral* node);
    void visitStringLiteral(const StringLiteral* node);
    void visitCharLiteral(const CharLiteral* node);
    void visitVariableAccess(const VariableAccess* node);
    void visitUnaryOp(const UnaryOp* node);
    void visitBinaryOp(const BinaryOp* node);
    void visitFunctionCall(const FunctionCall* node);
    void visitConditionalExpression(const ConditionalExpression* node);
    void visitValof(const Valof* node);
    void visitTableConstructor(const TableConstructor* node);
    void visitVectorConstructor(const VectorConstructor* node);
    void visitStringAccess(const StringAccess* node);
    void visitCharacterAccess(const CharacterAccess* node);

    // Code generation helpers
    void resolveLabels();
    void saveCallerSavedRegisters();
    void restoreCallerSavedRegisters();

    int allocateLocal(const std::string& name);
    int getLocalOffset(const std::string& name);
    size_t allocateGlobal();
    std::string getLabelFromComment(const std::string& comment);
    void addToListing(const std::string& instruction, const std::string& comment = "");
    void emitAddress(const std::string& label);
    std::string formatInstruction(const std::string& mnemonic,
                                const std::vector<std::string>& operands,
                                const std::string& comment = "");
    bool isSmallDenseRange(const std::vector<SwitchonStatement::SwitchCase>& cases);
    void generateJumpTable(const std::vector<SwitchonStatement::SwitchCase>& cases, int switchValueOffset, const std::string& defaultLabel);
    void generateBinarySearchTree(const std::vector<SwitchonStatement::SwitchCase>& cases, int switchValueOffset, const std::string& defaultLabel);
    void generateBinarySearchNode(const std::vector<SwitchonStatement::SwitchCase>& cases, size_t start, size_t end, const std::string& defaultLabel);
    bool isRegisterInUse(uint32_t reg);
    void saveCalleeSavedRegisters();
    void restoreCalleeSavedRegisters();
    void finalizeCode();
    void resolveBranchTargets();
    void performPeepholeOptimization();
    void generateAssemblyListing();
    bool canCombineLoadStore(const AArch64Instructions::Instruction& instr1, const AArch64Instructions::Instruction& instr2);
    void combineLoadStore(AArch64Instructions::Instruction& instr1, AArch64Instructions::Instruction& instr2);
};

#endif // CODEGENERATOR_H
