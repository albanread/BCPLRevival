#ifndef LABEL_MANAGER_H
#define LABEL_MANAGER_H

#include <string>
#include <unordered_map>
#include <vector>
#include <stdexcept>
#include <mutex>
#include <optional>

class LabelManager {
public:
    // BCPL-specific scope types
    enum class ScopeType {
        FUNCTION,     // For functions and routines
        VALOF,        // For VALOF blocks (needs RESULTIS)
        LOOP,         // For WHILE, UNTIL, FOR, REPEAT
        SWITCHON,     // For SWITCHON blocks (needs ENDCASE)
        COMPOUND      // For general $( ... $) blocks
    };

    struct Scope {
        ScopeType type;
        std::string endLabel;        // General end of scope
        std::string resultisLabel;   // For VALOF RESULTIS
        std::string repeatLabel;     // For REPEAT, REPEATWHILE, REPEATUNTIL
        std::string endcaseLabel;    // For SWITCHON ENDCASE
        std::unordered_map<std::string, size_t> localLabels;
    };

    struct Fixup {
        size_t instructionAddress;
        std::string labelName;
    };

    LabelManager() : label_counter_(0) {}

    // Scope management
    void pushScope(ScopeType type);
    void popScope();

    // Label generation and management
    std::string generateLabel(const std::string& prefix);

    void defineLabel(const std::string& label, size_t position);
    void requestLabelFixup(const std::string& label, size_t instructionAddress);
    std::vector<Fixup> getFixups();

    // BCPL-specific control flow label accessors
    std::string getCurrentResultisLabel() const;
    std::string getCurrentRepeatLabel() const;
    std::string getCurrentEndcaseLabel() const;
    std::string getCurrentEndLabel() const;
    std::string getCurrentReturnLabel() const;

    // Label resolution
    std::optional<size_t> getLabelPosition(const std::string& label) const;
    size_t getLabelAddress(const std::string& label) const;

private:
    size_t label_counter_;
    std::vector<Scope> scope_stack_;
    std::unordered_map<std::string, size_t> global_labels_;
    std::vector<Fixup> fixups_;

    mutable std::mutex label_mutex_; // For thread safety
};

#endif // LABEL_MANAGER_H