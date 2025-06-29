#include "LabelManager.h"
#include <algorithm>
#include <stdexcept>

void LabelManager::pushScope(ScopeType type) {
    Scope scope;
    scope.type = type;

    // Generate appropriate labels based on scope type
    switch (type) {
        case ScopeType::FUNCTION:
            scope.endLabel = generateLabel("return");
            break;
        case ScopeType::VALOF:
            scope.resultisLabel = generateLabel("resultis");
            scope.endLabel = generateLabel("valof_end");
            break;
        case ScopeType::LOOP:
            scope.endLabel = generateLabel("loop_end");
            scope.repeatLabel = generateLabel("repeat");
            break;
        case ScopeType::SWITCHON:
            scope.endLabel = generateLabel("switch_end");
            scope.endcaseLabel = generateLabel("endcase");
            break;
        case ScopeType::COMPOUND:
            scope.endLabel = generateLabel("block_end");
            break;
    }

    std::lock_guard<std::mutex> lock(label_mutex_);
    scope_stack_.push_back(scope);
}

void LabelManager::popScope() {
    std::lock_guard<std::mutex> lock(label_mutex_);
    if (scope_stack_.empty()) {
        throw std::runtime_error("Cannot pop from empty scope stack");
    }
    scope_stack_.pop_back();
}

std::string LabelManager::generateLabel(const std::string& prefix) {
    std::lock_guard<std::mutex> lock(label_mutex_);
    return prefix + "_" + std::to_string(label_counter_++);
}

void LabelManager::defineLabel(const std::string& label, size_t position) {
    std::lock_guard<std::mutex> lock(label_mutex_);

    if (scope_stack_.empty()) {
        // Global label
        if (global_labels_.find(label) != global_labels_.end()) {
            throw std::runtime_error("Label already defined globally: " + label);
        }
        global_labels_[label] = position;
    } else {
        // Local label (current scope)
        auto& current_scope = scope_stack_.back();
        if (current_scope.localLabels.find(label) != current_scope.localLabels.end()) {
            throw std::runtime_error("Label already defined in current scope: " + label);
        }
        if (global_labels_.find(label) != global_labels_.end()) {
            throw std::runtime_error("Label already defined globally: " + label);
        }
        current_scope.localLabels[label] = position;
    }
}

void LabelManager::requestLabelFixup(const std::string& label, size_t instructionAddress) {
    std::lock_guard<std::mutex> lock(label_mutex_);
    fixups_.push_back({instructionAddress, label});
}

std::vector<LabelManager::Fixup> LabelManager::getFixups() {
    std::lock_guard<std::mutex> lock(label_mutex_);
    std::vector<Fixup> fixups = std::move(fixups_);
    fixups_.clear();
    return fixups;
}

std::string LabelManager::getCurrentResultisLabel() const {
    std::lock_guard<std::mutex> lock(label_mutex_);
    for (auto it = scope_stack_.rbegin(); it != scope_stack_.rend(); ++it) {
        if (it->type == ScopeType::VALOF && !it->resultisLabel.empty()) {
            return it->resultisLabel;
        }
    }
    throw std::runtime_error("No RESULTIS label available (not in VALOF)");
}

std::string LabelManager::getCurrentRepeatLabel() const {
    std::lock_guard<std::mutex> lock(label_mutex_);
    for (auto it = scope_stack_.rbegin(); it != scope_stack_.rend(); ++it) {
        if (it->type == ScopeType::LOOP && !it->repeatLabel.empty()) {
            return it->repeatLabel;
        }
    }
    throw std::runtime_error("No REPEAT label available (not in loop)");
}

std::string LabelManager::getCurrentEndcaseLabel() const {
    std::lock_guard<std::mutex> lock(label_mutex_);
    for (auto it = scope_stack_.rbegin(); it != scope_stack_.rend(); ++it) {
        if (it->type == ScopeType::SWITCHON && !it->endcaseLabel.empty()) {
            return it->endcaseLabel;
        }
    }
    throw std::runtime_error("No ENDCASE label available (not in SWITCHON)");
}

std::string LabelManager::getCurrentEndLabel() const {
    std::lock_guard<std::mutex> lock(label_mutex_);
    if (scope_stack_.empty()) {
        throw std::runtime_error("No current scope");
    }
    return scope_stack_.back().endLabel;
}

std::string LabelManager::getCurrentReturnLabel() const {
    std::lock_guard<std::mutex> lock(label_mutex_);
    for (auto it = scope_stack_.rbegin(); it != scope_stack_.rend(); ++it) {
        if (it->type == ScopeType::FUNCTION) {
            return it->endLabel;
        }
    }
    throw std::runtime_error("Not in a function scope");
}

std::optional<size_t> LabelManager::getLabelPosition(const std::string& label) const {
    std::lock_guard<std::mutex> lock(label_mutex_);

    // Check global labels
    if (auto it = global_labels_.find(label); it != global_labels_.end()) {
        return it->second;
    }

    // Check local labels in the scope stack
    for (auto it = scope_stack_.rbegin(); it != scope_stack_.rend(); ++it) {
        if (auto lit = it->localLabels.find(label); lit != it->localLabels.end()) {
            return lit->second;
        }
    }

    return std::nullopt; // Label not found
}

#include <iostream>

size_t LabelManager::getLabelAddress(const std::string& label) const {
    std::cout << "Requesting address for label: " << label << std::endl;
    if (auto pos = getLabelPosition(label); pos.has_value()) {
        return *pos;
    }
    throw std::runtime_error("Label not found: " + label);
}
