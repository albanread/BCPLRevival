#ifndef JIT_RUNTIME_H
#define JIT_RUNTIME_H

#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <cstdint> // For uintptr_t and int64_t
#include <cstdio>  // For FILE, fopen, etc.
#include <cstdlib> // For exit, malloc, free

/**
 * @class JitRuntime
 * @brief A singleton class that manages the runtime environment for JIT-compiled BCPL code.
 *
 * This class is responsible for:
 * - Maintaining a symbol table to link functions and global variables by name,
 * serving as the modern replacement for the BCPL Global Vector.
 * - Providing a stable context accessible from JIT-compiled code, containing
 * pointers to C library functions and current I/O streams.
 * - Managing the BCPL I/O channels (current input and output streams).
 * - Implementing the core BCPL standard library functions that will be called
 * from the JIT-compiled code.
 */
class JitRuntime {
public:
    // --- Singleton Access ---
    static JitRuntime& getInstance() {
        static JitRuntime instance;
        return instance;
    }

    // --- Deleted Constructors to enforce singleton pattern ---
    JitRuntime(const JitRuntime&) = delete;
    JitRuntime& operator=(const JitRuntime&) = delete;

    // --- Symbol Table Management (Modern Global Vector) ---
    using SymbolTable = std::unordered_map<std::string, uintptr_t>;

    /**
     * @brief Registers a symbol (e.g., a function or global variable) with its memory address.
     * @param name The name of the symbol.
     * @param address The memory address (as a 64-bit integer) of the symbol.
     */
    void registerSymbol(const std::string& name, uintptr_t address);

    /**
     * @brief Retrieves the address of a symbol.
     * @param name The name of the symbol to look up.
     * @return The address of the symbol.
     * @throws std::runtime_error if the symbol is not found.
     */
    uintptr_t getSymbolAddress(const std::string& name) const;

    /**
     * @brief Gets a reference to the entire symbol table.
     * @return A const reference to the symbol table.
     */
    const SymbolTable& getSymbolTable() const;

    // --- Runtime Context for JIT-compiled Code ---
    // This structure's address will be loaded into the dedicated context register (x19)
    // for use by the compiled BCPL code, as per the ABI.
    struct RuntimeContext {
        // Pointers to C standard library functions
        FILE* (*c_fopen)(const char* filename, const char* mode);
        int (*c_fgetc)(FILE* stream);
        void (*c_wrch)(int character, FILE* stream);
        int (*c_fclose)(FILE* stream);
        void* (*c_malloc)(size_t size);
        void (*c_free)(void* ptr);
        void (*c_exit)(int status);

        // Pointers to the runtime's current I/O stream variables
        FILE** current_input_ptr;
        FILE** current_output_ptr;
    };

    /**
     * @brief Gets a pointer to the runtime context structure.
     * @return A pointer to the RuntimeContext, ready to be passed to JIT code.
     */
    RuntimeContext* getContext() {
        return &context;
    }

    // --- Public I/O Management ---
    // These are directly manipulated by the bcpl_* standard library functions.
    FILE* currentInputStream;
    FILE* currentOutputStream;

private:
    // --- Member Variables ---
    SymbolTable symbolTable;
    RuntimeContext context;

private:
    // --- Private Constructor for Singleton ---
    JitRuntime();
    ~JitRuntime();
};

// --- BCPL Standard Library Function Prototypes ---
// These are the C++ functions that implement the BCPL standard library.
// The JIT will compile calls to BCPL functions into calls to these C++ functions.
// Their addresses will be registered in the symbol table at startup.
// The 'extern "C"' is crucial for ensuring C-style name mangling and calling convention.
extern "C" {
    // I/O
    FILE* bcpl_findinput(JitRuntime* rt, const char* name);
    FILE* bcpl_findoutput(JitRuntime* rt, const char* name);
    void bcpl_selectinput(JitRuntime* rt, FILE* stream);
    void bcpl_selectoutput(JitRuntime* rt, FILE* stream);
    int bcpl_rdch(JitRuntime* rt);
    void bcpl_wrch(JitRuntime* rt, int ch);
    void bcpl_endread(JitRuntime* rt);
    void bcpl_endwrite(JitRuntime* rt);

    // System
    void bcpl_finish(JitRuntime* rt);
    void bcpl_stop(int n);
    
    // Memory
    uintptr_t bcpl_vec(int size_in_words);
    uintptr_t bcpl_unpack_string(const char* utf8_str);

    // Floating Point Conversions (as per BCPL float extension.md)
    double bcpl_float(int64_t n);
    int64_t bcpl_trunc(double f);
}

#endif // JIT_RUNTIME_H

