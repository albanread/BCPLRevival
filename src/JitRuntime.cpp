#include "JitRuntime.h"
#include <stdexcept>
#include <cstring>

// Private Constructor: Initialize runtime context and I/O streams
JitRuntime::JitRuntime() : currentInputStream(stdin), currentOutputStream(stdout) {
    // Initialize the runtime context with C standard library functions
    context.c_fopen = fopen;
    context.c_fgetc = fgetc;
    context.c_wrch = [](int ch, FILE* stream) { fputc(ch, stream); };
    context.c_fclose = fclose;
    context.c_malloc = malloc;
    context.c_free = free;
    context.c_exit = exit;

    // Set up pointers to I/O streams
    context.current_input_ptr = &currentInputStream;
    context.current_output_ptr = &currentOutputStream;
}

JitRuntime::~JitRuntime() {
    // Close any open files (except stdin/stdout)
    for (auto& pair : symbolTable) {
        if (pair.second != reinterpret_cast<uintptr_t>(stdin) &&
            pair.second != reinterpret_cast<uintptr_t>(stdout)) {
            // Check if this is a FILE pointer
            if (pair.first.find("file_") == 0) {
                FILE* fp = reinterpret_cast<FILE*>(pair.second);
                if (fp) fclose(fp);
            }
        }
    }
}

void JitRuntime::registerSymbol(const std::string& name, uintptr_t address) {
    symbolTable[name] = address;
}

uintptr_t JitRuntime::getSymbolAddress(const std::string& name) const {
    auto it = symbolTable.find(name);
    if (it == symbolTable.end()) {
        throw std::runtime_error("Symbol not found: " + name);
    }
    return it->second;
}

const JitRuntime::SymbolTable& JitRuntime::getSymbolTable() const {
    return symbolTable;
}

// BCPL Standard Library Implementation

extern "C" {

FILE* bcpl_findinput(JitRuntime* rt, const char* name) {
    FILE* fp = rt->getContext()->c_fopen(name, "r");
    if (!fp) return nullptr;
    return fp;
}

FILE* bcpl_findoutput(JitRuntime* rt, const char* name) {
    FILE* fp = rt->getContext()->c_fopen(name, "w");
    if (!fp) return nullptr;
    return fp;
}

void bcpl_selectinput(JitRuntime* rt, FILE* stream) {
    if (stream) {
        rt->currentInputStream = stream;
    }
}

void bcpl_selectoutput(JitRuntime* rt, FILE* stream) {
    if (stream) {
        rt->currentOutputStream = stream;
    }
}

int bcpl_rdch(JitRuntime* rt) {
    return rt->getContext()->c_fgetc(rt->currentInputStream);
}

void bcpl_wrch(JitRuntime* rt, int ch) {
    rt->getContext()->c_wrch(ch, rt->currentOutputStream);
}

void bcpl_endread(JitRuntime* rt) {
    if (rt->currentInputStream && rt->currentInputStream != stdin) {
        rt->getContext()->c_fclose(rt->currentInputStream);
        rt->currentInputStream = stdin;
    }
}

void bcpl_endwrite(JitRuntime* rt) {
    if (rt->currentOutputStream && rt->currentOutputStream != stdout) {
        rt->getContext()->c_fclose(rt->currentOutputStream);
        rt->currentOutputStream = stdout;
    }
}

void bcpl_finish(JitRuntime* rt) {
    rt->getContext()->c_exit(0);
}

void bcpl_stop(int n) {
    exit(n);
}

uintptr_t bcpl_vec(int size_in_words) {
    // Allocate memory for the vector (64-bit aligned)
    void* mem = aligned_alloc(8, size_in_words * sizeof(int64_t));
    if (!mem) {
        throw std::runtime_error("Failed to allocate vector");
    }
    // Initialize to zero
    memset(mem, 0, size_in_words * sizeof(int64_t));
    return reinterpret_cast<uintptr_t>(mem);
}

uintptr_t bcpl_unpack_string(const char* utf8_str) {
    size_t len = strlen(utf8_str);
    // Allocate memory for the 32-bit string, plus a 32-bit null terminator
    uint32_t* wide_str = (uint32_t*)malloc((len + 1) * sizeof(uint32_t));
    if (!wide_str) {
        throw std::runtime_error("Failed to allocate memory for string");
    }

    for (size_t i = 0; i < len; ++i) {
        wide_str[i] = (uint32_t)utf8_str[i];
    }
    wide_str[len] = 0; // Null terminator

    return reinterpret_cast<uintptr_t>(wide_str);
}

// Floating point operations
double bcpl_float(int64_t n) {
    return static_cast<double>(n);
}

int64_t bcpl_trunc(double f) {
    return static_cast<int64_t>(f);
}

} // extern "C"