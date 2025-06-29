// main.cpp
#include <iostream>
#include <fstream>
#include <string>
#include <set>
#include "Parser.h"
#include "CodeGenerator.h"
#include "JitRuntime.h"
#include "AST.h"
#include "DebugPrinter.h"

void printUsage(const char* programName) {
    std::cerr << "Usage: " << programName << " [options] <source_file.b>\n"
              << "Options:\n"
              << "  --debug     Print debug information (tokens and AST)\n"
              << "  --asm       Output generated assembly\n"
              << "  --help      Display this help message\n";
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printUsage(argv[0]);
        return 1;
    }

    // Parse command line arguments
    std::set<std::string> flags;
    std::string source_filename;

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "--help") {
            printUsage(argv[0]);
            return 0;
        }
        if (arg.rfind("--", 0) == 0) {
            flags.insert(arg);
        } else {
            source_filename = arg;
        }
    }

    if (source_filename.empty()) {
        std::cerr << "Error: No source file specified.\n";
        return 1;
    }

    // Read source file
    std::ifstream source_file(source_filename);
    if (!source_file) {
        std::cerr << "Error: Could not open source file '" << source_filename << "'\n";
        return 1;
    }

    std::string source_code(
        (std::istreambuf_iterator<char>(source_file)),
        std::istreambuf_iterator<char>()
    );

    try {
        std::cout << "=== BCPL Compiler ===\n";
        std::cout << "Source file: " << source_filename << "\n\n";

        // Parse source code
        std::cout << "Parsing...\n";
        ProgramPtr ast = Parser::getInstance().parse(source_code);
        std::cout << "Parsing complete.\n\n";

        // Print debug information if requested
        if (flags.count("--debug")) {
            std::cout << "=== Debug Information ===\n";
            DebugPrinter::getInstance().printTokens(source_code);
            DebugPrinter::getInstance().printAST(ast);

            std::cout << "\n";
        }

        // Generate code
        std::cout << "Generating code...\n";
        JitRuntime::getInstance().registerSymbol("bcpl_vec", (uintptr_t)bcpl_vec);
        JitRuntime::getInstance().registerSymbol("bcpl_unpack_string", (uintptr_t)bcpl_unpack_string);
        CodeGenerator codegen;
        codegen.compile(std::move(ast));
        std::cout << "Code generation complete.\n\n";

        // Print assembly if requested
        if (flags.count("--asm")) {
            std::cout << "=== Generated Assembly ===\n";
            codegen.printAsm();
            std::cout << "\n";
        }

        std::cout << "Compilation successful.\n";
        return 0;

    } catch (const std::exception& e) {
        std::cerr << "\n=== Compilation Failed ===\n";
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
}