# BCPL Revival

## Exploring BCPL

I like **Scheme** and **Forth**, so naturally, I have an appreciation for **BCPL**.

**BCPL** (Basic Combined Programming Language) holds both cultural and historical significance in the development of programming languages. Moreover, it is still actively maintained.

Martin Richards, the creator of BCPL, maintains an actively developed and platform-independent version of the language. You can visit his website for more details: [Martin Richards' BCPL Page](https://www.cl.cam.ac.uk/~mr10/).

If you are seriously into BCPL you might go back the source there.


### BCPL and Me

I first found the BCPL manual in a College of Technology library, and was fascinated by the language, At one point I had a 68K micro-computer running TRIPOS complete with BCPL and associated command line tools, sadly that TRIPOS computer eventually died. 

Far later I learned of the TRIPOS relationship with the Amiga, which is a huge but indirect topic.

### My Non goals

I have no plan to support some existing BCPL code base, I am more interested in the typeless high level assembler
spirit of the language, I would like a `BCPL like` computer language for my modern systems, idealy I would like to have a kind of 'Turbo-BCPL' with a text IDE, for writing and running relatively small programs, if that makes sense.

So what I am up to here, will never be commercially useful.

---

### A Wish for Modern BCPL Compilation

It would be exciting to have a **BCPL compiler** that directly generates modern, efficient **x86_64** and **ARM64** code. Such a tool would make BCPL more accessible and enjoyable for programmers today.

Unfortunately, even with the power of modern **LLM (Large Language Model) systems**, building a lexer, parser, AST, and code generator for BCPL has proven difficult. Achieving this remains a challenging task—one worth exploring.

Wouldn’t this make for an excellent challenge, **OpenAI** or **Google**? As of now, I don't believe these systems have demonstrated the capability to independently take a specification, apply existing compiler development literature, and fully implement a BCPL compiler.

Frankly, this is a reminder that **LLMs are not programmers**. While they offer great assistance, expecting them to autonomously build a full-fledged compiler is a step too far in their current form.

At one point, I likened an uncooperative LLM to a coworker who would deserve a cup of water thrown on their head for getting stuck at this point!

**Takeaway:** LLMs can augment human effort in programming but cannot replace training and fostering skilled programmers.

---
## My Attempt to work with Google Gemini to build a BCPL JIT compiler in rust.

The LLM suggested rust, I find rust more difficult than C personally, although it is good at catching mistakes and incomplete code, my gut feeling is the lexer and parser will be the easy part.

Well so far we have got to the Semantic analyser, and I am currently a bit stuck on that :) 

Code generation is going to be interesting ...

I am going to base the execution model on the model described by cintcode.

The nice thing about Gemini, is that in contrast to ChatGPT and Claude, it is far less inclined to
throw my existing code away when it makes a change, a habit that is incredibly annoying.

Gemini Pro are very tight fisted on LLM time though, and I am not paying hundreds for credits.


---

## Building a Modern BCPL Compiler

To create a modern BCPL compiler, here are a few potential approaches worth considering:

### 1. Using Martin Richards’ Implementation
Martin Richards’ version of BCPL is actively maintained, platform-independent, and installable on macOS. Start by visiting his [website](https://www.cl.cam.ac.uk/~mr10/) for setup instructions.

### 2. Developing a GCC Front End
Creating a GCC front end for BCPL provides access to a mature, multi-platform compiler infrastructure. This involves:
- Implementing a **parser** for BCPL syntax
- Generating an **intermediate representation (IR)** compatible with GCC
- Leveraging GCC’s **optimization and code generation** framework

This would allow legacy BCPL users to benefit from GCC's robust infrastructure.

### 3. Employing a Virtual Machine Approach
Run BCPL code within a simulated environment:
- Install a virtual machine emulator like **SIMH**
- Configure a **DEC PDP-10** emulation
- Install and run BCPL on the emulated system

This approach prioritizes preserving BCPL’s historical environment.

### 4. Compiling from Source
The BCPL system is often **self-hosting**, meaning:
- Download the BCPL source code from Martin Richards’ website.
- Set up a **minimal build environment** on your machine.
- Compile the BCPL compiler using an existing **C or C++ compiler**.
- Use the resulting compiler to recompile itself.

---

### Practical Advice for Getting Started
- Begin with **Martin Richards’ implementation**, as it is modern and reliable for current systems.
- Familiarize yourself with BCPL’s **language features** and **syntax**.
- Use tools like the **BCPL Cintcode System**, which includes an interpretive implementation with a command-line interface and debugger.
- For developers focused on performance, explore options like **native code generation** or creating a **GCC front end**.

**Note**: BCPL is a minimalist systems programming language. While it lacks modern features, it provides flexibility for low-level operations.

---

## Exploring Lexical Analysis (Lexer)

The **lexer**, or **scanner**, is the first phase of the compilation process. It converts the program's **character stream** into a **token stream**.

### Objectives of a Lexer
- **Identify Tokens**: Extract individual units like keywords, identifiers, operators, literals, and punctuation.
- **Classify Tokens**: Assign types to each token (e.g., integer literal, identifier, operator).
- **Ignore Comments/Whitespace**: Remove irrelevant parts of the input while preserving critical information.
- **Error Detection**: Detect invalid characters, unterminated literals, etc.

### Example
Consider this BCPL program:
```bcpl
GET x.
```
The lexer would produce the following tokens:
- `KEYWORD("GET")`
- `IDENTIFIER("x")`

### Implementing a Lexer
Approaches include:
- **Finite Automata**: Custom built or automatically generated lexers.
- **Tools like Flex**: Generate lexers using regular expressions.
- **Manual Lexers**: For smaller languages like BCPL, simple hand-coded lexers work well.

---

## Syntax Analysis (Parsing)

The **parser** processes tokens, ensuring they adhere to the grammar rules of BCPL and organizing them into an **Abstract Syntax Tree (AST)**.

### Objectives
- **Grammar Validation**: Verify the syntax against BCPL’s grammar (e.g., improper use of operators, missing semicolons).
- **AST Construction**: Build a tree-like structure that represents the program’s code.
- **Error Detection**: Identify and report syntax errors.

### AST Example
Consider this token sequence:
```bcpl
x = 10 + y;
```
The parser generates an AST resembling:
```
Assignment
├── Identifier: x
└── Expression
    ├── IntegerLiteral: 10
    ├── PlusOperator: +
    └── Identifier: y
```

---

## Intermediate Representation (IR)

After parsing, the compiler generates a lower-level **Intermediate Representation (IR)** from the AST.

### Purpose of IR
- Simplifies transitions between high-level code and machine code.
- Optimizations are easier to apply compared to working on raw ASTs.

### Example IR (Three-Address Code)
Input: `x = 10 + y`
Output:
```
t1 = 10
t2 = y
t3 = t1 + t2
x = t3
```

---

## Compiler Optimization and Code Generation

### Optimization
The goal is to improve IR performance via optimizations like:
- **Constant Folding**: Replace constant expressions with precomputed values.
- **Dead Code Elimination**: Remove unnecessary instructions.
- **Loop Optimization**: Enhance loop performance through unrolling or other techniques.

### Code Generation
Finally, machine-specific **code generation** translates optimized IR into executable assembly/machine code.

For example, the IR above might generate `x86-64` assembly:
```assembly
mov rax, 10       ; Load 10 into register
mov rbx, y        ; Load y’s value into register
add rax, rbx      ; Add rax and rbx
mov x, rax        ; Store the result in x
```

---

## Challenges with BCPL

### Lexer Challenges
- **String handling**: Manage escape characters correctly (e.g., handling `"\"text\""`).
- **Identifier Rules**: Ensure BCPL's identifier rules are followed (e.g., letters or underscores to start).

### Parser Challenges
- **Expression Parsing**: Correctly handle precedence and associativity (e.g., `*` vs. `+`).
- **VALOF Blocks**: BCPL’s unique block constructs add complexity.

---

## Observations About BCPL Cintcode and OCODE

- **Cintcode** and **OCODE** are BCPL’s **Intermediate Representations** and serve as bridges between parsing and machine code.
- These stack-based, machine-independent IRs allow BCPL to run across multiple platforms, albeit with some performance trade-offs.

---

## Conclusion

Building a modern BCPL compiler is a challenge worth pursuing! While tools like LLMs can assist in creating parts of the compiler (e.g., lexer, parser), they cannot yet replace human expertise for tasks like semantic analysis, advanced optimization, or target-specific code generation.

BCPL continues to be a fascinating historical language with opportunities for revival in modern programming. Who knows—maybe a **BCPL renaissance** is just around the corner!
