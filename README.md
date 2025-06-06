# SPL Compiler Project

## Overview

This project implements a full compiler pipeline for SPL (Simple Programming Language), including lexical analysis, parsing, abstract syntax tree (AST) generation, type checking, and intermediate representation (IR) code generation. The compiler was developed using JFlex for scanning and CUP for parsing, and integrates Scala and Java code to generate and process SPL programs.

## Technologies Used

* **JFlex** – Lexical Analyzer Generator
* **CUP** – LALR Parser Generator
* **Scala & Java** – For integration and processing
* **Maven** – Build tool
* **SPL Framework** – Provided educational compiler framework

## Features Implemented

### 1. **Scanner (spl.lex)**

* Constructed a scanner using JFlex to recognize SPL tokens.
* Recognized keywords, identifiers, operators, literals, delimiters, and comments.
* Verified output by comparing with reference scanner using test files in `/tests`.

### 2. **Parser (spl.cup)**

* Built a CUP parser with correct SPL grammar.
* Supported all SPL syntactic constructs including functions, procedures, expressions, statements, arrays, records, and tuples.
* Resolved shift/reduce conflicts using precedence and associativity.

### 3. **AST Generation**

* Added semantic actions to the CUP grammar to construct ASTs.
* Used case classes from `AST.scala` to represent program structure.
* Confirmed AST correctness by comparing against reference output.

### 4. **Type Checking (TypeCheck.scala)**

* Implemented type rules for expressions, declarations, and statements.
* Integrated a symbol table for scope and type checking.
* Enforced SPL typing rules including scope restrictions in loops and name equivalence.
* Output includes trace of type checking and error messages.

### 5. **Intermediate Representation (IR) Generation (Code.scala)**

* Translated AST into low-level IR code defined in `IR.scala`.
* Handled code generation for all constructs: expressions, control flow, function/procedure calls, arrays, and records.
* Managed stack frame offsets, static links, heap allocation, and system calls.
* Output matches reference IR structure from solution.

## How to Run
### Compile

```bash
mvn clean install
```


## Test Cases

The compiler was validated against a suite of SPL programs located in the `tests/` directory. Each file demonstrates different language features and edge cases. You can run individual tests or the entire suite.

### Available Test Cases

- `2d_array`
- `8q` 
- `factorial`
- `fib` 
- `hanoi` 
- `hello` 
- `prime`
- `sort`
- `square`
- `test1`
- `test2`
- `test3`
- `test4`
- `tree`
- `tsort`

###  Run a Single Test

```bash
scala lib/spl.jar <task_number> tests/factorial.spl
```
### Run All Tests
```bash
scala lib/spl.jar <task_number> tests/*.spl
```
Replace <task_number> with:

1 – Scanner

2 – Parser

3 – AST Generator

4 – Type Checker

5 – IR Generator

### Compare With Reference Solution
```bash
scala spl-solution.jar <task_number> tests/<test_case>.spl
```


## Directory Structure

```
spl/
├── src/
│   └── main/scala/edu/uta/spl/
│       ├── AST.scala
│       ├── Code.scala
│       ├── IR.scala
│       ├── SPL.scala
│       ├── SymbolTable.scala
│       ├── TypeCheck.scala
│       ├── spl.cup
│       └── spl.lex
├── tests/    # SPL test files (*.spl)
└── pom.xml   # Maven configuration
```

