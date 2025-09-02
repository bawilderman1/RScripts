# Testing Documentation

This document outlines the process for compiling and running the R and C++ test suites for the backtesting engine manually from the command line.

## 1. Prerequisites

To run the tests, you will need the following software installed and available in your system's PATH:

*   **R:** The R language environment.
*   **A C++ Compiler:** A modern C++ compiler that supports C++17, such as g++ (included with RTools for Windows or available via MinGW/MSYS2) or Clang.

## 2. R Test Suite

The R test suite uses the `testthat` framework and is located in the `handler_test.R` file. These tests focus on the integration between R and C++, ensuring that data is passed correctly to the C++ engine and that the results are returned to R as expected.

### Running the R Tests

To run the R test suite, open your console or terminal, navigate to the project's root directory, and execute the following command:

```sh
Rscript .\handler_test.R
```

This command will:
1.  Start an R script session.
2.  Load the necessary libraries (`testthat`, `Rcpp`).
3.  Compile the C++ code in `BacktestHandler.cpp` and `BacktestEngine.cpp` on the fly.
4.  Execute all the test cases defined in the `handler_test.R` file.
5.  Print the results of the tests to the console.

## 3. C++ Unit Tests

The C++ unit tests use the Catch2 testing framework and are located in the `engine_tests.cpp` file. These tests focus on the internal logic of the C++ backtesting engine itself, verifying the correctness of individual functions and calculations in isolation.

### Compiling the C++ Tests

To compile the C++ tests, you first need to create a test executable. From the project's root directory, run the following command:

```sh
g++ -std=c++17 -I. -o engine_tests.exe engine_tests.cpp BacktestEngine.cpp
```

Let's break down this command:
*   `g++`: Invokes the g++ compiler. You may need to substitute this with your compiler's command, such as `clang++`.
*   `-std=c++17`: Specifies that the code should be compiled using the C++17 standard.
*   `-I.`: Tells the compiler to look for included header files (`catch.hpp`, `BacktestEngine.h`) in the current directory.
*   `-o engine_tests.exe`: Specifies that the output file should be named `engine_tests.exe`.
*   `engine_tests.cpp BacktestEngine.cpp`: The source files to be compiled.

### Running the C++ Tests

Once the compilation is successful, you will have an `engine_tests.exe` file in your directory. To run the tests, simply execute this file:

```sh
.\engine_tests.exe
```

Catch2 will automatically discover and run all the `TEST_CASE` blocks defined in `engine_tests.cpp` and report the results to the console.
