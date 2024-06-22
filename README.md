# Scheme Interpreter

## Overview

This project contains an interpreter for the Scheme programming language. The interpreter consists of two main components: eval.rkt and parse.rkt.

- eval.rkt implements the evaluation logic of the interpreter.
- parse.rkt handles the parsing of Scheme expressions.

## Files

### eval.rkt

This file is responsible for evaluating Scheme expressions. It provides several key functions:

- **lookup**: Searches for a symbol in the environment and returns its value.
- **evaluate**: Evaluates an expression in a given environment.
- **special-form?**: Checks if an expression is a special form ('if', 'cond', or 'let').
- **evaluate-special-form**: Evaluates special forms like 'if', 'cond', and 'let'.
- **apply-function**: Applies a function to a list of arguments.

### parse.rkt

This file is responsible for parsing Scheme expressions from strings. It provides the main parsing functions and various helper functions.

#### Key Parsing Functions

- **N (Number)**: Parses a sequence of digits into a number.
- **D (Digit)**: Checks if the input character is a digit and converts it into its numeric value.
- **A (Symbolic Atom)**: Parses symbolic characters into symbolic atoms (e.g., variable names, function names).
- **E? (Expression)**: Checks if the input character can start an expression, which can be a digit, a symbolic atom, or an opening parenthesis.

## Usage

To use this interpreter, you can load the files in your Racket environment and call the 'evaluate' and 'parse' functions as needed. Here is an example of how to use these functions:

   racket
(require "eval.rkt")
(require "parse.rkt")

(define env '((x 10) (y 20)))

(define expr "(+ x y)")

(define parsed-expr (parse expr))

(evaluate parsed-expr env)
