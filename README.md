# ssft19-ai

# Intro

## Purpose
Understand and experiment with
- a strong parallel between the static analysis definition and its implementation
- how an abstract interpreter in a transitional style is constructed
- step-by-step process of an abstract interpreter
- different abstract domains and semantic operations
- Sparrow: https://github.com/ropas/sparrow/ (on Thursday)

## Note
- written in simple OCaml (avoiding functors, modules, etc)
- not optimized for efficiency

# Quick Build & Usage
```sh
$ sudo apt-get install ocaml
$ git clone https://github.com/ropas/ssft19-ai
$ cd ssft19-ai
$ make
$ ./run -pponly ex0-parse-test
$ ./run ex1a-val-seq
```

`./run [-pponly] [target program]`

# Overview

Takes input program -> parse/label -> abstract interpretation
- `main.ml`
- `parser.mly`, `lexer.mll` : for parsing
- `sil.ml`, `sill.ml` : (labelled) target language
- `val.ml`, `mem.ml` : abstract value & memory domains
- `ai.ml` : abstract interpreter (core analyzer)

## Target language
Simple imperative language (`pgm` in `sil.ml`, eaxmples in `ex*`)

## Abstract domains

### Value
- sign, const (const branch), interval (*exercise*, will cover on Thursday)
- replace `val.ml` with your own value abstraction, then modify `labels_of_val` and `cond` in `ai.ml` accordingly

### Memory
- variable -> value (`Mem.t`)

### State
- label -> memory (`Ai.state`)


# Examples

- `ex0*`: parse & label
- `ex1*`: value
- `ex2*`: while
- `ex3*`: goto

# Exercises
- Add zero to the sign domain: `val.t` as `BOT | TOP | POS | ZERO | NEG`
- Implement abstract value as constants (in const branch)
- Add refinements in (`sat*` in `val.ml` & `cond` in `ai.ml`)
- Implement abstract value as intervals, and add widening fuction for the domain in `ai.ml`

# References

- <i>Static Analysis: an Abstract Interpretation Perspective</i>, Yi and Rival, MIT Press, 2019 (to be published)
- http://fm.csl.sri.com/SSFT19 (slides & supplementary note under Kwangkeun Yi)
