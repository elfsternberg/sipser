![Language: Haskell](https://img.shields.io/badge/language-Haskell-yellowgreen.svg)
![Language: Rust](https://img.shields.io/badge/language-Rust-green.svg)
![Topic: Academic](https://img.shields.io/badge/topic-Academic-red.svg)
![Topic: Regular Expressions](https://img.shields.io/badge/topic-Regular_Expressions-red.svg)
![Status: In Progress](https://img.shields.io/badge/status-In_Progress-yellow.svg)

# Sipser: Introduction to the Theory of Computation, Chapters 1 and 2

I realized that as I've been working on the
[riggedregex](https://github.com/elfsternberg/riggedregex/) that there
is one thing I've truly neglected: linking the mechanical operation of
the finite automata that started the investigation into regular
expressions with their mathematical representations as played out with
Brzozowski and Glushkov.  I don't have an intuitional understanding of
how one leaps from one to the other.

To close that gap, I've decided to try and work my way through the first
two chapters of Sipser's book, the ones that deal with finite and
pushdown automata, which are the mathematical principles at the heart of
regular and recursive regular expressions.

My plan, such as it is, is to implement the following:

- [A Simple DFA in Haskell](./haskell/SimpleDfa)
- [A Simple NFA in Haskell](./haskell/SimpleNfa)
- [A Simple DFA in Rust](./rust/01_simpledfa)
- A Simple NFA in Rust
- NFA Closure under union, in Haskell
- NFA Closure under concatenation, in Haskell
- Converting an NFA into a DFA
- Converting a DFA into an NFA
- Converting a Kleene regular expression into an NFA
- Converting a Kleene regular expression into a DFA
- Converting a DFA into a Kleene regular expression via GNFA

I have not yet put much thought into what from chapter 2 will be
realized.

## LICENSE 

Copyright [Elf M. Sternberg](https://elfsternberg.com) (c) 2019, and
licensed with the Mozilla Public License vers. 2.0.  A copy of the
license file is included in the root folder.
