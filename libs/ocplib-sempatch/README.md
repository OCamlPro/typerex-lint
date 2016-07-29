# ocplib-sempatch

A semantic patches library

## Overview

`ocplib-sempatch` is a library which allows you to detect and replace specific
constructs in your code, using so called "semantic patches"

## Patching process

Each patch is converted to an automaton which is then matched against the
Parstree.

The structure of the automaton is generated during the compilation from the
definition of the Parsetree, thanks to the (really ad-hoc) ppx "abc" (in
lib/automaton/generator). This ppx also generates the functions used to
convert the patch into an automaton and to evaluate that automaton.

When the automaton is done running, the matches are filtered through the guard,
which is a boolean expression taking as input the metavariables of the match
(it is used to add some conditions which can't be expressed in the automaton,
like the equalilty of two variables).

## TODO

(At least the most important things)

### Rewrite the automaton

The current automaton has two flaws :

- It is a top-down automaton, which makes getting the AST after transformation
  quite hard for some reasons

- It has a ugly design and needs to be rewritten if we want to be able to optimize it.

A rewrite has begun
[here](https://github.com/ocamlpro-Hufschmitt/typerex-lint/tree/bup/libs/ocplib-sempatch/lib/automaton),
but the generation from the Parsetree has never been done.

(In previous link, the toplevel dir is a toy automaton on a simple tree, and
the "abc" dir contains the begining of the program supposed to generate the
corresponding automaton for the parsetree).

### Allow patching more than expressions and structures

For arbitrary reasons, the library only allows modifying expressions or
structures (in the sense of the type Parsetree.structure).

This is absurd as everything is here to allows patching every element of the
AST, but changing it would need to rethink a little the API.

### Pretty-print the modified AST

This could be quite useful, but a rewrite of the automaton is probably needed
to get the full modified AST.

### Extend the grammar and clarify it

A lot of things to do...
