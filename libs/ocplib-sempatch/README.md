# ocplib-sempatch

## Syntax of the patches

The patches are represented as

- A header, only containing -- for now -- the list of th e free-variables (also
  called meta-variables) in the patch

- The patch body itself

The patch body is represented as a caml parsetree. The following extension
nodes have a special meaning :

inside :   means that the payload of the node can appear everywhere in the AST
under the matched node

delete :   designs parts of the patch that are to be removed

add :   designs parts of the patch that aren't in the original AST, but are
going to be added

In addition, free variables can be matched against any expression in the AST

## Patching process

Each patch is converted to an automaton which is then matched against the
Parstree.
