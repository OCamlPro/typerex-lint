<!--- OASIS_START --->
<!--- DO NOT EDIT (digest: db7599d6b3bc86c98e084a84a845e64d) --->

foo - test
==========

See the file [INSTALL.md](INSTALL.md) for building and installation
instructions.

Copyright and license
---------------------

foo is distributed under the terms of the Do What The F*ck You Want To Public
License with OCaml linking exception.

<!--- OASIS_STOP --->

## Syntax of the patches

The patches are represented as

- A header, only containing -- for now -- the list of th e free-variables (also called meta-variables) in the patch

- The patch body itself

The patch body is represented as a caml parsetree. The following extension nodes have a special meaning :

inside
:   means that the payload of the node can appear everywhere in the AST under the matched node

delete
:   designs parts of the patch that are to be removed

add
:   designs parts of the patch that aren't in the original AST, but are going to be added

In addition, free variables can be matched against any expression in the AST

## Patching process

The algo walks through the source AST, and for each node tries to match the patch.

To match the patch, the algo walks through both ASTs at the same time, if possible.

- If it encouters a matching leaf L of the patch for a certain substitution S, it returns the couple (S, L') where L' is the application of S to L

- If it encounters an extension node labelled "delete" or "add", it's still a TODO

  - Force deletes and adds to stay close and be valide expressions

  - Preprocess more smartly the patch to get rid of this <-- +1

If everything else fails, the algo stops
