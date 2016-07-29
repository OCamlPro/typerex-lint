# The Automaton Builder Constructor

Abc is a small ad-hoc ppx to build the automaton used by sempatch.

It provides several extension nodes that are replaced by some code generated
from the Parsetree type definition

Important files here are :

- abc.ml : The core of the program

- generator.ml : A functor used by almost every generator. It doesn't help
  factorizing much code (maybe this could be improved), but it force every
  generator to have more or less the same structure)

- abc_common.ml : Some utility functions used by the rest

Every other file define a transformation from the parsetree type definition to
a piece of Parsetree.

## TODO

The automaton is almost fully generated.

Remaining stuff to generate are the evaluator (see the commented lines at the
end of ../automaton.ml to see what it is supposed to look like), and (not
mandatory), a pretty-printer to the dot format which can be helpful for
debugging (see ../export.ml).

After that, we should get a engine capable of computing structural identity on
two ASTs.  To get something useful, some nodes have to be treated specially
when building the automaton, like the `location` nodes, and the ones which have
a special meaning for ocplib-sempatch.
