#!/bin/sh

export OPAMYES=1 OPAMVERBOSE=1
eval `opam config env`

echo Architecture
uname -a
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version



./configure
make

# Do not try to install in /usr/lib
if [ "${OCAML_VERSION}" != "system" ] ; then

# The next line will modify the cached .opam directory, but
# ocp-build is supposed to correctly handle this situation,
# where a previous version is already installed
  make install
fi
