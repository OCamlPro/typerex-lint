#!/bin/sh

# Use ./gen-patch.sh to generate the patch

OLD_VERSION=4.03.0
VERSION=4.03.0
SOURCEDIR=${HOME}/BUILD/ocaml-${VERSION}

echo Applying patch lint-parsing-${OLD_VERSION}.patch into ${VERSION}.orig/

FILES="\
     parsing/ast_helper.ml \
     parsing/ast_iterator.ml \
     parsing/ast_iterator.mli \
     parsing/asttypes.mli \
     parsing/docstrings.ml \
     parsing/lexer.mll \
     parsing/location.ml \
     parsing/parse.ml \
     parsing/parse.mli \
     parsing/parser.mly \
     parsing/parsetree.mli \
     parsing/syntaxerr.ml"

rm -rf ${VERSION}.orig
mkdir ${VERSION}.orig
     
for file in ${FILES}; do
    basefile=$(basename ${file})
    module=$(echo $basefile | sed 's/./\U&/')
    cp ${SOURCEDIR}/${file} ${VERSION}.orig/lintParsing_${module}
done

cd ${VERSION}.orig
patch -p1 < ../lint-parsing-${OLD_VERSION}.patch
