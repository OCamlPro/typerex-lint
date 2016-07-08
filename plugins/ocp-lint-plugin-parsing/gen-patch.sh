#!/bin/sh

VERSION=4.03.0
SOURCEDIR=${HOME}/BUILD/ocaml-${VERSION}
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

rm -rf ${VERSION}.orig ${VERSION}.new
mkdir ${VERSION}.orig
mkdir ${VERSION}.new
     
for file in ${FILES}; do
    echo ${file}
    basefile=$(basename ${file})
    module=$(echo $basefile | sed 's/./\U&/')
    echo ${module}
    cp ${SOURCEDIR}/${file} ${VERSION}.orig/lintParsing_${module}
    cp lintParsing_${module} ${VERSION}.new/
done

diff -w -b -C 2 ${VERSION}.orig ${VERSION}.new > lint-parsing-${VERSION}.patch

rm -rf ${VERSION}.new ${VERSION}.old
cat  lint-parsing-${VERSION}.patch
