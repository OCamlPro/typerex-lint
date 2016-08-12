#!/bin/bash
TESTDIR='/tests'
CFG='../../../testsuite/.ocplint'
for i in `ls`;
do
  if [ -d  "$i$TESTDIR" ]; then
    cd $i$TESTDIR;
    cp $CFG .;
    rm -rf _olint/*;
    for j in `ls`;
    do
      if [ -d  "$j" ]; then
          if [ "$j" != "_olint" ]; then
            if [ "$i" != "ocp-lint-plugin-sempatch" ]; then
                rm -rf ../_olint/*;
                rm -rf $j/ocp-lint.result;
                ../../../_obuild/ocp-lint/ocp-lint.asm --path $j;
                cp -rf _olint/ $j/ocp-lint.result;
            else
              if [ -e "$j/sempatch.md" ]; then
                  rm -rf ../_olint/*;
                  rm -rf $j/ocp-lint.result;
                  OCPLINT_PATCHES=$j/ ../../../_obuild/ocp-lint/ocp-lint.asm --path $j;
                  cp -rf _olint/ $j/ocp-lint.result;
              else
                  rm -rf ../_olint/*;
                  rm -rf $j/ocp-lint.result;
                  ../../../_obuild/ocp-lint/ocp-lint.asm --path $j;
                  cp -rf _olint/ $j/ocp-lint.result;
              fi
            fi
          fi
      fi
    done;
    cd -;
  fi
done
