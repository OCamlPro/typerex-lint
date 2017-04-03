[![Travis-CI Build Status](https://travis-ci.org/OCamlPro/typerex-lint.svg?branch=master)](https://travis-ci.org/OCamlPro/typerex-lint)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/9b6liqyly6fcp3s0/branch/master?svg=true)](https://ci.appveyor.com/project/OCamlPro-Bozman/typerex-lint)

# The OCaml Style Checker

A style checker for [OCaml](http://ocaml.org/) language.

## Overview

The OCaml Style Checker is a tool for OCaml sources. Its allows to
automatically check the conformance of your code to some coding
rules. `ocp-lint` is highly configurable and easily extensible.

### Dependencies

`ocp-lint` is currently written for OCaml 4.02 and superior versions.

OPAM dependencies: ocp-build, menhir, yojson

You can use `make opam-deps` to install dependencies in the current switch.

### Build Instructions

Use the following instructions:

    $ make opam-deps (if you are using OPAM)
    $ ./configure
    $ make
    $ make install

to compile and install `ocp-lint` on your system.

## Running

#### Easiest Way

You can just run `ocp-lint` in the home directory of your project, and
the tool will recursively scan sub-directories, analyze your source
files and report warning and/or errors. If you are using emacs, it's
better to run the command inside the editor to be able to quickly jump
to locations of reported errors and/or warnings.

#### Initialization, Global Configuration and Database

As explained above, `ocp-lint` is ready to work by just running one
command, and without arguments. No pre-configurations or additional
information are required to start using it. However, it may be better
to run `ocp-lint --init` once to:

* create a database (directory _olint) to store `ocp-lint` results and
  avoid useless computations when files are not modified,

* create a global configuration file (called .ocp-lint) that you can
  customize for your needs.

#### Pre-commit Hooks

If you are using Git, it is highly preferable to add a pre-commit hook
to prevent you from commiting source files for which `ocp-lint` reports
some warning and/or errors. To do that, create a file
`.git/hooks/pre-commit` (with execution premissions) with the following content:

```
#!/bin/sh

LINT=ocp-lint # or the exact path to ocp-lint

$LINT --warn-error --path <absolute/path/to/your/project>

if [ "$?" = 0 ]; then
    exit 0
else
    echo "\n/!\\ ocp-lint: please fix the warnings before commiting./!\\"
    exit 1
fi
```

## Configuration File

TODO: give some more details

## Available Analyses

TODO: give some more details

## Contributing

#### Bug Reports

If you have some bugs, you can submit a bug report or you can fork this
repository and make a pull request with a bug fix.

All contributions are welcome !

## Licensing, Support and Dev on Demand

The OCaml Style Checker is a free-software. It is distributed under
the terms of GNU Public Licence version 3.0. However, to keep to
project alive, don't hesitate to support our development and/or
maintenance. We can also develop additional plugins for your
particular needs.
