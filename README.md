[![Travis-CI Build Status](https://travis-ci.org/OCamlPro/typerex-lint.svg?branch=master)](https://travis-ci.org/OCamlPro/typerex-lint)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/9b6liqyly6fcp3s0/branch/master?svg=true)](https://ci.appveyor.com/project/OCamlPro-Bozman/typerex-lint)

# The OCaml Style Checker

A style checker for [OCaml](http://ocaml.org/) language.

## Overview

The OCaml Style Checker is a tool for OCaml sources...

### Dependencies

`ocp-lint` is currently written for OCaml 4.02 and superior versions.

OPAM dependencies: ocp-build, menhir, yojson

You can use `make opam-deps` to install dependencies in the current switch.

### Build Instructions

Use the following instructions:
```
./configure
make
make install
```
to install `ocp-lint` on your system.

## Running
### Pre-commit hook
To use `ocp-lint` as a pre-commit hook, first compile and install `ocp-lint`:

    $ ./configure
    $ make
    $ make install

Then copy the file `scripts/pre-commit-lint`
in your `.git/hooks/` directory. The argument `--warn-error` is activated by
default.

This script will execute `ocp-lint` with the default configuration. You can also
create a `.ocplint` file in your project and configure it according to your needs.

## Configuration File

## Analyses

## Contributing

#### Bug Reports

If you have some bugs, you can submit a bug report or you can fork this
repository and make a pull request with a bug fix.

All contributions are welcome !

## License

The OCaml Style Checker is distributed under the terms of GNU Public
Licence version 3.0.
