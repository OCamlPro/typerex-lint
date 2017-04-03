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

OPAM dependencies: ocp-build, menhir, yojson, ppx_tools

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

You can just run `ocp-int` in the home directory of your project, and
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

$LINT --warn-error --path <absolute/path/to/your/project_source>

if [ "$?" = 0 ]; then
    exit 0
else
    echo "\n/!\\ ocp-lint: please fix the warnings before commiting./!\\"
    exit 1
fi
```

## Options

ocp-lint comes will a large set of options. With those options, you can tweek
the behavior of ocp-lint or configure the plugins that offers options.
In order to use those options, you either use the command-line or the
configuration file.
One thing to note is that the command-line have the priority over the
configuration file.

### Command-line options

#### Kernel arguments

ocp-lint offers a lot of options to change the way it runs. Here some examples:

* --path DIR: Give a project dir path

* --list: List of every plugins and linters

#### Plugin arguments

Each plugin comes with a set of options:

* --plugin-name.linter-name.warnings:
   Enable/Disable warnings from "Checks on module type name."

* --{enable,disable}-plugin-name.linter-name:
   Enable/Disable linter "Checks on module type name."

To those options the author of the plugins can add his own options. Eg:

* --plugin-text.code-length.max-line-length:
   allows to choose the maximum line length (default value: 80)

* --plugin-parsetree.code-identifier-length.min-identifier-length:
   allows to choose the minimum identifier length (default value:2)

### Configuration file

#### General use

ocp-lint allows you to manage the options in a configuration file.
This file is .ocplint.
You can generate it with the --save-config argument.

This file has the following format :
```
option_name = value
plugin1_name = {
  linter1_name = {
    option1_name = value
    option2_name = value
  }
  linter2_name = { .. }
}
plugin2_name = { .. }
```
ocp-lint will look for a .ocplint where you run it.

### Multi config files

You can use several configuration files in your source in order to manage
the options more accurely.
If you use an .ocplint in the subdir, the configuration will be use only for the
sources in this dir.
If you use 2 .ocplint files, one at the root of your project and one in a deep
subdir, then ocp-lint will use the root .ocplint for all sources except for the
source in the subdir. The sources in the subdir will options that result after
merging the 2 .ocplint.

## Available Analyses

You can check the generated documentation [here](https://www.typerex.org/ocp-lint).

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