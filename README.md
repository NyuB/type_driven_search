# type_driven_search

Type driven search tools for C/C++

## Development

- run the tests: `make test`
- update the tests' expectations: `make test-promote`
- format source code: `make fmt`

## Installation

Run `make install` to build and copy `type_driven_search` to your system. Installation folder is specified with the variable `INSTALL_ROOT` (default to `~/bin`), e.g.

`make install INSTALL_ROOT=/usr/custom/bin`

## TODO

- improve performance of file-based index while keeping the naive "search all matching signature" approach
Results evolution
