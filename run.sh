#!/bin/bash
dune build ./day${1}.exe || exit
echo 'example:'
./_build/default/day${1}.exe ./examples/day${1}.txt
echo 'actual:'
./_build/default/day${1}.exe ./inputs/day${1}.txt
