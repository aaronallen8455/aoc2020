#!/bin/bash
stack build

cat ./input/$1 | stack exec aoc2020-exe -- $1 --rts-opts +RTS -s
