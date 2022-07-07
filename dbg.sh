#!/bin/fish
env RUST_BACKTRACE=1 cargo with "gdb --args {bin} {args}" -- run -- -Cthreads 20 -Dtree -s $argv[1]
