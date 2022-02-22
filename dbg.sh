#!/bin/fish
env RUST_BACKTRACE=1 cargo with "gdb --args {bin} {args}" -- run -- -Cthreads 20 -Dtree -i examples/expr_4.rsh
