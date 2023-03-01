#!/bin/fish
env RUST_BACKTRACE=1 RUST_LOG=DEBUG cargo with "lldb {bin} -- {args}" -- run -- -Cthreads 20 -Dtree -i $1
