#!/usr/bin/perl

$counter = 1;

while (<>) {
    if (/before allocation/) {
        close(FILE);
        open(FILE, "> /tmp/test$counter.hs");
        print FILE <<EOF;
module Programs.Test$counter where

import Assembly
import LinearScan.Hoopl.DSL

test$counter :: Program (Node IRVar)
test$counter = do
EOF
    }

    if (/before allocation/ .. /^Unhandled/) {
        unless (/before allocation/ ||
                /^(Unhandled|Block|<)/) {
            s/^[0-9]+: //;
            print FILE $_;
        }
    }
}
