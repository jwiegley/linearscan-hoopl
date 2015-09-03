module Programs.Reservation4 where

import Assembly
import LinearScan.Hoopl.DSL

reservation4 :: Program (Node IRVar)
reservation4 = do
    label "L1" $ do
        call 0
        lc v7
        branch v4 "L4" "L8"

    label "L2" $ do
        move v1 v2
        call 5
        nop
        move v0 v1
        move v3 v5
        move v0 v2
        lc v1
        move v3 v8
        add v4 v1 v4
        copy v5 v4
        call 4
        move v1 v8
        branch v0 "L4" "L4"

    label "L3" $ do
        branch v3 "L1" "L8"

    label "L4" $ do
        copy v0 v4
        copy v7 v2
        move v6 v8
        nop
        offp v7 v8 v6
        lc v8
        branch v7 "L2" "L6"

    label "entry" $ do
        copy v3 v4
        copy v6 v5
        branch v6 "L2" "L4"

    label "L6" $ do
        lc v8
        copy v6 v7
        branch v3 "L3" "L2"

    label "L7" $ do
        return_

    label "L8" $ do
        call 2
        return_
