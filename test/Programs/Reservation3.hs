module Programs.Reservation3 where

import Assembly
import LinearScan.Hoopl.DSL

reservation3 :: Program (Node IRVar)
reservation3 = do
    label "L1" $ do
        copy v4 v7
        move v7 v6
        move v5 v6
        add v2 v0 v3
        move v0 v7
        lc v3
        move v0 v3
        call 2
        offlpi v5
        copy v6 v2
        add v2 v5 v5
        lc v3
        copy v3 v0
        jump "L7"

    label "L2" $ do
        move v5 v2
        move v3 v7
        lc v6
        lc v7
        offlpi v2
        copy v3 v0
        lc v7
        add v2 v2 v3
        lc v1
        branch v1 "L7" "L2"

    label "L3" $ do
        lc v7
        branch v4 "L7" "L5"

    label "entry" $ do
        branch v4 "L7" "L1"

    label "L5" $ do
        lc v2
        lc v1
        move v6 v5
        lc v6
        move v4 v0
        lc v1
        branch v1 "L1" "L6"

    label "L6" $ do
        offp v4 v2 v4
        jump "L6"

    label "L7" $ do
        move v0 v2
        move v1 v0
        call 1
        branch v7 "L1" "L3"
