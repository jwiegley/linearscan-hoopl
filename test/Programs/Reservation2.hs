module Programs.Reservation2 where

import Assembly
import LinearScan.Hoopl.DSL

reservation2 :: Program (Node IRVar)
reservation2 = do
    label "entry" $ do
        move v8 v2
        move v0 v4
        branch v1 "L11" "L10"

    label "L10" $ do
        jump "L5"

    label "L11" $ do
        jump "L4"

    label "L4" $ do
        call 3
        add v2 v5 v0
        lc v3
        add v4 v3 v2
        add v4 v1 v3
        move v1 v3
        lc v5
        move v6 v6
        jump "L5"

    label "L5" $ do
        lc v2
        jump "L7"

    label "L7" $ do
        branch v0 "L9" "L1"

    label "L1" $ do
        call 7
        jump "L2"

    label "L2" $ do
        offlpi v4
        jump "L7"

    label "L9" $ do
        jump "L4"
