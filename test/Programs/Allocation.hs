module Programs.Allocation where

import Assembly
import LinearScan.Hoopl.DSL

allocation :: Program (Node IRVar)
allocation = do
    label "entry" $ do
        copy v1 v11
        nop
        lc v2
        branch v6 "L4" "L1"

    label "L1" $ do
        lc v13
        move v8 v12
        call 2
        call 13
        call 4
        copy v2 v10
        add v13 v5 v0
        branch v5 "L14" "L12"

    label "L12" $ do
        offp v13 v1 v8
        call 11
        copy v13 v6
        move v10 v13
        move v7 v13
        call 0
        lc v7
        move v2 v6
        copy v9 v10
        return_

    label "L14" $ do
        jump "L3"

    label "L3" $ do
        jump "L8"

    label "L8" $ do
        lc v2
        nop
        move v9 v5
        lc pr6
        nop
        move v2 v7
        move v2 v7
        offp v8 v12 v13
        move v1 v12
        add v8 v5 v6
        move v10 v2
        move v6 v13
        move v5 v10
        lc v8
        offlpi v13
        call 7
        offp v5 pr31 v13
        lc v10
        add v2 v12 v4
        call 0
        add v4 v5 v0
        move v6 v1
        move v9 v1
        move v2 v9
        add v11 v0 v10
        move v3 v8
        move v5 v1
        move v7 v13
        lc v5
        move v8 v8
        jump "L3"

    label "L4" $ do
        return_
