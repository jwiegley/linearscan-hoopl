module Programs.Allocation3 where

import Assembly
import LinearScan.Hoopl.DSL

allocation3 :: Program (Node IRVar)
allocation3 = do
    label "entry" $ do
        move v14 v14
        lc v2
        lc v14
        lc v15
        jump "L2"

    label "L2" $ do
        add v0 v3 v15
        move v3 v13
        jump "L3"

    label "L3" $ do
        offp v15 v11 v8
        call 15
        offlpi v1
        jump "L4"

    label "L4" $ do
        branch v19 "L5" "L6"

    label "L5" $ do
        lc v5
        lc v15
        move v16 v10
        call 13
        move v7 v8
        call 6
        lc v14
        branch v3 "L7" "L8"

    label "L8" $ do
        move v16 v16
        add v12 v19 v13
        lc v12
        call 6
        move v13 v18
        move v2 v0
        jump "L9"

    label "L9" $ do
        move v11 v3
        branch v9 "L10" "L11"

    label "L10" $ do
        lc v9
        move v17 v1
        call 0
        branch v10 "L12" "L13"

    label "L13" $ do
        call 5
        branch v15 "L14" "L15"

    label "L15" $ do
        jump "L4"

    label "L12" $ do
        jump "L16"

    label "L16" $ do
        lc v19
        move v10 v7
        lc v19
        offlpi v13
        move v11 v18
        move v18 v17
        call 6
        jump "L16"

    label "L14" $ do
        add v12 v4 v14
        return_

    label "L11" $ do
        lc v7
        add v0 v17 v16
        lc v2
        lc v16
        offlpi v19
        return_

    label "L7" $ do
        lc v8
        return_

    label "L6" $ do
        return_
