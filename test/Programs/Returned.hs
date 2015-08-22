module Programs.Returned where

import Assembly
import LinearScan.Hoopl.DSL

freeBeforeReturn :: Program (Node IRVar)
freeBeforeReturn = do
    label "entry" $ do
        move v25 v31
        lc v33
        move v19 v10
        alloc (Just v31) v4
        move v21 v16
        jump "L27"

    label "L27" $ do
        offp v32 v1 v17
        lc v6
        move v8 v27
        lc v23
        nop
        jump "L12"

    label "L12" $ do
        move v7 v7
        offp v15 v6 v29
        move v26 v26
        branch v7 "L32" "L36"

    label "L36" $ do
        jump "L1"

    label "L32" $ do
        lc v7
        move v10 v31
        lc v21
        offp v32 v12 v7
        move v9 v12
        branch v11 "L14" "L9"

    label "L9" $ do
        call 3
        copy v18 v32
        move v17 v31
        branch v33 "L2" "L29"

    label "L29" $ do
        add v23 v18 v6
        lc v14
        branch v4 "L28" "L13"

    label "L13" $ do
        lc v27
        move v23 pr13
        lc v30
        return_

    label "L28" $ do
        return_

    label "L2" $ do
        move v18 v25
        jump "L20"

    label "L20" $ do
        branch v31 "L35" "L4"

    label "L4" $ do
        lc v26
        move v16 v6
        add v11 v27 v2
        lc v9
        add v32 v24 v21
        call 14
        nop
        move v19 v33
        return_

    label "L35" $ do
        jump "L1"

    label "L14" $ do
        offlpi v21
        add v17 v12 v15
        copy v17 v5
        branch v11 "L34" "L6"

    label "L6" $ do
        return_

    label "L34" $ do
        jump "L1"

    label "L1" $ do
        move v26 v3
        copy pr2 v1
        move v22 v31
        add v19 v0 v2
        move v30 v11
        copy v6 v29
        copy v32 v17
        lc v12
        jump "L31"

    label "L31" $ do
        add v23 v6 v10
        return_
