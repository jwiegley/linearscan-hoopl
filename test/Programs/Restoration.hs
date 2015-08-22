module Programs.Restoration where

import Assembly
import LinearScan.Hoopl.DSL

restoration :: Program (Node IRVar)
restoration = do
    label "entry" $ do
        offp v12 v15 v4
        jump "L30"

    label "L30" $ do
        move v17 v16
        nop
        offlpi v2
        move v2 v1
        copy v21 v29
        branch v19 "L42" "L17"

    label "L42" $ do
        jump "L7"

    label "L7" $ do
        move v30 v20
        move v31 v4
        copy v10 v30
        move v26 v16
        copy v14 v13
        lc v8
        add v7 v21 v10
        add v9 v25 v1
        branch v16 "L8" "L25"

    label "L25" $ do
        copy v30 v2
        branch v5 "L32" "L41"

    label "L41" $ do
        jump "L26"

    label "L32" $ do
        copy v20 v26
        move v10 v3
        call 8
        move v14 v34
        lc v2
        add v31 v18 v4
        jump "L20"

    label "L20" $ do
        branch v21 "L40" "L39"

    label "L39" $ do
        jump "L15"

    label "L15" $ do
        branch v4 "L9" "L2"

    label "L2" $ do
        branch v9 "L3" "L14"

    label "L14" $ do
        lc v11
        move v18 v34
        move v3 pr10
        move v18 v27
        move v31 v32
        lc v25
        offp v15 v14 v1
        offlpi v10
        jump "L15"

    label "L3" $ do
        copy v30 v22
        add v19 v29 v30
        move v31 v22
        branch v21 "L38" "L13"

    label "L13" $ do
        lc v18
        move v29 v15
        copy v9 v0
        jump "L31"

    label "L31" $ do
        copy v6 v25
        jump "L7"

    label "L9" $ do
        copy v17 v6
        branch v5 "L27" "L37"

    label "L27" $ do
        add v19 v21 v24
        move v3 v12
        call 20
        copy v13 v2
        nop
        move v10 v26
        copy v8 v28
        move v3 v1
        offp v10 v32 v31
        add v18 v5 v2
        call 23
        copy v24 v13
        lc v27
        move v11 v12
        lc v5
        move v8 v29
        call 0
        branch v29 "L0" "L33"

    label "L33" $ do
        jump "L26"

    label "L26" $ do
        branch v34 "L36" "L35"

    label "L35" $ do
        jump "L15"

    label "L36" $ do
        jump "L30"

    label "L37" $ do
        jump "L7"

    label "L38" $ do
        jump "L30"

    label "L40" $ do
        jump "L12"

    label "L12" $ do
        move v31 v17
        move v30 v30
        move v14 v25
        lc v18
        jump "L12"

    label "L8" $ do
        move v28 v26
        return_

    label "L17" $ do
        copy v34 v17
        call 1
        return_
