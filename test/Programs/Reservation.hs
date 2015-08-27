module Programs.Reservation where

import Assembly
import LinearScan.Hoopl.DSL

reservation :: Program (Node IRVar)
reservation = do
    label "entry" $ do
        move v2 v4
        lc v15
        copy v12 v11
        move v0 v25
        copy v4 v0
        copy v6 v11
        jump "L23"

    label "L23" $ do
        move v12 v13
        branch v12 "L36" "L20"

    label "L20" $ do
        branch v7 "L6" "L35"

    label "L35" $ do
        jump "L5"

    label "L5" $ do
        move v6 v23
        lc v1
        offlpi v1
        branch pr18 "L34" "L33"

    label "L33" $ do
        jump "L1"

    label "L34" $ do
        jump "L5"

    label "L6" $ do
        copy v20 v15
        copy v18 v18
        branch v0 "L22" "L2"

    label "L2" $ do
        move v6 v5
        lc v2
        lc v14
        add v26 v10 v26
        jump "L10"

    label "L10" $ do
        move v14 v20
        jump "L25"

    label "L25" $ do
        move v16 v4
        branch v14 "L32" "L9"

    label "L9" $ do
        call 1
        add v11 v18 v2
        lc v26
        nop
        move v6 v26
        move v21 v20
        add v22 v14 v8
        add v3 v15 v19
        move pr7 v3
        return_

    label "L32" $ do
        jump "L12"

    label "L22" $ do
        copy v6 v9
        lc v20
        move v7 v12
        alloc Nothing v10
        branch v17 "L31" "L15"

    label "L15" $ do
        move v11 v3
        branch v18 "L30" "L29"

    label "L29" $ do
        jump "L19"

    label "L30" $ do
        jump "L14"

    label "L14" $ do
        add v22 v1 v11
        copy v4 v26
        call 26
        lc v0
        move v11 v13
        lc v4
        call 24
        jump "L19"

    label "L19" $ do
        add v8 v19 v14
        nop
        call 15
        add v14 v0 v7
        nop
        jump "L12"

    label "L12" $ do
        lc v6
        add v2 v11 v0
        move v7 v3
        branch v11 "L13" "L28"

    label "L28" $ do
        jump "L14"

    label "L13" $ do
        move v8 v6
        move v17 v18
        return_

    label "L31" $ do
        jump "L4"

    label "L36" $ do
        jump "L4"

    label "L4" $ do
        jump "L16"

    label "L16" $ do
        lc v22
        move v0 v11
        move v8 v12
        move v22 v2
        move v11 v0
        offp v12 v12 v15
        lc v12
        copy v10 v7
        add v16 v21 v26
        move v2 v11
        move v22 v9
        move v11 v6
        lc v23
        add v11 v26 v14
        add v22 v13 v6
        move v7 v2
        lc v13
        copy v16 v25
        move v23 v11
        nop
        branch v3 "L27" "L11"

    label "L11" $ do
        nop
        lc v2
        move v11 v4
        move v18 v4
        return_

    label "L27" $ do
        jump "L1"

    label "L1" $ do
        lc v10
        offlpi v4
        lc v15
        move v25 v13
        call 2
        move v21 v7
        copy v12 v23
        move v11 v15
        copy v22 v3
        call 9
        move v22 v18
        move v16 v22
        add v10 v12 v8
        offp v16 v19 v2
        lc v0
        add v8 v16 v13
        add v6 v16 v11
        nop
        lc v2
        return_
