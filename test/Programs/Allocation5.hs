module Programs.Allocation5 where

import Assembly
import LinearScan.Hoopl.DSL

allocation5 :: Program (Node IRVar)
allocation5 = do
    label "entry" $ do
        move v0 v44
        branch v21 "L24" "L54"

    label "L54" $ do
        jump "L19"

    label "L24" $ do
        lc v31
        branch v44 "L21" "L53"

    label "L53" $ do
        jump "L18"

    label "L21" $ do
        branch v30 "L52" "L51"

    label "L51" $ do
        jump "L18"

    label "L18" $ do
        return_

    label "L52" $ do
        jump "L19"

    label "L19" $ do
        lc v44
        add v32 v43 v40
        offp v12 v33 v41
        move v8 pr23
        move v3 v26
        branch v12 "L11" "L50"

    label "L50" $ do
        jump "L10"

    label "L11" $ do
        jump "L23"

    label "L23" $ do
        lc v19
        branch v14 "L49" "L22"

    label "L22" $ do
        branch v9 "L45" "L48"

    label "L48" $ do
        jump "L40"

    label "L45" $ do
        return_

    label "L49" $ do
        jump "L10"

    label "L10" $ do
        branch v26 "L47" "L1"

    label "L1" $ do
        move v0 v4
        call 38
        move v26 v21
        move v6 v28
        lc v41
        branch v41 "L29" "L2"

    label "L2" $ do
        copy v11 v44
        move v32 pr4
        call 4
        move pr26 v41
        jump "L0"

    label "L29" $ do
        move v42 v11
        call 17
        move v24 v14
        return_

    label "L47" $ do
        jump "L40"

    label "L40" $ do
        move v21 v19
        branch v36 "L46" "L38"

    label "L38" $ do
        copy v40 v14
        branch v38 "L41" "L34"

    label "L41" $ do
        jump "L40"

    label "L34" $ do
        lc v36
        jump "L35"

    label "L35" $ do
        move v36 v27
        jump "L33"

    label "L33" $ do
        lc v19
        call 43
        add v10 v36 v5
        lc v8
        offlpi v10
        move v32 v39
        copy v41 v3
        add v12 v11 v17
        return_

    label "L46" $ do
        jump "L17"

    label "L17" $ do
        add v25 v24 v17
        move v30 v5
        move v5 v0
        nop
        jump "L14"

    label "L14" $ do
        move v29 v35
        jump "L17"
