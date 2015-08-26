module Programs.Residency3 where

import Assembly
import LinearScan.Hoopl.DSL

residency3 :: Program (Node IRVar)
residency3 = do
    label "entry" $ do
        copy v30 v6
        move v29 v25
        branch v31 "L49" "L19"

    label "L19" $ do
        add pr2 v19 v3
        move v58 v21
        copy v56 v29
        copy v20 v24
        add v19 v58 v11
        move v12 v24
        alloc Nothing v48
        offlpi v33
        move v31 v22
        move v19 v20
        offlpi v41
        move v51 v49
        call 10
        move v58 v31
        move v69 v1
        offp v67 v64 v61
        move v39 v17
        jump "L56"

    label "L49" $ do
        lc v34
        jump "L13"

    label "L13" $ do
        lc v47
        lc v22
        branch v34 "L8" "L56"

    label "L56" $ do
        lc v58
        return_

    label "L8" $ do
        lc v31
        copy v53 v50
        lc v25
        lc v13
        nop
        move v39 v64
        move v24 v20
        move v11 v1
        lc pr27
        add v3 v60 v66
        copy v62 v9
        copy v41 v49
        move v1 v7
        offlpi v6
        add v15 v61 v66
        call 64
        move v22 v14
        lc v28
        move v32 v29
        jump "L45"

    label "L45" $ do
        offp v42 v37 v60
        copy v36 v8
        branch v21 "L42" "L32"

    label "L32" $ do
        jump "L68"

    label "L68" $ do
        add v52 pr12 v24
        copy v15 v54
        copy v45 v45
        branch v30 "L72" "L63"

    label "L63" $ do
        return_

    label "L72" $ do
        jump "L13"

    label "L42" $ do
        branch v43 "L24" "L57"

    label "L57" $ do
        move v62 v38
        branch v0 "L46" "L31"

    label "L31" $ do
        call 6
        move v46 pr28
        add v59 pr21 v16
        jump "L48"

    label "L46" $ do
        return_

    label "L24" $ do
        move v44 v7
        move v59 v34
        call 23
        add v69 v43 v57
        jump "L40"

    label "L40" $ do
        branch v38 "L71" "L70"

    label "L70" $ do
        jump "L45"

    label "L71" $ do
        jump "L48"

    label "L48" $ do
        add v34 v45 v63
        return_
