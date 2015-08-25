module Programs.Allocation3 where

import Assembly
import LinearScan.Hoopl.DSL

allocation3 :: Program (Node IRVar)
allocation3 = do
    label "entry" $ do
        move v74 v37
        branch v53 "L7" "L83"

    label "L83" $ do
        branch v47 "L87" "L86"

    label "L86" $ do
        jump "L27"

    label "L87" $ do
        jump "L75"

    label "L75" $ do
        call 67
        lc v30
        copy v29 v67
        lc v38
        copy v70 v6
        jump "L74"

    label "L74" $ do
        offp v7 v47 pr5
        copy v80 v12
        move v16 v79
        call 81
        offp v38 v82 v57
        move v46 v1
        offp v19 v47 v45
        add v58 v67 v21
        copy v55 v80
        add v52 v13 v50
        nop
        offp v6 v30 v12
        add v70 v1 v46
        move v51 v46
        jump "L35"

    label "L35" $ do
        move v56 v7
        branch v67 "L81" "L85"

    label "L85" $ do
        jump "L27"

    label "L81" $ do
        move v69 v44
        move v69 v3
        branch v59 "L24" "L84"

    label "L84" $ do
        jump "L27"

    label "L27" $ do
        move v31 v53
        move v38 v75
        lc v77
        jump "L75"

    label "L24" $ do
        move v4 v70
        add v57 v62 v48
        move v64 v83
        offp v8 v49 v83
        return_

    label "L7" $ do
        add v26 v17 v41
        lc v80
        return_
