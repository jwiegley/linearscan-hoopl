module Programs.Ordered where

import Assembly
import LinearScan.Hoopl.DSL

regOrdered :: Program (Node IRVar)
regOrdered = do
    label "entry" $ do
        copy v99 v94
        lc v6
        move v96 v73
        copy v5 v67
        add v48 v96 pr7
        move v29 v23
        offp v95 v28 v51
        move v36 v96
        move v22 v32
        call 100
        jump "L72"

    label "L72" $ do
        add v67 v22 v13
        move v0 v30
        copy v56 v39
        branch v90 "L14" "L86"

    label "L86" $ do
        return_

    label "L14" $ do
        call 37
        move v31 v34
        nop
        add v10 v22 v60
        move v6 v54
        branch v89 "L97" "L18"

    label "L18" $ do
        offlpi v3
        jump "L83"

    label "L83" $ do
        move v37 v62
        branch v10 "L47" "L33"

    label "L33" $ do
        lc v74
        return_

    label "L47" $ do
        lc v80
        jump "L68"

    label "L68" $ do
        jump "L91"

    label "L91" $ do
        lc v38
        call 45
        move v50 v16
        lc v72
        add v3 pr9 v34
        move v52 v16
        copy v76 v73
        offlpi v92
        add v6 v54 v48
        add v27 v68 v49
        return_

    label "L97" $ do
        move v95 v30
        offp v77 v31 v75
        move v95 v88
        move v67 v69
        call 50
        return_
