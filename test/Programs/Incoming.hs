module Programs.Incoming where

import Assembly
import LinearScan.Hoopl.DSL

regsIncoming :: Program (Node IRVar)
regsIncoming = do
    label "entry" $ do
        move v71 v76
        copy v64 v0
        jump "L48"

    label "L48" $ do
        move v71 v2
        add v40 v71 v45
        add v10 v97 v96
        move v61 v12
        alloc (Just v7) v2
        add v1 v55 v46
        move v48 v44
        lc v70
        lc v99
        call 7
        move v16 v24
        copy v46 v37
        move v16 v47
        add v56 v76 v5
        call 22
        nop
        add v60 v81 v81
        move v37 v23
        move v92 v95
        copy v21 v40
        move v13 v15
        offlpi v67
        move v83 v89
        move v94 v8
        jump "L40"

    label "L40" $ do
        copy v48 v69
        jump "L80"

    label "L80" $ do
        move v4 v70
        copy v16 v71
        nop
        lc v59
        call 10
        move v93 v44
        add pr18 v86 v1
        move v24 v13
        lc v43
        lc v36
        copy v1 v13
        move v30 v98
        nop
        copy v41 v8
        move v13 v54
        call 47
        move v59 v26
        jump "L16"

    label "L16" $ do
        lc v2
        move v23 v55
        jump "L38"

    label "L38" $ do
        return_
