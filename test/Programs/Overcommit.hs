module Programs.Overcommit where

import Assembly
import LinearScan.Hoopl.DSL

overCommitted :: Program (Node IRVar)
overCommitted = do
    label "entry" $ do
        copy v53 v18
        move v2 v98
        lc v16
        lc v53
        copy v35 v47
        call 44
        copy v5 v18
        call 10
        copy v51 v91
        copy v67 v58
        lc v33
        move v85 v0
        move v49 v16
        offp v32 v94 v12
        lc v95
        copy v3 v77
        branch v11 "L115" "L114"

    label "L114" $ do
        jump "L62"

    label "L115" $ do
        jump "L77"

    label "L77" $ do
        lc v49
        branch v73 "L87" "L18"

    label "L18" $ do
        call 95
        lc v13
        copy v17 v24
        move v60 v65
        move v81 v22
        move v37 v95
        copy v21 v72
        lc v52
        branch v38 "L113" "L70"

    label "L70" $ do
        move v60 v74
        move v40 v34
        lc v29
        move v48 v53
        move v65 v64
        move v52 v98
        lc v27
        branch v93 "L53" "L34"

    label "L34" $ do
        jump "L100"

    label "L53" $ do
        add v89 v89 v83
        jump "L25"

    label "L25" $ do
        copy v36 v68
        lc v68
        lc v95
        branch v24 "L33" "L13"

    label "L13" $ do
        return_

    label "L33" $ do
        branch v82 "L32" "L72"

    label "L72" $ do
        call 60
        jump "L59"

    label "L59" $ do
        lc v7
        jump "L69"

    label "L69" $ do
        call 10
        branch v81 "L0" "L89"

    label "L89" $ do
        add v79 v70 v89
        offlpi v12
        lc v62
        branch v63 "L112" "L67"

    label "L67" $ do
        add v58 v23 v59
        branch v49 "L111" "L14"

    label "L14" $ do
        branch v56 "L110" "L109"

    label "L109" $ do
        jump "L69"

    label "L110" $ do
        jump "L62"

    label "L62" $ do
        branch v57 "L31" "L108"

    label "L108" $ do
        jump "L25"

    label "L31" $ do
        move v72 v52
        return_

    label "L111" $ do
        jump "L5"

    label "L5" $ do
        lc v52
        lc v58
        move v38 v76
        move v76 v99
        branch v22 "L93" "L107"

    label "L107" $ do
        jump "L23"

    label "L93" $ do
        move v36 v38
        nop
        copy v10 v11
        call 41
        branch v79 "L106" "L105"

    label "L105" $ do
        jump "L100"

    label "L100" $ do
        copy v56 v17
        move v78 v97
        branch v42 "L40" "L91"

    label "L91" $ do
        offlpi v37
        lc v38
        move v100 v58
        add v22 v85 v82
        lc v15
        return_

    label "L40" $ do
        call 34
        nop
        branch v68 "L36" "L104"

    label "L104" $ do
        jump "L23"

    label "L36" $ do
        lc v100
        branch v66 "L4" "L103"

    label "L103" $ do
        jump "L94"

    label "L4" $ do
        call 49
        add v16 v50 v81
        move v82 v16
        move v24 v0
        jump "L38"

    label "L106" $ do
        jump "L5"

    label "L112" $ do
        jump "L77"

    label "L32" $ do
        move v21 v18
        move v28 v2
        move v40 v97
        return_

    label "L113" $ do
        jump "L94"

    label "L94" $ do
        move v48 v82
        lc v96
        lc v23
        move v11 v53
        offp v12 v100 v89
        return_

    label "L87" $ do
        move v38 v30
        lc v34
        jump "L38"

    label "L38" $ do
        branch pr15 "L102" "L101"

    label "L101" $ do
        jump "L23"

    label "L23" $ do
        return_

    label "L102" $ do
        jump "L58"

    label "L58" $ do
        copy v76 v54
        call 75
        branch v5 "L64" "L0"

    label "L64" $ do
        offlpi v2
        jump "L58"
