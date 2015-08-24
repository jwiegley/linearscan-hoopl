module Programs.Residency where

import Assembly
import LinearScan.Hoopl.DSL

residency :: Program (Node IRVar)
residency = do
    label "entry" $ do
        branch v99 "L25" "L74"

    label "L74" $ do
        lc v98
        branch v94 "L84" "L36"

    label "L36" $ do
        call 23
        move v9 v2
        copy pr0 v63
        lc v56
        move v69 v29
        branch v89 "L49" "L107"

    label "L107" $ do
        jump "L45"

    label "L49" $ do
        offlpi v86
        branch v1 "L60" "L47"

    label "L47" $ do
        lc v64
        copy v56 v75
        branch v83 "L90" "L106"

    label "L106" $ do
        jump "L57"

    label "L90" $ do
        copy v79 v100
        jump "L31"

    label "L31" $ do
        lc v41
        copy v15 v92
        return_

    label "L60" $ do
        lc v27
        copy v6 v76
        branch v6 "L17" "L72"

    label "L72" $ do
        add v52 v61 v20
        return_

    label "L17" $ do
        move v60 v43
        jump "L57"

    label "L57" $ do
        branch v94 "L105" "L104"

    label "L104" $ do
        jump "L27"

    label "L105" $ do
        jump "L81"

    label "L84" $ do
        copy v86 v67
        jump "L81"

    label "L25" $ do
        move v65 v68
        move v79 v17
        move v74 v71
        lc v1
        call 93
        lc v17
        move v93 v95
        jump "L15"

    label "L15" $ do
        branch v14 "L103" "L68"

    label "L68" $ do
        lc v20
        offp v89 v27 v76
        lc v18
        lc v66
        branch v81 "L98" "L102"

    label "L102" $ do
        jump "L81"

    label "L81" $ do
        jump "L27"

    label "L27" $ do
        copy pr16 v92
        add v18 v77 v19
        jump "L59"

    label "L59" $ do
        offlpi v95
        branch v35 "L67" "L78"

    label "L78" $ do
        nop
        add v98 v21 v35
        add v56 v32 v69
        add pr4 v88 v60
        move v36 v9
        move v66 v65
        add v19 v92 v71
        nop
        move v7 v42
        offp v4 v52 v42
        copy v66 v12
        copy v29 v28
        jump "L87"

    label "L87" $ do
        add v65 v68 v89
        move v31 v39
        offp v35 v77 v90
        offp v87 v98 v97
        move v31 v55
        call 81
        nop
        lc v66
        move v44 v61
        copy v45 v49
        offlpi v66
        call 85
        lc v40
        jump "L71"

    label "L71" $ do
        call 32
        call 42
        lc v23
        move v19 v82
        lc v24
        lc v72
        call 79
        return_

    label "L67" $ do
        copy v45 v10
        branch v11 "L101" "L48"

    label "L48" $ do
        return_

    label "L101" $ do
        jump "L27"

    label "L98" $ do
        return_

    label "L103" $ do
        jump "L45"

    label "L45" $ do
        move v29 v48
        move v43 v24
        copy v16 v26
        add v30 v75 v28
        lc v57
        move v86 v100
        offp v86 v51 v75
        offlpi v71
        return_
