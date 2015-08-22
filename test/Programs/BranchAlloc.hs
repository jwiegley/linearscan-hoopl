module Programs.BranchAlloc where

import Assembly
import LinearScan.Hoopl.DSL

branchAlloc :: Program (Node IRVar)
branchAlloc = do
    label "entry" $ do
        branch v55 "L39" "L111"

    label "L111" $ do
        jump "L19"

    label "L19" $ do
        lc v0
        branch v6 "L49" "L32"

    label "L32" $ do
        branch v68 "L10" "L93"

    label "L93" $ do
        copy v33 v14
        call 33
        branch v36 "L11" "L9"

    label "L9" $ do
        lc v88
        branch v80 "L98" "L110"

    label "L110" $ do
        jump "L19"

    label "L10" $ do
        lc v79
        offp v56 v26 v99
        offlpi v45
        move v40 v75
        nop
        copy v73 v60
        copy v87 v6
        call 35
        nop
        offlpi v35
        lc v59
        move v32 v58
        move v18 v25
        move v53 v53
        add v30 v80 v64
        move v36 v62
        move v72 v16
        call 81
        alloc (Just v5) v91
        branch v45 "L40" "L21"

    label "L40" $ do
        branch v78 "L109" "L99"

    label "L109" $ do
        jump "L100"

    label "L100" $ do
        call 54
        move v54 v72
        add v90 v90 v81
        branch v73 "L77" "L88"

    label "L88" $ do
        call 9
        branch v35 "L108" "L37"

    label "L37" $ do
        lc v5
        call 48
        copy v6 v18
        add v11 v93 v99
        lc v10
        branch v7 "L44" "L80"

    label "L44" $ do
        copy v35 v24
        branch v91 "L67" "L58"

    label "L58" $ do
        add v26 v97 v25
        copy v7 v32
        branch v85 "L106" "L45"

    label "L45" $ do
        move v94 v55
        add v25 v26 v68
        nop
        move v1 v54
        move v70 v78
        lc v98
        call 90
        lc v22
        move v5 v43
        branch v39 "L57" "L16"

    label "L16" $ do
        move v52 v99
        jump "L50"

    label "L50" $ do
        move v81 v100
        branch v31 "L54" "L105"

    label "L105" $ do
        jump "L38"

    label "L38" $ do
        add v49 v12 v31
        branch v52 "L104" "L103"

    label "L103" $ do
        jump "L53"

    label "L104" $ do
        jump "L38"

    label "L106" $ do
        jump "L100"

    label "L39" $ do
        branch v92 "L4" "L101"

    label "L101" $ do
        jump "L53"

    label "L53" $ do
        jump "L53"

    label "L4" $ do
        copy v58 v16
        move v45 v96
        return_

    label "L54" $ do
        copy v88 v65
        add v40 v71 v58
        copy v39 v59
        offp v69 v46 v51
        offp v1 v12 v46
        call 82
        return_

    label "L57" $ do
        move v15 v32
        offp v21 v43 v57
        lc v5
        branch v83 "L102" "L46"

    label "L46" $ do
        add v23 v44 v84
        move v88 v37
        move v74 v96
        copy v29 v31
        alloc Nothing v60
        add v87 v20 v34
        move v98 v68
        move v52 v82
        copy v70 pr11
        copy v22 v95
        move v67 v22
        lc v71
        add v50 v77 v43
        nop
        move v28 v58
        move v24 v44
        nop
        nop
        lc v33
        offlpi v80
        copy v92 v1
        copy v72 v16
        lc v33
        lc v5
        lc v6
        return_

    label "L102" $ do
        jump "L62"

    label "L67" $ do
        move v17 v19
        add v96 v67 v48
        add v72 v58 v21
        call 75
        move v67 v82
        move v0 v43
        lc v10
        move v37 v10
        move v41 v39
        offp v20 v58 v17
        copy v100 v74
        move v36 v93
        return_

    label "L80" $ do
        call 38
        lc v54
        lc v97
        call 29
        call 20
        move v62 v9
        call 100
        lc v79
        nop
        add v53 v1 v75
        move v43 v56
        offlpi v67
        add v33 v62 v19
        copy v12 v26
        move v9 v36
        offp v100 v82 v99
        jump "L82"

    label "L82" $ do
        move v41 v100
        add v34 pr19 v37
        branch v84 "L85" "L107"

    label "L107" $ do
        jump "L60"

    label "L85" $ do
        move v87 v7
        offlpi v8
        jump "L62"

    label "L62" $ do
        move v2 v100
        offp v79 v71 v54
        call 34
        jump "L78"

    label "L78" $ do
        copy v96 v46
        return_

    label "L108" $ do
        jump "L60"

    label "L77" $ do
        add v39 v82 v37
        move v60 v80
        return_

    label "L99" $ do
        add v87 v36 v77
        lc v97
        lc v89
        copy v16 v37
        add v4 v73 v8
        add v83 v48 v59
        copy v24 v63
        return_

    label "L21" $ do
        alloc (Just v61) v42
        move v82 v58
        copy v62 v16
        nop
        move v0 v39
        move v92 v82
        move v54 v0
        move v58 v37
        add v68 v40 v20
        add v72 v26 v76
        lc v98
        lc v18
        call 48
        move v38 v35
        jump "L60"

    label "L60" $ do
        lc v92
        move v1 v64
        copy pr31 v15
        return_

    label "L98" $ do
        move pr21 v90
        return_

    label "L11" $ do
        jump "L86"

    label "L86" $ do
        add v51 v51 v28
        lc v9
        return_

    label "L49" $ do
        copy v56 v81
        move v19 v57
        call 46
        offlpi v19
        branch v15 "L71" "L31"

    label "L31" $ do
        lc v19
        call 38
        branch v87 "L59" "L30"

    label "L30" $ do
        move v1 v28
        return_

    label "L59" $ do
        move v14 v63
        move v33 v23
        alloc Nothing v19
        copy v81 v28
        return_

    label "L71" $ do
        lc v38
        call 76
        return_
