module Programs.Allocation4 where

import Assembly
import LinearScan.Hoopl.DSL

allocation4 :: Program (Node IRVar)
allocation4 = do
    label "entry" $ do
        move v33 v73
        jump "L98"

    label "L98" $ do
        nop
        move v52 v21
        move v40 v86
        add v54 v95 v43
        copy v21 v87
        call 87
        add v45 v69 v97
        offp v8 v38 v30
        jump "L45"

    label "L45" $ do
        offp v24 v5 v15
        copy v1 v63
        branch v97 "L16" "L80"

    label "L80" $ do
        lc v14
        branch v99 "L72" "L58"

    label "L58" $ do
        move v86 v33
        call 16
        move v93 v97
        move v76 v57
        move v79 v62
        move v99 v26
        lc v63
        lc v64
        copy v94 v43
        add v2 v96 v88
        branch v67 "L40" "L57"

    label "L40" $ do
        branch v26 "L107" "L38"

    label "L38" $ do
        call 88
        branch v49 "L106" "L61"

    label "L61" $ do
        lc v1
        move v65 v24
        move v61 v62
        lc v66
        copy v91 v60
        add v33 v10 v91
        move v66 v60
        jump "L63"

    label "L106" $ do
        jump "L45"

    label "L107" $ do
        jump "L25"

    label "L16" $ do
        move v78 v68
        call 40
        copy v69 v87
        move v35 v95
        move v61 v62
        lc v61
        move v42 v90
        jump "L63"

    label "L63" $ do
        call 93
        branch v91 "L54" "L18"

    label "L18" $ do
        branch v77 "L2" "L96"

    label "L2" $ do
        move v95 v88
        branch v72 "L105" "L104"

    label "L104" $ do
        jump "L79"

    label "L105" $ do
        jump "L25"

    label "L25" $ do
        move v92 v62
        copy v63 v17
        move v13 v52
        call 75
        jump "L79"

    label "L79" $ do
        move v98 v16
        move v47 v80
        branch v27 "L64" "L50"

    label "L50" $ do
        copy v96 v59
        call 3
        move pr1 v23
        nop
        add v45 v49 v93
        branch v19 "L28" "L91"

    label "L91" $ do
        move v24 v80
        move v79 v29
        offp v67 v7 v13
        branch v65 "L66" "L39"

    label "L39" $ do
        lc v45
        branch v39 "L62" "L103"

    label "L103" $ do
        jump "L8"

    label "L62" $ do
        branch v43 "L48" "L102"

    label "L102" $ do
        jump "L79"

    label "L64" $ do
        add v85 v59 v18
        jump "L8"

    label "L54" $ do
        move v87 v96
        branch v87 "L84" "L73"

    label "L73" $ do
        move v45 v49
        move v97 v78
        copy v27 v36
        lc v65
        move v86 v47
        add v21 v55 v66
        branch v20 "L83" "L100"

    label "L100" $ do
        jump "L8"

    label "L8" $ do
        offlpi v55
        alloc (Just v21) v68
        move v68 v26
        jump "L8"

    label "L83" $ do
        move v82 v71
        jump "L23"

    label "L23" $ do
        jump "L0"

    label "L84" $ do
        jump "L43"

    label "L48" $ do
        move v35 v92
        return_

    label "L66" $ do
        copy v54 v88
        move v79 v39
        nop
        offlpi v9
        add v45 v66 v12
        lc v2
        move v3 v29
        move v50 v17
        lc v19
        offp v36 v45 v89
        move v41 v24
        jump "L14"

    label "L14" $ do
        copy v82 v68
        return_

    label "L28" $ do
        call 38
        add v12 v35 v96
        branch v5 "L101" "L32"

    label "L32" $ do
        copy v99 v84
        return_

    label "L101" $ do
        jump "L43"

    label "L43" $ do
        add v41 v30 v64
        nop
        lc v38
        move v24 v40
        move v89 v45
        copy v32 v44
        lc v45
        lc v29
        move v55 v94
        nop
        return_

    label "L96" $ do
        call 27
        move v27 v74
        call 37
        nop
        call 46
        move v55 v62
        move v14 v81
        offp v29 v61 v67
        call 42
        return_

    label "L57" $ do
        add v25 v47 v65
        offlpi v51
        branch v64 "L86" "L41"

    label "L41" $ do
        offlpi v10
        offlpi v63
        move v5 v31
        lc v80
        move v39 v92
        move v54 v78
        offlpi v27
        return_

    label "L86" $ do
        move v94 v95
        offp v66 v36 v80
        jump "L0"

    label "L72" $ do
        return_
