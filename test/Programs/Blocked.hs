module Programs.Blocked where

import Assembly
import LinearScan.Hoopl.DSL

regBlocked :: Program (Node IRVar)
regBlocked = do
    label "entry" $ do
        lc v23
        add v64 v64 pr11
        call 80
        jump "L61"

    label "L61" $ do
        add v86 v39 v98
        nop
        add v77 v47 v60
        add v71 v4 v5
        move v57 v18
        copy v20 v42
        offp v20 pr19 v19
        call 57
        add pr31 v99 pr4
        nop
        nop
        move pr5 v41
        add v47 v57 v98
        alloc (Just pr21) v28
        branch v26 "L44" "L9"

    label "L9" $ do
        copy v9 v52
        copy v42 v36
        branch v24 "L74" "L52"

    label "L52" $ do
        jump "L60"

    label "L74" $ do
        copy pr14 v8
        move v34 pr20
        branch v92 "L51" "L50"

    label "L50" $ do
        jump "L39"

    label "L51" $ do
        jump "L5"

    label "L5" $ do
        alloc (Just v3) v79
        move v81 v76
        lc v55
        call 21
        lc v70
        add v99 v20 v89
        lc v5
        move v72 pr12
        offp v90 pr13 v0
        lc v2
        offlpi v78
        branch pr21 "L29" "L49"

    label "L49" $ do
        jump "L60"

    label "L60" $ do
        lc v84
        move pr30 v47
        branch v0 "L33" "L18"

    label "L18" $ do
        call 22
        jump "L35"

    label "L35" $ do
        copy v91 pr19
        move v43 v91
        branch v37 "L48" "L17"

    label "L17" $ do
        move v66 v27
        jump "L10"

    label "L10" $ do
        add v9 pr11 pr29
        branch v54 "L98" "L21"

    label "L21" $ do
        move pr14 pr14
        copy v15 v43
        move v79 v77
        copy v4 v79
        call 61
        move pr13 v31
        alloc (Just v59) v37
        return_

    label "L98" $ do
        lc v25
        return_

    label "L33" $ do
        add v35 pr3 v59
        lc v42
        branch v62 "L94" "L63"

    label "L63" $ do
        move pr23 v47
        return_

    label "L94" $ do
        move v27 v74
        call 87
        add v48 pr2 v95
        copy v11 v92
        copy pr0 v35
        lc pr25
        copy v80 v59
        add pr28 v96 pr17
        move v92 v88
        move v77 v92
        jump "L40"

    label "L40" $ do
        branch v13 "L25" "L47"

    label "L47" $ do
        jump "L57"

    label "L57" $ do
        move v59 v3
        alloc (Just v27) pr6
        jump "L4"

    label "L4" $ do
        branch v59 "L12" "L46"

    label "L46" $ do
        jump "L48"

    label "L48" $ do
        jump "L61"

    label "L12" $ do
        branch v84 "L8" "L90"

    label "L90" $ do
        call 63
        offp v71 v37 pr15
        add v16 v91 v9
        call 18
        branch v100 "L53" "L15"

    label "L15" $ do
        nop
        add pr24 v23 v73
        jump "L66"

    label "L66" $ do
        branch v78 "L42" "L41"

    label "L41" $ do
        jump "L61"

    label "L42" $ do
        jump "L57"

    label "L53" $ do
        add v32 v3 pr16
        jump "L87"

    label "L87" $ do
        return_

    label "L8" $ do
        move v67 v2
        offlpi v85
        lc v39
        jump "L26"

    label "L26" $ do
        copy v58 v91
        return_

    label "L25" $ do
        move v5 pr30
        move pr12 v5
        move v74 v19
        copy pr1 v50
        branch v31 "L71" "L40"

    label "L71" $ do
        branch v42 "L39" "L0"

    label "L39" $ do
        offlpi v7
        branch v82 "L44" "L43"

    label "L43" $ do
        jump "L5"

    label "L44" $ do
        jump "L61"

    label "L29" $ do
        return_
