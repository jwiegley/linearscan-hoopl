module Programs.Overlapped where

import Assembly
import LinearScan.Hoopl.DSL

overlapped :: Program (Node IRVar)
overlapped = do
    label "entry" $ do
        nop
        add v20 v61 v49
        add v72 v34 pr11
        offp v82 v52 v13
        offp v63 v34 v67
        offlpi v23
        copy v83 v6
        call 60
        move v52 v34
        copy v22 v85
        move v80 v11
        add v30 v65 v32
        lc v32
        move v57 v51
        branch v25 "L93" "L58"

    label "L58" $ do
        move v2 v29
        branch v79 "L77" "L92"

    label "L92" $ do
        jump "L32"

    label "L77" $ do
        offlpi v8
        call 74
        branch v54 "L91" "L79"

    label "L79" $ do
        add v23 v10 v64
        copy v19 v54
        lc v29
        lc v67
        offlpi v33
        move v55 v11
        lc v6
        move v13 v15
        offp v53 v40 v73
        branch v83 "L84" "L13"

    label "L13" $ do
        lc v57
        copy v38 v56
        branch v61 "L65" "L18"

    label "L18" $ do
        lc v74
        copy v66 v67
        branch v84 "L31" "L47"

    label "L47" $ do
        move v14 v3
        copy v45 v84
        return_

    label "L31" $ do
        lc v33
        lc v44
        offp v79 v8 v52
        add v60 v38 v42
        branch v20 "L90" "L6"

    label "L6" $ do
        move v1 v19
        return_

    label "L90" $ do
        jump "L82"

    label "L65" $ do
        copy v7 v54
        branch v33 "L53" "L56"

    label "L56" $ do
        branch v24 "L51" "L41"

    label "L41" $ do
        add v0 v80 v71
        jump "L50"

    label "L50" $ do
        alloc (Just v40) v20
        call 2
        call 45
        add v19 v24 v28
        call 46
        call 1
        copy v80 v49
        return_

    label "L51" $ do
        call 63
        move v71 v36
        lc v15
        branch v78 "L89" "L78"

    label "L78" $ do
        lc v7
        branch v57 "L80" "L48"

    label "L48" $ do
        call 9
        offp v44 v31 v56
        branch v67 "L88" "L62"

    label "L62" $ do
        offlpi v73
        return_

    label "L88" $ do
        jump "L82"

    label "L80" $ do
        lc v58
        move v48 v31
        lc v52
        move v59 v38
        move v45 v82
        copy v2 v48
        lc v42
        offp v85 v51 v34
        return_

    label "L89" $ do
        jump "L30"

    label "L53" $ do
        jump "L82"

    label "L82" $ do
        copy v47 v17
        lc v21
        copy v84 v33
        jump "L25"

    label "L25" $ do
        jump "L15"

    label "L15" $ do
        lc v85
        move v87 v27
        copy v30 v5
        move v46 v25
        move v6 v31
        copy v68 v10
        return_

    label "L84" $ do
        return_

    label "L91" $ do
        jump "L32"

    label "L32" $ do
        move v66 v51
        return_

    label "L93" $ do
        jump "L30"

    label "L30" $ do
        call 10
        return_
