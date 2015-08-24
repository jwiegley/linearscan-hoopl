module Programs.UponEntry where

import Assembly
import LinearScan.Hoopl.DSL

uponEntry :: Program (Node IRVar)
uponEntry = do
    label "entry" $ do
        call 48
        branch pr10 "L84" "L17"

    label "L17" $ do
        lc v12
        copy v41 v0
        branch v78 "L20" "L5"

    label "L5" $ do
        call 25
        add v47 v39 v63
        return_

    label "L20" $ do
        branch v69 "L41" "L83"

    label "L83" $ do
        jump "L16"

    label "L41" $ do
        lc v56
        call 52
        branch v76 "L19" "L19"

    label "L19" $ do
        lc v28
        offlpi v47
        copy v12 v47
        add v13 v49 v53
        move v35 v35
        copy v29 v16
        add v31 v30 v0
        call 60
        jump "L25"

    label "L25" $ do
        jump "L61"

    label "L61" $ do
        lc v10
        add v24 v0 v62
        call 26
        branch v53 "L26" "L66"

    label "L66" $ do
        lc v64
        branch v18 "L10" "L21"

    label "L21" $ do
        copy v77 v72
        branch v19 "L1" "L52"

    label "L52" $ do
        move v29 v38
        lc v65
        move pr12 v64
        offlpi v29
        copy v20 v43
        copy v5 v69
        move v22 v19
        jump "L57"

    label "L57" $ do
        move v40 v24
        jump "L28"

    label "L28" $ do
        offlpi v77
        add v75 v28 v11
        move v48 v49
        jump "L9"

    label "L9" $ do
        branch v30 "L82" "L78"

    label "L78" $ do
        nop
        lc v47
        branch v27 "L55" "L0"

    label "L55" $ do
        lc v6
        call 59
        add v40 v39 v25
        return_

    label "L82" $ do
        jump "L27"

    label "L1" $ do
        move v29 v1
        offlpi v14
        nop
        return_

    label "L10" $ do
        branch v17 "L81" "L80"

    label "L80" $ do
        jump "L75"

    label "L75" $ do
        offp v21 v26 v44
        lc v0
        branch v70 "L67" "L24"

    label "L24" $ do
        move v41 v72
        add v15 v47 v60
        move v18 v78
        call 6
        copy v47 v42
        offlpi v48
        move v8 v66
        move v41 v46
        lc v73
        add v2 v7 pr8
        jump "L75"

    label "L67" $ do
        move v72 v2
        branch v7 "L15" "L58"

    label "L58" $ do
        call 14
        copy v16 v37
        jump "L71"

    label "L71" $ do
        add v47 v59 v28
        nop
        move v52 v6
        jump "L59"

    label "L15" $ do
        lc v28
        copy v35 v1
        return_

    label "L81" $ do
        jump "L27"

    label "L27" $ do
        add v63 v33 v69
        move v7 v18
        move v45 v47
        move v3 v5
        return_

    label "L26" $ do
        call 59
        copy v33 v47
        lc v10
        call 31
        move v72 v35
        return_

    label "L84" $ do
        jump "L59"

    label "L59" $ do
        branch v47 "L47" "L79"

    label "L79" $ do
        jump "L16"

    label "L16" $ do
        copy v77 v28
        move v18 v21
        return_

    label "L47" $ do
        add v3 v3 v39
        nop
        return_
