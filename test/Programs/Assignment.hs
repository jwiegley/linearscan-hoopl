module Programs.Assignment where

import Assembly
import LinearScan.Hoopl.DSL

assignment :: Program (Node IRVar)
assignment = do
    label "entry" $ do
        copy v1 v73
        move v48 v38
        jump "L58"

    label "L58" $ do
        move v55 v23
        move v38 v17
        jump "L5"

    label "L5" $ do
        jump "L73"

    label "L73" $ do
        branch v51 "L46" "L34"

    label "L34" $ do
        offp v28 v10 v57
        lc v26
        branch v69 "L61" "L47"

    label "L47" $ do
        call 70
        nop
        add v79 v47 v67
        return_

    label "L61" $ do
        copy v52 v34
        branch v91 "L43" "L91"

    label "L91" $ do
        copy v69 v52
        alloc (Just v90) v90
        call 59
        return_

    label "L43" $ do
        call 59
        branch v18 "L20" "L39"

    label "L39" $ do
        lc v7
        lc v73
        return_

    label "L20" $ do
        jump "L15"

    label "L15" $ do
        branch v71 "L41" "L19"

    label "L19" $ do
        move v0 v30
        nop
        move v42 v83
        copy v57 v12
        jump "L22"

    label "L22" $ do
        add v23 v6 v49
        return_

    label "L41" $ do
        move v41 v46
        nop
        jump "L33"

    label "L33" $ do
        add v4 v27 v15
        add v65 v5 v3
        move v11 v13
        lc v66
        jump "L42"

    label "L42" $ do
        lc v71
        copy v70 v22
        lc v33
        move v49 v18
        lc v67
        copy v25 v72
        move v59 v83
        call 11
        return_

    label "L46" $ do
        return_
