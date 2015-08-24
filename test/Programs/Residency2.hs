module Programs.Residency2 where

import Assembly
import LinearScan.Hoopl.DSL

residency2 :: Program (Node IRVar)
residency2 = do
    label "entry" $ do
        jump "L28"

    label "L28" $ do
        branch v27 "L44" "L66"

    label "L66" $ do
        jump "L3"

    label "L3" $ do
        move v49 v31
        add v12 v24 v12
        lc v30
        branch v30 "L41" "L24"

    label "L24" $ do
        branch v19 "L7" "L65"

    label "L65" $ do
        jump "L13"

    label "L7" $ do
        add v27 v10 v7
        call 1
        add v13 v37 v57
        branch v32 "L19" "L64"

    label "L64" $ do
        jump "L38"

    label "L38" $ do
        offp v41 v50 v28
        jump "L38"

    label "L41" $ do
        move v0 v34
        copy v48 v26
        jump "L39"

    label "L39" $ do
        call 29
        copy v38 v55
        branch v1 "L20" "L63"

    label "L63" $ do
        jump "L13"

    label "L13" $ do
        move v15 v60
        add v61 v35 v50
        move v47 v60
        call 47
        jump "L40"

    label "L40" $ do
        add v21 v25 v23
        jump "L36"

    label "L36" $ do
        lc pr20
        branch v17 "L62" "L48"

    label "L62" $ do
        jump "L3"

    label "L48" $ do
        move v17 v57
        jump "L11"

    label "L20" $ do
        return_

    label "L19" $ do
        return_

    label "L44" $ do
        offp v44 v53 v58
        offlpi v10
        add v61 v48 pr22
        jump "L51"

    label "L51" $ do
        lc v39
        offlpi v48
        move v48 pr13
        nop
        move v42 v18
        move v11 v44
        add v35 v9 v49
        lc v20
        jump "L11"

    label "L11" $ do
        copy v32 v26
        call 48
        lc v36
        copy v7 v22
        offlpi v1
        return_
