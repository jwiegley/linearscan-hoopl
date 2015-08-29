module Programs.Allocation6 where

import Assembly
import LinearScan.Hoopl.DSL

allocation6 :: Program (Node IRVar)
allocation6 = do
    label "entry" $ do
        move v27 v40
        branch v15 "L4" "L31"

    label "L31" $ do
        move v22 v3
        branch v17 "L53" "L52"

    label "L52" $ do
        jump "L21"

    label "L21" $ do
        call 36
        move v29 v25
        add v19 v28 v11
        move v39 v17
        branch v12 "L26" "L5"

    label "L5" $ do
        branch v22 "L28" "L24"

    label "L24" $ do
        move v11 v10
        copy v5 v7
        branch v39 "L6" "L51"

    label "L51" $ do
        jump "L18"

    label "L6" $ do
        move pr11 v4
        lc v40
        offlpi v11
        offp v30 v16 v14
        call 4
        add v27 v30 v23
        move v22 v29
        move v16 v43
        copy v6 v5
        lc v34
        offlpi v9
        offlpi v2
        add v13 v23 v3
        move v11 v16
        add v5 v3 v40
        copy v22 v41
        add v14 v40 v24
        move v24 v20
        add v38 v14 v3
        add v37 v34 v1
        alloc (Just v19) v24
        move v1 v14
        offp v43 v41 v3
        add v38 v15 v14
        move pr11 v5
        copy v42 v18
        move v27 v21
        move v40 pr11
        branch v3 "L38" "L20"

    label "L20" $ do
        add v43 v19 v31
        call 43
        move v5 v34
        move v28 v39
        lc v0
        branch v14 "L7" "L50"

    label "L50" $ do
        jump "L18"

    label "L18" $ do
        return_

    label "L7" $ do
        add v21 v35 v14
        move v43 v42
        lc v35
        add v11 v33 v6
        lc v10
        branch v18 "L49" "L48"

    label "L48" $ do
        jump "L21"

    label "L49" $ do
        jump "L30"

    label "L38" $ do
        copy v41 v32
        move v42 v2
        add v13 v9 v31
        branch v40 "L47" "L46"

    label "L46" $ do
        jump "L41"

    label "L47" $ do
        jump "L16"

    label "L28" $ do
        move v5 v43
        offp v3 v18 v20
        copy v16 v17
        lc v23
        move v34 v4
        move v29 v41
        branch v43 "L36" "L36"

    label "L36" $ do
        copy v1 v7
        alloc (Just v4) v38
        jump "L16"

    label "L16" $ do
        add v30 v0 v12
        lc v35
        jump "L42"

    label "L26" $ do
        add v26 v23 v7
        copy v12 v12
        branch v17 "L23" "L32"

    label "L32" $ do
        copy v20 v22
        add v33 v6 v27
        offlpi v28
        move v13 v10
        jump "L11"

    label "L11" $ do
        move v24 v4
        jump "L17"

    label "L17" $ do
        branch v37 "L22" "L45"

    label "L45" $ do
        jump "L11"

    label "L22" $ do
        add v25 v20 v12
        offlpi v34
        add v34 v25 v3
        add v10 v21 v10
        move v17 v22
        move v0 v18
        nop
        add v13 v31 v9
        move v31 v6
        branch v27 "L35" "L40"

    label "L40" $ do
        lc v29
        move v15 v24
        add pr15 v29 v6
        call 37
        copy v24 v19
        move v14 v22
        call 30
        lc v1
        call 11
        copy v25 v23
        move v7 v7
        move v22 v39
        add v36 v1 v17
        add v27 v32 v34
        move v31 v35
        branch v7 "L44" "L43"

    label "L43" $ do
        lc v41
        jump "L42"

    label "L42" $ do
        return_

    label "L44" $ do
        jump "L30"

    label "L30" $ do
        call 6
        return_

    label "L35" $ do
        offp v20 v25 v32
        lc v9
        return_

    label "L23" $ do
        return_

    label "L53" $ do
        jump "L41"

    label "L41" $ do
        lc v22
        return_

    label "L4" $ do
        call 15
        lc v20
        call 17
        return_
