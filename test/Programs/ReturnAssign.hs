module Programs.ReturnAssign where

import Assembly
import LinearScan.Hoopl.DSL

returnAssign :: Program (Node IRVar)
returnAssign = do
    label "entry" $ do
        copy v72 v78
        lc v64
        branch v52 "L57" "L22"

    label "L22" $ do
        call 79
        return_

    label "L57" $ do
        add v11 v9 v24
        branch v59 "L23" "L59"

    label "L59" $ do
        copy v48 v70
        call 38
        lc v80
        offlpi v60
        call 68
        move v15 v47
        add v48 v66 v28
        call 45
        copy v58 v31
        jump "L68"

    label "L68" $ do
        lc v63
        branch v23 "L41" "L17"

    label "L17" $ do
        offp v70 v65 v78
        call 60
        move v31 v18
        copy v22 v63
        nop
        branch v37 "L72" "L35"

    label "L35" $ do
        copy v60 v12
        branch v65 "L82" "L74"

    label "L74" $ do
        copy v0 v50
        move v4 v61
        lc v63
        lc v22
        call 16
        lc v78
        lc v56
        call 46
        move v7 v57
        branch v61 "L9" "L70"

    label "L70" $ do
        lc v19
        return_

    label "L9" $ do
        offlpi v45
        return_

    label "L82" $ do
        jump "L36"

    label "L72" $ do
        call 18
        jump "L36"

    label "L36" $ do
        copy v1 v14
        call 60
        return_

    label "L41" $ do
        lc v44
        move v73 v29
        add v72 v55 v58
        add v2 v45 v78
        lc v36
        offlpi v44
        move v66 v50
        copy v12 v20
        move v38 v64
        lc v15
        move v24 v9
        jump "L78"

    label "L78" $ do
        return_

    label "L23" $ do
        move v80 v59
        return_
