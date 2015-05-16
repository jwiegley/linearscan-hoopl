module Main where

import AsmTest
import Assembly
import LinearScan.Hoopl.DSL
import Test.Hspec

-- | The objective of these tests is to present a program to the register
--   allocator algorithm, and verify that for certain inputs we get the
--   expected outputs.

main :: IO ()
main = hspec $ do
  describe "Sanity tests" sanityTests
  describe "Spill tests" spillTests
  describe "Block tests" blockTests
  describe "Call tests" callTests
  describe "Loop tests" loopTests

sanityTests :: SpecWith ()
sanityTests = do
  it "Single instruction" $ asmTest 32
    (label "entry" $ do
        add v0 v1 v2
        return_) $

    label "entry" $ do
        add 0 1 0
        return_

  it "Single, repeated instruction" $ asmTest 32
    (label "entry" $ do
        add v0 v1 v2
        add v0 v1 v2
        add v0 v1 v2
        return_) $

    label "entry" $ do
        add 0 1 2
        add 0 1 2
        add 0 1 2
        return_

  it "Multiple instructions" $ asmTest 32
    (label "entry" $ do
        add v0 v1 v2
        add v0 v1 v3
        add v0 v1 v2
        return_) $

    label "entry" $ do
        add 0 1 2
        add 0 1 3
        add 0 1 2
        return_

  it "More variables used than registers" $ asmTest 6
    (label "entry" $ do
        lc v0
        lc v1
        lc v3
        lc v4
        lc v6
        lc v7
        lc v9
        lc v10
        lc v12
        lc v13
        add v0 v1 v2
        add v3 v4 v5
        add v6 v7 v8
        add v9 v10 v11
        add v12 v13 v14
        return_) $

    label "entry" $ do
        lc 0
        lc 1
        lc 2
        lc 3
        lc 4
        lc 5
        save  4 0
        lc 4
        save  4 8
        lc 4
        save  4 16
        lc 4
        save  4 24
        lc 4
        add 0 1 0
        add 2 3 1
        restore  0 3
        add 3 5 2
        save  0 32
        restore  8 5
        restore  16 0
        add 5 0 3
        restore  24 5
        add 5 4 0
        return_

  it "Single long-lived variable" $ asmTest 32
    (label "entry" $ do
        add v0 v1 v2
        add v0 v4 v5
        add v0 v7 v8
        add v0 v10 v11
        return_) $

    label "entry" $ do
        add 0 1 1
        add 0 2 2
        add 0 3 3
        add 0 4 0
        return_

  it "Two long-lived variables" $ asmTest 32
    (label "entry" $ do
        add v0 v1 v2
        add v0 v4 v5
        add v0 v4 v8
        add v0 v4 v11
        return_) $

    label "entry" $ do
        add 0 1 1
        add 0 2 3
        add 0 2 4
        add 0 2 0
        return_

  it "One variable with a long interval" $ asmTest 32
    (label "entry" $ do
        add v0   v1  v2
        add v3   v4  v5
        add v6   v7  v8
        add v9  v10 v11
        add v12 v13 v14
        add v15 v16 v17
        add v18 v19 v20
        add v21 v22 v23
        add v24 v25 v26
        add v27 v28 v29
        add v30 v31 v32
        add v0  v34 v35
        return_) $

    label "entry" $ do
        add 0 1 1
        add 2 3 2
        add 4 5 3
        add 6 7 4
        add 8 9 5
        add 10 11 6
        add 12 13 7
        add 14 15 8
        add 16 17 9
        add 18 19 10
        add 20 21 11
        add 0 22 0
        return_

  it "Many variables with long intervals" $ asmTest 32
    (label "entry" $ do
        add v0   v1  v2
        add v3   v4  v5
        add v6   v7  v8
        add v9  v10 v11
        add v12 v13 v14
        add v15 v16 v17
        add v18 v19 v20
        add v21 v22 v23
        add v24 v25 v26
        add v27 v28 v29
        add v0   v1  v2
        add v3   v4  v5
        add v6   v7  v8
        add v9  v10 v11
        add v12 v13 v14
        add v15 v16 v17
        add v18 v19 v20
        add v21 v22 v23
        add v24 v25 v26
        add v27 v28 v29
        return_) $

    label "entry" $ do
        add 0 1 20
        add 2 3 21
        add 4 5 22
        add 6 7 23
        add 8 9 24
        add 10 11 25
        add 12 13 26
        add 14 15 27
        add 16 17 28
        add 18 19 29
        add 0 1 20
        add 2 3 21
        add 4 5 22
        add 6 7 23
        add 8 9 24
        add 10 11 25
        add 12 13 26
        add 14 15 27
        add 16 17 28
        add 18 19 29
        return_

spillTests :: SpecWith ()
spillTests = do
  it "Spilling one variable" $ asmTest 32
    (label "entry" $ do
        add v0   v1  v2
        add v3   v4  v5
        add v6   v7  v8
        add v9  v10 v11
        add v12 v13 v14
        add v15 v16 v17
        add v18 v19 v20
        add v21 v22 v23
        add v24 v25 v26
        add v27 v28 v29
        add v30 v31 v32
        add v0   v1  v2
        add v3   v4  v5
        add v6   v7  v8
        add v9  v10 v11
        add v12 v13 v14
        add v15 v16 v17
        add v18 v19 v20
        add v21 v22 v23
        add v24 v25 v26
        add v27 v28 v29
        add v30 v31 v32
        return_) $

    label "entry" $ do
        add 0 1 22
        add 2 3 23
        add 4 5 24
        add 6 7 25
        add 8 9 26
        add 10 11 27
        add 12 13 28
        add 14 15 29
        add 16 17 30
        add 18 19 31

        -- When we reach the 32nd variable considered (which happens to be
        -- v30), we must spill a register because there are not 32 registers.
        -- So we pick the first register, counting from 0, whose next use
        -- position is the furthest from this position.  That happens to be
        -- 18, which is next used at position 41.
        save 18 0
        add 20 21 18
        add 0 1 22
        add 2 3 23
        add 4 5 24
        add 6 7 25
        add 8 9 26
        add 10 11 27
        add 12 13 28
        add 14 15 29
        add 16 17 30

        -- When it comes time to reload v29 (which had been allocated to 18),
        -- we pick the first available register which happens to be 0 in this
        -- case.
        restore 0 0
        add 0 19 31
        add 20 21 18
        return_

  it "Higher register pressure" $ asmTest 3
    (label "entry" $ do
        lc v0
        lc v1
        add v0 v1 v2
        add v2 v1 v3
        add v3 v2 v4
        add v4 v3 v5
        add v2 v3 v6
        add v4 v5 v7
        add v6 v7 v8
        add v8 v1 v0
        return_) $

    label "entry" $ do
        lc 0
        lc 1
        add 0 1 2
        add 2 1 0
        save  1 0
        add 0 2 1
        save  2 8
        add 1 0 2
        save  1 16
        restore  8 1
        add 1 0 0
        save  0 24
        restore  16 0
        add 0 2 1
        restore  24 2
        add 2 1 0
        restore  0 1
        add 0 1 0
        return_

  it "Inserts necessary saves and restores" $ asmTest 4
    (label "entry" $ do
        lc v0
        lc v1
        add v0 v1 v2
        add v2 v1 v3
        add v3 v2 v4
        add v4 v3 v5
        add v2 v3 v6
        add v4 v5 v7
        add v6 v7 v8
        add v8 v1 v0
        return_) $

    label "entry" $ do
        lc 0
        lc 1
        add 0 1 2
        add 2 1 3
        add 3 2 0
        save 1 0
        add 0 3 1
        add 2 3 2
        add 0 1 0
        add 2 0 0
        restore 0 1
        add 0 1 0
        return_

blockTests :: SpecWith ()
blockTests = do
  it "Allocates across blocks" $ asmTest 32
    (do label "entry" $ do
            add v0 v1 v2
            jump "L2"

        label "L2" $ do
            add v2 v3 v4
            add v2 v4 v5
            jump "L3"

        label "L3" $ do
            add v2 v5 v6
            add v2 v6 v7
            add v2 v7 v8
            return_) $

    do label "entry" $ do
           add 0 1 0
           jump "L2"

       label "L2" $ do
           add 0 2 1
           add 0 1 1
           jump "L3"

       label "L3" $ do
           add 0 1 1
           add 0 1 1
           add 0 1 0
           return_

  it "Inserts resolving moves" $ asmTest 3
    (do label "entry" $ do
            lc v0
            lc v1
            add v0 v1 v2
            branch Zero v2 "B3" "B2"

        label "B2" $ do
            add v1 v2 v3
            add v0 v3 v4
            add v0 v4 v5
            add v4 v5 v6
            add v6 v3 v7
            jump "B4"

        label "B3" $ do
            add v1 v2 v3
            jump "B4"

        label "B4" $ do
            add v3 v3 v0
            return_) $

    do label "entry" $ do
           lc 0
           lc 1
           add 0 1 2
           branch Zero 2 "B2" "B3"

       label "B2" $ do
           move 1 0
           add 0 2 1
           jump "B4"

       label "B3" $ do
           save 0 0
           add 1 2 0
           restore 0 2
           add 2 0 1
           save 0 8
           add 2 1 0
           add 1 0 0
           restore 8 1
           add 0 1 0
           jump "B4"

       label "B4" $ do
           add 1 1 0
           return_

  it "When resolving moves are not needed" $ asmTest 4
    (do label "entry" $ do
            add v0 v1 v2
            branch Zero v2 "B3" "B2"

        label "B2" $ do
            add v1 v2 v3
            jump "B4"

        label "B3" $ do
            add v1 v2 v3
            add v0 v0 v4
            add v0 v0 v5
            add v0 v4 v6
            add v0 v5 v6
            jump "B4"

        label "B4" $ do
            add v3 v3 v0
            return_) $

    do label "entry" $ do
           add 0 1 2
           branch Zero 2 "B2" "B3"

       label "B2" $ do
           add 1 2 3
           add 0 0 1
           add 0 0 2
           add 0 1 1
           add 0 2 1
           jump "B4"

       label "B3" $ do
           add 1 2 3
           jump "B4"

       label "B4" $ do
           add 3 3 0
           return_

  it "Another resolution case" $ asmTest 4
    (do label "entry" $ do
            lc v3
            lc v4
            lc v15
            lc v20
            jump "L3"

        label "L3" $ do
            move v3 v9
            move v9 v11
            move v11 v10
            move v10 v12
            move v12 v13
            lc v14
            move v15 v5
            jump "L6"

        label "L6" $
            branch Zero v4 "L3" "L2"

        label "L2" $ do
            lc v21
            move v21 v18
            move v5 v4
            lc v19
            move v20 v17
            jump "L6") $

    do label "entry" $ do
           lc 0
           lc 1
           lc 2
           lc 3
           save 3 0
           jump "L3"

       label "L3" $ do
           move 0 3
           move 3 3
           move 3 3
           move 3 3
           move 3 3
           save 0 8
           lc 0
           save 0 16
           move 2 0
           jump "L6"

       label "L2" $ do
           lc 3
           save 2 24
           move 3 2
           move 0 1
           save 0 32
           lc 0
           save 1 48
           save 0 40
           restore 0 1
           move 1 0
           save 1 0
           restore 24 2
           restore 32 0
           restore 48 1
           jump "L6"

       label "L6" $
           branch Zero 1 "L5" "L2"

       label "L5" $ do
           save 1 48
           restore 0 1
           save 2 24
           save 1 0
           restore 24 2
           restore 48 1
           restore 8 0
           jump "L3"

callTests :: SpecWith ()
callTests = do
  it "Spills all registers at call" $ asmTest 4
    (label "entry" $ do
        lc v0
        lc v1
        add v0 v1 v2
        add v2 v1 v3
        add v3 v2 v4
        add v4 v3 v5
        call 1000
        add v2 v3 v6
        add v4 v5 v7
        add v6 v7 v8
        add v8 v1 v0
        return_) $

    label "entry" $ do
        lc 0
        lc 1
        add 0 1 2
        add 2 1 3
        add 3 2 0
        save 1 0
        add 0 3 1
        save 2 32
        save 3 24
        save 0 16
        save 1 8
        call 1000
        restore 32 1
        restore 24 2
        add 1 2 0
        restore 16 2
        restore 8 3
        add 2 3 1
        add 0 1 0
        restore 0 1
        add 0 1 0
        return_

loopTests :: SpecWith ()
loopTests = do
  it "Correctly orders loop blocks" $ asmTest 4
    (do label "entry" $ do
            trace "B0"
            jump "B1"

        label "B1" $ do
            trace "B1"
            branch Zero v1 "B3" "B2"

        label "B2" $ do
            trace "B2"
            branch Zero v1 "B5" "B4"

        label "B3" $ do
            trace "B3"
            jump "B1"

        label "B4" $ do
            trace "B4"
            branch Zero v1 "B7" "B6"

        label "B5" $ do
            trace "B5"
            return_

        label "B6" $ do
            trace "B6"
            jump "B4"

        label "B7" $ do
            trace "B7"
            jump "B1") $

    do label "entry" $ do
           trace "B0"
           jump "B1"

       label "B1" $ do
           trace "B1"
           branch Zero 0 "B3" "B2"

       label "B2" $ do
           trace "B2"
           branch Zero 0 "B5" "B8"

       label "B3" $ do
           trace "B3"
           jump "B1"

       label "B4" $ do
           trace "B4"
           branch Zero 0 "B7" "B6"

       label "B5" $ do
           trace "B5"
           return_

       label "B6" $ do
           trace "B6"
           jump "B4"

       label "B7" $ do
           trace "B7"
           jump "B1"

       label "B8" $ do
           jump "B4"
