module Main where

import AsmTest
import Assembly
import LinearScan.Hoopl.DSL
import Programs.Exhaustion1
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
        add (r0 v0) (r1 v1) (r0 v2)
        return_

  it "Single, repeated instruction" $ asmTest 32
    (label "entry" $ do
        add v0 v1 v2
        add v0 v1 v2
        add v0 v1 v2
        return_) $

    label "entry" $ do
        add (r0 v0) (r1 v0) (r2 v0)
        add (r0 v0) (r1 v0) (r2 v0)
        add (r0 v0) (r1 v0) (r2 v0)
        return_

  it "Multiple instructions" $ asmTest 32
    (label "entry" $ do
        add v0 v1 v2
        add v0 v1 v3
        add v0 v1 v2
        return_) $

    label "entry" $ do
        add (r0 v0) (r1 v0) (r2 v0)
        add (r0 v0) (r1 v0) (r3 v0)
        add (r0 v0) (r1 v0) (r2 v0)
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
        lc (r0 v0)
        lc (r1 v0)
        lc (r2 v0)
        lc (r3 v0)
        lc (r4 v0)
        lc (r5 v0)
        save (r4 v0) 0
        lc (r4 v0)
        save (r4 v0) 8
        lc (r4 v0)
        save (r4 v0) 16
        lc (r4 v0)
        save (r4 v0) 24
        lc (r4 v0)
        add (r0 v0) (r1 v0) (r0 v0)
        add (r2 v0) (r3 v0) (r1 v0)
        restore 0 (r3 v0)
        add (r3 v0) (r5 v0) (r2 v0)
        save (r0 v0) 32
        restore 8 (r5 v0)
        restore 16 (r0 v0)
        add (r5 v0) (r0 v0) (r3 v0)
        restore 24 (r5 v0)
        add (r5 v0) (r4 v0) (r0 v0)
        return_

  it "Single long-lived variable" $ asmTest 32
    (label "entry" $ do
        add v0 v1 v2
        add v0 v4 v5
        add v0 v7 v8
        add v0 v10 v11
        return_) $

    label "entry" $ do
        add (r0 v0) (r1 v0) (r1 v0)
        add (r0 v0) (r2 v0) (r2 v0)
        add (r0 v0) (r3 v0) (r3 v0)
        add (r0 v0) (r4 v0) (r0 v0)
        return_

  it "Two long-lived variables" $ asmTest 32
    (label "entry" $ do
        add v0 v1 v2
        add v0 v4 v5
        add v0 v4 v8
        add v0 v4 v11
        return_) $

    label "entry" $ do
        add (r0 v0) (r1 v0) (r1 v0)
        add (r0 v0) (r2 v0) (r3 v0)
        add (r0 v0) (r2 v0) (r4 v0)
        add (r0 v0) (r2 v0) (r0 v0)
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
        add (r0 v0) (r1 v0) (r1 v0)
        add (r2 v0) (r3 v0) (r2 v0)
        add (r4 v0) (r5 v0) (r3 v0)
        add (r6 v0) (r7 v0) (r4 v0)
        add (r8 v0) (r9 v0) (r5 v0)
        add (r10 v0) (r11 v0) (r6 v0)
        add (r12 v0) (r13 v0) (r7 v0)
        add (r14 v0) (r15 v0) (r8 v0)
        add (r16 v0) (r17 v0) (r9 v0)
        add (r18 v0) (r19 v0) (r10 v0)
        add (r20 v0) (r21 v0) (r11 v0)
        add (r0 v0) (r22 v0) (r0 v0)
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
        add (r0 v0) (r1 v0) (r20 v0)
        add (r2 v0) (r3 v0) (r21 v0)
        add (r4 v0) (r5 v0) (r22 v0)
        add (r6 v0) (r7 v0) (r23 v0)
        add (r8 v0) (r9 v0) (r24 v0)
        add (r10 v0) (r11 v0) (r25 v0)
        add (r12 v0) (r13 v0) (r26 v0)
        add (r14 v0) (r15 v0) (r27 v0)
        add (r16 v0) (r17 v0) (r28 v0)
        add (r18 v0) (r19 v0) (r29 v0)
        add (r0 v0) (r1 v0) (r20 v0)
        add (r2 v0) (r3 v0) (r21 v0)
        add (r4 v0) (r5 v0) (r22 v0)
        add (r6 v0) (r7 v0) (r23 v0)
        add (r8 v0) (r9 v0) (r24 v0)
        add (r10 v0) (r11 v0) (r25 v0)
        add (r12 v0) (r13 v0) (r26 v0)
        add (r14 v0) (r15 v0) (r27 v0)
        add (r16 v0) (r17 v0) (r28 v0)
        add (r18 v0) (r19 v0) (r29 v0)
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
        add (r0 v0) (r1 v0) (r22 v0)
        add (r2 v0) (r3 v0) (r23 v0)
        add (r4 v0) (r5 v0) (r24 v0)
        add (r6 v0) (r7 v0) (r25 v0)
        add (r8 v0) (r9 v0) (r26 v0)
        add (r10 v0) (r11 v0) (r27 v0)
        add (r12 v0) (r13 v0) (r28 v0)
        add (r14 v0) (r15 v0) (r29 v0)
        add (r16 v0) (r17 v0) (r30 v0)
        add (r18 v0) (r19 v0) (r31 v0)

        -- When we reach the 32nd variable considered (which happens to be
        -- v30), we must spill a register because there are not 32 registers.
        -- So we pick the first register, counting from 0, whose next use
        -- position is the furthest from this position.  That happens to be
        -- 18, which is next used at position 41.
        save (r18 v0) 0
        add (r20 v0) (r21 v0) (r18 v0)
        add (r0 v0) (r1 v0) (r22 v0)
        add (r2 v0) (r3 v0) (r23 v0)
        add (r4 v0) (r5 v0) (r24 v0)
        add (r6 v0) (r7 v0) (r25 v0)
        add (r8 v0) (r9 v0) (r26 v0)
        add (r10 v0) (r11 v0) (r27 v0)
        add (r12 v0) (r13 v0) (r28 v0)
        add (r14 v0) (r15 v0) (r29 v0)
        add (r16 v0) (r17 v0) (r30 v0)

        -- When it comes time to reload v29 (which had been allocated to 18),
        -- we pick the first available register which happens to be 0 in this
        -- case.
        restore 0 (r0 v0)
        add (r0 v0) (r19 v0) (r31 v0)
        add (r20 v0) (r21 v0) (r18 v0)
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
        lc (r0 v0)
        lc (r1 v0)
        add (r0 v0) (r1 v0) (r2 v0)
        add (r2 v0) (r1 v0) (r0 v0)
        save (r1 v0) 0
        add (r0 v0) (r2 v0) (r1 v0)
        save (r2 v0) 8
        add (r1 v0) (r0 v0) (r2 v0)
        save (r1 v0) 16
        restore 8 (r1 v0)
        add (r1 v0) (r0 v0) (r0 v0)
        save (r0 v0) 24
        restore 16 (r0 v0)
        add (r0 v0) (r2 v0) (r1 v0)
        restore 24 (r2 v0)
        add (r2 v0) (r1 v0) (r0 v0)
        restore 0 (r1 v0)
        add (r0 v0) (r1 v0) (r0 v0)
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
        lc (r0 v0)
        lc (r1 v0)
        add (r0 v0) (r1 v0) (r2 v0)
        add (r2 v0) (r1 v0) (r3 v0)
        add (r3 v0) (r2 v0) (r0 v0)
        save (r1 v0) 0
        add (r0 v0) (r3 v0) (r1 v0)
        add (r2 v0) (r3 v0) (r2 v0)
        add (r0 v0) (r1 v0) (r0 v0)
        add (r2 v0) (r0 v0) (r0 v0)
        restore 0 (r1 v0)
        add (r0 v0) (r1 v0) (r0 v0)
        return_

  it "Handles a case of near exhaustion" $ asmTest_ 32 exhaustion1

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
           add (r0 v0) (r1 v0) (r0 v0)
           jump "L2"

       label "L2" $ do
           add (r0 v0) (r2 v0) (r1 v0)
           add (r0 v0) (r1 v0) (r1 v0)
           jump "L3"

       label "L3" $ do
           add (r0 v0) (r1 v0) (r1 v0)
           add (r0 v0) (r1 v0) (r1 v0)
           add (r0 v0) (r1 v0) (r0 v0)
           return_

  it "Inserts resolving moves" $ asmTest 3
    (do label "entry" $ do
            lc v0
            lc v1
            add v0 v1 v2
            branch v2 "B3" "B2"

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
           lc (r0 v0)
           lc (r1 v0)
           add (r0 v0) (r1 v0) (r2 v0)
           branch (r2 v0) "B2" "B3"

       label "B2" $ do
           move (r1 v0) (r0 v0)
           add (r0 v0) (r2 v0) (r1 v0)
           jump "B4"

       label "B3" $ do
           save (r0 v0) 0
           add (r1 v0) (r2 v0) (r0 v0)
           restore 0 (r2 v0)
           add (r2 v0) (r0 v0) (r1 v0)
           save (r0 v0) 8
           add (r2 v0) (r1 v0) (r0 v0)
           add (r1 v0) (r0 v0) (r0 v0)
           restore 8 (r1 v0)
           add (r0 v0) (r1 v0) (r0 v0)
           jump "B4"

       label "B4" $ do
           add (r1 v0) (r1 v0) (r0 v0)
           return_

  it "When resolving moves are not needed" $ asmTest 4
    (do label "entry" $ do
            add v0 v1 v2
            branch v2 "B3" "B2"

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
           add (r0 v0) (r1 v0) (r2 v0)
           branch (r2 v0) "B2" "B3"

       label "B2" $ do
           add (r1 v0) (r2 v0) (r3 v0)
           add (r0 v0) (r0 v0) (r1 v0)
           add (r0 v0) (r0 v0) (r2 v0)
           add (r0 v0) (r1 v0) (r1 v0)
           add (r0 v0) (r2 v0) (r1 v0)
           jump "B4"

       label "B3" $ do
           add (r1 v0) (r2 v0) (r3 v0)
           jump "B4"

       label "B4" $ do
           add (r3 v0) (r3 v0) (r0 v0)
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
            branch v4 "L3" "L2"

        label "L2" $ do
            lc v21
            move v21 v18
            move v5 v4
            lc v19
            move v20 v17
            jump "L6") $

    do label "entry" $ do
           lc (r0 v0)
           lc (r1 v0)
           lc (r2 v0)
           lc (r3 v0)
           save (r3 v0) 0
           jump "L3"

       label "L3" $ do
           move (r0 v0) (r3 v0)
           move (r3 v0) (r3 v0)
           move (r3 v0) (r3 v0)
           move (r3 v0) (r3 v0)
           move (r3 v0) (r3 v0)
           save (r0 v0) 8
           lc (r0 v0)
           save (r0 v0) 16
           move (r2 v0) (r0 v0)
           jump "L6"

       label "L2" $ do
           lc (r3 v0)
           save (r2 v0) 24
           move (r3 v0) (r2 v0)
           move (r0 v0) (r1 v0)
           save (r0 v0) 32
           lc (r0 v0)
           save (r1 v0) 48
           save (r0 v0) 40
           restore 0 (r1 v0)
           move (r1 v0) (r0 v0)
           save (r1 v0) 0
           restore 24 (r2 v0)
           restore 32 (r0 v0)
           restore 48 (r1 v0)
           jump "L6"

       label "L6" $
           branch (r1 v0) "L5" "L2"

       label "L5" $ do
           save (r1 v0) 48
           restore 0 (r1 v0)
           save (r2 v0) 24
           save (r1 v0) 0
           restore 24 (r2 v0)
           restore 48 (r1 v0)
           restore 8 (r0 v0)
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
        lc (r0 v0)
        lc (r1 v1)
        add (r0 v0) (r1 v1) (r2 v2)
        add (r2 v2) (r1 v1) (r3 v3)
        add (r3 v3) (r2 v2) (r0 v4)
        save (r1 v1) 0
        add (r0 v4) (r3 v3) (r1 v5)
        save (r2 v2) 32
        save (r3 v3) 24
        save (r0 v4) 16
        save (r1 v5) 8
        call 1000
        restore 32 (r1 v2)
        restore 24 (r2 v3)
        add (r1 v2) (r2 v3) (r0 v6)
        restore 16 (r2 v4)
        restore 8 (r3 v5)
        add (r2 v4) (r3 v5) (r1 v7)
        add (r0 v6) (r1 v7) (r0 v8)
        restore 0 (r1 v1)
        add (r0 v8) (r1 v1) (r0 v0)
        return_

loopTests :: SpecWith ()
loopTests = do
  it "Correctly orders loop blocks" $ asmTestLiteral 4
    (do label "entry" $ do
            trace "B0"
            jump "B1"

        label "B1" $ do
            trace "B1"
            branch v1 "B3" "B2"

        label "B2" $ do
            trace "B2"
            branch v1 "B5" "B4"

        label "B3" $ do
            trace "B3"
            jump "B1"

        label "B4" $ do
            trace "B4"
            branch v1 "B7" "B6"

        label "B5" $ do
            trace "B5"
            return_

        label "B6" $ do
            trace "B6"
            jump "B4"

        label "B7" $ do
            trace "B7"
            jump "B1") $

    Just "L1:\n\
\\ttrace  \"B0\"\n\
\\tjump L2\n\
\L2:\n\
\\ttrace  \"B1\"\n\
\\tbranch 0 L3 L4\n\
\L3:\n\
\\ttrace  \"B3\"\n\
\\tjump L2\n\
\L4:\n\
\\ttrace  \"B2\"\n\
\\tbranch 0 L5 L9\n\
\L5:\n\
\\ttrace  \"B5\"\n\
\\treturn [] nop\n\
\L6:\n\
\\ttrace  \"B4\"\n\
\\tbranch 0 L7 L8\n\
\L7:\n\
\\ttrace  \"B7\"\n\
\\tjump L2\n\
\L8:\n\
\\ttrace  \"B6\"\n\
\\tjump L6\n\
\L9:\n\
\\tjump L6\n"
