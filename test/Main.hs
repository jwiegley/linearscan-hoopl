{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import AsmTest
import Assembly
import Generated
import LinearScan (UseVerifier(..))
import LinearScan.Hoopl.DSL
-- import Programs.Blocked
import Programs.Exhaustion
import Programs.Ordered
import Programs.Overcommit
import Programs.Incoming
import Programs.Residency
import Programs.Residency2
import Programs.BranchAlloc
import Programs.Returned
import Programs.Restoration
import Programs.Allocation
import Programs.Allocation2
import Programs.Allocation3
import Programs.Allocation4
import Programs.Allocation5
import Programs.UponEntry
import Programs.Overlapped
import Programs.ReturnAssign
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

  describe "Edge-case tests" $ do
    let runTest k = asmTestLiteral VerifyEnabled 32 k Nothing
    it "Near exhaustion program" $ asmTest_ 32 exhaustion1
    -- it "Blocked register program"                  $ runTest regBlocked
    it "Orders reservations"                       $ runTest regOrdered
    it "Guards against over-committing"            $ runTest overCommitted
    it "Properly reserves incoming registers"      $ runTest regsIncoming
    it "Handles edge-case 1 residency scenario"    $ runTest residency
    it "Handles edge-case 2 residency scenario"    $ runTest residency2
    it "A case of residency involving branches"    $ runTest branchAlloc
    it "Frees registers properly before returning" $ runTest freeBeforeReturn
    it "Restoration after a graph edge split"      $ runTest restoration
    it "Handles edge-case 1 allocation scenario"   $ runTest allocation
    it "Handles edge-case 2 allocation scenario"   $ runTest allocation2
    it "Handles edge-case 3 allocation scenario"   $ runTest allocation3
    it "Handles edge-case 4 allocation scenario"   $ runTest allocation4
    it "Handles edge-case 5 allocation scenario"   $ runTest allocation5
    it "Allocates correctly on block entry"        $ runTest uponEntry
    it "Register over-allocation edge-case"        $ runTest overlapped
    it "Does not assign after a return_"             $ runTest returnAssign

  describe "Generated tests" generatedTests

sanityTests :: SpecWith ()
sanityTests = do
  it "Single instruction" $ asmTest 32
    (label "entry" $ do
        lc v0
        lc v1
        add v0 v1 v2
        return_) $

    label "entry" $ do
        lc (r0 v0)
        lc (r1 v1)
        add (r0 v0) (r1 v1) (r0 v2)
        return_

  it "Single, repeated instruction" $ asmTest 32
    (label "entry" $ do
        lc v0
        lc v1
        add v0 v1 v2
        add v0 v1 v2
        add v0 v1 v2
        return_) $

    label "entry" $ do
        lc (r0 v0)
        lc (r1 v1)
        add (r0 v0) (r1 v1) (r2 v2)
        add (r0 v0) (r1 v1) (r2 v2)
        add (r0 v0) (r1 v1) (r2 v2)
        return_

  it "Multiple instructions" $ asmTest 32
    (label "entry" $ do
        lc v0
        lc v1
        add v0 v1 v2
        add v0 v1 v3
        add v0 v1 v2
        return_) $

    label "entry" $ do
        lc (r0 v0)
        lc (r1 v1)
        add (r0 v0) (r1 v1) (r2 v2)
        add (r0 v0) (r1 v1) (r3 v3)
        add (r0 v0) (r1 v1) (r2 v2)
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
        lc (r1 v1)
        lc (r2 v3)
        lc (r3 v4)
        lc (r4 v6)
        lc (r5 v7)
        save (r5 v7) 0
        lc (r5 v9)
        save (r5 v9) 8
        lc (r5 v10)
        save (r5 v10) 16
        lc (r5 v12)
        save (r5 v12) 24
        lc (r5 v13)
        add (r0 v0) (r1 v1) (r0 v2)
        add (r2 v3) (r3 v4) (r1 v5)
        restore 0 (r2 v7)
        add (r4 v6) (r2 v7) (r2 v8)
        restore 16 (r4 v10)
        restore 8 (r3 v9)
        add (r3 v9) (r4 v10) (r3 v11)
        restore 24 (r4 v12)
        add (r4 v12) (r5 v13) (r4 v14)
        return_

  it "Single long-lived variable" $ asmTest 32
    (label "entry" $ do
        lc v0
        lc v1
        lc v4
        lc v7
        lc v10
        add v0 v1 v2
        add v0 v4 v5
        add v0 v7 v8
        add v0 v10 v11
        return_) $

    label "entry" $ do
        lc (r0 v0)
        lc (r1 v1)
        lc (r2 v4)
        lc (r3 v7)
        lc (r4 v10)
        add (r0 v0) (r1 v1) (r1 v2)
        add (r0 v0) (r2 v4) (r2 v5)
        add (r0 v0) (r3 v7) (r3 v8)
        add (r0 v0) (r4 v10) (r0 v11)
        return_

  it "Two long-lived variables" $ asmTest 32
    (label "entry" $ do
        lc v0
        lc v1
        lc v4
        add v0 v1 v2
        add v0 v4 v5
        add v0 v4 v8
        add v0 v4 v11
        return_) $

    label "entry" $ do
        lc (r0 v0)
        lc (r1 v1)
        lc (r2 v4)
        add (r0 v0) (r1 v1) (r1 v2)
        add (r0 v0) (r2 v4) (r3 v5)
        add (r0 v0) (r2 v4) (r4 v8)
        add (r0 v0) (r2 v4) (r0 v11)
        return_

  it "One variable with a long interval" $ asmTest 32
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
        lc v15
        lc v16
        lc v18
        lc v19
        lc v21
        lc v22
        lc v24
        lc v25
        lc v27
        lc v28
        lc v30
        lc v31
        lc v34
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
        lc (r0 v0)
        lc (r1 v1)
        lc (r2 v3)
        lc (r3 v4)
        lc (r4 v6)
        lc (r5 v7)
        lc (r6 v9)
        lc (r7 v10)
        lc (r8 v12)
        lc (r9 v13)
        lc (r10 v15)
        lc (r11 v16)
        lc (r12 v18)
        lc (r13 v19)
        lc (r14 v21)
        lc (r15 v22)
        lc (r16 v24)
        lc (r17 v25)
        lc (r18 v27)
        lc (r19 v28)
        lc (r20 v30)
        lc (r21 v31)
        lc (r22 v34)
        add (r0  v0)  (r1  v1)  (r1  v2)
        add (r2  v3)  (r3  v4)  (r2  v5)
        add (r4  v6)  (r5  v7)  (r3  v8)
        add (r6  v9)  (r7  v10) (r4  v11)
        add (r8  v12) (r9  v13) (r5  v14)
        add (r10 v15) (r11 v16) (r6  v17)
        add (r12 v18) (r13 v19) (r7  v20)
        add (r14 v21) (r15 v22) (r8  v23)
        add (r16 v24) (r17 v25) (r9  v26)
        add (r18 v27) (r19 v28) (r10 v29)
        add (r20 v30) (r21 v31) (r11 v32)
        add (r0  v0)  (r22 v34) (r0  v35)
        return_

  it "Many variables with long intervals" $ asmTest 32
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
        lc v15
        lc v16
        lc v18
        lc v19
        lc v21
        lc v22
        lc v24
        lc v25
        lc v27
        lc v28
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
        lc (r0 v0)
        lc (r1 v1)
        lc (r2 v3)
        lc (r3 v4)
        lc (r4 v6)
        lc (r5 v7)
        lc (r6 v9)
        lc (r7 v10)
        lc (r8 v12)
        lc (r9 v13)
        lc (r10 v15)
        lc (r11 v16)
        lc (r12 v18)
        lc (r13 v19)
        lc (r14 v21)
        lc (r15 v22)
        lc (r16 v24)
        lc (r17 v25)
        lc (r18 v27)
        lc (r19 v28)
        add (r0  v0)  (r1 v1)   (r20 v2)
        add (r2  v3)  (r3 v4)   (r21 v5)
        add (r4  v6)  (r5 v7)   (r22 v8)
        add (r6  v9)  (r7 v10)  (r23 v11)
        add (r8  v12) (r9 v13)  (r24 v14)
        add (r10 v15) (r11 v16) (r25 v17)
        add (r12 v18) (r13 v19) (r26 v20)
        add (r14 v21) (r15 v22) (r27 v23)
        add (r16 v24) (r17 v25) (r28 v26)
        add (r18 v27) (r19 v28) (r29 v29)
        add (r0  v0)  (r1 v1)   (r20 v2)
        add (r2  v3)  (r3 v4)   (r21 v5)
        add (r4  v6)  (r5 v7)   (r22 v8)
        add (r6  v9)  (r7 v10)  (r23 v11)
        add (r8  v12) (r9 v13)  (r24 v14)
        add (r10 v15) (r11 v16) (r25 v17)
        add (r12 v18) (r13 v19) (r26 v20)
        add (r14 v21) (r15 v22) (r27 v23)
        add (r16 v24) (r17 v25) (r28 v26)
        add (r18 v27) (r19 v28) (r29 v29)
        return_

spillTests :: SpecWith ()
spillTests = do
  it "No spilling necessary" $ asmTest 32
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
        lc v15
        lc v16
        lc v18
        lc v19
        lc v21
        lc v22
        lc v24
        lc v25
        lc v27
        lc v28
        lc v30
        lc v31
        lc v33
        lc v34
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
        add v33 v34 v35
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
        add v33 v34 v35
        return_) $

    label "entry" $ do
        lc (r0 v0)
        lc (r1 v1)
        lc (r2 v3)
        lc (r3 v4)
        lc (r4 v6)
        lc (r5 v7)
        lc (r6 v9)
        lc (r7 v10)
        lc (r8 v12)
        lc (r9 v13)
        lc (r10 v15)
        lc (r11 v16)
        lc (r12 v18)
        lc (r13 v19)
        lc (r14 v21)
        lc (r15 v22)
        lc (r16 v24)
        lc (r17 v25)
        lc (r18 v27)
        lc (r19 v28)
        lc (r20 v30)
        lc (r21 v31)
        lc (r22 v33)
        lc (r23 v34)
        add (r0  v0) (r1 v1) (r24 v2)
        add (r2  v3) (r3 v4) (r25 v5)
        add (r4  v6) (r5 v7) (r26 v8)
        add (r6  v9) (r7 v10) (r27 v11)
        add (r8  v12) (r9 v13) (r28 v14)
        add (r10 v15) (r11 v16) (r29 v17)
        add (r12 v18) (r13 v19) (r30 v20)
        add (r14 v21) (r15 v22) (r31 v23)
        add (r16 v24) (r17 v25) (r31 v26)
        add (r18 v27) (r19 v28) (r31 v29)
        add (r20 v30) (r21 v31) (r31 v32)
        add (r22 v33) (r23 v34) (r31 v35)
        add (r0 v0) (r1 v1) (r24 v2)
        add (r2 v3) (r3 v4) (r25 v5)
        add (r4 v6) (r5 v7) (r26 v8)
        add (r6 v9) (r7 v10) (r27 v11)
        add (r8 v12) (r9 v13) (r28 v14)
        add (r10 v15) (r11 v16) (r29 v17)
        add (r12 v18) (r13 v19) (r30 v20)
        add (r14 v21) (r15 v22) (r0 v23)
        add (r16 v24) (r17 v25) (r1 v26)
        add (r18 v27) (r19 v28) (r2 v29)
        add (r20 v30) (r21 v31) (r3 v32)
        add (r22 v33) (r23 v34) (r31 v35)
        return_

  it "Spilling one variable" $ asmTest 32
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
        lc v15
        lc v16
        lc v18
        lc v19
        lc v21
        lc v22
        lc v24
        lc v25
        lc v27
        lc v28
        lc v30
        lc v31
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
        add v27 v29 v28
        add v30 v32 v31
        return_) $

    label "entry" $ do
        lc (r0 v0)
        lc (r1 v1)
        lc (r2 v3)
        lc (r3 v4)
        lc (r4 v6)
        lc (r5 v7)
        lc (r6 v9)
        lc (r7 v10)
        lc (r8 v12)
        lc (r9 v13)
        lc (r10 v15)
        lc (r11 v16)
        lc (r12 v18)
        lc (r13 v19)
        lc (r14 v21)
        lc (r15 v22)
        lc (r16 v24)
        lc (r17 v25)
        lc (r18 v27)
        lc (r19 v28)
        lc (r20 v30)
        lc (r21 v31)
        add (r0  v0)  (r1  v1)  (r22 v2)
        add (r2  v3)  (r3  v4)  (r23 v5)
        add (r4  v6)  (r5  v7)  (r24 v8)
        add (r6  v9)  (r7  v10) (r25 v11)
        add (r8  v12) (r9  v13) (r26 v14)
        add (r10 v15) (r11 v16) (r27 v17)
        add (r12 v18) (r13 v19) (r28 v20)
        add (r14 v21) (r15 v22) (r29 v23)
        add (r16 v24) (r17 v25) (r30 v26)
        add (r18 v27) (r19 v28) (r31 v29)

        -- When we reach the 32nd variable considered (which happens to be
        -- v30), we must spill a register because there are not 32 registers.
        -- So we pick the first register, counting from 0, whose next use
        -- position is the furthest from this position.
        save (r31 v29) 0

        add (r20 v30) (r21 v31) (r31 v32)

        add (r0 v0)   (r1 v1)   (r22 v2)
        add (r2 v3)   (r3 v4)   (r23 v5)
        add (r4 v6)   (r5 v7)   (r24 v8)
        add (r6 v9)   (r7 v10)  (r25 v11)
        add (r8 v12)  (r9 v13)  (r26 v14)
        add (r10 v15) (r11 v16) (r27 v17)
        add (r12 v18) (r13 v19) (r28 v20)
        add (r14 v21) (r15 v22) (r29 v23)
        add (r16 v24) (r17 v25) (r30 v26)

        -- When it comes time to reload v29, we pick the first available
        -- register.
        restore 0 (r0 v29)

        add (r18 v27) (r0 v29) (r19 v28)
        add (r20 v30) (r31 v32) (r21 v31)
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
        lc (r1 v1)
        add (r0 v0) (r1 v1) (r2 v2)
        add (r2 v2) (r1 v1) (r0 v3)
        save (r1 v1) 0
        add (r0 v3) (r2 v2) (r1 v4)
        save (r2 v2) 8
        add (r1 v4) (r0 v3) (r2 v5)
        save (r2 v5) 16
        restore 8 (r2 v2)
        add (r2 v2) (r0 v3) (r0 v6)
        restore 16 (r2 v5)
        add (r1 v4) (r2 v5) (r1 v7)
        add (r0 v6) (r1 v7) (r0 v8)
        restore 0 (r1 v1)
        add (r0 v8) (r1 v1) (r0 v0)
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
        lc (r1 v1)
        add (r0 v0) (r1 v1) (r2 v2)
        add (r2 v2) (r1 v1) (r3 v3)
        save (r1 v1) 0
        add (r3 v3) (r2 v2) (r1 v4)
        add (r1 v4) (r3 v3) (r0 v5)
        add (r2 v2) (r3 v3) (r2 v6)
        add (r1 v4) (r0 v5) (r0 v7)
        add (r2 v6) (r0 v7) (r0 v8)
        restore 0 (r1 v1)
        add (r0 v8) (r1 v1) (r0 v0)
        return_

blockTests :: SpecWith ()
blockTests = do
  it "Allocates across blocks" $ asmTest 32
    (do label "entry" $ do
            lc v0
            lc v1
            add v0 v1 v2
            jump "L2"

        label "L2" $ do
            lc v3
            add v2 v3 v4
            add v2 v4 v5
            jump "L3"

        label "L3" $ do
            add v2 v5 v6
            add v2 v6 v7
            add v2 v7 v8
            return_) $ do

    label "entry" $ do
        lc (r0 v0)
        lc (r1 v1)
        add (r0 v0) (r1 v1) (r0 v2)
        jump "L2"

    label "L2" $ do
        lc (r1 v3)
        add (r0 v2) (r1 v3) (r1 v4)
        add (r0 v2) (r1 v4) (r1 v5)
        jump "L3"

    label "L3" $ do
        add (r0 v2) (r1 v5) (r1 v6)
        add (r0 v2) (r1 v6) (r1 v7)
        add (r0 v2) (r1 v7) (r0 v8)
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
            return_) $ do

    label "entry" $ do
        lc (r0 v0)
        lc (r1 v1)
        add (r0 v0) (r1 v1) (r2 v2)
        branch (r2 v2) "B3" "B2"

    label "B2" $ do
        save (r0 v0) 0
        add (r1 v1) (r2 v2) (r0 v3)
        restore 0 (r2 v0)
        add (r2 v0) (r0 v3) (r1 v4)
        save (r0 v3) 8
        add (r2 v0) (r1 v4) (r0 v5)
        add (r1 v4) (r0 v5) (r0 v6)
        restore 8 (r1 v3)
        add (r0 v6) (r1 v3) (r0 v7)
        jump "B4"

    label "B3" $ do
        move (r2 v2) (r0 v2)
        move (r1 v1) (r2 v1)
        add (r2 v1) (r0 v2) (r1 v3)
        jump "B4"

    label "B4" $ do
        add (r1 v3) (r1 v3) (r0 v0)
        return_

  it "When resolving moves are not needed" $ asmTest 4
    (do label "entry" $ do
            lc v0
            lc v1
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
            return_) $ do

    label "entry" $ do
        lc (r0 v0)
        lc (r1 v1)
        add (r0 v0) (r1 v1) (r2 v2)
        branch (r2 v2) "B2" "B3"

    label "B2" $ do
        add (r1 v1) (r2 v2) (r3 v3)
        add (r0 v0) (r0 v0) (r1 v4)
        add (r0 v0) (r0 v0) (r2 v5)
        add (r0 v0) (r1 v4) (r1 v6)
        add (r0 v0) (r2 v5) (r1 v6)
        jump "B4"

    label "B3" $ do
        add (r1 v1) (r2 v2) (r3 v3)
        jump "B4"

    label "B4" $ do
        add (r3 v3) (r3 v3) (r0 v0)
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
           lc (r0 v3)
           lc (r1 v4)
           lc (r2 v15)
           lc (r3 v20)
           save (r3 v20) 0
           jump "L3"

       label "L3" $ do
           move (r0 v3) (r3 v9)
           move (r3 v9) (r3 v11)
           move (r3 v11) (r3 v10)
           move (r3 v10) (r3 v12)
           move (r3 v12) (r3 v13)
           save (r0 v3) 8
           lc (r0 v14)
           save (r0 v14) 16
           move (r2 v15) (r0 v5)
           jump "L6"

       label "L2" $ do
           lc (r3 v21)
           save (r2 v15) 24
           move (r3 v21) (r2 v18)
           move (r0 v5) (r1 v4)
           save (r3 v21) 32
           lc (r3 v19)
           save (r2 v18) 40
           save (r3 v19) 48
           restore 0 (r3 v20)
           move (r3 v20) (r2 v17)
           save (r3 v20) 0
           restore 24 (r2 v15)
           jump "L6"

       label "L6" $
           branch (r1 v4) "L5" "L2"

       label "L5" $ do
           restore 0 (r3 v20)   -- jww (2015-05-26): should be unnecessary
           save (r2 v15) 24
           save (r3 v20) 0
           restore 24 (r2 v15)
           restore 8 (r0 v3)
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
        lc (r3 v1)
        add (r0 v0) (r3 v1) (r2 v2)
        add (r2 v2) (r3 v1) (r1 v3)
        save (r3 v1) 0
        add (r1 v3) (r2 v2) (r3 v4)
        save (r2 v2) 8
        add (r3 v4) (r1 v3) (r2 v5)
        save (r1 v3) 16
        save (r3 v4) 24
        save (r2 v5) 32
        call 1000
        restore 16 (r2 v3)
        restore 8 (r1 v2)
        add (r1 v2) (r2 v3) (r1 v6)
        restore 32 (r3 v5)
        restore 24 (r2 v4)
        add (r2 v4) (r3 v5) (r2 v7)
        add (r1 v6) (r2 v7) (r1 v8)
        restore 0 (r2 v1)
        add (r1 v8) (r2 v1) (r0 v0)
        return_

  it "Allocates between call instructions" $ asmTest 32
    (do label "entry" $ do
            lc v2
            lc v3
            lc v12
            lc v16
            lc v17
            lc v35
            lc v42
            lc v45
            lc v51
            lc v53
            lc v90
            lc v100
            call 97
            add v51 v45 v98
            copy v12 v64
            branch v42 "L92" "L62"

        label "L62" $ do
            lc v50
            jump "L12"

        label "L12" $ do
            nop
            move v100 v44
            call 30
            call 32
            jump "L15"

        label "L15" $ do
            lc v3
            call 35
            lc v73
            return_

        label "L92" $ do
            lc v95
            move v90 v43
            call 95
            call 64
            copy v53 v58
            add v98 v16 v100
            add v35 v3 v67
            copy v2 v24
            lc v13
            move v17 v32
            return_) $ do

    label "entry" $ do
        lc (r31 v2)
        save (r31 v2) 0
        lc (r31 v3)
        save (r31 v3) 8
        lc (r31 v12)
        save (r31 v12) 16
        lc (r31 v16)
        save (r31 v16) 24
        lc (r31 v17)
        save (r31 v17) 32
        lc (r31 v35)
        save (r31 v35) 40
        lc (r31 v42)
        save (r31 v42) 48
        lc (r31 v45)
        save (r31 v45) 56
        lc (r31 v51)
        save (r31 v51) 64
        lc (r31 v53)
        save (r31 v53) 72
        lc (r31 v90)
        save (r31 v90) 80
        lc (r31 v100)
        save (r31 v100) 88
        call 97
        restore 56 (r0 v45)
        restore 64 (r1 v51)
        add (r1 v51) (r0 v45) (r31 v98)
        restore 16 (r0 v12)
        move (r0 v12) (r0 v64)
        restore 48 (r1 v42)
        branch (r1 v42) "L2" "L3"

    label "L2" $ do
        save (r31 v98) 104
        lc (r31 v95)
        restore 80 (r1 v90)
        save (r31 v95) 112
        move (r1 v90) (r31 v43)
        save (r31 v43) 120
        call 95
        call 64
        restore 72 (r1 v53)
        move (r1 v53) (r1 v58)
        restore 24 (r2 v16)
        restore 104 (r3 v98)
        add (r3 v98) (r2 v16) (r0 v100)
        restore 8 (r3 v3)
        restore 40 (r2 v35)
        add (r2 v35) (r3 v3) (r2 v67)
        restore 0 (r3 v2)
        move (r3 v2) (r3 v24)
        lc (r4 v13)
        restore 32 (r5 v17)
        move (r5 v17) (r5 v32)
        return_

    label "L3" $ do
        lc (r0 v50)
        jump "L4"

    label "L4" $ do
        nop
        restore 88 (r0 v100)
        move (r0 v100) (r31 v44)
        save (r31 v44) 96
        call 30
        call 32
        jump "L5"

    label "L5" $ do
        lc (r31 v3)
        save (r31 v3) 8
        call 35
        lc (r1 v73)
        return_

loopTests :: SpecWith ()
loopTests = do
  it "Correctly orders loop blocks" $ asmTestLiteral VerifyEnabledStrict 4
    (do label "entry" $ do
            trace "B0"
            jump "B1"

        label "B1" $ do
            trace "B1"
            lc v1
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

    Just "    label \"L1\" $ do\n\
\        trace  \"B0\"\n\
\        jump \"L2\"\n\
\    label \"L2\" $ do\n\
\        trace  \"B1\"\n\
\        lc (r0 v1)\n\
\        branch (r0 v1) \"L3\" \"L4\"\n\
\    label \"L3\" $ do\n\
\        trace  \"B3\"\n\
\        jump \"L2\"\n\
\    label \"L4\" $ do\n\
\        trace  \"B2\"\n\
\        branch (r0 v1) \"L5\" \"L9\"\n\
\    label \"L5\" $ do\n\
\        trace  \"B5\"\n\
\        return_\n\
\    label \"L6\" $ do\n\
\        trace  \"B4\"\n\
\        branch (r0 v1) \"L7\" \"L8\"\n\
\    label \"L7\" $ do\n\
\        trace  \"B7\"\n\
\        jump \"L2\"\n\
\    label \"L8\" $ do\n\
\        trace  \"B6\"\n\
\        jump \"L6\"\n\
\    label \"L9\" $ do\n\
\        jump \"L6\"\n"
