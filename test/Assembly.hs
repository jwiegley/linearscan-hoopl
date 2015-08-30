{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Assembly where

import           Compiler.Hoopl as Hoopl hiding ((<*>))
import           Control.Applicative
import           Data.Foldable as F
import qualified Data.List
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Traversable
import           Lens.Family hiding (Constant)
import           LinearScan
import           LinearScan.Hoopl
import           LinearScan.Hoopl.DSL

default (Int)

-- | The basic instructions that have nothing to do with control flow.
data Instruction v
    = Add v v v
    | Offp v v v
    | Offlpi v
    | Nop
    deriving (Eq, Functor, Foldable, Traversable)

instance Show r => Show (Instruction r) where
    show (Add x1 x2 x3) =
        "add " ++ show x1 ++ " " ++ show x2 ++ " " ++ show x3
    show (Offp x1 x2 x3) =
        "offp " ++ show x1 ++ " " ++ show x2 ++ " " ++ show x3
    show (Offlpi x) = "offlpi " ++ show x
    show Nop = "nop"

-- | Tests used for branching (correspond to branching instructions)
data Test = Zero                -- ^ beq
          | NonZero             -- ^ bne
          | Positive            -- ^ bgt
          | Negative            -- ^ blt
          deriving (Eq, Show)

data CConv = InlineC
           | CConvC { ccArgs     :: [PhysReg]
                    , ccResults  :: [PhysReg]
                    , ccIsBrack  :: Bool
                    }
    deriving (Eq, Show)

type Src a      = a -- ^ Type synonym for indicating source operands
type Dst a      = a -- ^ Type synonym for indicating destination operands
type Success a  = a -- ^ Type synonym for indicating success or true branch
type Failure a  = a -- ^ Type synonym for indicating failure or false branch

data Node v e x where
    Label :: Label -> Node v C O

    Alloc     :: Maybe (Src v) -> Dst v -> Node v O O
    Reclaim   :: Src v -> Node v O O
    Instr     :: Instruction v -> Node v O O
    Call      :: CConv -> Int -> Node v O O
    LoadConst :: Int -> Dst v -> Node v O O
    Move      :: Src v -> Dst v -> Node v O O
    Copy      :: Src v -> Dst v -> Node v O O
    Save      :: Src v -> Dst Int -> Node v O O
    Restore   :: Src Int -> Dst v -> Node v O O
    Trace     :: String -> Node v O O

    Jump        :: Label -> Node v O C
    Branch      :: Test -> v -> Success Label -> Failure Label -> Node v O C
    ReturnInstr :: [PhysReg] -> Instruction v -> Node v O C

deriving instance Eq v => Eq (Node v e x)

instance (Show v, VarReg v) => Show (Node v e x) where
    show (Label l)         = "    label \"" ++ show l ++ "\" $ do"
    show (Alloc (Just x1) x2) =
        "        alloc (Just " ++ show x1 ++ ") " ++ show x2
    show (Alloc x1 x2)     = "        alloc " ++ show x1 ++ " " ++ show x2
    show (Reclaim v)       = "        reclaim " ++ show v
    show (Instr i)         = "        " ++ show i
    show (Call _ l)        = "        call " ++ show l
    show (LoadConst _c v)  = "        lc " ++ show v -- ++ " " ++ show c
    show (Move x1 x2)      = "        move " ++ show x1 ++ " " ++ show x2
    show (Copy x1 x2)      = "        copy " ++ show x1 ++ " " ++ show x2
    show (Save src dst)    = "        save " ++ show src ++ " " ++ show dst
    show (Restore src dst) = "        restore " ++ show src ++ " " ++ show dst
    show (Trace str)       = "        trace " ++ " " ++ show str
    show (Jump l)          = "        jump \"" ++ show l ++ "\""
    show (Branch _ v t f)  = "        branch " ++ show v
                          ++ " \"" ++ show t ++ "\" \"" ++ show f ++ "\""
    show (ReturnInstr _regs _i) = "        return_"

instance NonLocal (Node v) where
  entryLabel (Label l) = l

  successors (Jump l)          = [l]
  successors (Branch _ _ t f)  = [t, f]
  successors (ReturnInstr _ _) = []

instance HooplNode (Node v) where
    mkBranchNode = Jump
    mkLabelNode  = Label

vinfo k en = VarInfo
    { varId       = en
    , varKind     = k
    , regRequired = True
    }

vinfos k en = [vinfo k en]

instrVars :: Applicative f
          => LensLike f (Instruction v1) (Instruction v2)
                        (v1, Either PhysReg VarId -> [VarInfo]) v2
instrVars f = go
  where
    go Nop             = pure Nop
    go (Add s1 s2 d1)  = Add <$> f (s1, vinfos Input)
                             <*> f (s2, vinfos Input)
                             <*> f (d1, vinfos Output)
    go (Offp x1 x2 x3) = Offp <$> f (x1, vinfos Input)
                              <*> f (x2, vinfos Input)
                              <*> f (x3, vinfos Output)
    go (Offlpi x)      = Offlpi <$> f (x, vinfos InputOutput)

-- Traverse the input and temp/output variables of each instruction.
variables :: Applicative f
          => LensLike f (Node v1 e x) (Node v2 e x)
                        (v1, Either PhysReg VarId -> [VarInfo]) v2
variables f = go
  where
    go (Alloc msrc dst) =
        Alloc <$> traverse (\x -> f (x, vinfos Input)) msrc
              <*> f (dst, \en -> vinfos Output en <> blockRegs2to16)
      where
        blockRegs2to16 = [ vinfo Output (Left n) | n <- [2..16] ]

    go (Reclaim src)          = Reclaim <$> f (src, vinfos InputOutput)
    go (Instr i)              = Instr <$> sequenceA (over instrVars f i)
    go (LoadConst c dst)      = LoadConst c <$> f (dst, vinfos Output)
    go (Move src dst)         = Move <$> f (src, vinfos Input)
                                     <*> f (dst, vinfos Output)
    go (Copy src dst)         = Copy <$> f (src, vinfos Input)
                                     <*> f (dst, vinfos Output)
    go (Save src x)           = Save <$> f (src, vinfos Input) <*> pure x
    go (Restore x src)        = Restore x <$> f (src, vinfos Output)
    go (Trace str)            = pure $ Trace str
    go (Branch x1 cond x2 x3) = Branch x1 <$> f (cond, vinfos Input)
                                          <*> pure x2
                                          <*> pure x3
    go (Call cc i)            = pure $ Call cc i
    go (ReturnInstr rs i)     = ReturnInstr rs
                                    <$> sequenceA (over instrVars f i)
    go (Label x)              = pure $ Label x
    go (Jump x)               = pure $ Jump x

add :: v -> v -> v -> BodyNode (Node v)
add x0 x1 x2 = bodyNode $ Instr (Add x0 x1 x2)

offp :: v -> v -> v -> BodyNode (Node v)
offp x0 x1 x2 = bodyNode $ Instr (Offp x0 x1 x2)

offlpi :: v -> BodyNode (Node v)
offlpi x = bodyNode $ Instr (Offlpi x)

alloc :: Maybe v -> v -> BodyNode (Node v)
alloc src dst = bodyNode $ Alloc src dst

nop :: BodyNode (Node v)
nop = bodyNode $ Instr Nop

move :: v -> v -> BodyNode (Node v)
move x0 x1 = bodyNode $ Move x0 x1

copy :: v -> v -> BodyNode (Node v)
copy = move

call :: Int -> BodyNode (Node v)
call dst = bodyNode $ Call InlineC dst

lc :: v -> BodyNode (Node v)
lc x0 = bodyNode $ LoadConst 0 x0

save :: v -> Dst PhysReg -> BodyNode (Node v)
save r dst = bodyNode $ Save r dst

restore :: Src PhysReg -> v -> BodyNode (Node v)
restore src r = bodyNode $ Restore src r

trace :: String -> BodyNode (Node v)
trace str = bodyNode $ Trace str

branch :: v -> String -> String -> EndNode (Node v)
branch v good bad =
    endNode $ Branch Zero v <$> getLabel good <*> getLabel bad

return_ :: EndNode (Node v)
return_ = endNode $ return $ ReturnInstr [] Nop

data Assign a b = Assign a b

instance Show a => Show (Assign a PhysReg) where
    show (Assign v (-1)) = "<<v" ++ show v ++ ">>"
    show (Assign v r)    = "(r" ++ show r ++ " v" ++ show v ++ ")"

class VarReg a where
    varReg :: a -> PhysReg

instance VarReg IRVar where
    varReg (PhysicalIV r) = r
    varReg v = error $ "No register known for " ++ show v

instance VarReg (Assign a PhysReg) where
    varReg (Assign _ b) = b

data IRVar = PhysicalIV PhysReg | VirtualIV VarId deriving Eq

instance Show IRVar where
    show (PhysicalIV r) = "pr" ++ show r
    show (VirtualIV n)  = "v" ++ show n

instance NodeAlloc (Node IRVar) (Node (Assign VarId PhysReg)) where
    isCall (Call {}) = True
    isCall _         = False

    isBranch (Jump {})   = True
    isBranch (Branch {}) = True
    isBranch _           = False

    retargetBranch (Jump _) _ lab = Jump lab
    retargetBranch (Branch b v x y) old lab
        | x == old  = Branch b v lab y
        | otherwise = Branch b v x lab
    retargetBranch x _ _ = error $ "Cannot retarget " ++ show x

    mkLabelOp = Label
    mkJumpOp  = Jump

    getReferences = (^. variables.to f)
      where
        f (PhysicalIV r, k) = k (Left r)
        f (VirtualIV v,  k) = k (Right v)

    setRegisters m = return . over variables go
      where
        go (PhysicalIV r, _) = Assign (-1) r
        go (VirtualIV  n, k) = case k (Right n) of
            [] -> error "No kind assigned to variable"
            v : _ ->
                Assign n (fromMaybe (-1) (Data.List.lookup (n, varKind v) m))

    mkMoveOps sreg svar dreg =
        return [Move (Assign svar sreg) (Assign svar dreg)]

    mkSaveOps sreg dvar = do
        off <- getStackSlot (Just dvar)
        return [Save (Assign dvar sreg) off]

    mkRestoreOps svar dreg = do
        off <- getStackSlot (Just svar)
        return [Restore off (Assign svar dreg)]

    op1ToString = show

var :: Int -> IRVar
var = VirtualIV

v0   = var 0
v1   = var 1
v2   = var 2
v3   = var 3
v4   = var 4
v5   = var 5
v6   = var 6
v7   = var 7
v8   = var 8
v9   = var 9
v10  = var 10
v11  = var 11
v12  = var 12
v13  = var 13
v14  = var 14
v15  = var 15
v16  = var 16
v17  = var 17
v18  = var 18
v19  = var 19
v20  = var 20
v21  = var 21
v22  = var 22
v23  = var 23
v24  = var 24
v25  = var 25
v26  = var 26
v27  = var 27
v28  = var 28
v29  = var 29
v30  = var 30
v31  = var 31
v32  = var 32
v33  = var 33
v34  = var 34
v35  = var 35
v36  = var 36
v37  = var 37
v38  = var 38
v39  = var 39
v40  = var 40
v41  = var 41
v42  = var 42
v43  = var 43
v44  = var 44
v45  = var 45
v46  = var 46
v47  = var 47
v48  = var 48
v49  = var 49
v50  = var 50
v51  = var 51
v52  = var 52
v53  = var 53
v54  = var 54
v55  = var 55
v56  = var 56
v57  = var 57
v58  = var 58
v59  = var 59
v60  = var 60
v61  = var 61
v62  = var 62
v63  = var 63
v64  = var 64
v65  = var 65
v66  = var 66
v67  = var 67
v68  = var 68
v69  = var 69
v70  = var 70
v71  = var 71
v72  = var 72
v73  = var 73
v74  = var 74
v75  = var 75
v76  = var 76
v77  = var 77
v78  = var 78
v79  = var 79
v80  = var 80
v81  = var 81
v82  = var 82
v83  = var 83
v84  = var 84
v85  = var 85
v86  = var 86
v87  = var 87
v88  = var 88
v89  = var 89
v90  = var 90
v91  = var 91
v92  = var 92
v93  = var 93
v94  = var 94
v95  = var 95
v96  = var 96
v97  = var 97
v98  = var 98
v99  = var 99
v100 = var 100
v101 = var 101
v102 = var 102
v103 = var 103
v104 = var 104
v105 = var 105
v106 = var 106
v107 = var 107
v108 = var 108
v109 = var 109
v110 = var 110
v111 = var 111
v112 = var 112
v113 = var 113
v114 = var 114
v115 = var 115
v116 = var 116
v117 = var 117
v118 = var 118
v119 = var 119
v120 = var 120
v121 = var 121
v122 = var 122
v123 = var 123
v124 = var 124
v125 = var 125
v126 = var 126
v127 = var 127
v128 = var 128
v129 = var 129
v130 = var 130
v131 = var 131
v132 = var 132
v133 = var 133
v134 = var 134
v135 = var 135
v136 = var 136
v137 = var 137
v138 = var 138
v139 = var 139
v140 = var 140
v141 = var 141
v142 = var 142
v143 = var 143
v144 = var 144
v145 = var 145
v146 = var 146
v147 = var 147
v148 = var 148
v149 = var 149
v150 = var 150
v151 = var 151
v152 = var 152
v153 = var 153
v154 = var 154
v155 = var 155
v156 = var 156
v157 = var 157
v158 = var 158
v159 = var 159
v160 = var 160
v161 = var 161
v162 = var 162
v163 = var 163
v164 = var 164
v165 = var 165
v166 = var 166
v167 = var 167
v168 = var 168
v169 = var 169
v170 = var 170
v171 = var 171
v172 = var 172
v173 = var 173
v174 = var 174
v175 = var 175
v176 = var 176
v177 = var 177
v178 = var 178
v179 = var 179
v180 = var 180
v181 = var 181
v182 = var 182
v183 = var 183
v184 = var 184
v185 = var 185
v186 = var 186
v187 = var 187
v188 = var 188
v189 = var 189
v190 = var 190
v191 = var 191
v192 = var 192
v193 = var 193
v194 = var 194
v195 = var 195
v196 = var 196
v197 = var 197
v198 = var 198
v199 = var 199
v200 = var 200
v201 = var 201
v202 = var 202
v203 = var 203
v204 = var 204
v205 = var 205
v206 = var 206
v207 = var 207
v208 = var 208
v209 = var 209
v210 = var 210
v211 = var 211
v212 = var 212
v213 = var 213
v214 = var 214
v215 = var 215
v216 = var 216
v217 = var 217
v218 = var 218
v219 = var 219
v220 = var 220
v221 = var 221
v222 = var 222
v223 = var 223
v224 = var 224
v225 = var 225
v226 = var 226
v227 = var 227
v228 = var 228
v229 = var 229
v230 = var 230
v231 = var 231
v232 = var 232
v233 = var 233
v234 = var 234
v235 = var 235
v236 = var 236
v237 = var 237
v238 = var 238
v239 = var 239
v240 = var 240
v241 = var 241
v242 = var 242
v243 = var 243
v244 = var 244
v245 = var 245
v246 = var 246
v247 = var 247
v248 = var 248
v249 = var 249
v250 = var 250
v251 = var 251
v252 = var 252
v253 = var 253
v254 = var 254
v255 = var 255
v256 = var 256
v257 = var 257
v258 = var 258
v259 = var 259
v260 = var 260
v261 = var 261
v262 = var 262
v263 = var 263
v264 = var 264
v265 = var 265
v266 = var 266
v267 = var 267
v268 = var 268
v269 = var 269
v270 = var 270
v271 = var 271
v272 = var 272
v273 = var 273
v274 = var 274
v275 = var 275
v276 = var 276
v277 = var 277
v278 = var 278
v279 = var 279
v280 = var 280
v281 = var 281
v282 = var 282
v283 = var 283
v284 = var 284
v285 = var 285
v286 = var 286
v287 = var 287
v288 = var 288
v289 = var 289
v290 = var 290
v291 = var 291
v292 = var 292
v293 = var 293
v294 = var 294
v295 = var 295
v296 = var 296
v297 = var 297
v298 = var 298
v299 = var 299
v300 = var 300
v301 = var 301
v302 = var 302
v303 = var 303
v304 = var 304
v305 = var 305
v306 = var 306
v307 = var 307
v308 = var 308
v309 = var 309
v310 = var 310
v311 = var 311
v312 = var 312
v313 = var 313
v314 = var 314
v315 = var 315
v316 = var 316
v317 = var 317
v318 = var 318
v319 = var 319
v320 = var 320
v321 = var 321
v322 = var 322
v323 = var 323
v324 = var 324
v325 = var 325
v326 = var 326
v327 = var 327
v328 = var 328
v329 = var 329
v330 = var 330
v331 = var 331
v332 = var 332
v333 = var 333
v334 = var 334
v335 = var 335
v336 = var 336
v337 = var 337
v338 = var 338
v339 = var 339
v340 = var 340
v341 = var 341
v342 = var 342
v343 = var 343
v344 = var 344
v345 = var 345
v346 = var 346
v347 = var 347
v348 = var 348
v349 = var 349
v350 = var 350
v351 = var 351
v352 = var 352
v353 = var 353
v354 = var 354
v355 = var 355
v356 = var 356
v357 = var 357
v358 = var 358
v359 = var 359
v360 = var 360
v361 = var 361
v362 = var 362
v363 = var 363
v364 = var 364
v365 = var 365
v366 = var 366
v367 = var 367
v368 = var 368
v369 = var 369
v370 = var 370
v371 = var 371
v372 = var 372
v373 = var 373
v374 = var 374
v375 = var 375
v376 = var 376
v377 = var 377
v378 = var 378
v379 = var 379
v380 = var 380
v381 = var 381
v382 = var 382
v383 = var 383
v384 = var 384
v385 = var 385
v386 = var 386
v387 = var 387
v388 = var 388
v389 = var 389
v390 = var 390
v391 = var 391
v392 = var 392
v393 = var 393
v394 = var 394
v395 = var 395
v396 = var 396
v397 = var 397
v398 = var 398
v399 = var 399
v400 = var 400
v401 = var 401
v402 = var 402
v403 = var 403
v404 = var 404
v405 = var 405
v406 = var 406
v407 = var 407
v408 = var 408
v409 = var 409
v410 = var 410
v411 = var 411
v412 = var 412
v413 = var 413
v414 = var 414
v415 = var 415
v416 = var 416
v417 = var 417
v418 = var 418
v419 = var 419
v420 = var 420
v421 = var 421
v422 = var 422
v423 = var 423
v424 = var 424
v425 = var 425
v426 = var 426
v427 = var 427
v428 = var 428
v429 = var 429
v430 = var 430
v431 = var 431
v432 = var 432
v433 = var 433
v434 = var 434
v435 = var 435
v436 = var 436
v437 = var 437
v438 = var 438
v439 = var 439
v440 = var 440
v441 = var 441
v442 = var 442
v443 = var 443
v444 = var 444
v445 = var 445
v446 = var 446
v447 = var 447
v448 = var 448
v449 = var 449
v450 = var 450
v451 = var 451
v452 = var 452
v453 = var 453
v454 = var 454
v455 = var 455
v456 = var 456
v457 = var 457
v458 = var 458
v459 = var 459
v460 = var 460
v461 = var 461
v462 = var 462
v463 = var 463
v464 = var 464
v465 = var 465
v466 = var 466
v467 = var 467
v468 = var 468
v469 = var 469
v470 = var 470
v471 = var 471
v472 = var 472
v473 = var 473
v474 = var 474
v475 = var 475
v476 = var 476
v477 = var 477
v478 = var 478
v479 = var 479
v480 = var 480
v481 = var 481
v482 = var 482
v483 = var 483
v484 = var 484
v485 = var 485
v486 = var 486
v487 = var 487
v488 = var 488
v489 = var 489
v490 = var 490
v491 = var 491
v492 = var 492
v493 = var 493
v494 = var 494
v495 = var 495
v496 = var 496
v497 = var 497
v498 = var 498
v499 = var 499
v500 = var 500

physreg :: PhysReg -> IRVar
physreg = PhysicalIV

pr0  = physreg 0
pr1  = physreg 1
pr2  = physreg 2
pr3  = physreg 3
pr4  = physreg 4
pr5  = physreg 5
pr6  = physreg 6
pr7  = physreg 7
pr8  = physreg 8
pr9  = physreg 9
pr10 = physreg 10
pr11 = physreg 11
pr12 = physreg 12
pr13 = physreg 13
pr14 = physreg 14
pr15 = physreg 15
pr16 = physreg 16
pr17 = physreg 17
pr18 = physreg 18
pr19 = physreg 19
pr20 = physreg 20
pr21 = physreg 21
pr22 = physreg 22
pr23 = physreg 23
pr24 = physreg 24
pr25 = physreg 25
pr26 = physreg 26
pr27 = physreg 27
pr28 = physreg 28
pr29 = physreg 29
pr30 = physreg 30
pr31 = physreg 31
pr32 = physreg 32

reg :: PhysReg -> IRVar -> Assign VarId PhysReg
reg _ (PhysicalIV _) = error "Don't use reg to reference a literal register"
reg r (VirtualIV v) = Assign v r

r0  = reg 0
r1  = reg 1
r2  = reg 2
r3  = reg 3
r4  = reg 4
r5  = reg 5
r6  = reg 6
r7  = reg 7
r8  = reg 8
r9  = reg 9
r10 = reg 10
r11 = reg 11
r12 = reg 12
r13 = reg 13
r14 = reg 14
r15 = reg 15
r16 = reg 16
r17 = reg 17
r18 = reg 18
r19 = reg 19
r20 = reg 20
r21 = reg 21
r22 = reg 22
r23 = reg 23
r24 = reg 24
r25 = reg 25
r26 = reg 26
r27 = reg 27
r28 = reg 28
r29 = reg 29
r30 = reg 30
r31 = reg 31
r32 = reg 32
