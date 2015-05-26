module Programs.Exhaustion1 where

import Assembly
import LinearScan.Hoopl.DSL

exhaustion1 :: Program (Node IRVar)
exhaustion1 = do
    label "entry" $ do
        move pr1 v4
        move pr2 v5
        move pr3 v6
        alloc Nothing pr0
        move v4 v1
        move v5 v2
        move v6 v3
        lc v8
        move v8 v9
        lc v10
        move v10 v11
        lc v12
        move v12 v13
        move v1 v16
        copy v16 v14
        offlpi v16
        copy v16 v15
        move v14 v17
        move v15 v18
        move v2 v21
        copy v21 v19
        offlpi v21
        copy v21 v20
        move v19 v22
        move v20 v23
        move v3 v26
        copy v26 v24
        offlpi v26
        copy v26 v25
        move v25 v28
        move v17 v30
        move v30 v29
        move v29 v32
        move v22 v34
        move v34 v33
        move v33 v36
        move v18 v39
        move v39 v37
        move v37 v40
        move v23 v43
        move v43 v41
        move v41 v44
        move v28 v47
        move v47 v45
        move v45 v48
        jump "L2"

    label "L2" $ do
        move v9 v51
        move v32 v52
        lc v53
        move v53 v54
        lc v55
        move v55 v56
        move v51 v58
        move v52 v59
        add v58 v59 v60
        move v60 v57
        jump "L9"

    label "L9" $ do
        move v57 v61
        branch v61 "L10" "L11"

    label "L11" $ do
        move v54 v62
        jump "L12"

    label "L10" $ do
        move v56 v62
        jump "L12"

    label "L12" $ do
        move v62 v50
        jump "L8"

    label "L8" $ do
        branch v50 "L5" "L6"

    label "L6" $ do
        lc v49
        jump "L7"

    label "L5" $ do
        move v11 v64
        move v36 v65
        lc v66
        move v66 v67
        lc v68
        move v68 v69
        move v64 v71
        move v65 v72
        add v71 v72 v73
        move v73 v70
        jump "L18"

    label "L18" $ do
        move v70 v74
        branch v74 "L19" "L20"

    label "L20" $ do
        move v67 v75
        jump "L21"

    label "L19" $ do
        move v69 v75
        jump "L21"

    label "L21" $ do
        move v75 v63
        jump "L17"

    label "L17" $ do
        branch v63 "L14" "L15"

    label "L15" $ do
        lc v49
        jump "L7"

    label "L14" $ do
        move v13 v76
        lc v77
        lc v78
        move v78 v79
        lc v80
        move v80 v81
        move v76 v83
        move v77 v84
        add v83 v84 v85
        move v85 v82
        jump "L24"

    label "L24" $ do
        move v82 v86
        branch v86 "L25" "L26"

    label "L26" $ do
        move v79 v87
        jump "L27"

    label "L25" $ do
        move v81 v87
        jump "L27"

    label "L27" $ do
        move v87 v49
        jump "L7"

    label "L7" $ do
        branch v49 "L3" "L75"

    label "L3" $ do
        move v40 v90
        move v9 v91
        move v91 v92
        offp v92 v90 v89
        copy v89 v88
        move v88 v93
        move v44 v96
        move v11 v97
        move v97 v98
        offp v98 v96 v95
        copy v95 v94
        move v94 v99
        move v93 v113
        copy v113 v101
        move v99 v114
        copy v114 v102
        lc v103
        move v103 v104
        lc v105
        move v105 v106
        move v101 v108
        move v102 v109
        add v108 v109 v110
        move v110 v107
        jump "L33"

    label "L33" $ do
        move v107 v111
        branch v111 "L34" "L35"

    label "L35" $ do
        move v104 v112
        jump "L36"

    label "L34" $ do
        move v106 v112
        jump "L36"

    label "L36" $ do
        move v112 v100
        jump "L32"

    label "L32" $ do
        branch v100 "L29" "L30"

    label "L30" $ do
        move v44 v130
        move v11 v131
        move v131 v132
        offp v132 v130 v129
        copy v129 v128
        move v48 v134
        move v13 v135
        move v135 v136
        offp v136 v134 v133
        move v128 v133
        move v11 v138
        lc v139
        add v138 v139 v140
        move v140 v137
        jump "L39"

    label "L39" $ do
        move v137 v11
        move v93 v145
        copy v145 v142
        move v99 v146
        copy v146 v143
        add v142 v143 v144
        move v144 v141
        jump "L43"

    label "L43" $ do
        branch v141 "L40" "L73"

    label "L40" $ do
        move v9 v148
        lc v149
        add v148 v149 v150
        move v150 v147
        jump "L44"

    label "L44" $ do
        move v147 v9
        jump "L31"

    label "L29" $ do
        move v40 v117
        move v9 v118
        move v118 v119
        offp v119 v117 v116
        copy v116 v115
        move v48 v121
        move v13 v122
        move v122 v123
        offp v123 v121 v120
        move v115 v120
        move v9 v125
        lc v126
        add v125 v126 v127
        move v127 v124
        jump "L38"

    label "L38" $ do
        move v124 v9
        jump "L31"

    label "L31" $ do
        move v13 v152
        lc v153
        add v152 v153 v154
        move v154 v151
        jump "L45"

    label "L45" $ do
        move v151 v13
        jump "L2"

    label "L73" $ do
        move v206 v210
        branch v210 "L74" "L75"

    label "L75" $ do
        jump "L46"

    label "L46" $ do
        move v9 v157
        move v32 v158
        lc v159
        move v159 v160
        lc v161
        move v161 v162
        move v157 v164
        move v158 v165
        add v164 v165 v166
        move v166 v163
        jump "L53"

    label "L53" $ do
        move v163 v167
        branch v167 "L54" "L55"

    label "L55" $ do
        move v160 v168
        jump "L56"

    label "L54" $ do
        move v162 v168
        jump "L56"

    label "L56" $ do
        move v168 v156
        jump "L52"

    label "L52" $ do
        branch v156 "L49" "L50"

    label "L50" $ do
        lc v155
        jump "L51"

    label "L49" $ do
        move v13 v169
        lc v170
        lc v171
        move v171 v172
        lc v173
        move v173 v174
        move v169 v176
        move v170 v177
        add v176 v177 v178
        move v178 v175
        jump "L59"

    label "L59" $ do
        move v175 v179
        branch v179 "L60" "L61"

    label "L61" $ do
        move v172 v180
        jump "L62"

    label "L60" $ do
        move v174 v180
        jump "L62"

    label "L62" $ do
        move v180 v155
        jump "L51"

    label "L51" $ do
        branch v155 "L47" "L74"

    label "L47" $ do
        move v40 v183
        move v9 v184
        move v184 v185
        offp v185 v183 v182
        copy v182 v181
        move v48 v187
        move v13 v188
        move v188 v189
        offp v189 v187 v186
        move v181 v186
        move v9 v191
        lc v192
        add v191 v192 v193
        move v193 v190
        jump "L64"

    label "L64" $ do
        move v190 v9
        move v13 v195
        lc v196
        add v195 v196 v197
        move v197 v194
        jump "L65"

    label "L65" $ do
        move v194 v13
        jump "L46"

    label "L74" $ do
        jump "L66"

    label "L66" $ do
        move v11 v200
        move v36 v201
        lc v202
        move v202 v203
        lc v204
        move v204 v205
        move v200 v207
        move v201 v208
        add v207 v208 v209
        move v209 v206
        jump "L73"
