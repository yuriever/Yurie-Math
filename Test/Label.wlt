

(* Label.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Label.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Label.nb"
]

VerificationTest[
    label[x, 1, Symbol]
    ,
    x1
    ,
    TestID->"[2] Label.nb"
]

VerificationTest[
    {label[x | y, 1, Symbol]}
    ,
    {x1, y1}
    ,
    TestID->"[3] Label.nb"
]

VerificationTest[
    Outer[label[x, #1, #2] & , {1, c, "c"}, {Symbol, Function, head}]
    ,
    {{x1, x[1], head[x, 1]}, {xc, x[c], head[x, c]}, {xc, x["c"], head[x, "c"]}}
    ,
    TestID->"[4] Label.nb"
]

VerificationTest[
    {label[(x_) | (y_), 1, Symbol]}
    ,
    {x1_, y1_}
    ,
    TestID->"[5] Label.nb"
]

VerificationTest[
    Outer[label[x_, #1, #2] & , {1, c, "c"}, {Symbol, Function, head}]
    ,
    {{x1_, (x_)[1], head[x_, 1]}, {xc_, (x_)[c], head[x_, c]}, {xc_, (x_)["c"], head[x_, "c"]}}
    ,
    TestID->"[6] Label.nb"
]

VerificationTest[
    {label[{x_, y__, z___, w_List, u:pat}, 1, Symbol]}
    ,
    {x1_, y1__, z1___, w1_List, u1:pat}
    ,
    TestID->"[7] Label.nb"
]

VerificationTest[
    ToExpression["Format[phat]:=OverHat[p]"]
    ,
    Null
    ,
    TestID->"[8] Label.nb"
]

VerificationTest[
    FullForm[label[phat, 1]]
    ,
    FullForm[phat[1]]
    ,
    TestID->"[9] Label.nb"
]

VerificationTest[
    FullForm[labelSplit[phat][phat1]]
    ,
    FullForm[phat[1]]
    ,
    TestID->"[10] Label.nb"
]

VerificationTest[
    ClearAll[phat]
    ,
    Null
    ,
    TestID->"[11] Label.nb"
]

VerificationTest[
    (labelRange[x, {1, 2}, #1] & ) /@ {Symbol, Function, head}
    ,
    {x1, x2, x[1], x[2], head[x, 1], head[x, 2]}
    ,
    TestID->"[12] Label.nb"
]

VerificationTest[
    (labelRange[x | y, {1}, #1] & ) /@ {Symbol, Function, head}
    ,
    {x1, y1, x[1], y[1], head[x, 1], head[y, 1]}
    ,
    TestID->"[13] Label.nb"
]

VerificationTest[
    (labelRange[x | y, {1, 2}, #1] & ) /@ {Symbol, Function, head}
    ,
    {x1, y1, x2, y2, x[1], y[1], x[2], y[2], head[x, 1], head[y, 1], head[x, 2], head[y, 2]}
    ,
    TestID->"[14] Label.nb"
]

VerificationTest[
    (label[x, -1, #1] & ) /@ {Symbol, Function, head}
    ,
    {Failure["InvalidSymbol", Association["MessageTemplate" :> Yurie`Math`label::InvalidSymbol, "MessageParameters" -> {Global`x, -1}, "Var" -> Global`x, "Label" -> -1]], x[-1], head[x, -1]}
    ,
    TestID->"[15] Label.nb"
]

VerificationTest[
    (label[x[], 1, #1] & ) /@ {Symbol, Function, head}
    ,
    {Failure["InvalidSymbol", Association["MessageTemplate" :> Yurie`Math`label::InvalidSymbol, "MessageParameters" -> {Global`x[], 1}, "Var" -> Global`x[], "Label" -> 1]], x[][1], head[x[], 1]}
    ,
    TestID->"[16] Label.nb"
]

VerificationTest[
    (labelAt[x, 1 -> 1, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{x1 -> 1}], ReplaceAll[{x[1] -> 1}], ReplaceAll[{head[x, 1] -> 1}]}
    ,
    TestID->"[17] Label.nb"
]

VerificationTest[
    (labelAt[x, {1, 2} -> 1, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{x1 -> 1, x2 -> 1}], ReplaceAll[{x[1] -> 1, x[2] -> 1}], ReplaceAll[{head[x, 1] -> 1, head[x, 2] -> 1}]}
    ,
    TestID->"[18] Label.nb"
]

VerificationTest[
    (labelAt[x, {1, 2} -> {1, 2}, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{x1 -> 1, x2 -> 2}], ReplaceAll[{x[1] -> 1, x[2] -> 2}], ReplaceAll[{head[x, 1] -> 1, head[x, 2] -> 2}]}
    ,
    TestID->"[19] Label.nb"
]

VerificationTest[
    (labelAt[x, 1 | 2 -> 1, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{x1 -> 1, x2 -> 1}], ReplaceAll[{x[1] -> 1, x[2] -> 1}], ReplaceAll[{head[x, 1] -> 1, head[x, 2] -> 1}]}
    ,
    TestID->"[20] Label.nb"
]

VerificationTest[
    (labelAt[x, 1 -> 1, {2, 3} -> {2, 3}, {4, 5} -> 4, 6 | 7 -> 5, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{x1 -> 1, x2 -> 2, x3 -> 3, x4 -> 4, x5 -> 4, x6 -> 5, x7 -> 5}], ReplaceAll[{x[1] -> 1, x[2] -> 2, x[3] -> 3, x[4] -> 4, x[5] -> 4, x[6] -> 5, x[7] -> 5}], ReplaceAll[{head[x, 1] -> 1, head[x, 2] -> 2, head[x, 3] -> 3, head[x, 4] -> 4, head[x, 5] -> 4, head[x, 6] -> 5, head[x, 7] -> 5}]}
    ,
    TestID->"[21] Label.nb"
]

VerificationTest[
    (labelAt[x, _ -> 1, #1] & ) /@ {Function, head}
    ,
    {ReplaceAll[{x[_] -> 1}], ReplaceAll[{head[x, _] -> 1}]}
    ,
    TestID->"[22] Label.nb"
]

VerificationTest[
    (labelAt[x, 2 -> 2, _ -> 1, #1] & ) /@ {Function, head}
    ,
    {ReplaceAll[{x[2] -> 2, x[_] -> 1}], ReplaceAll[{head[x, 2] -> 2, head[x, _] -> 1}]}
    ,
    TestID->"[23] Label.nb"
]

VerificationTest[
    (labelAt[x | y, {1, 2} -> a, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{x1 -> a, x2 -> a, y1 -> a, y2 -> a}], ReplaceAll[{x[1] -> a, x[2] -> a, y[1] -> a, y[2] -> a}], ReplaceAll[{head[x, 1] -> a, head[x, 2] -> a, head[y, 1] -> a, head[y, 2] -> a}]}
    ,
    TestID->"[24] Label.nb"
]

VerificationTest[
    (labelAt[x | y, 1 -> a, 2 -> b, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{x1 -> a, x2 -> b, y1 -> a, y2 -> b}], ReplaceAll[{x[1] -> a, x[2] -> b, y[1] -> a, y[2] -> b}], ReplaceAll[{head[x, 1] -> a, head[x, 2] -> b, head[y, 1] -> a, head[y, 2] -> b}]}
    ,
    TestID->"[25] Label.nb"
]

VerificationTest[
    labelAt[x, _ -> 1, Symbol]
    ,
    ReplaceAll[{Failure["InvalidSymbol", Association["MessageTemplate" :> Yurie`Math`label::InvalidSymbol, "MessageParameters" -> {Global`x, _}, "Var" -> Global`x, "Label" -> _]] -> 1}]
    ,
    TestID->"[26] Label.nb"
]

VerificationTest[
    labelAt[x, 2 -> 2, _ -> 1, Symbol]
    ,
    ReplaceAll[{x2 -> 2, Failure["InvalidSymbol", Association["MessageTemplate" :> Yurie`Math`label::InvalidSymbol, "MessageParameters" -> {Global`x, _}, "Var" -> Global`x, "Label" -> _]] -> 1}]
    ,
    TestID->"[27] Label.nb"
]

VerificationTest[
    labelConvert[x, Symbol -> Function][x1]
    ,
    x[1]
    ,
    TestID->"[28] Label.nb"
]

VerificationTest[
    labelConvert[x | y, Symbol -> Function][x1 + y2]
    ,
    x[1] + y[2]
    ,
    TestID->"[29] Label.nb"
]

VerificationTest[
    MatrixForm[Outer[{#1, #2} -> labelConvert[x | y, #1 -> #2][{x1, x[1], head[x, 1]}] & , {Symbol, Function, head}, {Symbol, Function, head}]]
    ,
    MatrixForm[{{{Symbol, Symbol} -> {x1, x[1], head[x, 1]}, {Symbol, Function} -> {x[1], x[1], head[x, 1]}, {Symbol, head} -> {head[x, 1], x[1], head[x, 1]}}, {{Function, Symbol} -> {x1, x1, head[x, 1]}, {Function, Function} -> {x1, x[1], head[x, 1]}, {Function, head} -> {x1, head[x, 1], head[x, 1]}}, {{head, Symbol} -> {x1, x[1], x1}, {head, Function} -> {x1, x[1], x[1]}, {head, head} -> {x1, x[1], head[x, 1]}}}]
    ,
    TestID->"[30] Label.nb"
]

VerificationTest[
    (labelJoin[x | y, #1][{x1, x[1], head[x, 1]}] & ) /@ {Symbol, Function, head}
    ,
    {{x1, x[1], head[x, 1]}, {x1, x1, head[x, 1]}, {x1, x[1], x1}}
    ,
    TestID->"[31] Label.nb"
]

VerificationTest[
    (labelSplit[x | y, #1][{x1, x[1], head[x, 1]}] & ) /@ {Symbol, Function, head}
    ,
    {{x1, x[1], head[x, 1]}, {x[1], x[1], head[x, 1]}, {head[x, 1], x[1], head[x, 1]}}
    ,
    TestID->"[32] Label.nb"
]

VerificationTest[
    labelConvert[x, Symbol -> Function, "LabelType" -> "UndefinedType"][x1]
    ,
    Quiet[x1]
    ,
    {Yurie`Math`label::UndefinedType}
    ,
    TestID->"[33] Label.nb"
]

VerificationTest[
    labelConvert[x, Symbol -> Function, "LabelType" -> "PositiveInteger"][{x1, x0}]
    ,
    {x[1], x0}
    ,
    TestID->"[34] Label.nb"
]

VerificationTest[
    labelConvert[z | zb, Symbol -> Function, "LabelType" -> "PositiveInteger"][zb1]
    ,
    zb[1]
    ,
    TestID->"[35] Label.nb"
]

VerificationTest[
    labelConvert[z | zb, Symbol -> Function][zb1]
    ,
    z[b1]
    ,
    TestID->"[36] Label.nb"
]

VerificationTest[
    labelJoin[x][Hold[x[1] + x[2]]]
    ,
    Hold[x1 + x2]
    ,
    TestID->"[37] Label.nb"
]

VerificationTest[
    labelJoin[x][HoldComplete[x[1]]]
    ,
    HoldComplete[x1]
    ,
    TestID->"[38] Label.nb"
]

VerificationTest[
    labelSplit[x][Hold[x1 + x2]]
    ,
    Hold[x[1] + x[2]]
    ,
    TestID->"[39] Label.nb"
]

VerificationTest[
    labelSplit[x][HoldComplete[x1]]
    ,
    HoldComplete[x[1]]
    ,
    TestID->"[40] Label.nb"
]

VerificationTest[
    (labelToZero[z | zb, {1, 2}, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{z1 -> 0, z2 -> 0, zb1 -> 0, zb2 -> 0}], ReplaceAll[{z[1] -> 0, z[2] -> 0, zb[1] -> 0, zb[2] -> 0}], ReplaceAll[{head[z, 1] -> 0, head[z, 2] -> 0, head[zb, 1] -> 0, head[zb, 2] -> 0}]}
    ,
    TestID->"[41] Label.nb"
]

VerificationTest[
    (labelToEqual[z | zb, 1 -> 2, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{z1 -> z2, zb1 -> zb2}], ReplaceAll[{z[1] -> z[2], zb[1] -> zb[2]}], ReplaceAll[{head[z, 1] -> head[z, 2], head[zb, 1] -> head[zb, 2]}]}
    ,
    TestID->"[42] Label.nb"
]

VerificationTest[
    (labelToDiff[z | zb, 1 -> 2, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{z1 -> z12 + z2, zb1 -> zb12 + zb2}], ReplaceAll[{z[1] -> z[2] + z["12"], zb[1] -> zb[2] + zb["12"]}], ReplaceAll[{head[z, 1] -> head[z, 2] + head[z, "12"], head[zb, 1] -> head[zb, 2] + head[zb, "12"]}]}
    ,
    TestID->"[43] Label.nb"
]

VerificationTest[
    (labelToDiffZero[z | zb, 1 -> 2, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{z1 -> z12, z2 -> 0, zb1 -> zb12, zb2 -> 0}], ReplaceAll[{z[1] -> z["12"], z[2] -> 0, zb[1] -> zb["12"], zb[2] -> 0}], ReplaceAll[{head[z, 1] -> head[z, "12"], head[z, 2] -> 0, head[zb, 1] -> head[zb, "12"], head[zb, 2] -> 0}]}
    ,
    TestID->"[44] Label.nb"
]

VerificationTest[
    (labelToDiffBack[z | zb, 1 -> 2, #1] & ) /@ {Symbol, Function, head}
    ,
    {ReplaceAll[{z12 -> z1 - z2, zb12 -> zb1 - zb2}], ReplaceAll[{z["12"] -> z[1] - z[2], zb["12"] -> zb[1] - zb[2]}], ReplaceAll[{head[z, "12"] -> head[z, 1] - head[z, 2], head[zb, "12"] -> head[zb, 1] - head[zb, 2]}]}
    ,
    TestID->"[45] Label.nb"
]

VerificationTest[
    labelToZero[z[], {1}, Symbol]
    ,
    ReplaceAll[Failure["InvalidSymbol", Association["MessageTemplate" :> Yurie`Math`label::InvalidSymbol, "MessageParameters" -> {Global`z[], 1}, "Var" -> Global`z[], "Label" -> 1]] -> 0]
    ,
    TestID->"[46] Label.nb"
]

VerificationTest[
    labelToZero[z, {-1}, Symbol]
    ,
    ReplaceAll[Failure["InvalidSymbol", Association["MessageTemplate" :> Yurie`Math`label::InvalidSymbol, "MessageParameters" -> {Global`z, -1}, "Var" -> Global`z, "Label" -> -1]] -> 0]
    ,
    TestID->"[47] Label.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Label.nb"
]