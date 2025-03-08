

(*Label.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Label.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Label.nb"
]

VerificationTest[
    (label[x, 1, #1] & ) /@ {Symbol, Construct, Function, Subscript, Superscript}
    ,
    {x1, x[1], x[1], Subscript[x, 1], Superscript[x, 1]}
    ,
    TestID->"2-Label.nb"
]

VerificationTest[
    (label[x, {1, 2}, #1] & ) /@ {Symbol, Construct, Function, Subscript, Superscript}
    ,
    {x1, x2, x[1], x[2], x[1], x[2], Subscript[x, 1], Subscript[x, 2], Superscript[x, 1], Superscript[x, 2]}
    ,
    TestID->"3-Label.nb"
]

VerificationTest[
    (label[{x, y}, 1, #1] & ) /@ {Symbol, Construct, Function, Subscript, Superscript}
    ,
    {x1, y1, x[1], y[1], x[1], y[1], Subscript[x, 1], Subscript[y, 1], Superscript[x, 1], Superscript[y, 1]}
    ,
    TestID->"4-Label.nb"
]

VerificationTest[
    (label[{x, y}, {1, 2}, #1] & ) /@ {Symbol, Construct, Function, Subscript, Superscript}
    ,
    {x1, y1, x2, y2, x[1], y[1], x[2], y[2], x[1], y[1], x[2], y[2], Subscript[x, 1], Subscript[y, 1], Subscript[x, 2], Subscript[y, 2], Superscript[x, 1], Superscript[y, 1], Superscript[x, 2], Superscript[y, 2]}
    ,
    TestID->"5-Label.nb"
]

VerificationTest[
    (label[x, Null, #1] & ) /@ {Symbol, Construct, Function, Subscript, Superscript}
    ,
    {x, x, x, x, x}
    ,
    TestID->"6-Label.nb"
]

VerificationTest[
    label[{x, y}, 1, f]
    ,
    Quiet[HoldComplete[{x, y}]]
    ,
    {Yurie`Math`label::posnotmatch}
    ,
    TestID->"7-Label.nb"
]

VerificationTest[
    (label[x, -1, #1] & ) /@ {Symbol, Construct, Function, Subscript, Superscript}
    ,
    Quiet[{HoldComplete[x], x[-1], x[-1], Subscript[x, -1], Superscript[x, -1]}]
    ,
    {Yurie`Math`label::badlab}
    ,
    TestID->"8-Label.nb"
]

VerificationTest[
    MatrixForm[Outer[{#1, #2} -> labelConvert[{x, y}, #1 -> #2][{x1, x[1], Subscript[x, 1], Superscript[x, 1]}] & , {Symbol, Construct, Function, Subscript, Superscript}, {Symbol, Construct, Function, Subscript, Superscript}]]
    ,
    Quiet[MatrixForm[{{{Symbol, Symbol} -> HoldComplete[{x1, x[1], Subscript[x, 1], Superscript[x, 1]}], {Symbol, Construct} -> {x[1], x[1], Subscript[x, 1], Superscript[x, 1]}, {Symbol, Function} -> {x[1], x[1], Subscript[x, 1], Superscript[x, 1]}, {Symbol, Subscript} -> {Subscript[x, 1], x[1], Subscript[x, 1], Superscript[x, 1]}, {Symbol, Superscript} -> {Superscript[x, 1], x[1], Subscript[x, 1], Superscript[x, 1]}}, {{Construct, Symbol} -> {x1, x1, Subscript[x, 1], Superscript[x, 1]}, {Construct, Construct} -> HoldComplete[{x1, x[1], Subscript[x, 1], Superscript[x, 1]}], {Construct, Function} -> {x1, x[1], Subscript[x, 1], Superscript[x, 1]}, {Construct, Subscript} -> {x1, Subscript[x, 1], Subscript[x, 1], Superscript[x, 1]}, {Construct, Superscript} -> {x1, Superscript[x, 1], Subscript[x, 1], Superscript[x, 1]}}, {{Function, Symbol} -> {x1, x1, Subscript[x, 1], Superscript[x, 1]}, {Function, Construct} -> {x1, x[1], Subscript[x, 1], Superscript[x, 1]}, {Function, Function} -> HoldComplete[{x1, x[1], Subscript[x, 1], Superscript[x, 1]}], {Function, Subscript} -> {x1, Subscript[x, 1], Subscript[x, 1], Superscript[x, 1]}, {Function, Superscript} -> {x1, Superscript[x, 1], Subscript[x, 1], Superscript[x, 1]}}, {{Subscript, Symbol} -> {x1, x[1], x1, Superscript[x, 1]}, {Subscript, Construct} -> {x1, x[1], x[1], Superscript[x, 1]}, {Subscript, Function} -> {x1, x[1], x[1], Superscript[x, 1]}, {Subscript, Subscript} -> HoldComplete[{x1, x[1], Subscript[x, 1], Superscript[x, 1]}], {Subscript, Superscript} -> {x1, x[1], Superscript[x, 1], Superscript[x, 1]}}, {{Superscript, Symbol} -> {x1, x[1], Subscript[x, 1], x1}, {Superscript, Construct} -> {x1, x[1], Subscript[x, 1], x[1]}, {Superscript, Function} -> {x1, x[1], Subscript[x, 1], x[1]}, {Superscript, Subscript} -> {x1, x[1], Subscript[x, 1], Subscript[x, 1]}, {Superscript, Superscript} -> HoldComplete[{x1, x[1], Subscript[x, 1], Superscript[x, 1]}]}}]]
    ,
    {Yurie`Math`labelConvert::posequal,Yurie`Math`labelConvert::posequal,Yurie`Math`labelConvert::posequal,General::stop}
    ,
    TestID->"9-Label.nb"
]

VerificationTest[
    (labelToZero[{z, zb}, {1, 2}, #1] & ) /@ {Symbol, Construct, Function, Subscript, Superscript}
    ,
    {ReplaceAll[{z1 -> 0, z2 -> 0, zb1 -> 0, zb2 -> 0}], ReplaceAll[{z[1] -> 0, z[2] -> 0, zb[1] -> 0, zb[2] -> 0}], ReplaceAll[{z[1] -> 0, z[2] -> 0, zb[1] -> 0, zb[2] -> 0}], ReplaceAll[{Subscript[z, 1] -> 0, Subscript[z, 2] -> 0, Subscript[zb, 1] -> 0, Subscript[zb, 2] -> 0}], ReplaceAll[{Superscript[z, 1] -> 0, Superscript[z, 2] -> 0, Superscript[zb, 1] -> 0, Superscript[zb, 2] -> 0}]}
    ,
    TestID->"10-Label.nb"
]

VerificationTest[
    (labelToEqual[{z, zb}, {1 -> 2}, #1] & ) /@ {Symbol, Construct, Function, Subscript, Superscript}
    ,
    {ReplaceAll[{z1 -> z2, zb1 -> zb2}], ReplaceAll[{z[1] -> z[2], zb[1] -> zb[2]}], ReplaceAll[{z[1] -> z[2], zb[1] -> zb[2]}], ReplaceAll[{Subscript[z, 1] -> Subscript[z, 2], Subscript[zb, 1] -> Subscript[zb, 2]}], ReplaceAll[{Superscript[z, 1] -> Superscript[z, 2], Superscript[zb, 1] -> Superscript[zb, 2]}]}
    ,
    TestID->"11-Label.nb"
]

VerificationTest[
    (labelToDiff[{z, zb}, {1 -> 2}, #1] & ) /@ {Symbol, Construct, Function, Subscript, Superscript}
    ,
    {ReplaceAll[{z1 -> z12 + z2, zb1 -> zb12 + zb2}], ReplaceAll[{z[1] -> z[2] + z["12"], zb[1] -> zb[2] + zb["12"]}], ReplaceAll[{z[1] -> z[2] + z["12"], zb[1] -> zb[2] + zb["12"]}], ReplaceAll[{Subscript[z, 1] -> Subscript[z, 2] + Subscript[z, "12"], Subscript[zb, 1] -> Subscript[zb, 2] + Subscript[zb, "12"]}], ReplaceAll[{Superscript[z, 1] -> Superscript[z, 2] + Superscript[z, "12"], Superscript[zb, 1] -> Superscript[zb, 2] + Superscript[zb, "12"]}]}
    ,
    TestID->"12-Label.nb"
]

VerificationTest[
    (labelToDiffZero[{z, zb}, {1 -> 2}, #1] & ) /@ {Symbol, Construct, Function, Subscript, Superscript}
    ,
    {ReplaceAll[{z1 -> z12, z2 -> 0, zb1 -> zb12, zb2 -> 0}], ReplaceAll[{z[1] -> z["12"], z[2] -> 0, zb[1] -> zb["12"], zb[2] -> 0}], ReplaceAll[{z[1] -> z["12"], z[2] -> 0, zb[1] -> zb["12"], zb[2] -> 0}], ReplaceAll[{Subscript[z, 1] -> Subscript[z, "12"], Subscript[z, 2] -> 0, Subscript[zb, 1] -> Subscript[zb, "12"], Subscript[zb, 2] -> 0}], ReplaceAll[{Superscript[z, 1] -> Superscript[z, "12"], Superscript[z, 2] -> 0, Superscript[zb, 1] -> Superscript[zb, "12"], Superscript[zb, 2] -> 0}]}
    ,
    TestID->"13-Label.nb"
]

VerificationTest[
    (labelToDiffBack[{z, zb}, {1 -> 2}, #1] & ) /@ {Symbol, Construct, Function, Subscript, Superscript}
    ,
    {ReplaceAll[{z12 -> z1 - z2, zb12 -> zb1 - zb2}], ReplaceAll[{z["12"] -> z[1] - z[2], zb["12"] -> zb[1] - zb[2]}], ReplaceAll[{z["12"] -> z[1] - z[2], zb["12"] -> zb[1] - zb[2]}], ReplaceAll[{Subscript[z, "12"] -> Subscript[z, 1] - Subscript[z, 2], Subscript[zb, "12"] -> Subscript[zb, 1] - Subscript[zb, 2]}], ReplaceAll[{Superscript[z, "12"] -> Superscript[z, 1] - Superscript[z, 2], Superscript[zb, "12"] -> Superscript[zb, 1] - Superscript[zb, 2]}]}
    ,
    TestID->"14-Label.nb"
]

VerificationTest[
    labelToZero[{z, zb}, {1, 2}, f]
    ,
    Quiet[HoldComplete[Identity]]
    ,
    {Yurie`Math`label::posnotmatch}
    ,
    TestID->"15-Label.nb"
]

VerificationTest[
    labelToZero[{z}, {-1}, Symbol]
    ,
    Quiet[HoldComplete[Identity]]
    ,
    {Yurie`Math`label::badlab}
    ,
    TestID->"16-Label.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Label.nb"
]