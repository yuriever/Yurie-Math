

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
    (label[x, 1, "LabelPosition" -> #1] & ) /@ {Symbol, Construct, Subscript, Superscript}
    ,
    {x1, x[1], Subscript[x, 1], Superscript[x, 1]}
    ,
    TestID->"2-Label.nb"
]

VerificationTest[
    (label[x, {1, 2}, "LabelPosition" -> #1] & ) /@ {Symbol, Construct, Subscript, Superscript}
    ,
    {x1, x2, x[1], x[2], Subscript[x, 1], Subscript[x, 2], Superscript[x, 1], Superscript[x, 2]}
    ,
    TestID->"3-Label.nb"
]

VerificationTest[
    (label[{x, y}, 1, "LabelPosition" -> #1] & ) /@ {Symbol, Construct, Subscript, Superscript}
    ,
    {x1, y1, x[1], y[1], Subscript[x, 1], Subscript[y, 1], Superscript[x, 1], Superscript[y, 1]}
    ,
    TestID->"4-Label.nb"
]

VerificationTest[
    (label[{x, y}, {1, 2}, "LabelPosition" -> #1] & ) /@ {Symbol, Construct, Subscript, Superscript}
    ,
    {x1, y1, x2, y2, x[1], y[1], x[2], y[2], Subscript[x, 1], Subscript[y, 1], Subscript[x, 2], Subscript[y, 2], Superscript[x, 1], Superscript[y, 1], Superscript[x, 2], Superscript[y, 2]}
    ,
    TestID->"5-Label.nb"
]

VerificationTest[
    (label[x, Null, "LabelPosition" -> #1] & ) /@ {Symbol, Construct, Subscript, Superscript}
    ,
    {x, x, x, x}
    ,
    TestID->"6-Label.nb"
]

VerificationTest[
    label[{x, y}, 1, "LabelPosition" -> f]
    ,
    Quiet[{x, y}]
    ,
    {Yurie`Math`label::posnotmatch}
    ,
    TestID->"7-Label.nb"
]

VerificationTest[
    (label[x, -1, "LabelPosition" -> #1] & ) /@ {Symbol, Construct, Subscript, Superscript}
    ,
    Quiet[{x, x[-1], Subscript[x, -1], Superscript[x, -1]}]
    ,
    {Yurie`Math`label::badlab}
    ,
    TestID->"8-Label.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Label.nb"
]