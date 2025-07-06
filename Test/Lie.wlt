

(*Lie.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Lie.nb"
]

VerificationTest[
    Get["Yurie`Math`Lie`"]
    ,
    Null
    ,
    TestID->"1-Lie.nb"
]

VerificationTest[
    testData[max_] := testData[max] = Module[{n}, Join[Table[lie["A", n], {n, Range[1, max]}], Table[lie["B", n], {n, Range[2, max]}], Table[lie["C", n], {n, Range[3, max]}], Table[lie["D", n], {n, Range[4, max]}], Table[lie["E", n], {n, Range[6, 8]}], {lie["F", 4], lie["G", 2]}]]; 
    ,
    Null
    ,
    TestID->"2-Lie.nb"
]

VerificationTest[
    Yurie`Math`Lie`Private`lieRank /@ testData[4] == lieCartan /* Dimensions /* First /@ testData[4]
    ,
    True
    ,
    TestID->"3-Lie.nb"
]

VerificationTest[
    (Map[lieCartan])[testData[4]]
    ,
    {{{2}}, {{2, -1}, {-1, 2}}, {{2, -1, 0}, {-1, 2, -1}, {0, -1, 2}}, {{2, -1, 0, 0}, {-1, 2, -1, 0}, {0, -1, 2, -1}, {0, 0, -1, 2}}, {{2, -2}, {-1, 2}}, {{2, -1, 0}, {-1, 2, -2}, {0, -1, 2}}, {{2, -1, 0, 0}, {-1, 2, -1, 0}, {0, -1, 2, -2}, {0, 0, -1, 2}}, {{2, -1, 0}, {-1, 2, -1}, {0, -2, 2}}, {{2, -1, 0, 0}, {-1, 2, -1, 0}, {0, -1, 2, -1}, {0, 0, -2, 2}}, {{2, -1, 0, 0}, {-1, 2, -1, -1}, {0, -1, 2, 0}, {0, -1, 0, 2}}, {{2, -1, 0, 0, 0, 0}, {-1, 2, -1, 0, 0, 0}, {0, -1, 2, -1, 0, -1}, {0, 0, -1, 2, -1, 0}, {0, 0, 0, -1, 2, 0}, {0, 0, -1, 0, 0, 2}}, {{2, -1, 0, 0, 0, 0, 0}, {-1, 2, -1, 0, 0, 0, 0}, {0, -1, 2, -1, 0, 0, -1}, {0, 0, -1, 2, -1, 0, 0}, {0, 0, 0, -1, 2, -1, 0}, {0, 0, 0, 0, -1, 2, 0}, {0, 0, -1, 0, 0, 0, 2}}, {{2, -1, 0, 0, 0, 0, 0, 0}, {-1, 2, -1, 0, 0, 0, 0, 0}, {0, -1, 2, -1, 0, 0, 0, -1}, {0, 0, -1, 2, -1, 0, 0, 0}, {0, 0, 0, -1, 2, -1, 0, 0}, {0, 0, 0, 0, -1, 2, -1, 0}, {0, 0, 0, 0, 0, -1, 2, 0}, {0, 0, -1, 0, 0, 0, 0, 2}}, {{2, -1, 0, 0}, {-1, 2, -2, 0}, {0, -1, 2, -1}, {0, 0, -1, 2}}, {{2, -1}, {-3, 2}}}
    ,
    TestID->"4-Lie.nb"
]

VerificationTest[
    (Map[lieDynkinDiagram /* EdgeList])[testData[4]]
    ,
    {{}, {UndirectedEdge[1, 2]}, {UndirectedEdge[1, 2], UndirectedEdge[2, 3]}, {UndirectedEdge[1, 2], UndirectedEdge[2, 3], UndirectedEdge[3, 4]}, {DirectedEdge[1, 2], DirectedEdge[1, 2], DirectedEdge[2, 1]}, {DirectedEdge[1, 2], DirectedEdge[2, 1], DirectedEdge[2, 3], DirectedEdge[2, 3], DirectedEdge[3, 2]}, {DirectedEdge[1, 2], DirectedEdge[2, 1], DirectedEdge[2, 3], DirectedEdge[3, 2], DirectedEdge[3, 4], DirectedEdge[3, 4], DirectedEdge[4, 3]}, {DirectedEdge[1, 2], DirectedEdge[2, 1], DirectedEdge[2, 3], DirectedEdge[3, 2], DirectedEdge[3, 2]}, {DirectedEdge[1, 2], DirectedEdge[2, 1], DirectedEdge[2, 3], DirectedEdge[3, 2], DirectedEdge[3, 4], DirectedEdge[4, 3], DirectedEdge[4, 3]}, {UndirectedEdge[1, 2], UndirectedEdge[2, 3], UndirectedEdge[2, 4]}, {UndirectedEdge[1, 2], UndirectedEdge[2, 3], UndirectedEdge[3, 4], UndirectedEdge[3, 6], UndirectedEdge[4, 5]}, {UndirectedEdge[1, 2], UndirectedEdge[2, 3], UndirectedEdge[3, 4], UndirectedEdge[3, 7], UndirectedEdge[4, 5], UndirectedEdge[5, 6]}, {UndirectedEdge[1, 2], UndirectedEdge[2, 3], UndirectedEdge[3, 4], UndirectedEdge[3, 8], UndirectedEdge[4, 5], UndirectedEdge[5, 6], UndirectedEdge[6, 7]}, {DirectedEdge[1, 2], DirectedEdge[2, 1], DirectedEdge[2, 3], DirectedEdge[2, 3], DirectedEdge[3, 2], DirectedEdge[3, 4], DirectedEdge[4, 3]}, {DirectedEdge[1, 2], DirectedEdge[2, 1], DirectedEdge[2, 1], DirectedEdge[2, 1]}}
    ,
    TestID->"5-Lie.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Lie.nb"
]