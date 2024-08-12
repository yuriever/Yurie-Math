

(*Quest.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-Quest.nb"
]

VerificationTest[
	Get["Yurie`Math`"]
	,
	Null
	,
	TestID->"1-Quest.nb"
]

VerificationTest[
	Simplify[Sin[n*Pi], Assumptions -> isZ[n]]
	,
	0
	,
	TestID->"2-Quest.nb"
]

VerificationTest[
	Simplify[Sqrt[x^2], Assumptions -> isRP[x]]
	,
	x
	,
	TestID->"3-Quest.nb"
]

VerificationTest[
	linearQ[x, y][a^2*x - y + b^2]
	,
	True
	,
	TestID->"4-Quest.nb"
]

VerificationTest[
	linearQ[x, y][1/x]
	,
	False
	,
	TestID->"5-Quest.nb"
]

VerificationTest[
	linearQ[x, y][f[x]]
	,
	False
	,
	TestID->"6-Quest.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-Quest.nb"
]