

(*DLMF.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-DLMF.nb"
]

VerificationTest[
	Get["Yurie`Math`"]
	,
	Null
	,
	TestID->"1-DLMF.nb"
]

VerificationTest[
	DLMF["BinomialSwapA"][Binomial[a, n]]
	,
	(-1)^n*Binomial[-1 - a + n, n]
	,
	TestID->"2-DLMF.nb"
]

VerificationTest[
	DLMF["15.4.20", "IgnoreCondition" -> True][Hypergeometric2F1[a, b, c, 1]]
	,
	(Gamma[c]*Gamma[-a - b + c])/(Gamma[-a + c]*Gamma[-b + c])
	,
	TestID->"3-DLMF.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-DLMF.nb"
]