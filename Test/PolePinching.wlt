

(*PolePinching.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-PolePinching.nb"
]

VerificationTest[
	Get["Yurie`Math`"]
	,
	Null
	,
	TestID->"1-PolePinching.nb"
]

VerificationTest[
	Yurie`Math`PolePinching`Private`poleStructureLinearQ[{x, y}][Yurie`Math`PolePinching`Private`getPoleData[{}][multiGamma[{x + y, y}, {z}]]]
	,
	True
	,
	TestID->"2-PolePinching.nb"
]

VerificationTest[
	Yurie`Math`PolePinching`Private`poleStructureLinearQ[{x, y}][Yurie`Math`PolePinching`Private`getPoleData[{}][multiGamma[{x^2, y}, {z}]]]
	,
	False
	,
	TestID->"3-PolePinching.nb"
]

VerificationTest[
	Yurie`Math`PolePinching`Private`poleStructureLinearQ[{y, z}][Yurie`Math`PolePinching`Private`getPoleData[{}][multiGamma[{x^2, y}, {z}]]]
	,
	True
	,
	TestID->"4-PolePinching.nb"
]

VerificationTest[
	Yurie`Math`PolePinching`Private`poleStructureLinearQ[{x, y, z}][Yurie`Math`PolePinching`Private`getPoleData[{}][multiGamma[{x^2, y}, {z}]]]
	,
	False
	,
	TestID->"5-PolePinching.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-PolePinching.nb"
]