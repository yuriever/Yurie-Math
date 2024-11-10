

(*Simplify.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-Simplify.nb"
]

VerificationTest[
	Get["Yurie`Math`"]
	,
	Null
	,
	TestID->"1-Simplify.nb"
]

VerificationTest[
	expr = ((w^a)^b*((w*(x - y))/z)^(a + b)*z^(2*a))/((-x + y)/z)^b; 
	powerCollect[a][powerCollect[b][expr]]
	,
	(-w^(1 + a))^b*(w*(x - y)*z)^a
	,
	TestID->"2-Simplify.nb"
]

VerificationTest[
	powerCollect[a][powerCollect[b][expr]] == powerCollect[b, a][expr]
	,
	True
	,
	TestID->"3-Simplify.nb"
]

VerificationTest[
	powerCollect[a, b][expr]
	,
	(-w)^b*(w^(1 + b)*(x - y)*z)^a
	,
	TestID->"4-Simplify.nb"
]

VerificationTest[
	powerApart[expr]
	,
	(w^(a + b + a*b)*(x - y)^(a + b)*z^a)/(-x + y)^b
	,
	TestID->"5-Simplify.nb"
]

VerificationTest[
	powerTogether[expr]
	,
	(-1)^b*w^(a + b + a*b)*((x - y)*z)^a
	,
	TestID->"6-Simplify.nb"
]

VerificationTest[
	powerSim[expr]
	,
	(-1)^b*w^(a + b + a*b)*((x - y)*z)^a
	,
	TestID->"7-Simplify.nb"
]

VerificationTest[
	expr = {(-1)^a*x^a, (-x)^a, 1/x^2, I^a*x^a, (I*x)^(4*a), 1/x^2}; 
	powerApart[expr]
	,
	{(-1)^a*x^a, (-1)^a*x^a, 1/x^2, E^((I*a*Pi)/2)*x^a, E^(2*I*a*Pi)*x^(4*a), 1/x^2}
	,
	TestID->"8-Simplify.nb"
]

VerificationTest[
	powerTogether[expr]
	,
	{(-1)^a*x^a, (-1)^a*x^a, 1/x^2, E^((I*a*Pi)/2)*x^a, E^(2*I*a*Pi)*x^(4*a), 1/x^2}
	,
	TestID->"9-Simplify.nb"
]

VerificationTest[
	powerSim[expr]
	,
	{(-1)^a*x^a, (-1)^a*x^a, 1/x^2, E^((I*a*Pi)/2)*x^a, E^(2*I*a*Pi)*x^(4*a), 1/x^2}
	,
	TestID->"10-Simplify.nb"
]

VerificationTest[
	m = n; 
	Table[m, {n, 2}]
	,
	{1, 2}
	,
	TestID->"11-Simplify.nb"
]

VerificationTest[
	modularize[Table[m, {n, 2}]]
	,
	{n, n}
	,
	TestID->"12-Simplify.nb"
]

VerificationTest[
	{block[{n = 1}][m], m, n}
	,
	{1, n, n}
	,
	TestID->"13-Simplify.nb"
]

VerificationTest[
	{module[{m = 1}][m], m, n}
	,
	{1, n, n}
	,
	TestID->"14-Simplify.nb"
]

VerificationTest[
	ClearAll[m]; 
	,
	Null
	,
	TestID->"15-Simplify.nb"
]

VerificationTest[
	separateBy[EvenQ][Range[10]]
	,
	{{2, 4, 6, 8, 10}, {1, 3, 5, 7, 9}}
	,
	TestID->"16-Simplify.nb"
]

VerificationTest[
	separateBy[FreeQ[s]][((-z)^s*Gamma[c]*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/(Gamma[a]*Gamma[b]*Gamma[c + s])]
	,
	{Gamma[c]/(Gamma[a]*Gamma[b]), ((-z)^s*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/Gamma[c + s]}
	,
	TestID->"17-Simplify.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-Simplify.nb"
]