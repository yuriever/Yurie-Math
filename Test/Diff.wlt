

(*Diff.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-Diff.nb"
]

VerificationTest[
	Get["Yurie`Math`"]
	,
	Null
	,
	TestID->"1-Diff.nb"
]

VerificationTest[
	PD[z[1]]*PD[z[1]]
	,
	PD[z[1], z[1]]
	,
	TestID->"2-Diff.nb"
]

VerificationTest[
	PD[z[1]]*PD[z[2]]
	,
	PD[z[1], z[2]]
	,
	TestID->"3-Diff.nb"
]

VerificationTest[
	PD[z[1], z[2]]*PD[z[3]]^2
	,
	PD[z[1], z[2], z[3], z[3]]
	,
	TestID->"4-Diff.nb"
]

VerificationTest[
	expr = PD[x, y]*f[x] + PD[x]*g[x] + h[y]
	,
	h[y] + g[x]*PD[x] + f[x]*PD[x, y]
	,
	TestID->"5-Diff.nb"
]

VerificationTest[
	expr2 = PD[x, y]*f[x] + g[PD[x]*g[x]]
	,
	g[g[x]*PD[x]] + f[x]*PD[x, y]
	,
	TestID->"6-Diff.nb"
]

VerificationTest[
	PDCoefficient[expr]
	,
	{{x} -> g[x], {x, y} -> f[x], {} -> h[y]}
	,
	TestID->"7-Diff.nb"
]

VerificationTest[
	PDCoefficient[expr2]
	,
	Quiet[g[g[x]*PD[x]] + f[x]*PD[x, y]]
	,
	{Yurie`Math`PDCoefficient::nonlinear}
	,
	TestID->"8-Diff.nb"
]

VerificationTest[
	expr = D[f[x, y, z, w], {x, 1}, {y, 1}]*D[g[x, y, z, w], {z, 1}, {w, 1}]; 
	IBP[f][expr]
	,
	f[x, y, z, w]*Derivative[1, 1, 1, 1][g][x, y, z, w]
	,
	TestID->"9-Diff.nb"
]

VerificationTest[
	IBP[f, x][expr]
	,
	(-Derivative[0, 1, 0, 0][f][x, y, z, w])*Derivative[1, 0, 1, 1][g][x, y, z, w]
	,
	TestID->"10-Diff.nb"
]

VerificationTest[
	IBP[f, x, y][expr]
	,
	f[x, y, z, w]*Derivative[1, 1, 1, 1][g][x, y, z, w]
	,
	TestID->"11-Diff.nb"
]

VerificationTest[
	expr = f[x]*D[f[x], {x, 1}]; 
	IBP[f][expr]
	,
	(-f[x])*Derivative[1][f][x]
	,
	TestID->"12-Diff.nb"
]

VerificationTest[
	integrateChange[t^a, {t == 1 - x}, {t}, {x}]
	,
	-(1 - x)^a
	,
	TestID->"13-Diff.nb"
]

VerificationTest[
	integrateChange[t, {x == t^2}, {t}, {x}]
	,
	1/2
	,
	TestID->"14-Diff.nb"
]

VerificationTest[
	integrateChange[t, {x == t^2}, {t}, {x}, "FirstSolution" -> False]
	,
	{1/2, 1/2}
	,
	TestID->"15-Diff.nb"
]

VerificationTest[
	integrateChange[t^a, {t -> 1 - x}, {t}, {x}]
	,
	-(1 - x)^a
	,
	TestID->"16-Diff.nb"
]

VerificationTest[
	integrateChange[{t -> 1 - x}, {t}, {x}][t^a]
	,
	-(1 - x)^a
	,
	TestID->"17-Diff.nb"
]

VerificationTest[
	integrateChange[{x == 2*t}, {x}, {t}][INT[x]*x^a]
	,
	2^(1 + a)*t^a*INT[t]
	,
	TestID->"18-Diff.nb"
]

VerificationTest[
	Simplify[diffChange[D[f[x, t], {t, 2}] == c^2*D[f[x, t], {x, 2}], {u == x + c*t, v == x - c*t}, {x, t}, {u, v}, {f[x, t]}]]
	,
	c*Derivative[1, 1][f][u, v] == 0
	,
	TestID->"19-Diff.nb"
]

VerificationTest[
	diffChange[g[t] + Derivative[1][f][t], {x == t^2}, {t}, {x}, {f[t], g[t]}]
	,
	g[x] - 2*Sqrt[x]*Derivative[1][f][x]
	,
	TestID->"20-Diff.nb"
]

VerificationTest[
	diffChange[g[t] + Derivative[1][f][t], {x == t^2}, {t}, {x}, {f[t], g[t]}, "FirstSolution" -> False]
	,
	{g[x] - 2*Sqrt[x]*Derivative[1][f][x], g[x] + 2*Sqrt[x]*Derivative[1][f][x]}
	,
	TestID->"21-Diff.nb"
]

VerificationTest[
	diffChange[g[t] + Derivative[1][f][t], {x -> t^2}, {t}, {x}, {f[t], g[t]}]
	,
	g[x] - 2*Sqrt[x]*Derivative[1][f][x]
	,
	TestID->"22-Diff.nb"
]

VerificationTest[
	diffChange[{x -> t^2}, {t}, {x}, {f[t], g[t]}][g[t] + Derivative[1][f][t]]
	,
	g[x] - 2*Sqrt[x]*Derivative[1][f][x]
	,
	TestID->"23-Diff.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-Diff.nb"
]