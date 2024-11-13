

(*Hyper.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-Hyper.nb"
]

VerificationTest[
	Get["Yurie`Math`"]
	,
	Null
	,
	TestID->"1-Hyper.nb"
]

VerificationTest[
	Null
	,
	Null
	,
	TestID->"2-Hyper.nb"
]

VerificationTest[
	expr = f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2; 
	,
	Null
	,
	TestID->"3-Hyper.nb"
]

VerificationTest[
	hyperToMellinBarnes[s, t, u][expr]
	,
	f[hyperMellinBarnes[((-z1)^s*Gamma[c1]*Gamma[-s]*Gamma[a1 + s]*Gamma[b1 + s])/(Gamma[a1]*Gamma[b1]*Gamma[c1 + s])]] + hyperMellinBarnes[((-z2)^t*Gamma[c2]*Gamma[-t]*Gamma[a2 + t]*Gamma[b2 + t])/(Gamma[a2]*Gamma[b2]*Gamma[c2 + t])]*hyperMellinBarnes[((-z2)^u*Gamma[c2]*Gamma[-u]*Gamma[a2 + u]*Gamma[b2 + u])/(Gamma[a2]*Gamma[b2]*Gamma[c2 + u])]
	,
	TestID->"4-Hyper.nb"
]

VerificationTest[
	hyperToMellinBarnes[s, t][expr]
	,
	Quiet[f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2]
	,
	{Yurie`Math`hyperTo::symbolNotEnough}
	,
	TestID->"5-Hyper.nb"
]

VerificationTest[
	hyperToTaylor[n, m, k][expr]
	,
	f[hyperTaylor[(z1^n*Gamma[c1]*Gamma[a1 + n]*Gamma[b1 + n])/(Gamma[a1]*Gamma[b1]*Gamma[1 + n]*Gamma[c1 + n])]] + hyperTaylor[(z2^k*Gamma[c2]*Gamma[a2 + k]*Gamma[b2 + k])/(Gamma[a2]*Gamma[b2]*Gamma[1 + k]*Gamma[c2 + k])]*hyperTaylor[(z2^m*Gamma[c2]*Gamma[a2 + m]*Gamma[b2 + m])/(Gamma[a2]*Gamma[b2]*Gamma[1 + m]*Gamma[c2 + m])]
	,
	TestID->"6-Hyper.nb"
]

VerificationTest[
	hyperToTaylor[n, m][expr]
	,
	Quiet[f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2]
	,
	{Yurie`Math`hyperTo::symbolNotEnough}
	,
	TestID->"7-Hyper.nb"
]

VerificationTest[
	wilsonPolynomialToHyper[wilsonPolynomialFromHyper[HypergeometricPFQ[{-n, -1 + a + b + c + d + n, a - I*x, a + I*x}, {a + b, a + c, a + d}, 1]]]
	,
	HypergeometricPFQ[{-n, -1 + a + b + c + d + n, a - I*Sqrt[x^2], a + I*Sqrt[x^2]}, {a + b, a + c, a + d}, 1]
	,
	TestID->"8-Hyper.nb"
]

VerificationTest[
	wilsonPolynomialFromHyper[wilsonPolynomialToHyper[wilsonPolynomial[a, b, c, d, n, x]]]
	,
	wilsonPolynomial[a, b, c, d, n, x]
	,
	TestID->"9-Hyper.nb"
]

VerificationTest[
	SSA[z > 0][jacobiPhiFromHyper[jacobiPhiToHyper[jacobiPhi[a, b, c, z]]]]
	,
	jacobiPhi[a, b, c, z]
	,
	TestID->"10-Hyper.nb"
]

VerificationTest[
	jacobiPhiToHyper[jacobiPhiFromHyper[Hypergeometric2F1[a, b, c, z]]]
	,
	Hypergeometric2F1[a, b, c, z]
	,
	TestID->"11-Hyper.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-Hyper.nb"
]