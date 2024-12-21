

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
	hyperUnregularize[HypergeometricPFQRegularized[{a1, b1, c1}, {d1, e1}, z]]
	,
	HypergeometricPFQ[{a1, b1, c1}, {d1, e1}, z]/(Gamma[d1]*Gamma[e1])
	,
	TestID->"2-Hyper.nb"
]

VerificationTest[
	hyperUnregularize[Hypergeometric2F1Regularized[a1, b1, c1, z]]
	,
	Hypergeometric2F1[a1, b1, c1, z]/Gamma[c1]
	,
	TestID->"3-Hyper.nb"
]

VerificationTest[
	hyperUnregularize[Hypergeometric1F1Regularized[a1, b1, z]]
	,
	Hypergeometric1F1[a1, b1, z]/Gamma[b1]
	,
	TestID->"4-Hyper.nb"
]

VerificationTest[
	hyperUnregularize[Hypergeometric0F1Regularized[a1, z]]
	,
	Hypergeometric0F1[a1, z]/Gamma[a1]
	,
	TestID->"5-Hyper.nb"
]

VerificationTest[
	expr = f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2; 
	,
	Null
	,
	TestID->"6-Hyper.nb"
]

VerificationTest[
	hyperToMellinBarnes[s, t, u][expr]
	,
	f[hyperMellinBarnes[((-z1)^s*Gamma[c1]*Gamma[-s]*Gamma[a1 + s]*Gamma[b1 + s])/(Gamma[a1]*Gamma[b1]*Gamma[c1 + s])]] + hyperMellinBarnes[((-z2)^t*Gamma[c2]*Gamma[-t]*Gamma[a2 + t]*Gamma[b2 + t])/(Gamma[a2]*Gamma[b2]*Gamma[c2 + t])]*hyperMellinBarnes[((-z2)^u*Gamma[c2]*Gamma[-u]*Gamma[a2 + u]*Gamma[b2 + u])/(Gamma[a2]*Gamma[b2]*Gamma[c2 + u])]
	,
	TestID->"7-Hyper.nb"
]

VerificationTest[
	hyperToMellinBarnes[s, t][expr]
	,
	Quiet[f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2]
	,
	{Yurie`Math`hyperTo::symbolNotEnough}
	,
	TestID->"8-Hyper.nb"
]

VerificationTest[
	hyperToTaylor[n, m, k][expr]
	,
	f[hyperTaylor[(z1^n*Gamma[c1]*Gamma[a1 + n]*Gamma[b1 + n])/(Gamma[a1]*Gamma[b1]*Gamma[1 + n]*Gamma[c1 + n])]] + hyperTaylor[(z2^k*Gamma[c2]*Gamma[a2 + k]*Gamma[b2 + k])/(Gamma[a2]*Gamma[b2]*Gamma[1 + k]*Gamma[c2 + k])]*hyperTaylor[(z2^m*Gamma[c2]*Gamma[a2 + m]*Gamma[b2 + m])/(Gamma[a2]*Gamma[b2]*Gamma[1 + m]*Gamma[c2 + m])]
	,
	TestID->"9-Hyper.nb"
]

VerificationTest[
	hyperToTaylor[n, m][expr]
	,
	Quiet[f[Hypergeometric2F1[a1, b1, c1, z1]] + Hypergeometric2F1[a2, b2, c2, z2]^2]
	,
	{Yurie`Math`hyperTo::symbolNotEnough}
	,
	TestID->"10-Hyper.nb"
]

VerificationTest[
	(ReplaceAll[hyperTaylor -> Identity])[hyperToTaylor[n][Hypergeometric2F1[a, b, c, z]]]
	,
	(z^n*Gamma[c]*Gamma[a + n]*Gamma[b + n])/(Gamma[a]*Gamma[b]*Gamma[1 + n]*Gamma[c + n])
	,
	TestID->"11-Hyper.nb"
]

VerificationTest[
	powerSim[gammaTakeResidue[s, n, -s][(ReplaceAll[hyperMellinBarnes -> Identity])[hyperToMellinBarnes[s][Hypergeometric2F1[a, b, c, z]]]]]
	,
	-((z^n*Gamma[c]*Gamma[a + n]*Gamma[b + n])/(n!*Gamma[a]*Gamma[b]*Gamma[c + n]))
	,
	TestID->"12-Hyper.nb"
]

VerificationTest[
	(ReplaceAll[hyperMellinBarnes -> Identity])[hyperToMellinBarnes2[s][Hypergeometric2F1[a, b, c, z]]]
	,
	((1 - z)^s*Gamma[c]*Gamma[-a - b + c - s]*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/(Gamma[a]*Gamma[b]*Gamma[-a + c]*Gamma[-b + c])
	,
	TestID->"13-Hyper.nb"
]

VerificationTest[
	hyperToTaylor[n, m, k, l][{Hypergeometric2F1[a, b, c, x], Hypergeometric1F1[a, b, x], Hypergeometric0F1[a, x], HypergeometricPFQ[{a, b}, {c}, x]}]
	,
	{hyperTaylor[(x^n*Gamma[c]*Gamma[a + n]*Gamma[b + n])/(Gamma[a]*Gamma[b]*Gamma[1 + n]*Gamma[c + n])], hyperTaylor[(x^m*Gamma[b]*Gamma[a + m])/(Gamma[a]*Gamma[1 + m]*Gamma[b + m])], hyperTaylor[(x^k*Gamma[a])/(Gamma[1 + k]*Gamma[a + k])], hyperTaylor[(x^l*Gamma[c]*Gamma[a + l]*Gamma[b + l])/(Gamma[a]*Gamma[b]*Gamma[1 + l]*Gamma[c + l])]}
	,
	TestID->"14-Hyper.nb"
]

VerificationTest[
	hyperToMellinBarnes[s, t, u, v][{Hypergeometric2F1[a, b, c, x], Hypergeometric1F1[a, b, x], Hypergeometric0F1[a, x], HypergeometricPFQ[{a, b}, {c}, x]}]
	,
	{hyperMellinBarnes[((-x)^s*Gamma[c]*Gamma[-s]*Gamma[a + s]*Gamma[b + s])/(Gamma[a]*Gamma[b]*Gamma[c + s])], hyperMellinBarnes[((-x)^t*Gamma[b]*Gamma[-t]*Gamma[a + t])/(Gamma[a]*Gamma[b + t])], hyperMellinBarnes[((-x)^u*Gamma[a]*Gamma[-u])/Gamma[a + u]], hyperMellinBarnes[((-x)^v*Gamma[c]*Gamma[-v]*Gamma[a + v]*Gamma[b + v])/(Gamma[a]*Gamma[b]*Gamma[c + v])]}
	,
	TestID->"15-Hyper.nb"
]

VerificationTest[
	wilsonPolynomialToHyper[wilsonPolynomialFromHyper[HypergeometricPFQ[{-n, -1 + a + b + c + d + n, a - I*x, a + I*x}, {a + b, a + c, a + d}, 1]]]
	,
	HypergeometricPFQ[{-n, -1 + a + b + c + d + n, a - I*Sqrt[x^2], a + I*Sqrt[x^2]}, {a + b, a + c, a + d}, 1]
	,
	TestID->"16-Hyper.nb"
]

VerificationTest[
	wilsonPolynomialFromHyper[wilsonPolynomialToHyper[wilsonPolynomial[a, b, c, d, n, x]]]
	,
	wilsonPolynomial[a, b, c, d, n, x]
	,
	TestID->"17-Hyper.nb"
]

VerificationTest[
	SSA[z > 0][jacobiPhiFromHyper[jacobiPhiToHyper[jacobiPhi[a, b, c, z]]]]
	,
	jacobiPhi[a, b, c, z]
	,
	TestID->"18-Hyper.nb"
]

VerificationTest[
	jacobiPhiToHyper[jacobiPhiFromHyper[Hypergeometric2F1[a, b, c, z]]]
	,
	Hypergeometric2F1[a, b, c, z]
	,
	TestID->"19-Hyper.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-Hyper.nb"
]