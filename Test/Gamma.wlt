

(*Gamma.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-Gamma.nb"
]

VerificationTest[
	Get["Yurie`Math`"]
	,
	Null
	,
	TestID->"1-Gamma.nb"
]

VerificationTest[
	gammaFrom[n!]
	,
	Gamma[1 + n]
	,
	TestID->"2-Gamma.nb"
]

VerificationTest[
	gammaFrom[Sin[x]*Beta[a, b], "Transformation" -> {"Beta"}]
	,
	(Gamma[a]*Gamma[b]*Sin[x])/Gamma[a + b]
	,
	TestID->"3-Gamma.nb"
]

VerificationTest[
	gammaFrom[Sin[x], "ActivateGamma" -> False]
	,
	Pi/(Inactive[Gamma][x/Pi]*Inactive[Gamma][1 - x/Pi])
	,
	TestID->"4-Gamma.nb"
]

VerificationTest[
	gammaFrom[multiGamma[{a}, {b}]]
	,
	Gamma[a]/Gamma[b]
	,
	TestID->"5-Gamma.nb"
]

VerificationTest[
	gammaFrom[multiGamma[{a}, {}]]
	,
	Gamma[a]
	,
	TestID->"6-Gamma.nb"
]

VerificationTest[
	gammaFrom[multiGamma[{}, {b}]]
	,
	1/Gamma[b]
	,
	TestID->"7-Gamma.nb"
]

VerificationTest[
	gammaFrom[multiGamma[{multiGamma[{x}, {}]}, {}]]
	,
	Gamma[multiGamma[{x}, {}]]
	,
	TestID->"8-Gamma.nb"
]

VerificationTest[
	gammaSplit[F[a]*Gamma[x]*Gamma[y]]
	,
	{Gamma[x]*Gamma[y], F[a]}
	,
	TestID->"9-Gamma.nb"
]

VerificationTest[
	gammaSplit[Gamma[x]*Gamma[y]*H[a]]
	,
	{Gamma[x]*Gamma[y], H[a]}
	,
	TestID->"10-Gamma.nb"
]

VerificationTest[
	gammaSplit[Gamma[x]]
	,
	{Gamma[x], 1}
	,
	TestID->"11-Gamma.nb"
]

VerificationTest[
	gammaSplit[Gamma[x] + Gamma[y]]
	,
	{1, Gamma[x] + Gamma[y]}
	,
	TestID->"12-Gamma.nb"
]

VerificationTest[
	gammaSplit[x]
	,
	{1, x}
	,
	TestID->"13-Gamma.nb"
]

VerificationTest[
	gammaTakeResidue[x, n, Gamma[-x]][Gamma[x]]
	,
	Quiet[Gamma[x]]
	,
	{Yurie`Math`gammaTakeResidue::gammaNotInExpr}
	,
	TestID->"14-Gamma.nb"
]

VerificationTest[
	gammaTakeResidue[x, x, Gamma[-x]][Gamma[x]]
	,
	Quiet[Gamma[x]]
	,
	{Yurie`Math`gammaTakeResidue::indexConflict}
	,
	TestID->"15-Gamma.nb"
]

VerificationTest[
	gammaTakeResidue[x, n, Gamma[f[x]]][Gamma[x]]
	,
	Quiet[Gamma[x]]
	,
	{Yurie`Math`gammaTakeResidue::gammaNotMatchVar}
	,
	TestID->"16-Gamma.nb"
]

VerificationTest[
	gammaTakeResidue[y, n, Gamma[f[x]]][Gamma[x]]
	,
	Quiet[Gamma[x]]
	,
	{Yurie`Math`gammaTakeResidue::gammaNotMatchVar}
	,
	TestID->"17-Gamma.nb"
]

VerificationTest[
	gammaTakeResidue[x, n, Gamma[-x], "ShowPole" -> True][Gamma[-x]]
	,
	{-((-1)^n/n!), x -> n}
	,
	TestID->"18-Gamma.nb"
]

VerificationTest[
	gammaTakeResidue[x, n, Gamma[x], "ShowPole" -> False][Gamma[x]]
	,
	(-1)^n/n!
	,
	TestID->"19-Gamma.nb"
]

VerificationTest[
	gammaTakeResidue[x, n, Gamma[x], "ShowPole" -> Return][Gamma[x]]
	,
	x -> -n
	,
	TestID->"20-Gamma.nb"
]

VerificationTest[
	gammaTakeResidue[x, n, Gamma[(-a)*x]][Gamma[(-a)*x]*Gamma[x]]
	,
	-(((-1)^n*Gamma[n/a])/(a*n!))
	,
	TestID->"21-Gamma.nb"
]

VerificationTest[
	gammaTakeResidue[x, n, Gamma[x], "SimplePole" -> False][Gamma[x]^2]
	,
	(2*(-1)^(2*n)*PolyGamma[0, 1 + n])/n!^2
	,
	TestID->"22-Gamma.nb"
]

VerificationTest[
	Simplify[Table[SeriesCoefficient[Gamma[x]^2, {x, -n, -1}] - (2*(-1)^(2*n)*PolyGamma[0, 1 + n])/n!^2, {n, Range[4]}]]
	,
	{0, 0, 0, 0}
	,
	TestID->"23-Gamma.nb"
]

VerificationTest[
	multiGamma[{}, {}]
	,
	1
	,
	TestID->"24-Gamma.nb"
]

VerificationTest[
	multiGamma[{x}, {x}]
	,
	1
	,
	TestID->"25-Gamma.nb"
]

VerificationTest[
	multiGamma[{y, x}, {}]
	,
	multiGamma[{x, y}, {}]
	,
	TestID->"26-Gamma.nb"
]

VerificationTest[
	multiGamma[{x, y}, {x}]
	,
	multiGamma[{y}, {}]
	,
	TestID->"27-Gamma.nb"
]

VerificationTest[
	multiGammaFrom[Gamma[a] + Gamma[b]*Gamma[c] + f[Gamma[b]/Gamma[d]]]
	,
	f[multiGamma[{b}, {d}]] + multiGamma[{a}, {}] + multiGamma[{b, c}, {}]
	,
	TestID->"28-Gamma.nb"
]

VerificationTest[
	multiGammaFrom[(1 + Gamma[a])/x]
	,
	(1 + multiGamma[{a}, {}])/x
	,
	TestID->"29-Gamma.nb"
]

VerificationTest[
	multiGammaFrom[(1 + Gamma[a])/Gamma[b]^2]
	,
	multiGamma[{}, {b, b}]*(1 + multiGamma[{a}, {}])
	,
	TestID->"30-Gamma.nb"
]

VerificationTest[
	multiGammaFrom[(1 + Gamma[a]^2)/Gamma[b]]
	,
	multiGamma[{}, {b}]*(1 + multiGamma[{a, a}, {}])
	,
	TestID->"31-Gamma.nb"
]

VerificationTest[
	multiGammaFrom[Gamma[Gamma[x]]]
	,
	multiGamma[{Gamma[x]}, {}]
	,
	TestID->"32-Gamma.nb"
]

VerificationTest[
	multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, c}, {d}]]
	,
	multiGamma[{a1 + b1, a2 + b1, a1 + b2, a2 + b2, c}, {a1 + a2 + b1 + b2, d}]
	,
	TestID->"33-Gamma.nb"
]

VerificationTest[
	multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, c, s}, {d}]]
	,
	Quiet[multiGamma[{c, a1 - s, a2 - s, s, b1 + s, b2 + s}, {d}]]
	,
	{Yurie`Math`multiGammaReduceByBarnesLemma::notMatch}
	,
	TestID->"34-Gamma.nb"
]

VerificationTest[
	multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, c}, {d, s}]]
	,
	Quiet[multiGamma[{c, a1 - s, a2 - s, b1 + s, b2 + s}, {d, s}]]
	,
	{Yurie`Math`multiGammaReduceByBarnesLemma::notMatch}
	,
	TestID->"35-Gamma.nb"
]

VerificationTest[
	multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, b3 + s, c}, {a1 + a2 + b1 + b2 + b3 + s, d}]]
	,
	multiGamma[{a1 + b1, a2 + b1, a1 + b2, a2 + b2, a1 + b3, a2 + b3, c}, {a1 + a2 + b1 + b2, a1 + a2 + b1 + b3, a1 + a2 + b2 + b3, d}]
	,
	TestID->"36-Gamma.nb"
]

VerificationTest[
	multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, b3 + s, c, s}, {a1 + a2 + b1 + b2 + b3 + s, d}]]
	,
	Quiet[multiGamma[{c, a1 - s, a2 - s, s, b1 + s, b2 + s, b3 + s}, {d, a1 + a2 + b1 + b2 + b3 + s}]]
	,
	{Yurie`Math`multiGammaReduceByBarnesLemma::notMatch}
	,
	TestID->"37-Gamma.nb"
]

VerificationTest[
	multiGammaReduceByBarnesLemma[s][multiGamma[{a1 - s, a2 - s, b1 + s, b2 + s, b3 + s, c}, {a1 + a2 + b1 + b2 + b3 + s, d, s}]]
	,
	Quiet[multiGamma[{c, a1 - s, a2 - s, b1 + s, b2 + s, b3 + s}, {d, s, a1 + a2 + b1 + b2 + b3 + s}]]
	,
	{Yurie`Math`multiGammaReduceByBarnesLemma::notMatch}
	,
	TestID->"38-Gamma.nb"
]

VerificationTest[
	(multiGammaSimplify[#1, "Assumptions" -> Element[x, Integers]] & )[multiGamma[{}, {x, 1 - x}]]
	,
	0
	,
	TestID->"39-Gamma.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-Gamma.nb"
]