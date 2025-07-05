<!-- Deprecation.wl -->

* `#!wl relationPowerMono` - relation for branch cut of Power at zero.

* `#!wl collectDerivative` - collect by derivatives.

* `#!wl powerExpandFactor` - factor the base of powers and then expand.


<!-- Diff.wl -->

* `#!wl PD` - head of partial derivative.

* `#!wl INT` - head of integral.

* `#!wl SUM` - head of sum.

* `#!wl integrate` - operator form of Integrate.

* `#!wl summation` - operator form of Sum.

* `#!wl diffChange` - change variables in differential equations.

* `#!wl integrateChange` - change variables in integrals.

* `#!wl IBP` - perform integration by parts.

* `#!wl jacobianMatrix` - Jacobian matrix.

* `#!wl jacobianDet` - Jacobian determinant.

* `#!wl PDCoefficient` - extract the coefficients of PD[__].

* `#!wl PDCollect` - collect the terms with respect to PD[__].

* `#!wl diffCoefficient` - extract the coefficients of Derivative[__][_][__].

* `#!wl diffCollect` - collect the terms with respect to Derivative[__][_][__].

* `#!wl diffReplace` - replace the derivatives of the function.

* `#!wl diffComm` - diffComm[X,Y]=-(X[Y[#]]-Y[X[#]])&.


<!-- DLMF.wl -->

* `#!wl DLMF` - simplify expressions by the rules in DLMFData.

* `#!wl DLMFAs` - simplify expressions by the rules in DLMFData with the specified conditions.

* `#!wl DLMFAsTrue` - simplify expressions by the rules in DLMFData ignoring the conditions.

* `#!wl DLMFRule` - return the rules in DLMFData.

* `#!wl DLMFRuleShow` - show the rules without context marks in DLMFData.


<!-- Dye.wl -->

* `#!wl dye` - dye[expr_]: color the elements at the first level of expression.

* `#!wl dyeIn` - dyeIn[levelspec_:1][expr_]: color the elements at the specific levels of expression.

* `#!wl dyeBy` - dyeBy[pattern_,levelspec_,n_][expr_]: color the occurrences of pattern in expression.

* `#!wl dyeAt` - dyeAt[positions_][expr_]: color the expression at the specified positions in expression.

* `#!wl dyeOff` - dyeOff[expr_]: eliminate the colors from dye.


<!-- Gamma.wl -->

* `#!wl gammaSimplify` - simplify Gamma factors in the expression. Developer\`GammaSimplify

* `#!wl gammaFrom` - expand everything to Gamma factors.

* `#!wl gammaSeparate` - split a product into Gamma factors and the rests.

* `#!wl gammaTakeResidue` - take residue of Gamma factors.

* `#!wl multiGamma` - head of multi-Gamma symbol.

* `#!wl multiGammaFrom` - collect Gamma factors into multi-Gamma symbols.

* `#!wl multiGammaSimplify` - simplify the multi-Gamma symbol.

* `#!wl multiGammaReduceByBarnesLemma` - reduce the multi-Gamma symbol by the Barnes lemmas.


<!-- Hyper.wl -->

* `#!wl hyper` - head used by hyperConvert.

* `#!wl JacobiPhi` - Jacobi Phi function, JacobiPhi[a,b,c,z].

* `#!wl WilsonPolynomial` - Wilson polynomial, WilsonPolynomial[a,b,c,d,n,x].

* `#!wl hyperSeparate` - split a product into hypergeometric functions and the rests.

* `#!wl hyperUnregularize` - convert regularized hypergeometric function to the normal one.

* `#!wl hyperRegularize` - convert hypergeometric function to the regularized one.

* `#!wl hyperToTaylor` - convert hypergeometric function to Taylor series.

* `#!wl hyperToMellinBarnes` - convert hypergeometric function to Mellin-Barnes integral.

* `#!wl hyperToMellinBarnes2` - convert hypergeometric function to Mellin-Barnes integral in terms of (1-z).

* `#!wl hyperFromAppellF1` - convert Appell F1 function to hypergeometric summation.

* `#!wl JacobiPhiToHyper` - convert Jacobi Phi to Hypergeometric2F1.

* `#!wl JacobiPhiFromHyper` - convert Hypergeometric2F1 to Jacobi Phi.

* `#!wl WilsonPolynomialToHyper` - convert Wilson polynomial to Hypergeometric4F3.

* `#!wl WilsonPolynomialFromHyper` - convert Hypergeometric4F3 to Wilson polynomial.

* `#!wl AppellF1FromIntegral` - convert integral to Appell F1.


<!-- Label.wl -->

* `#!wl label` - join the variable(s) and label(s) into a (sequence of) labeled object(s).

* `#!wl labelAt` - take the specific value(s) of the labeled object(s).

* `#!wl labelConvert` - convert the labeled object(s) according to the two specified label heads.

* `#!wl labelJoin` - labelConvert: _->Symbol.

* `#!wl labelSplit` - labelConvert: Symbol->_.

* `#!wl labelToZero` - x1->0.

* `#!wl labelToEqual` - x1->x2.

* `#!wl labelToDiff` - x1->x12+x2.

* `#!wl labelToDiffZero` - x1->x12, x2->0.

* `#!wl labelToDiffBack` - x12->x1-x2.


<!-- Lie.wl -->

* `#!wl lie` - simple Lie algebras.

* `#!wl lieSimpleRoot` - orthogonal simple roots of simple Lie algebras.

* `#!wl lieCartan` - Cartan matrix of simple Lie algebras.

* `#!wl lieCartanInverse` - inverse Cartan matrix of simple Lie algebras.

* `#!wl lieDynkinDiagram` - Dynkin diagram of simple Lie algebras.


<!-- Matrix.wl -->

* `#!wl matSquareQ` - matSquareQ[matrix]: test if the matrix is square.

* `#!wl matComm` - matComm[a, b]: compute the commutator of the two matrices. Sketch: a.b - b.a.

* `#!wl matJordan` - matJordan[dim, a, b]: construct a Jordan matrix of specified dimension. a: the common diagonal element. b: the common super-diagonal element. Default[b]: 1.

* `#!wl matAngularMomentum` - matAngularMomentum[j][direction]: generate angular momentum matrices for the spin-j representation. Value[direction]: {"x", "y", "z"|0, 1, -1}. The column/row indices run from j to -j.


<!-- OperatorForm.wl -->

* `#!wl SS` - OperatorForm: Simplify.

* `#!wl FS` - OperatorForm: FullSimplify.

* `#!wl FE` - OperatorForm: FunctionExpand.

* `#!wl FES` - OperatorForm: FunctionExpand + Simplify.

* `#!wl AS` - OperatorForm: Assuming.

* `#!wl SSA` - OperatorForm: Simplify + Assuming.

* `#!wl FSA` - OperatorForm: FullSimplify + Assuming.

* `#!wl FEA` - OperatorForm: FunctionExpand + Assuming.

* `#!wl FESA` - OperatorForm: FunctionExpand + Simplify + Assuming.

* `#!wl modularize` - modularize[scope[code, iterators]]: modularize the scoping construction (e.g. Table, Sum, and Integrate) such that the iterators are lexically scoped.

* `#!wl block` - OperatorForm: Block.

* `#!wl with` - OperatorForm: With.

* `#!wl module` - OperatorForm: Module.

* `#!wl rep` - rep[rules][expr]: operator form of ReplaceAll with the rules being flattened.

* `#!wl repdeep` - repdeep[rules][level][expr]: operator form of Replace with the rules being flattened. Default[level]: All.

* `#!wl part` - OperatorForm: Part.

* `#!wl plus` - OperatorForm: Plus.

* `#!wl minus` - OperatorForm: Minus.

* `#!wl times` - OperatorForm: Times.

* `#!wl divide` - OperatorForm: Divide.

* `#!wl series` - OperatorForm: Series + Normal.

* `#!wl limit` - OperatorForm: Limit.

* `#!wl solve` - OperatorForm: Solve.

* `#!wl solve1` - OperatorForm: Solve + First.

* `#!wl collect` - OperatorForm: Collect.


<!-- Quest.wl -->

* `#!wl isN` - isN[x..]: test whether the arguments are natural numbers.

* `#!wl isZ` - isZ[x..]: test whether the arguments are integers.

* `#!wl isZP` - isZP[x..]: test whether the arguments are positive integers.

* `#!wl isZN` - isZN[x..]: test whether the arguments are negative integers.

* `#!wl isZP0` - isZP0[x..]: test whether the arguments are zero or positive integers.

* `#!wl isZN0` - isZN0[x..]: test whether the arguments are zero or negative integers.

* `#!wl isQ` - isQ[x..]: test whether the arguments are rationals.

* `#!wl isR` - isR[x..]: test whether the arguments are reals.

* `#!wl isRP` - isRP[x..]: test whether the arguments are positive reals.

* `#!wl isRN` - isRN[x..]: test whether the arguments are negative reals.

* `#!wl isRP0` - isRP0[x..]: test whether the arguments are zero or positive reals.

* `#!wl isRN0` - isRN0[x..]: test whether the arguments are zero or negative reals.

* `#!wl isC` - isC[x..]: test whether the arguments are complex numbers.

* `#!wl presentQ` - presentQ[pattern][expr]: test whether the pattern occurs in the expression.Not + FreeQ.

* `#!wl linearQ` - linearQ[expr, var]: test whether the expression is linear in the variable and the variable is present. linearQ[expr, varList]: test whether the expression is linear in all the variables and at least one is present.

* `#!wl syntacticNegativeQ` - syntacticNegativeQ[expr]: test whether the expression is syntactically negative.

* `#!wl patternPresentQ` - patternPresentQ[expr]: test whether any pattern construction occurs in the expression.

* `#!wl patternFreeQ` - patternFreeQ[expr]: test whether no pattern construction occurs in the expression.


<!-- Relation.wl -->

* `#!wl relationMellinBarnes` - relationMellinBarnes[(x+y)^a, x, s]: generate Mellin-Barnes integral representation for the power factor. Example: (x+y)^a -> mg*x^s*y^(a-s)*INT[s].

* `#!wl relationFeynman` - relationFeynman[x^a*y^b, x, s]: generate Feynman-Schwinger integral representation for combining the two power factors. Example: x^a*y^b -> mg*(x+s*y)^(a+b)*s^(-b-1)*INT[s].

* `#!wl relationPowerPhase` - relationPowerPhase[base, expanded, expanded2, sign]: generate transformation rule for separating the power factor. base: specify the power base. expanded: specify the numerator factors to separate. expanded2: specify the denominator factors to separate and can be omitted. sign: specify the phase direction.


<!-- SimplifyUnsafe.wl -->

* `#!wl unsafePowerTogether` - unsafePowerTogether[expr]: try to combine power factors. May produce mathematically invalid result.

* `#!wl unsafePowerApart` - unsafePowerApart[expr]: try to separate power factors. May produce mathematically invalid result.

* `#!wl unsafePowerSimplify` - unsafePowerSimplify[expr]: try to simplify power factors. May produce mathematically invalid result.

* `#!wl unsafeExprTogether` - unsafeExprTogether[expr]: try to combine power factors, logarithms, and absolute values. May produce mathematically invalid result.

* `#!wl unsafeExprApart` - unsafeExprApart[expr]: try to separate power factors, logarithms, and absolute values. May produce mathematically invalid result.

* `#!wl unsafeExprSimplify` - unsafeExprSimplify[expr]: try to simplify power factors, logarithms, and absolute values. May produce mathematically invalid result.


<!-- Simplify.wl -->

* `#!wl freeze` - freeze[pattern, operation, level][expr]: freeze subexpressions matching the pattern, then perform the operation and unfreeze. freeze[pattern->transform, operation, level][expr]: freeze subexpressions matching the pattern, then perform the operation and unfreeze. Value[pattern->transform]: _->Positive, _->Negative, _->{_,_}. Default[operation]: Simplify. Default[level]: Infinity.

* `#!wl freezeNegative` - freezeNegative[pattern, operation, level][expr]: variant of freeze with Negative as the default transformation.

* `#!wl focus` - focus[pattern, operation, level][expr]: apply the operation to the arguments of functions with the specified heads. If level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used. Default[operation]: Simplify.

* `#!wl fracFocus` - fracFocus[operation, level][expr]: apply the operation to fractions (expressions containing negative powers). If level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used. Default[operation]: Simplify.

* `#!wl fracReduce` - fracReduce[operation, factor][expr]: multiply the factor to the numerator and denominator, then apply the operation separately to them. Default[operation]: Simplify. Default[factor]: 1.

* `#!wl powerFocus` - powerFocus[operation, level][expr]: apply the operation to the base and exponent of power factors. If level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used. Default[operation]: Simplify.

* `#!wl powerBaseFocus` - powerBaseFocus[operation, level][expr]: apply the operation to the base of power factors only. If level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used. Default[operation]: Simplify.

* `#!wl powerExponentFocus` - powerExponentFocus[operation, level][expr]: apply the operation to the exponent of power factors only. If level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used. Default[operation]: Simplify.

* `#!wl powerSeparate` - powerSeparate[baseP][expr]: separate the product expression into power factors and non-power factors. baseP: specify the pattern of power bases to match.

* `#!wl powerBaseTogether` - powerBaseTogether[baseP, basePreservedP, baseExpandedP][expr]: take together the bases of power factors. baseP: specify the bases to combine. basePreservedP: specify the bases to preserve. baseExpandedP: specify the bases to expand manually. To skip baseP/basePreservedP, use All/None.

* `#!wl powerExpand` - powerExpand[baseP, basePreservedP, baseExpandedP][expr]: combine power bases using powerBaseTogether, then expand power factors, and finally simplify power exponents. baseP: specify the bases to combine. basePreservedP: specify the bases to preserve. baseExpandedP: specify the bases to expand manually. To skip baseP/basePreservedP, use All/None.

* `#!wl powerExponentCollect` - powerExponentCollect[powers...][expr]: collect and combine power factors with common exponents. Default[]: try to collect all power factors.

* `#!wl trigPhaseReduce` - trigPhaseReduce[vars..][expr]: reduce phase factors in trigonometric functions using periodicity. vars: specify the variables to consider for periodicity.

* `#!wl deltaReduce` - deltaReduce[expr]: reduce the Dirac delta function and its derivatives in the expression.

* `#!wl swap` - swap[a, b][expr]: swap the two symbols in the expression. swap[{a1, b1}, {a2, b2}, ...][expr]: swap the pairs simultaneously.

* `#!wl separate` - separate[criterion][expr_]: separate the elements based on whether they satisfy the criterion.

* `#!wl stripPattern` - stripPattern[expr, head]: strip off pattern-related functions from the expression and wrap it with head. Default[head]: Defer.

* `#!wl vanishing` - vanishing[expr]: clean up the expression by removing redundant vanishing terms. Sketch: Simplify + Flatten + DeleteDuplicates.

* `#!wl extractSymbol` - extractSymbol[expr, exclusionList]: extract user-defined symbols from the expression. exclusionList: specify the contexts to exclude.

* `#!wl extractVariable` - extractVariable[expr, exclusionList]: extract user-defined variables from the expression. exclusionList: specify the contexts to exclude.