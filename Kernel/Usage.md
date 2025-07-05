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

* `#!wl matSquareQ` - testing if is a square matrix.

* `#!wl matComm` - matComm[a,b]=a.b-b.a.

* `#!wl matJordan` - Jordan matrix. matJordan[dim_Integer,a_Diagonal,b_OffDiagonal:1].

* `#!wl matAngularMomentum` - spin-j representation of angular momentum in the unit of hbar. The column/row indices run from j to -j.


<!-- OperatorForm.wl -->

* `#!wl SS` - Simplify.

* `#!wl FS` - FullSimplify.

* `#!wl FE` - FunctionExpand.

* `#!wl FES` - FunctionExpand + Simplify.

* `#!wl AS` - operator form: Assuming.

* `#!wl SSA` - operator form: Simplify + Assuming.

* `#!wl FSA` - operator form: FullSimplify + Assuming.

* `#!wl FEA` - operator form: FunctionExpand + Assuming.

* `#!wl FESA` - operator form: FunctionExpand + Simplify + Assuming.

* `#!wl modularize` - modularize[scope[code, iterators]]: modularize the scoping construction (e.g. Table, Sum, and Integrate) such that the iterators are lexically scoped.

* `#!wl block` - operator form: Block.

* `#!wl with` - operator form: With.

* `#!wl module` - operator form: Module.

* `#!wl rep` - operator form: ReplaceAll.

* `#!wl part` - operator form: Part.

* `#!wl plus` - operator form: Plus.

* `#!wl minus` - operator form: Minus.

* `#!wl times` - operator form: Times.

* `#!wl divide` - operator form: Divide.

* `#!wl series` - operator form: Series + Normal.

* `#!wl limit` - operator form: Limit.

* `#!wl solve` - operator form: Solve.

* `#!wl solve1` - operator form: Solve + First.

* `#!wl collect` - operator form: Collect.


<!-- Quest.wl -->

* `#!wl isN` - zero or positive integers.

* `#!wl isZ` - integers.

* `#!wl isZP` - positive integers.

* `#!wl isZN` - negative integers.

* `#!wl isZP0` - zero or positive integers.

* `#!wl isZN0` - zero or negative integers.

* `#!wl isQ` - rational numbers.

* `#!wl isR` - real numbers.

* `#!wl isRP` - positive real numbers.

* `#!wl isRN` - negative real numbers.

* `#!wl isRP0` - zero or positive real numbers.

* `#!wl isRN0` - zero or negative real numbers.

* `#!wl isC` - complex numbers.

* `#!wl presentQ` - Not + FreeQ.

* `#!wl linearQ` - linearQ[expr,var|varList]: whether the expression is linear and at least one variable is present.

* `#!wl syntacticNegativeQ` - syntacticNegativeQ[expr]: whether the expression is syntactically negative.

* `#!wl patternPresentQ` - patternPresentQ[expr]: whether any pattern occurs in the expression.

* `#!wl patternFreeQ` - patternFreeQ[expr]: whether no pattern occurs in the expression.


<!-- Relation.wl -->

* `#!wl relationMellinBarnes` - Mellin-Barnes relation.

* `#!wl relationFeynman` - Feynman-Schwinger relation.

* `#!wl relationPowerPhase` - relation for power phase.


<!-- SimplifyUnsafe.wl -->

* `#!wl unsafePowerTogether` - unsafePowerTogether[expr]: try to combine power factors. May produce mathematically invalid result.

* `#!wl unsafePowerApart` - unsafePowerApart[expr]: try to separate power factors. May produce mathematically invalid result.

* `#!wl unsafePowerSimplify` - unsafePowerSimplify[expr]: try to simplify power factors. May produce mathematically invalid result.

* `#!wl unsafeExprTogether` - unsafeExprTogether[expr]: try to combine power factors, logarithms, and absolute values. May produce mathematically invalid result.

* `#!wl unsafeExprApart` - unsafeExprApart[expr]: try to separate power factors, logarithms, and absolute values. May produce mathematically invalid result.

* `#!wl unsafeExprSimplify` - unsafeExprSimplify[expr]: try to simplify power factors, logarithms, and absolute values. May produce mathematically invalid result.


<!-- Simplify.wl -->

* `#!wl freeze` - freeze[pattern, operation, level][expr]: freeze subexpressions matching the pattern, then perform the operation and unfreeze. The supported transformation rules are: _->Positive, _->Negative, _->{_,_}. The default operation is Simplify.

* `#!wl freezeNegative` - freezeNegative[pattern, operation, level][expr]: variant of freeze with Negative as the default transformation.

* `#!wl focus` - focus[pattern, operation, level][expr]: apply the operation to the arguments of functions with the specified heads. The default operation is Simplify.

* `#!wl fracFocus` - fracFocus[operation, level][expr]: apply the operation to fractions (expressions containing negative powers). The default operation is Simplify.

* `#!wl fracReduce` - fracReduce[operation, factor][expr]: multiply the factor to the numerator and denominator, then apply the operation separately to them. The default operation is Simplify. The default factor is 1.

* `#!wl powerFocus` - powerFocus[operation, level][expr]: apply the operation to the base and exponent of power factors. The default operation is Simplify.

* `#!wl powerBaseFocus` - powerBaseFocus[operation, level][expr]: apply the operation to the base of power factors only. The default operation is Simplify.

* `#!wl powerExponentFocus` - powerExponentFocus[operation, level][expr]: apply the operation to the exponent of power factors only. The default operation is Simplify.

* `#!wl powerSeparate` - powerSeparate[baseP][expr]: separate the product expression into power factors and non-power factors. baseP specifies the pattern of power bases to match.

* `#!wl powerBaseTogether` - powerBaseTogether[baseP, basePreservedP, baseExpandedP][expr]: take together the bases of power factors. baseP specifies which bases to combine. basePreservedP specifies which bases to preserve. baseExpandedP specifies which bases to expand manually.

* `#!wl powerExpand` - powerExpand[baseP, basePreservedP, baseExpandedP][expr]: combine power bases using powerBaseTogether, then expand power factors, and finally simplify power exponents. baseP specifies which bases to combine. basePreservedP specifies which bases to preserve. baseExpandedP specifies which bases to expand manually.

* `#!wl powerExponentCollect` - powerExponentCollect[powers...][expr]: collect and combine power factors with common exponents. The default is to try to collect all power factors.

* `#!wl trigPhaseReduce` - trigPhaseReduce[vars..][expr]: reduce phase factors in trigonometric functions using periodicity. vars specifies the variables to consider for periodicity.

* `#!wl deltaReduce` - deltaReduce[expr]: reduce the Dirac delta function and its derivatives in the expression.

* `#!wl swap` - swap[a, b][expr]: swap the two symbols in the expression. swap[{a1, b1}, {a2, b2}, ...][expr]: swap the pairs simultaneously.

* `#!wl separate` - separate[criterion][expr_]: separate the elements based on whether they satisfy the criterion.

* `#!wl stripPattern` - stripPattern[expr, head]: strip off pattern-related functions from the expression and wrap it with head. The default head is Defer.

* `#!wl vanishing` - vanishing[expr]: clean up the expression by removing redundant vanishing terms. This is equivalent to Simplify + Flatten + DeleteDuplicates.

* `#!wl extractSymbol` - extractSymbol[expr, exclusionList]: extract user-defined symbols from the expression. exclusionList specifies the contexts to exclude.

* `#!wl extractVariable` - extractVariable[expr, exclusionList]: extract user-defined variables from the expression. exclusionList specifies the contexts to exclude.