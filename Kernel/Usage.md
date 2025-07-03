<!-- Deprecation.wl -->

* `#!wl relationPowerMono` - relation for branch cut of Power at zero.


<!-- Diff.wl -->

* `#!wl PD` - head of partial derivative.

* `#!wl INT` - head of integral.

* `#!wl SUM` - head of sum.

* `#!wl integrate` - operator form of Integrate.

* `#!wl summation` - operator form of Sum.

* `#!wl diffChange` - diffChange[expr,transformations,oldVars,newVars,functions]  diffChange[] gives the example.

* `#!wl integrateChange` - integrateChange[expr,transformations,oldVars,newVars]  integrateChange[] gives the example.

* `#!wl IBP` - integration by parts.

* `#!wl jacobianMatrix` - jacobianMatrix.

* `#!wl jacobianDet` - jacobianDet.

* `#!wl diffComm` - diffComm[X,Y]=-(X[Y[#]]-Y[X[#]])&.

* `#!wl PDCoefficient` - collect the coefficients of PD[___].


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

* `#!wl AS` - operator form of Assuming.

* `#!wl SSA` - Simplify + Assuming.

* `#!wl FSA` - FullSimplify + Assuming.

* `#!wl FEA` - FunctionExpand + Assuming.

* `#!wl FESA` - FunctionExpand + Simplify + Assuming.

* `#!wl modularize` - modularize scoping constructions.

* `#!wl block` - operator form of Block.

* `#!wl with` - operator form of With.

* `#!wl module` - operator form of Module.

* `#!wl rep` - operator form of ReplaceAll.

* `#!wl part` - operator form of Part, GeneralUtilities\`Slice.

* `#!wl plus` - operator form of Plus.

* `#!wl minus` - operator form of Minus.

* `#!wl times` - operator form of Times.

* `#!wl divide` - operator form of Divide.

* `#!wl series` - operator form of Series + Normal.

* `#!wl limit` - opeartor form of Limit.

* `#!wl solve` - operator form of Solve.

* `#!wl solve1` - operator form of Solve + First.

* `#!wl collect` - operator form of Collect.


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

* `#!wl unsafePowerTogether` - take powers together.

* `#!wl unsafePowerApart` - take powers apart, similar to PowerExpand.

* `#!wl unsafePowerSimplify` - simplify powers.

* `#!wl unsafeExprTogether` - take powers, logs and abs together.

* `#!wl unsafeExprApart` - take powers, logs and abs apart.

* `#!wl unsafeExprSimplify` - simplify powers, logs and abs.

* `#!wl unsafeDeltaSimplify` - simplify Delta functions.


<!-- Simplify.wl -->

* `#!wl freeze` - freeze subexpressions matching the pattern and then perform the operation.

* `#!wl freezeNegative` - variant of freeze. Negative is used as the default transformation.

* `#!wl focus` - simplify the argument(s) of the specified head(s).

* `#!wl fracFocus` - simplify the numerator and denominator of fractions.

* `#!wl fracReduce` - reduce the fraction by multiplying a common factor onto the numerator and denominator.

* `#!wl powerFocus` - simplify the base and exponent of powers.

* `#!wl powerBaseFocus` - simplify the base of powers.

* `#!wl powerExponentFocus` - simplify the exponent of powers.

* `#!wl powerSeparate` - split a product into powers with specified base(s) and the rests.

* `#!wl powerBaseTogether` - make together the specified base(s) of powers.

* `#!wl powerExpand` - expand the powers with the specified base(s).

* `#!wl powerExpandFactor` - factor the base of powers and then expand.

* `#!wl powerExponentCollect` - collect powers by the specified exponent(s).

* `#!wl trigPhaseReduce` - reduce phase factors in trigonometric functions by the given assumptions.

* `#!wl deltaReduce` - reduce the Dirac delta function.

* `#!wl collectDerivative` - collect by derivatives.

* `#!wl swap` - swap two symbols in an expression.

* `#!wl separate` - separate the elements by whether or not satisfying the criteria.

* `#!wl stripPattern` - strip off pattern-related functions in expressions.

* `#!wl vanishing` - Simplify + Flatten + DeleteDuplicates.

* `#!wl extractSymbol` - extract symbols from the expression.

* `#!wl extractVariable` - extract variables from the expression.