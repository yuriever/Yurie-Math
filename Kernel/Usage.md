<!-- Deprecation.wl -->

* `#!wl hyperRegToUnreg` - convert Hypergeometric2F1Regularized to Hypergeometric2F1.

* `#!wl powerBaseSimplify` - simplify the power bases.

* `#!wl trigPhaseSimplify` - reduce the phase factor in trigonometric functions.

* `#!wl separateBy` - separate the elements by whether or not satisfying the criteria.

* `#!wl solveFirst` - operator form of Solve + First.

* `#!wl jordanBlock` - matJordan[dim_Integer,a_Diagonal,b_OffDiagonal:1].

* `#!wl gammaSplit` - split a product into a list containing Gamma factors and the rests.

* `#!wl hyperSplit` - split a product into a list containing Hypergeometric2F1 factors and the rests.


<!-- Diff.wl -->

* `#!wl jacobianMatrix` - jacobianMatrix.

* `#!wl jacobianDet` - jacobianDet.

* `#!wl diffComm` - diffComm[X,Y]=-(X[Y[#]]-Y[X[#]])&.

* `#!wl diffChange` - diffChange[expr,transformations,oldVars,newVars,functions]  diffChange[] gives the example.

* `#!wl integrateChange` - integrateChange[expr,transformations,oldVars,newVars]  integrateChange[] gives the example.

* `#!wl IBP` - integration by parts.


<!-- DLMF.wl -->

* `#!wl DLMF` - simplify expressions by the rules in DLMFData.

* `#!wl DLMFRule` - return the rules in DLMFData.

* `#!wl DLMFRuleShow` - show the rules without context marks in DLMFData.


<!-- Dye.wl -->

* `#!wl dye` - dye[expr_]: color the elements at the first level of expression.

* `#!wl dyeIn` - dyeIn[levelspec_:1][expr_]: color the elements at the specific levels of expression.

* `#!wl dyeBy` - dyeBy[pattern_,levelspec_,n_][expr_]: color the occurrences of pattern in expression.

* `#!wl dyeAt` - dyeAt[positions_][expr_]: color the expression at the specified positions in expression.

* `#!wl dyeOff` - dyeOff[expr_]: eliminate the colors from dye.


<!-- Gamma.wl -->

* `#!wl gammaSimplify` - simplify Gamma factors in the expression.\n Developer\`GammaSimplify

* `#!wl gammaFrom` - expand everything to Gamma factors.

* `#!wl gammaSeparate` - split a product into a list containing Gamma factors and the rests.

* `#!wl gammaTakeResidue` - take residue of Gamma factors.

* `#!wl multiGamma` - head of multi-Gamma symbol.

* `#!wl multiGammaFrom` - collect Gamma factors into multi-Gamma symbols.

* `#!wl multiGammaSimplify` - simplify the multi-Gamma symbol.

* `#!wl multiGammaReduceByBarnesLemma` - reduce the multi-Gamma symbol by the Barnes lemmas.

* `#!wl relationMellinBarnes` - Mellin-Barnes relation.

* `#!wl relationFeynman` - Feynman-Schwinger relation.


<!-- Head.wl -->

* `#!wl PD` - head of partial derivative that acts on the rest of the expression.

* `#!wl INT` - head of integral that acts on the rest of the expression.

* `#!wl SUM` - head of sum that acts on the rest of the expression.

* `#!wl PDCoefficient` - collect the coefficients of PD[___].


<!-- Hyper.wl -->

* `#!wl hyperSeparate` - split a product into a list containing Hypergeometric2F1 factors and the rests.

* `#!wl hyperUnregularize` - convert Hypergeometric2F1Regularized to Hypergeometric2F1.

* `#!wl hyperTaylor` - head used by hyperToTaylor.

* `#!wl hyperMellinBarnes` - head used by hyperToMellinBarnes and hyperToMellinBarnes2.

* `#!wl hyperToTaylor` - convert Hypergeometric2F1 factors to Taylor terms.

* `#!wl hyperToMellinBarnes` - convert Hypergeometric2F1 factors to Mellin-Barnes integrands.

* `#!wl hyperToMellinBarnes2` - convert Hypergeometric2F1 factors to Mellin-Barnes integrands in terms of (1-z).

* `#!wl jacobiPhi` - head of Jacobi Phi, jacobiPhi[a,b,c,z], DLMF:15.9.11.

* `#!wl jacobiPhiToHyper` - convert Jacobi Phi to Hypergeometric2F1.

* `#!wl jacobiPhiFromHyper` - convert Hypergeometric2F1 to Jacobi Phi.

* `#!wl wilsonPolynomial` - head of Wilson polynomial, wilsonPolynomial[a,b,c,d,n,x].

* `#!wl wilsonPolynomialToHyper` - convert Wilson polynomial to Hypergeometric4F3.

* `#!wl wilsonPolynomialFromHyper` - convert Hypergeometric4F3 to Wilson polynomial.


<!-- Index.wl -->

* `#!wl indexize` - join the variable and index(s) into a symbol.

* `#!wl indexify` - join the variable(s) and index(s) into a sequence of symbols.

* `#!wl indexJoin` - join indexed variables into symbols in the expression.

* `#!wl indexSplit` - split symbols into indexed variables in the expression.

* `#!wl indexToZero` - x1->0.

* `#!wl indexToEqual` - x1->x2.

* `#!wl indexToDiff` - x1->x12+x2.

* `#!wl indexToDiffZero` - x1->x12,x2->0.

* `#!wl indexToDiffBack` - x12->x1-x2.


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


<!-- Simplify.wl -->

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

* `#!wl collect` - operator form of Collect.

* `#!wl exprTogether` - take powers, logs and abs together.

* `#!wl exprApart` - take powers, logs and abs apart.

* `#!wl exprSim` - simplify powers, logs and abs.

* `#!wl powerTogether` - take powers together.

* `#!wl powerApart` - take powers apart, similar to PowerExpand.

* `#!wl powerCollect` - collect powers by exponents.

* `#!wl powerSim` - simplify powers.

* `#!wl deltaSim` - simplify Delta functions.

* `#!wl fracSimplify` - simplify the numerator and denominator.

* `#!wl trigPhaseReduce` - reduce the phase factor in trigonometric functions.

* `#!wl collectDerivative` - collect by derivatives.

* `#!wl stripPattern` - strip off pattern-related functions in expressions.

* `#!wl vanishing` - Simplify + Flatten + DeleteDuplicates.

* `#!wl swap` - swap two symbols in an expression.

* `#!wl separate` - separate the elements by whether or not satisfying the criteria.

* `#!wl freeze` - freeze subexpressions matching the pattern and then perform the operation.

* `#!wl focus` - simplify the arguments of the specified heads.