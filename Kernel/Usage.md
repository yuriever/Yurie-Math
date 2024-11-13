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

* `#!wl dye2` - dye2[levelspec_:1][expr_]: color the elements at the specific levels of expression.

* `#!wl dyeBy` - dyeBy[pattern_,levelspec_,n_][expr_]: color the occurrences of pattern in expression.

* `#!wl dyeAt` - dyeAt[positions_][expr_]: color the expression at the specified positions in expression.

* `#!wl dyeOff` - dyeOff[expr_]: eliminate the colors from dye.


<!-- Gamma.wl -->

* `#!wl gammaFrom` - expand everything to Gamma factors.

* `#!wl gammaSplit` - split a product into a list containing Gamma factors and the rests.

* `#!wl gammaTakeResidue` - take residue of Gamma factors.

* `#!wl multiGamma` - head of multi-Gamma symbol.

* `#!wl multiGammaFrom` - collect Gamma factors into multi-Gamma symbols.

* `#!wl multiGammaSimplify` - simplify the multi-Gamma symbol.

* `#!wl multiGammaReduceByBarnesLemma` - reduce the multi-Gamma symbol by the Barnes lemmas.


<!-- Hyper.wl -->

* `#!wl hyperSplit` - split a product into a list containing Hypergeometric2F1 factors and the rests.

* `#!wl hyperRegToUnreg` - convert Hypergeometric2F1Regularized to Hypergeometric2F1.

* `#!wl hyperSwap` - swap the first two arguments of Hypergeometric2F1.

* `#!wl hyperTo` - convert Hypergeometric2F1 factors according to the prototype rule.

* `#!wl hyperToTaylor` - convert Hypergeometric2F1 factors to Taylor terms.

* `#!wl hyperToMellinBarnes` - convert Hypergeometric2F1 factors to Mellin-Barnes integrands.

* `#!wl hyperTaylor` - head used by hyperToTaylor.

* `#!wl hyperMellinBarnes` - head used by hyperToMellinBarnes.

* `#!wl jacobiPhi` - head of Jacobi Phi, jacobiPhi[a,b,c,z], DLMF:15.9.11.

* `#!wl jacobiPhiToHyper` - convert Jacobi Phi to Hypergeometric2F1.

* `#!wl jacobiPhiFromHyper` - convert Hypergeometric2F1 to Jacobi Phi.

* `#!wl wilsonPolynomial` - head of Wilson polynomial, wilsonPolynomial[a,b,c,d,n,x].

* `#!wl wilsonPolynomialToHyper` - convert Wilson polynomial to Hypergeometric4F3.

* `#!wl wilsonPolynomialFromHyper` - convert Hypergeometric4F3 to Wilson polynomial.


<!-- Lie.wl -->

* `#!wl lie` - simple Lie algebras.

* `#!wl lieSimpleRoot` - orthogonal simple roots of simple Lie algebras.

* `#!wl lieCartan` - Cartan matrix of simple Lie algebras.

* `#!wl lieCartanInverse` - inverse Cartan matrix of simple Lie algebras.

* `#!wl lieDynkinDiagram` - Dynkin diagram of simple Lie algebras.


<!-- Matrix.wl -->

* `#!wl matSquareQ` - testing if is a square matrix.

* `#!wl matComm` - matComm[a,b]=a.b-b.a.

* `#!wl jordanBlock` - jordanBlock[dim_Integer,a_OffDiagonal,b_Diagonal:1] jordanBlock[dim_Integer,a_].

* `#!wl sparseBlockMatrix` - SparseArray\`SparseBlockMatrix.


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

* `#!wl linearQ` - whether the expression is linear with respect to the variables.

* `#!wl presentQ` - Not + FreeQ.

* `#!wl patternPresentQ` - Internal\`PatternPresentQ.

* `#!wl patternFreeQ` - Internal\`PatternFreeQ.


<!-- Simplify.wl -->

* `#!wl SS` - Simplify.

* `#!wl FS` - FullSimplify.

* `#!wl FE` - FunctionExpand.

* `#!wl AS` - operator form of Assuming.

* `#!wl SSA` - Simplify + Assuming.

* `#!wl FSA` - FullSimplify + Assuming.

* `#!wl FEA` - FunctionExpand + Assuming.

* `#!wl modularize` - modularize scoping constructions.

* `#!wl block` - operator form of Block.

* `#!wl with` - operator form of With.

* `#!wl module` - operator form of Module.

* `#!wl times` - operator form of Times.

* `#!wl plus` - operator form of Plus.

* `#!wl series` - operator form of Series + Normal.

* `#!wl limit` - opeartor form of Limit.

* `#!wl solve` - operator form of Solve.

* `#!wl solveFirst` - operator form of Solve + First.

* `#!wl part` - operator form of Part, GeneralUtilities\`Slice.

* `#!wl collect` - operator form of Collect.

* `#!wl exprTogether` - take powers, logs and abs together.

* `#!wl exprApart` - take powers, logs and abs apart.

* `#!wl exprSim` - simplify powers, logs and abs.

* `#!wl powerTogether` - take powers together.

* `#!wl powerApart` - take powers apart, similar to PowerExpand.

* `#!wl powerCollect` - collect powers by exponents.

* `#!wl powerSim` - simplify powers.

* `#!wl deltaSim` - simplify Delta functions.

* `#!wl collectDerivative` - collect by derivatives.

* `#!wl fracSimplify` - simplify the Numerator and Denominator by multiplying a factor.

* `#!wl vanishing` - Simplify + Flatten + DeleteDuplicates.

* `#!wl swap` - swap two symbols in an expression.

* `#!wl stripPattern` - strip off pattern-related functions in expressions.

* `#!wl separateBy` - separate the elements by whether or not satisfying the criteria.

* `#!wl freeze` - free subexpressions matching the pattern and perform the operation.