<!-- Deprecation.wl -->

* `#!wl relationPowerMono` - relation for branch cut of Power at zero.

* `#!wl collectDerivative` - collect by derivatives.

* `#!wl powerExpandFactor` - factor the base of powers and then expand.


<!-- Diff.wl -->

* `#!wl PD` - PD[vars]: head of partial derivative.

* `#!wl INT` - INT[vars]: head of integration.

* `#!wl SUM` - SUM[vars]: head of summation.

* `#!wl integrate` - integrate[args][expr]: operator form of Integrate. Default[GenerateConditions]: False.

* `#!wl summation` - summation[args][expr]: operator form of Sum. Default[GenerateConditions]: False.

* `#!wl diffChange` - diffChange[eqList, oldList, newList, funList][expr]: change variables in differential equations. Def[funList]: list of functions to transform. Default["Solution"]: 1. Default["ShowSolution"]: False.

* `#!wl integrateChange` - integrateChange[eqList, oldList, newList, sign][expr]: change variables in integrals. Def[sign]: Jacobian sign. Value[sign]: {-1, 1}. Default["Solution"]: 1. Default["ShowSolution"]: False.

* `#!wl IBP` - IBP[fun][expr]: perform integration by parts. IBP[fun, vars][expr]: perform integration by parts with respect to specific variables.

* `#!wl jacobianMatrix` - jacobianMatrix[funList, varList]: Jacobian matrix.

* `#!wl jacobianDet` - jacobianDet[funList, varList]: Jacobian determinant.

* `#!wl PDCoefficient` - PDCoefficient[post, opts][expr]: extract the coefficients of PD[__]. Def[post]: post-operation applied to the coefficients. Default[post]: Identity. Default["CheckLinearity"]: True.

* `#!wl PDCollect` - PDCollect[args][expr]: collect the terms with respect to PD[__]. Def[args]: inherited from Collect.

* `#!wl diffCoefficient` - diffCoefficient[fun, post, opts][expr]: extract the coefficients of Derivative[__][_][__]. Def[fun]: the head of the function. Def[post]: post-operation applied to the coefficients. Default[post]: Identity. Default["CheckLinearity"]: True.

* `#!wl diffCollect` - diffCollect[fun, args][expr]: collect the terms with respect to Derivative[__][_][__]. diffCollect[funList, args][expr]: collect terms for multiple functions. Def[fun]: the head of the function. Def[args]: inherited from Collect.

* `#!wl diffReplace` - diffReplace[fun->res, ...]: replace the derivatives of the function.

* `#!wl diffComm` - diffComm[X, Y]: compute the commutator of differential operators. Sketch: -(X[Y[#]]-Y[X[#]])&.


<!-- DLMF.wl -->

* `#!wl DLMF` - DLMF[rules, opts][expr]: simplify the expression by the DLMF rules. Default["IgnoreCondition"]: False.

* `#!wl DLMFAs` - DLMFAs[rules, as][expr]: simplify the expression by the DLMF rules under the assumption. Def[as]: the assumption.

* `#!wl DLMFAsTrue` - DLMFAsTrue[rules][expr]: simplify the expression by the DLMF rules ignoring all the conditions. Hint: this is equivalent to DLMF with "IgnoreCondition"->True.

* `#!wl DLMFRule` - DLMFRule[rules, opts]: return the DLMF rules. Default["IgnoreCondition"]: False.

* `#!wl DLMFRuleShow` - DLMFRuleShow[rules, opts]: show the DLMF rules without context marker. Default["IgnoreCondition"]: False.


<!-- Dye.wl -->

* `#!wl dye` - dye[expr]: color the subexpressions at the first level.

* `#!wl dyeIn` - dyeIn[level][expr]: color the subexpressions at the specified level. dyeIn[color, level][expr]: specify the color.

* `#!wl dyeBy` - dyeBy[pattern, level, opts][expr]: color the subexpressions matching the pattern. dyeBy[color, pattern, level, opts][expr]: specify the color. Default[level]: {0, Infinity}, inherited from Position.

* `#!wl dyeAt` - dyeAt[positions][expr]: color the subexpressions at the specified positions. dyeAt[color, positions][expr]: specify the color.

* `#!wl dyeOff` - dyeOff[expr]: remove the coloring applied by dye functions.


<!-- Gamma.wl -->

* `#!wl gammaSimplify` - gammaSimplify[expr]: simplify Gamma functions in the expression. Sketch: Developer\`GammaSimplify.

* `#!wl gammaFrom` - gammaFrom[expr, opts]: expand everything to Gamma functions. Default["Transformation"]: Automatic. Default["ActivateGamma"]: True.

* `#!wl gammaSeparate` - gammaSeparate[expr]: separate a product into Gamma functions and the rest.

* `#!wl gammaTakeResidue` - gammaTakeResidue[variable, index, gamma, sign, opts][expr]: take residue of a series of poles from the Gamma factor. gammaTakeResidue[variable, index->n, gamma, sign, opts][expr]: specify one pole in the series. Def[index]: the index of the poles. Def[gamma]: the argument of the Gamma function. Def[sign]: the direction of contour. Value[sign]: {1, -1, Left, Right}. Default[sign]: 1. Default["SimplePole"]: True. Default["ShowPole"]: True.

* `#!wl multiGamma` - multiGamma[num, denom]: represent a product of Gamma functions in numerator and denominator. Def[num]: list of arguments for Gamma functions in the numerator. Def[denom]: list of arguments for Gamma functions in the denominator.

* `#!wl multiGammaFrom` - multiGammaFrom[expr]: convert Gamma functions into multi-Gamma symbols.

* `#!wl multiGammaSimplify` - multiGammaSimplify[expr]: simplify the multi-Gamma symbol with the assumption. Default[assume]: True.

* `#!wl multiGammaReduceByBarnesLemma` - multiGammaReduceByBarnesLemma[s][expr]: reduce the multi-Gamma symbol by the first and second Barnes lemmas. Def[s]: the variable parameter in the Barnes lemma reduction.


<!-- Hyper.wl -->

* `#!wl hyper` - hyper[type, var][expr]: head used by hypergeometric conversion functions.

* `#!wl JacobiPhi` - JacobiPhi[a, b, c, z]: Jacobi Phi function.

* `#!wl WilsonPolynomial` - WilsonPolynomial[a, b, c, d, n, x]: Wilson polynomial.

* `#!wl hyperSeparate` - hyperSeparate[expr]: separate a product into hypergeometric functions and the rest.

* `#!wl hyperUnregularize` - hyperUnregularize[expr]: convert regularized hypergeometric function to the normal one.

* `#!wl hyperRegularize` - hyperRegularize[expr]: convert hypergeometric function to the regularized one.

* `#!wl hyperToTaylor` - hyperToTaylor[symbols][expr]: convert hypergeometric function to Taylor series. hyperToTaylor[symbols, indicator][expr]: indicate the summation. Default[indicator]: SUM.

* `#!wl hyperToMellinBarnes` - hyperToMellinBarnes[symbols][expr]: convert hypergeometric function to Mellin-Barnes integral. hyperToMellinBarnes[symbols, indicator][expr]: indicate the integration. Default[indicator]: INT.

* `#!wl hyperToMellinBarnes2` - hyperToMellinBarnes2[symbols][expr]: convert hypergeometric function to Mellin-Barnes integral in terms of (1-z). hyperToMellinBarnes2[symbols, indicator][expr]: indicate the integration. Default[indicator]: INT.

* `#!wl hyperFromAppellF1` - hyperFromAppellF1[symbols][expr]: convert Appell F1 function to hypergeometric summation. hyperFromAppellF1[symbols, indicator][expr]: indicate the summation. Default[indicator]: SUM.

* `#!wl JacobiPhiToHyper` - JacobiPhiToHyper[head][expr]: convert Jacobi Phi function to Hypergeometric2F1. Default[head]: Inactive.

* `#!wl JacobiPhiFromHyper` - JacobiPhiFromHyper[head][expr]: convert Hypergeometric2F1 to Jacobi Phi function. Default[head]: Inactive.

* `#!wl WilsonPolynomialToHyper` - WilsonPolynomialToHyper[head][expr]: convert Wilson polynomial to Hypergeometric4F3. Default[head]: Inactive.

* `#!wl WilsonPolynomialFromHyper` - WilsonPolynomialFromHyper[head][expr]: convert Hypergeometric4F3 to Wilson polynomial. Default[head]: Inactive.

* `#!wl AppellF1FromIntegral` - AppellF1FromIntegral[var, head][expr]: convert integral to Appell F1. Def[var]: integration variable to match. Default[var]: All. Default[head]: Inactive.


<!-- Label.wl -->

* `#!wl label` - label[vars, labs, head]: join the variables and labels into labeled objects using specified head. Default[head]: Function.

* `#!wl labelAt` - labelAt[vars, rules, head]: take the specific values of the labeled objects according to rules. Default[head]: Function.

* `#!wl labelConvert` - labelConvert[vars, head1->head2]: convert the labeled objects according to the two specified label heads.

* `#!wl labelJoin` - labelJoin[vars, head]: convert labeled objects from any head to Symbol. Default[head]: Function. Sketch: labelConvert with _->Symbol.

* `#!wl labelSplit` - labelSplit[vars, head]: convert labeled objects from Symbol to any head. Default[head]: Function. Sketch: labelConvert with Symbol->_.

* `#!wl labelToZero` - labelToZero[vars, labs, head]: shift to zero. Default[head]: Function. Example: x1->0.

* `#!wl labelToEqual` - labelToEqual[vars, rules, head]: shift the first to the second. Default[head]: Function. Example: x1->x2.

* `#!wl labelToDiff` - labelToDiff[vars, rules, head]: shift the first to the difference plus the second. Default[head]: Function. Example: x1->x12+x2.

* `#!wl labelToDiffZero` - labelToDiffZero[vars, rules, head]: shift the first to the difference and the second to zero. Default[head]: Function. Example: x1->x12, x2->0.

* `#!wl labelToDiffBack` - labelToDiffBack[vars, rules, head]: shift the difference back to the original two. Default[head]: Function. Example: x12->x1-x2.


<!-- Matrix.wl -->

* `#!wl matSquareQ` - matSquareQ[matrix]: test if the matrix is square.

* `#!wl matComm` - matComm[a, b]: compute the commutator of the two matrices. Sketch: a.b - b.a.

* `#!wl matJordan` - matJordan[dim, a, b]: construct a Jordan matrix of specified dimension. Def[a]: the common diagonal element. Def[b]: the common super-diagonal element. Default[b]: 1.

* `#!wl matAngularMomentum` - matAngularMomentum[j][direction]: generate angular momentum matrices for the spin-j representation. Value[direction]: {"x", "y", "z"|0, 1, -1}. Hint: the column/row indices run from j to -j.


<!-- OperatorForm.wl -->

* `#!wl SS` - Sketch: Simplify.

* `#!wl FS` - Sketch: FullSimplify.

* `#!wl FE` - Sketch: FunctionExpand.

* `#!wl FES` - Sketch: FunctionExpand + Simplify.

* `#!wl AS` - Sketch: Assuming.

* `#!wl SSA` - Sketch: Simplify + Assuming.

* `#!wl FSA` - Sketch: FullSimplify + Assuming.

* `#!wl FEA` - Sketch: FunctionExpand + Assuming.

* `#!wl FESA` - Sketch: FunctionExpand + Simplify + Assuming.

* `#!wl modularize` - modularize[scope[code, iterators]]: modularize the scoping construction (e.g. Table, Sum, and Integrate) such that the iterators are lexically scoped.

* `#!wl block` - Sketch: Block.

* `#!wl with` - Sketch: With.

* `#!wl module` - Sketch: Module.

* `#!wl rep` - rep[rules][expr]: operator form of ReplaceAll with the rules being flattened.

* `#!wl repdeep` - repdeep[rules][level][expr]: operator form of Replace with the rules being flattened. Default[level]: All.

* `#!wl part` - Sketch: Part.

* `#!wl plus` - Sketch: Plus.

* `#!wl minus` - Sketch: Minus.

* `#!wl times` - Sketch: Times.

* `#!wl divide` - Sketch: Divide.

* `#!wl series` - Sketch: Series + Normal.

* `#!wl limit` - Sketch: Limit.

* `#!wl solve` - Sketch: Solve.

* `#!wl solve1` - Sketch: Solve + First.

* `#!wl collect` - Sketch: Collect.


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

* `#!wl presentQ` - presentQ[pattern][expr]: test whether the pattern occurs in the expression. Sketch: Not + FreeQ.

* `#!wl linearQ` - linearQ[expr, var]: test whether the expression is linear in the variable and the variable is present. linearQ[expr, varList]: test linearity for all the variables.

* `#!wl syntacticNegativeQ` - syntacticNegativeQ[expr]: test whether the expression is syntactically negative.

* `#!wl patternPresentQ` - patternPresentQ[expr]: test whether any pattern construction occurs in the expression.

* `#!wl patternFreeQ` - patternFreeQ[expr]: test whether no pattern construction occurs in the expression.


<!-- Relation.wl -->

* `#!wl relationMellinBarnes` - relationMellinBarnes[(x+y)^a, x, s]: generate Mellin-Barnes integral representation for the power factor. Example: (x+y)^a -> mg*x^s*y^(a-s)*INT[s].

* `#!wl relationFeynman` - relationFeynman[x^a*y^b, x, s]: generate Feynman-Schwinger integral representation for combining the two power factors. Example: x^a*y^b -> mg*(x+s*y)^(a+b)*s^(-b-1)*INT[s].

* `#!wl relationPowerPhase` - relationPowerPhase[base, expanded, expanded2, sign]: generate transformation rule for separating the power factor. Def[base]: the power base. Def[expanded]: the numerator factors to separate. Def[expanded2]: the denominator factors to separate. This argument is optional. Def[sign]: the phase direction. Default[sign]: 1.


<!-- SimplifyUnsafe.wl -->

* `#!wl unsafePowerTogether` - unsafePowerTogether[expr]: try to combine power factors. Hint: may produce mathematically invalid result.

* `#!wl unsafePowerApart` - unsafePowerApart[expr]: try to separate power factors. Hint: may produce mathematically invalid result.

* `#!wl unsafePowerSimplify` - unsafePowerSimplify[expr]: try to simplify power factors. Hint: may produce mathematically invalid result.

* `#!wl unsafeExprTogether` - unsafeExprTogether[expr]: try to combine power factors, logarithms, and absolute values. Hint: may produce mathematically invalid result.

* `#!wl unsafeExprApart` - unsafeExprApart[expr]: try to separate power factors, logarithms, and absolute values. Hint: may produce mathematically invalid result.

* `#!wl unsafeExprSimplify` - unsafeExprSimplify[expr]: try to simplify power factors, logarithms, and absolute values. Hint: may produce mathematically invalid result.


<!-- Simplify.wl -->

* `#!wl freeze` - freeze[pattern, operation, level][expr]: freeze subexpressions matching the pattern, then perform the operation and unfreeze. freeze[pattern->transform, operation, level][expr]: additionally perform the transform to the frozen subexpressions. Value[pattern->transform]: _->Positive, _->Negative, _->{_,_}. Default[operation]: Simplify. Default[level]: Infinity.

* `#!wl freezeNegative` - freezeNegative[pattern, operation, level][expr]: variant of freeze with Negative as the default transformation.

* `#!wl focus` - focus[pattern, operation, level][expr]: apply the operation to the arguments of functions with the specified heads. Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used. Default[operation]: Simplify.

* `#!wl fracFocus` - fracFocus[operation, level][expr]: apply the operation to fractions (expressions containing negative powers). Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used. Default[operation]: Simplify.

* `#!wl fracReduce` - fracReduce[operation, factor][expr]: multiply the factor to the numerator and denominator, then apply the operation separately to them. Default[operation]: Simplify. Default[factor]: 1.

* `#!wl powerFocus` - powerFocus[operation, level][expr]: apply the operation to the base and exponent of power factors. Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used. Default[operation]: Simplify.

* `#!wl powerBaseFocus` - powerBaseFocus[operation, level][expr]: apply the operation to the base of power factors only. Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used. Default[operation]: Simplify.

* `#!wl powerExponentFocus` - powerExponentFocus[operation, level][expr]: apply the operation to the exponent of power factors only. Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used. Default[operation]: Simplify.

* `#!wl powerSeparate` - powerSeparate[baseP][expr]: separate the product expression into power factors and non-power factors. Def[baseP]: the pattern of power bases to match.

* `#!wl powerBaseTogether` - powerBaseTogether[baseP, basePreservedP][expr]: take together the bases of power factors. Def[baseP]: the pattern of power bases to combine. Def[basePreservedP]: the pattern of power bases to preserve.

* `#!wl powerExpand` - powerExpand[baseP, basePreservedP, baseExpandedP][expr]: expand the power factors after combining power bases. Def[baseP]: the pattern of power bases to combine. Def[basePreservedP]: the pattern of power bases to preserve.

* `#!wl powerExpandBy` - powerExpandBy[rules..][expr]: expand the power factors according to the rules. Def[rules]: rules of the form base->{factor1, factor2, ...}.

* `#!wl powerExponentCollect` - powerExponentCollect[exponents...][expr]: collect and combine power factors with common exponents. Hint: if no exponent is specified, try to collect all power factors.

* `#!wl trigPhaseReduce` - trigPhaseReduce[vars..][expr]: reduce phase factors in trigonometric functions using periodicity. Def[vars]: the variables to consider for periodicity.

* `#!wl deltaReduce` - deltaReduce[expr]: reduce the Dirac delta function and its derivatives in the expression.

* `#!wl swap` - swap[a, b][expr]: swap the two symbols in the expression. swap[{a, b}..][expr]: swap the pairs simultaneously.

* `#!wl separate` - separate[criterion][expr_]: separate the elements based on whether they satisfy the criterion.

* `#!wl stripPattern` - stripPattern[expr, head]: strip off pattern-related functions from the expression and wrap it with head. Default[head]: Defer.

* `#!wl vanishing` - vanishing[expr]: clean up the expression by removing redundant vanishing terms. Sketch: Simplify + Flatten + DeleteDuplicates.

* `#!wl extractSymbol` - extractSymbol[expr, exclusionList]: extract user-defined symbols from the expression. Def[exclusionList]: the contexts to exclude.

* `#!wl extractVariable` - extractVariable[expr, exclusionList]: extract user-defined variables from the expression. Def[exclusionList]: the contexts to exclude.