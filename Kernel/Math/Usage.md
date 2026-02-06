# Usage

## Deprecation.wl

* `#!wl label2` - label2[var, lab]: variant of label with Symbol as head.

* `#!wl labelRange` - labelRange[var, range, head]: join the variable(s) and labels in the range using the specified head.

    * Default[head]: Function.

    * Example: labelRange[x, 3] gives x[1], x[2], x[3].

* `#!wl labelRange2` - labelRange2[var, range]: variant of labelRange with Symbol as head.

## Diff.wl

* `#!wl PD` - PD[vars]: head of partial derivative.

* `#!wl INT` - INT[vars]: head of integration.

* `#!wl SUM` - SUM[vars]: head of summation.

* `#!wl integration` - integration[args][expr]: operator form of Integrate.

    * Default[GenerateConditions]: False.

* `#!wl summation` - summation[args][expr]: operator form of Sum.

    * Default[GenerateConditions]: False.

* `#!wl integrationChange` - integrationChange[equations, oldVars, newVars, signs][expr]: change variables in integrals.

    * Info[signs]: Jacobian signs.

* `#!wl diffChange` - diffChange[equations, oldVars, newVars, funs][expr]: change variables in differential equations.

    * Info[funs]: list of functions to transform.

* `#!wl IBP` - IBP[fun][expr]: perform integration by parts.

    * IBP[fun, vars][expr]: perform integration by parts with respect to specific variables.

* `#!wl jacobianMatrix` - jacobianMatrix[funList, varList]: Jacobian matrix.

* `#!wl jacobianDet` - jacobianDet[funList, varList]: Jacobian determinant.

* `#!wl PDCoefficient` - PDCoefficient[post, opts][expr]: extract the coefficients of PD[\_\_].

    * Info[post]: post-operation applied to the coefficients.

    * Default[post]: Identity.

* `#!wl PDCollect` - PDCollect[args][expr]: collect the terms with respect to PD[\_\_].

    * Info[args]: inherited from Collect.

* `#!wl diffCoefficient` - diffCoefficient[fun, post, opts][expr]: extract the coefficients of Derivative[\_\_][\_][\_\_].

    * Info[post]: post-operation applied to the coefficients.

    * Default[post]: Identity.

* `#!wl diffCollect` - diffCollect[fun, args][expr]: collect the terms with respect to Derivative[\_\_][\_][\_\_].

    * diffCollect[funList, args][expr]: collect terms for multiple functions.

    * Info[args]: inherited from Collect.

* `#!wl diffReplace` - diffReplace[fun->res...]: replace the derivatives of the function.

    * diffReplace[fun->res..., head]: prevent the evaluation of symbolic derivatives.

* `#!wl diffComm` - diffComm[X, Y]: compute the commutator of differential operators.

    * Sketch: sign*(X[Y[#]]-Y[X[#]])&.

* `#!wl diffSymbolicOrder` - diffSymbolicOrder[x, n][fun]: take the derivative with symbolic order.

* `#!wl INTCancel` - INTCancel[vars][expr]: cancel the possible INT head in the expression.

* `#!wl SUMCancel` - SUMCancel[vars][expr]: cancel the possible SUM head in the expression.

## DLMF.wl

* `#!wl DLMF` - DLMF[rules, opts][expr]: simplify the expression by the DLMF rules.

* `#!wl DLMFAs` - DLMFAs[rules, as][expr]: simplify the expression by the DLMF rules under the assumption.

    * Info[as]: the assumption.

* `#!wl DLMFAsTrue` - DLMFAsTrue[rules][expr]: simplify the expression by the DLMF rules ignoring all the conditions.

    * Hint: this is equivalent to DLMF with "IgnoreCondition"->True.

* `#!wl DLMFRule` - DLMFRule[rules, opts]: return the DLMF rules.

* `#!wl DLMFRuleShow` - DLMFRuleShow[rules, opts]: show the DLMF rules without context marker.

## Dye.wl

* `#!wl dye` - dye[expr]: color the subexpressions at the first level.

* `#!wl dyeIn` - dyeIn[level][expr]: color the subexpressions at the specified level.

    * dyeIn[color, level][expr]: specify the color.

* `#!wl dyeBy` - dyeBy[pattern, level, opts][expr]: color the subexpressions matching the pattern.

    * dyeBy[color, pattern, level, opts][expr]: specify the color.

    * Default[level]: {0, Infinity}, inherited from Position.

* `#!wl dyeAt` - dyeAt[positions][expr]: color the subexpressions at the specified positions.

    * dyeAt[color, positions][expr]: specify the color.

* `#!wl dyeOff` - dyeOff[expr]: remove the coloring applied by dye functions.

## Gamma.wl

* `#!wl gammaSimplify` - gammaSimplify[expr]: simplify Gamma functions in the expression.

    * Sketch: Developer\`GammaSimplify.

* `#!wl gammaShift` - gammaShift[var, shift][expr]: shift the argument of Gamma functions by the specified integer.

* `#!wl gammaFrom` - gammaFrom[expr, opts]: expand everything to Gamma functions.

* `#!wl gammaSeparate` - gammaSeparate[expr]: separate a product into Gamma functions and the rest.

* `#!wl gammaTakeResidue` - gammaTakeResidue[var, index, gamma, sign, opts][expr]: take residue of a series of poles from the Gamma factor.

    * gammaTakeResidue[var, index->n, gamma, sign, opts][expr]: specify one pole in the series.

    * Info[index]: the index of the poles.

    * Info[gamma]: the argument of the Gamma function.

    * Info[sign]: the direction of contour.

    * Value["ShowPole"]: {True, False, Full}.

    * Default[sign]: 1.

* `#!wl multiGamma` - multiGamma[num, denom]: represent a product of Gamma functions in numerator and denominator.

    * Info[num]: list of arguments for Gamma functions in the numerator.

    * Info[denom]: list of arguments for Gamma functions in the denominator.

* `#!wl multiGammaFrom` - multiGammaFrom[expr]: convert Gamma functions into multi-Gamma symbols.

* `#!wl multiGammaSimplify` - multiGammaSimplify[expr]: simplify the multi-Gamma symbol with the assumption.

    * Default[assume]: True.

* `#!wl multiGammaReduceByBarnesLemma` - multiGammaReduceByBarnesLemma[s][expr]: reduce the multi-Gamma symbol by the first and second Barnes lemmas.

    * Info[s]: the variable parameter in the Barnes lemma reduction.

## Hyper.wl

* `#!wl hyper` - hyper[type, var][expr]: head used by hypergeometric conversion functions.

* `#!wl JacobiPhi` - JacobiPhi[a, b, c, z]: Jacobi Phi function.

* `#!wl WilsonPolynomial` - WilsonPolynomial[a, b, c, d, n, x]: Wilson polynomial.

* `#!wl hyperSeparate` - hyperSeparate[expr]: separate a product into hypergeometric functions and the rest.

* `#!wl hyperUnregularize` - hyperUnregularize[expr]: convert regularized hypergeometric function to the normal one.

* `#!wl hyperRegularize` - hyperRegularize[expr]: convert hypergeometric function to the regularized one.

* `#!wl hyperToTaylor` - hyperToTaylor[symbols][expr]: convert hypergeometric function to Taylor series.

    * hyperToTaylor[symbols, indicator][expr]: indicate the summation.

    * Default[indicator]: SUM.

* `#!wl hyperToEuler` - hyperToEuler[symbols][expr]: convert hypergeometric function to Euler integral.

    * hyperToEuler[symbols, indicator][expr]: indicate the integration.

    * Default[indicator]: INT.

* `#!wl hyperToMellinBarnes` - hyperToMellinBarnes[symbols][expr]: convert hypergeometric function to Mellin-Barnes integral.

    * hyperToMellinBarnes[symbols, indicator][expr]: indicate the integration.

    * Default[indicator]: INT.

* `#!wl hyperToMellinBarnes2` - hyperToMellinBarnes2[symbols][expr]: convert hypergeometric function to Mellin-Barnes integral in terms of (1-z).

    * hyperToMellinBarnes2[symbols, indicator][expr]: indicate the integration.

    * Default[indicator]: INT.

* `#!wl hyperFromAppellF1` - hyperFromAppellF1[symbols][expr]: convert Appell F1 function to hypergeometric summation.

    * hyperFromAppellF1[symbols, indicator][expr]: indicate the summation.

    * Default[indicator]: SUM.

* `#!wl JacobiPhiToHyper` - JacobiPhiToHyper[head][expr]: convert Jacobi Phi function to Hypergeometric2F1.

    * Default[head]: Identity.

* `#!wl JacobiPhiFromHyper` - JacobiPhiFromHyper[head][expr]: convert Hypergeometric2F1 to Jacobi Phi function.

    * Default[head]: Identity.

* `#!wl WilsonPolynomialToHyper` - WilsonPolynomialToHyper[head][expr]: convert Wilson polynomial to Hypergeometric4F3.

    * Default[head]: Identity.

* `#!wl WilsonPolynomialFromHyper` - WilsonPolynomialFromHyper[head][expr]: convert Hypergeometric4F3 to Wilson polynomial.

    * Default[head]: Identity.

* `#!wl hyperFromIntegral` - hyperFromIntegral[var, head][expr]: convert integral to hypergeometric function.

    * Info[var]: integration variable to match.

    * Default[var]: All.

    * Default[head]: Identity.

* `#!wl AppellF1FromIntegral` - AppellF1FromIntegral[var, head][expr]: convert integral to Appell F1.

    * Info[var]: integration variable to match.

    * Default[var]: All.

    * Default[head]: Identity.

## Label.wl

* `#!wl label` - label[var, lab, head]: join the variable(s) and label into labeled objects using the specified head.

    * Default[head]: Function.

* `#!wl labell` - labell[var, lab]: variant of label with Symbol as head.

* `#!wl labels` - labels[var, range, head]: join the variable(s) and labels in the range using the specified head.

    * Default[head]: Function.

    * Example: labels[x, 3] gives x[1], x[2], x[3].

* `#!wl labells` - labells[var, range]: variant of labels with Symbol as head.

* `#!wl labelAt` - labelAt[var, rules, head]: take the specific values of the labeled objects according to rules.

    * Default[head]: Function.

* `#!wl labelConvert` - labelConvert[var, head1->head2]: convert the labeled objects according to the two specified label heads.

* `#!wl labelJoin` - labelJoin[var, head]: convert labeled objects from any head to Symbol.

    * Default[head]: Function.

    * Sketch: labelConvert with \_->Symbol.

* `#!wl labelSplit` - labelSplit[var, head]: convert labeled objects from Symbol to any head.

    * Default[head]: Function.

    * Sketch: labelConvert with Symbol->\_.

* `#!wl labelToZero` - labelToZero[var, labs, head]: shift to zero.

    * Default[head]: Function.

    * Example: x1->0.

* `#!wl labelToEqual` - labelToEqual[var, rules, head]: shift the first to the second.

    * Default[head]: Function.

    * Example: x1->x2.

* `#!wl labelToDiff` - labelToDiff[var, rules, head]: shift the first to the difference plus the second.

    * Default[head]: Function.

    * Example: x1->x12+x2.

* `#!wl labelToDiffZero` - labelToDiffZero[var, rules, head]: shift the first to the difference and the second to zero.

    * Default[head]: Function.

    * Example: x1->x12, x2->0.

* `#!wl labelToDiffBack` - labelToDiffBack[var, rules, head]: shift the difference back to the original two.

    * Default[head]: Function.

    * Example: x12->x1-x2.

## Matrix.wl

* `#!wl matSquareQ` - matSquareQ[matrix]: test if the matrix is square.

* `#!wl matComm` - matComm[a, b]: compute the commutator of the two matrices.

    * Sketch: a.b - b.a.

* `#!wl matJordan` - matJordan[dim, a, b]: construct a Jordan matrix of specified dimension.

    * Info[a]: the common diagonal element.

    * Info[b]: the common super-diagonal element.

    * Default[b]: 1.

* `#!wl matAngularMomentum` - matAngularMomentum[j][direction]: generate angular momentum matrices for the spin-j representation.

    * Value[direction]: {"x", "y", "z"|0, 1, -1}.

    * Hint: the column/row indices run from j to -j.

## OperatorForm.wl

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

* `#!wl repdeep` - repdeep[rules, level][expr]: operator form of Replace with the rules being flattened.

    * Default[level]: All.

* `#!wl repcheck` - repcheck[rules, sametest][expr]: operator form of ReplaceAll with simple rules being checked.

* `#!wl replimit` - replimit[rules][expr]: operator form of ReplaceAll with limit being tried for simple rules.

* `#!wl part` - Sketch: Part.

* `#!wl plus` - Sketch: Plus.

* `#!wl minus` - Sketch: Subtract.

* `#!wl times` - Sketch: Times.

* `#!wl divide` - Sketch: Divide.

* `#!wl timesOverPlus` - timesOverPlus[args][expr]: operator form of Times that automatically threads over Plus.

* `#!wl divideOverPlus` - divideOverPlus[args][expr]: operator form of Divide that automatically threads over Plus.

* `#!wl series` - Sketch: Series + Normal.

* `#!wl seriesc` - Sketch: SeriesCoefficient.

* `#!wl limit` - Sketch: Limit.

* `#!wl solve` - Sketch: Solve + Part.

* `#!wl solve1` - Sketch: Solve + First.

* `#!wl collect` - Sketch: Collect.

## Quest.wl

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

* `#!wl levelQ` - levelQ[level]: test whether the argument is a valid level specification.

    * Hint: All==={0,Infinity}, Infinity==={1,Infinity}, n\_Integer==={1,n}, {\_Integer}, {\_Integer,\_Integer}.

* `#!wl presentQ` - presentQ[pattern][expr]: test whether the pattern occurs in the expression.

    * Sketch: Not + FreeQ.

* `#!wl linearQ` - linearQ[expr, var]: test whether the expression is linear in the variable and the variable is present.

    * linearQ[expr, varList]: test linearity for all the variables.

* `#!wl plusQ` - plusQ[expr]: test whether the expression is syntactically positive.

* `#!wl minusQ` - minusQ[expr]: test whether the expression is syntactically negative.

* `#!wl patternPresentQ` - patternPresentQ[expr]: test whether any pattern construction occurs in the expression.

* `#!wl patternFreeQ` - patternFreeQ[expr]: test whether no pattern construction occurs in the expression.

## Random.wl

* `#!wl randomize` - randomize[domain, range][expr]: randomize the expression by replacing variables with random numbers.

    * Example: randomize[][x+y] -> x1+y1, where x1 and y1 are random numbers.

## Relation.wl

* `#!wl relationMellinBarnes` - relationMellinBarnes[(x+y)^a, x, s]: generate Mellin-Barnes integral representation for the power factor.

    * Example: (x+y)^a -> mg*x^s*y^(a-s)*INT[s].

* `#!wl relationFeynman` - relationFeynman[x^a*y^b, x, s]: generate Feynman-Schwinger integral representation for combining the two power factors.

    * Example: x^a*y^b -> mg*(x+s*y)^(a+b)*s^(-b-1)*INT[s].

## SimplifyUnsafe.wl

* `#!wl unsafePowerTogether` - unsafePowerTogether[expr]: try to combine power factors.

    * Hint: may produce mathematically invalid result.

* `#!wl unsafePowerApart` - unsafePowerApart[expr]: try to separate power factors.

    * Hint: may produce mathematically invalid result.

* `#!wl unsafePowerSimplify` - unsafePowerSimplify[expr]: try to simplify power factors.

    * Hint: may produce mathematically invalid result.

* `#!wl unsafeExprTogether` - unsafeExprTogether[expr]: try to combine power factors, logarithms, and absolute values.

    * Hint: may produce mathematically invalid result.

* `#!wl unsafeExprApart` - unsafeExprApart[expr]: try to separate power factors, logarithms, and absolute values.

    * Hint: may produce mathematically invalid result.

* `#!wl unsafeExprSimplify` - unsafeExprSimplify[expr]: try to simplify power factors, logarithms, and absolute values.

    * Hint: may produce mathematically invalid result.

## Simplify.wl

* `#!wl freeze` - freeze[pattern, operation, level][expr]: freeze subexpressions matching the pattern, then perform the operation and unfreeze.

    * freeze[pattern->transform, operation, level][expr]: additionally perform the transform to the frozen subexpressions.

    * Value[pattern->transform]: \_->Positive, \_->Negative, \_->{\_,\_}.

    * Default[operation]: Simplify.

    * Default[level]: Infinity.

* `#!wl freezeNegative` - freezeNegative[pattern, operation, level][expr]: variant of freeze with Negative as the default transformation.

* `#!wl focus` - focus[pattern, operation, level][expr]: apply the operation to the arguments of functions with the specified heads.

    * Info[level]: ReplaceAll/Replace is used without/with specified level.

    * Default[operation]: Simplify.

* `#!wl fracFocus` - fracFocus[operation, level][expr]: apply the operation to fractions (expressions containing negative powers).

    * Info[level]: ReplaceAll/Replace is used without/with specified level.

    * Default[operation]: Simplify.

* `#!wl fracReduce` - fracReduce[operation, factor][expr]: multiply the factor to the numerator and denominator, then apply the operation separately to them.

    * Default[operation]: Simplify.

    * Default[factor]: 1.

* `#!wl powerFocus` - powerFocus[operation, level][expr]: apply the operation to the base and exponent of power factors.

    * Info[level]: ReplaceAll/Replace is used without/with specified level.

    * Default[operation]: Simplify.

* `#!wl powerBaseFocus` - powerBaseFocus[operation, level][expr]: apply the operation to the base of power factors only.

    * Info[level]: ReplaceAll/Replace is used without/with specified level.

    * Default[operation]: Simplify.

* `#!wl powerExponentFocus` - powerExponentFocus[operation, level][expr]: apply the operation to the exponent of power factors only.

    * Info[level]: ReplaceAll/Replace is used without/with specified level.

    * Default[operation]: Simplify.

* `#!wl powerBaseTogether` - powerBaseTogether[operation, level][expr]: take together the bases of power factors and then apply the operation to the combined base.

    * Info[level]: ReplaceAll/Replace is used without/with specified level.

    * Default[operation]: Simplify.

* `#!wl powerExpand` - powerExpand[operation, level][expr]: expand the power factors after combining power bases.

    * Info[level]: ReplaceAll/Replace is used without/with specified level.

    * Default[operation]: Simplify.

* `#!wl powerExpandBy` - powerExpandBy[rules..][expr]: expand the power factors according to the rules.

    * Info[rules]: base->{factor1, factor2, ..., tag}.

    * Info[tag:Positive|Negative]: specify the phase direction.

    * Info[tag:Optional]: match powers with exponent 1.

* `#!wl powerExpandPhase` - phaseFrom[{ε, sign}, assumption]: expand the phase factor from iε prescription of power factors.

    * Info[sign]: sign indicator.

    * Info[assumption]: assumption for simplifying the phase.

* `#!wl powerSeparate` - powerSeparate[baseP][expr]: separate the product expression into power factors and non-power factors.

    * Info[baseP]: pattern of power bases to match.

* `#!wl powerExponentCollect` - powerExponentCollect[exponents..., Inactive][expr]: collect and combine power factors with common exponents.

    * Info[Inactive]: avoid auto-expansion of power factors with integer base.

    * Hint: if no exponent is specified, try to collect all power factors.

* `#!wl phase` - phase[expr]: phase factor.

* `#!wl phaseIgnore` - phaseIgnore[expr]: ignore the phase factor in the product.

* `#!wl togetherBy` - togetherBy[base][expr]: take together the terms with the specified base pattern in the polynomial expression.

    * Info[base]: the pattern of base to match.

* `#!wl trigPhaseReduce` - trigPhaseReduce[vars..][expr]: reduce phase factors in trigonometric/exponential functions using periodicity.

    * Info[vars]: integer variables.

* `#!wl trigFromExp` - trigFromExp[expr]: variant of ExpToTrig that only affects Exp.

* `#!wl deltaReduce` - deltaReduce[expr]: reduce the Dirac delta function and its derivatives in the expression.

* `#!wl swap` - swap[a, b][expr]: swap the two symbols in the expression.

    * swap[{a, b}..][expr]: swap the pairs simultaneously.

* `#!wl separate` - separate[crit][expr\_]: separate the elements based on whether they satisfy the crit.

* `#!wl separateLongest` - separateLongest[crit][expr\_]: separate the longest term in the sum/product.

* `#!wl stripPattern` - stripPattern[expr, head]: strip off pattern-related functions from the expression and wrap it with head.

    * Default[head]: Identity.

* `#!wl vanishing` - vanishing[expr]: clean up the expression by removing redundant vanishing terms.

    * Sketch: Simplify + Flatten + DeleteDuplicates.

* `#!wl extractSymbol` - extractSymbol[expr, exclusionList]: extract user-defined symbols from the expression.

    * Info[exclusionList]: the contexts to exclude.

* `#!wl extractVariable` - extractVariable[expr, exclusionList]: extract user-defined variables from the expression.

    * Info[exclusionList]: the contexts to exclude.