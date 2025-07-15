(* Deprecation.wl *)

relationPowerMono::usage =
    "relation for branch cut of Power at zero.";

collectDerivative::usage =
    "collect by derivatives.";

powerExpandFactor::usage =
    "factor the base of powers and then expand.";


(* Diff.wl *)

PD::usage =
    "PD[vars]: head of partial derivative.";

INT::usage =
    "INT[vars]: head of integration.";

SUM::usage =
    "SUM[vars]: head of summation.";

integrate::usage =
    StringJoin["integrate[args][expr]: operator form of Integrate.", "\n", "Default[GenerateConditions]: False."];

summation::usage =
    StringJoin["summation[args][expr]: operator form of Sum.", "\n", "Default[GenerateConditions]: False."];

integrateChange::usage =
    StringJoin["integrateChange[equations, oldVars, newVars, signs][expr]: change variables in integrals.", "\n", "Info[signs]: Jacobian signs.", "\n", "Default[\"Solution\"]: 1.", "\n", "Default[\"ShowSolution\"]: False.", "\n", "Default[\"ShowJacobian\"]: False."];

diffChange::usage =
    StringJoin["diffChange[equations, oldVars, newVars, funs][expr]: change variables in differential equations.", "\n", "Info[funs]: list of functions to transform.", "\n", "Default[\"Solution\"]: 1.", "\n", "Default[\"ShowSolution\"]: False."];

IBP::usage =
    StringJoin["IBP[fun][expr]: perform integration by parts.", "\n", "IBP[fun, vars][expr]: perform integration by parts with respect to specific variables."];

jacobianMatrix::usage =
    "jacobianMatrix[funList, varList]: Jacobian matrix.";

jacobianDet::usage =
    "jacobianDet[funList, varList]: Jacobian determinant.";

PDCoefficient::usage =
    StringJoin["PDCoefficient[post, opts][expr]: extract the coefficients of PD[__].", "\n", "Info[post]: post-operation applied to the coefficients.", "\n", "Default[post]: Identity.", "\n", "Default[\"CheckLinearity\"]: True."];

PDCollect::usage =
    StringJoin["PDCollect[args][expr]: collect the terms with respect to PD[__].", "\n", "Info[args]: inherited from Collect."];

diffCoefficient::usage =
    StringJoin["diffCoefficient[fun, post, opts][expr]: extract the coefficients of Derivative[__][_][__].", "\n", "Info[fun]: the head of the function.", "\n", "Info[post]: post-operation applied to the coefficients.", "\n", "Default[post]: Identity.", "\n", "Default[\"CheckLinearity\"]: True."];

diffCollect::usage =
    StringJoin["diffCollect[fun, args][expr]: collect the terms with respect to Derivative[__][_][__].", "\n", "diffCollect[funList, args][expr]: collect terms for multiple functions.", "\n", "Info[fun]: the head of the function.", "\n", "Info[args]: inherited from Collect."];

diffReplace::usage =
    StringJoin["diffReplace[fun->res...]: replace the derivatives of the function.", "\n", "diffReplace[fun->res..., head]: prevent the evaluation of symbolic derivatives."];

diffComm::usage =
    StringJoin["diffComm[X, Y]: compute the commutator of differential operators.", "\n", "Sketch: -(X[Y[#]]-Y[X[#]])&."];


(* DLMF.wl *)

DLMF::usage =
    StringJoin["DLMF[rules, opts][expr]: simplify the expression by the DLMF rules.", "\n", "Default[\"IgnoreCondition\"]: False."];

DLMFAs::usage =
    StringJoin["DLMFAs[rules, as][expr]: simplify the expression by the DLMF rules under the assumption.", "\n", "Info[as]: the assumption."];

DLMFAsTrue::usage =
    StringJoin["DLMFAsTrue[rules][expr]: simplify the expression by the DLMF rules ignoring all the conditions.", "\n", "Hint: this is equivalent to DLMF with \"IgnoreCondition\"->True."];

DLMFRule::usage =
    StringJoin["DLMFRule[rules, opts]: return the DLMF rules.", "\n", "Default[\"IgnoreCondition\"]: False."];

DLMFRuleShow::usage =
    StringJoin["DLMFRuleShow[rules, opts]: show the DLMF rules without context marker.", "\n", "Default[\"IgnoreCondition\"]: False."];


(* Dye.wl *)

dye::usage =
    "dye[expr]: color the subexpressions at the first level.";

dyeIn::usage =
    StringJoin["dyeIn[level][expr]: color the subexpressions at the specified level.", "\n", "dyeIn[color, level][expr]: specify the color."];

dyeBy::usage =
    StringJoin["dyeBy[pattern, level, opts][expr]: color the subexpressions matching the pattern.", "\n", "dyeBy[color, pattern, level, opts][expr]: specify the color.", "\n", "Default[level]: {0, Infinity}, inherited from Position."];

dyeAt::usage =
    StringJoin["dyeAt[positions][expr]: color the subexpressions at the specified positions.", "\n", "dyeAt[color, positions][expr]: specify the color."];

dyeOff::usage =
    "dyeOff[expr]: remove the coloring applied by dye functions.";


(* Gamma.wl *)

gammaSimplify::usage =
    StringJoin["gammaSimplify[expr]: simplify Gamma functions in the expression.", "\n", "Sketch: Developer`GammaSimplify."];

gammaFrom::usage =
    StringJoin["gammaFrom[expr, opts]: expand everything to Gamma functions.", "\n", "Default[\"Transformation\"]: Automatic.", "\n", "Default[\"ActivateGamma\"]: True."];

gammaSeparate::usage =
    "gammaSeparate[expr]: separate a product into Gamma functions and the rest.";

gammaTakeResidue::usage =
    StringJoin["gammaTakeResidue[variable, index, gamma, sign, opts][expr]: take residue of a series of poles from the Gamma factor.", "\n", "gammaTakeResidue[variable, index->n, gamma, sign, opts][expr]: specify one pole in the series.", "\n", "Info[index]: the index of the poles.", "\n", "Info[gamma]: the argument of the Gamma function.", "\n", "Info[sign]: the direction of contour.", "\n", "Value[sign]: {1, -1, Left, Right}.", "\n", "Default[sign]: 1.", "\n", "Default[\"SimplePole\"]: True.", "\n", "Default[\"ShowPole\"]: True."];

multiGamma::usage =
    StringJoin["multiGamma[num, denom]: represent a product of Gamma functions in numerator and denominator.", "\n", "Info[num]: list of arguments for Gamma functions in the numerator.", "\n", "Info[denom]: list of arguments for Gamma functions in the denominator."];

multiGammaFrom::usage =
    "multiGammaFrom[expr]: convert Gamma functions into multi-Gamma symbols.";

multiGammaSimplify::usage =
    StringJoin["multiGammaSimplify[expr]: simplify the multi-Gamma symbol with the assumption.", "\n", "Default[assume]: True."];

multiGammaReduceByBarnesLemma::usage =
    StringJoin["multiGammaReduceByBarnesLemma[s][expr]: reduce the multi-Gamma symbol by the first and second Barnes lemmas.", "\n", "Info[s]: the variable parameter in the Barnes lemma reduction."];


(* Hyper.wl *)

hyper::usage =
    "hyper[type, var][expr]: head used by hypergeometric conversion functions.";

JacobiPhi::usage =
    "JacobiPhi[a, b, c, z]: Jacobi Phi function.";

WilsonPolynomial::usage =
    "WilsonPolynomial[a, b, c, d, n, x]: Wilson polynomial.";

hyperSeparate::usage =
    "hyperSeparate[expr]: separate a product into hypergeometric functions and the rest.";

hyperUnregularize::usage =
    "hyperUnregularize[expr]: convert regularized hypergeometric function to the normal one.";

hyperRegularize::usage =
    "hyperRegularize[expr]: convert hypergeometric function to the regularized one.";

hyperToTaylor::usage =
    StringJoin["hyperToTaylor[symbols][expr]: convert hypergeometric function to Taylor series.", "\n", "hyperToTaylor[symbols, indicator][expr]: indicate the summation.", "\n", "Default[indicator]: SUM."];

hyperToMellinBarnes::usage =
    StringJoin["hyperToMellinBarnes[symbols][expr]: convert hypergeometric function to Mellin-Barnes integral.", "\n", "hyperToMellinBarnes[symbols, indicator][expr]: indicate the integration.", "\n", "Default[indicator]: INT."];

hyperToMellinBarnes2::usage =
    StringJoin["hyperToMellinBarnes2[symbols][expr]: convert hypergeometric function to Mellin-Barnes integral in terms of (1-z).", "\n", "hyperToMellinBarnes2[symbols, indicator][expr]: indicate the integration.", "\n", "Default[indicator]: INT."];

hyperFromAppellF1::usage =
    StringJoin["hyperFromAppellF1[symbols][expr]: convert Appell F1 function to hypergeometric summation.", "\n", "hyperFromAppellF1[symbols, indicator][expr]: indicate the summation.", "\n", "Default[indicator]: SUM."];

JacobiPhiToHyper::usage =
    StringJoin["JacobiPhiToHyper[head][expr]: convert Jacobi Phi function to Hypergeometric2F1.", "\n", "Default[head]: Identity."];

JacobiPhiFromHyper::usage =
    StringJoin["JacobiPhiFromHyper[head][expr]: convert Hypergeometric2F1 to Jacobi Phi function.", "\n", "Default[head]: Identity."];

WilsonPolynomialToHyper::usage =
    StringJoin["WilsonPolynomialToHyper[head][expr]: convert Wilson polynomial to Hypergeometric4F3.", "\n", "Default[head]: Identity."];

WilsonPolynomialFromHyper::usage =
    StringJoin["WilsonPolynomialFromHyper[head][expr]: convert Hypergeometric4F3 to Wilson polynomial.", "\n", "Default[head]: Identity."];

AppellF1FromIntegral::usage =
    StringJoin["AppellF1FromIntegral[var, head][expr]: convert integral to Appell F1.", "\n", "Info[var]: integration variable to match.", "\n", "Default[var]: All.", "\n", "Default[head]: Identity."];


(* Label.wl *)

label::usage =
    StringJoin["label[vars, labs, head]: join the variables and labels into labeled objects using specified head.", "\n", "Default[head]: Function."];

labelAt::usage =
    StringJoin["labelAt[vars, rules, head]: take the specific values of the labeled objects according to rules.", "\n", "Default[head]: Function."];

labelConvert::usage =
    "labelConvert[vars, head1->head2]: convert the labeled objects according to the two specified label heads.";

labelJoin::usage =
    StringJoin["labelJoin[vars, head]: convert labeled objects from any head to Symbol.", "\n", "Default[head]: Function.", "\n", "Sketch: labelConvert with _->Symbol."];

labelSplit::usage =
    StringJoin["labelSplit[vars, head]: convert labeled objects from Symbol to any head.", "\n", "Default[head]: Function.", "\n", "Sketch: labelConvert with Symbol->_."];

labelToZero::usage =
    StringJoin["labelToZero[vars, labs, head]: shift to zero.", "\n", "Default[head]: Function.", "\n", "Example: x1->0."];

labelToEqual::usage =
    StringJoin["labelToEqual[vars, rules, head]: shift the first to the second.", "\n", "Default[head]: Function.", "\n", "Example: x1->x2."];

labelToDiff::usage =
    StringJoin["labelToDiff[vars, rules, head]: shift the first to the difference plus the second.", "\n", "Default[head]: Function.", "\n", "Example: x1->x12+x2."];

labelToDiffZero::usage =
    StringJoin["labelToDiffZero[vars, rules, head]: shift the first to the difference and the second to zero.", "\n", "Default[head]: Function.", "\n", "Example: x1->x12, x2->0."];

labelToDiffBack::usage =
    StringJoin["labelToDiffBack[vars, rules, head]: shift the difference back to the original two.", "\n", "Default[head]: Function.", "\n", "Example: x12->x1-x2."];


(* Matrix.wl *)

matSquareQ::usage =
    "matSquareQ[matrix]: test if the matrix is square.";

matComm::usage =
    StringJoin["matComm[a, b]: compute the commutator of the two matrices.", "\n", "Sketch: a.b - b.a."];

matJordan::usage =
    StringJoin["matJordan[dim, a, b]: construct a Jordan matrix of specified dimension.", "\n", "Info[a]: the common diagonal element.", "\n", "Info[b]: the common super-diagonal element.", "\n", "Default[b]: 1."];

matAngularMomentum::usage =
    StringJoin["matAngularMomentum[j][direction]: generate angular momentum matrices for the spin-j representation.", "\n", "Value[direction]: {\"x\", \"y\", \"z\"|0, 1, -1}.", "\n", "Hint: the column/row indices run from j to -j."];


(* OperatorForm.wl *)

SS::usage =
    "Sketch: Simplify.";

FS::usage =
    "Sketch: FullSimplify.";

FE::usage =
    "Sketch: FunctionExpand.";

FES::usage =
    "Sketch: FunctionExpand + Simplify.";

AS::usage =
    "Sketch: Assuming.";

SSA::usage =
    "Sketch: Simplify + Assuming.";

FSA::usage =
    "Sketch: FullSimplify + Assuming.";

FEA::usage =
    "Sketch: FunctionExpand + Assuming.";

FESA::usage =
    "Sketch: FunctionExpand + Simplify + Assuming.";

modularize::usage =
    "modularize[scope[code, iterators]]: modularize the scoping construction (e.g. Table, Sum, and Integrate) such that the iterators are lexically scoped.";

block::usage =
    "Sketch: Block.";

with::usage =
    "Sketch: With.";

module::usage =
    "Sketch: Module.";

rep::usage =
    "rep[rules][expr]: operator form of ReplaceAll with the rules being flattened.";

repdeep::usage =
    StringJoin["repdeep[rules][level][expr]: operator form of Replace with the rules being flattened.", "\n", "Default[level]: All."];

part::usage =
    "Sketch: Part.";

plus::usage =
    "Sketch: Plus.";

minus::usage =
    "Sketch: Minus.";

times::usage =
    "Sketch: Times.";

divide::usage =
    "Sketch: Divide.";

series::usage =
    "Sketch: Series + Normal.";

limit::usage =
    "Sketch: Limit.";

solve::usage =
    "Sketch: Solve + Part.";

solve1::usage =
    "Sketch: Solve + First.";

collect::usage =
    "Sketch: Collect.";


(* Quest.wl *)

isN::usage =
    "isN[x..]: test whether the arguments are natural numbers.";

isZ::usage =
    "isZ[x..]: test whether the arguments are integers.";

isZP::usage =
    "isZP[x..]: test whether the arguments are positive integers.";

isZN::usage =
    "isZN[x..]: test whether the arguments are negative integers.";

isZP0::usage =
    "isZP0[x..]: test whether the arguments are zero or positive integers.";

isZN0::usage =
    "isZN0[x..]: test whether the arguments are zero or negative integers.";

isQ::usage =
    "isQ[x..]: test whether the arguments are rationals.";

isR::usage =
    "isR[x..]: test whether the arguments are reals.";

isRP::usage =
    "isRP[x..]: test whether the arguments are positive reals.";

isRN::usage =
    "isRN[x..]: test whether the arguments are negative reals.";

isRP0::usage =
    "isRP0[x..]: test whether the arguments are zero or positive reals.";

isRN0::usage =
    "isRN0[x..]: test whether the arguments are zero or negative reals.";

isC::usage =
    "isC[x..]: test whether the arguments are complex numbers.";

levelQ::usage =
    StringJoin["levelQ[level]: test whether the argument is a valid level specification.", "\n", "Hint: All==={0,Infinity}, Infinity==={1,Infinity}, n_Integer==={1,n}, {_Integer}, {_Integer,_Integer}."];

presentQ::usage =
    StringJoin["presentQ[pattern][expr]: test whether the pattern occurs in the expression.", "\n", "Sketch: Not + FreeQ."];

linearQ::usage =
    StringJoin["linearQ[expr, var]: test whether the expression is linear in the variable and the variable is present.", "\n", "linearQ[expr, varList]: test linearity for all the variables."];

syntacticNegativeQ::usage =
    "syntacticNegativeQ[expr]: test whether the expression is syntactically negative.";

patternPresentQ::usage =
    "patternPresentQ[expr]: test whether any pattern construction occurs in the expression.";

patternFreeQ::usage =
    "patternFreeQ[expr]: test whether no pattern construction occurs in the expression.";


(* Random.wl *)

randomize::usage =
    StringJoin["randomize[domain, range, excludedList][expr]: randomize the expression by replacing variables with random numbers.", "\n", "Example: randomize[x+y] -> x1+y1, where x1 and y1 are random numbers."];


(* Relation.wl *)

relationMellinBarnes::usage =
    StringJoin["relationMellinBarnes[(x+y)^a, x, s]: generate Mellin-Barnes integral representation for the power factor.", "\n", "Example: (x+y)^a -> mg*x^s*y^(a-s)*INT[s]."];

relationFeynman::usage =
    StringJoin["relationFeynman[x^a*y^b, x, s]: generate Feynman-Schwinger integral representation for combining the two power factors.", "\n", "Example: x^a*y^b -> mg*(x+s*y)^(a+b)*s^(-b-1)*INT[s]."];

relationPowerPhase::usage =
    StringJoin["relationPowerPhase[base, expanded, expanded2, sign]: generate transformation rule for separating the power factor.", "\n", "Info[base]: the power base.", "\n", "Info[expanded]: the numerator factors to separate.", "\n", "Info[expanded2]: the denominator factors to separate. This argument is optional.", "\n", "Info[sign]: the phase direction.", "\n", "Default[sign]: 1."];


(* SimplifyUnsafe.wl *)

unsafePowerTogether::usage =
    StringJoin["unsafePowerTogether[expr]: try to combine power factors.", "\n", "Hint: may produce mathematically invalid result."];

unsafePowerApart::usage =
    StringJoin["unsafePowerApart[expr]: try to separate power factors.", "\n", "Hint: may produce mathematically invalid result."];

unsafePowerSimplify::usage =
    StringJoin["unsafePowerSimplify[expr]: try to simplify power factors.", "\n", "Hint: may produce mathematically invalid result."];

unsafeExprTogether::usage =
    StringJoin["unsafeExprTogether[expr]: try to combine power factors, logarithms, and absolute values.", "\n", "Hint: may produce mathematically invalid result."];

unsafeExprApart::usage =
    StringJoin["unsafeExprApart[expr]: try to separate power factors, logarithms, and absolute values.", "\n", "Hint: may produce mathematically invalid result."];

unsafeExprSimplify::usage =
    StringJoin["unsafeExprSimplify[expr]: try to simplify power factors, logarithms, and absolute values.", "\n", "Hint: may produce mathematically invalid result."];


(* Simplify.wl *)

freeze::usage =
    StringJoin["freeze[pattern, operation, level][expr]: freeze subexpressions matching the pattern, then perform the operation and unfreeze.", "\n", "freeze[pattern->transform, operation, level][expr]: additionally perform the transform to the frozen subexpressions.", "\n", "Value[pattern->transform]: _->Positive, _->Negative, _->{_,_}.", "\n", "Default[operation]: Simplify.", "\n", "Default[level]: Infinity."];

freezeNegative::usage =
    "freezeNegative[pattern, operation, level][expr]: variant of freeze with Negative as the default transformation.";

focus::usage =
    StringJoin["focus[pattern, operation, level][expr]: apply the operation to the arguments of functions with the specified heads.", "\n", "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

fracFocus::usage =
    StringJoin["fracFocus[operation, level][expr]: apply the operation to fractions (expressions containing negative powers).", "\n", "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

fracReduce::usage =
    StringJoin["fracReduce[operation, factor][expr]: multiply the factor to the numerator and denominator, then apply the operation separately to them.", "\n", "Default[operation]: Simplify.", "\n", "Default[factor]: 1."];

powerFocus::usage =
    StringJoin["powerFocus[operation, level][expr]: apply the operation to the base and exponent of power factors.", "\n", "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

powerBaseFocus::usage =
    StringJoin["powerBaseFocus[operation, level][expr]: apply the operation to the base of power factors only.", "\n", "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

powerExponentFocus::usage =
    StringJoin["powerExponentFocus[operation, level][expr]: apply the operation to the exponent of power factors only.", "\n", "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

powerBaseTogether::usage =
    StringJoin["powerBaseTogether[operation, level][expr]: take together the bases of power factors and then apply the operation to the combined base.", "\n", "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

powerExpand::usage =
    StringJoin["powerExpand[operation, level][expr]: expand the power factors after combining power bases.", "\n", "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

powerExpandBy::usage =
    StringJoin["powerExpandBy[rules..][expr]: expand the power factors according to the rules.", "\n", "Info[rules]: rules of the form base->{factor1, factor2, ...}."];

powerSeparate::usage =
    StringJoin["powerSeparate[baseP][expr]: separate the product expression into power factors and non-power factors.", "\n", "Info[baseP]: the pattern of power bases to match."];

powerExponentCollect::usage =
    StringJoin["powerExponentCollect[exponents...][expr]: collect and combine power factors with common exponents.", "\n", "Hint: if no exponent is specified, try to collect all power factors."];

trigPhaseReduce::usage =
    StringJoin["trigPhaseReduce[vars..][expr]: reduce phase factors in trigonometric functions using periodicity.", "\n", "Info[vars]: the variables to consider for periodicity."];

deltaReduce::usage =
    "deltaReduce[expr]: reduce the Dirac delta function and its derivatives in the expression.";

swap::usage =
    StringJoin["swap[a, b][expr]: swap the two symbols in the expression.", "\n", "swap[{a, b}..][expr]: swap the pairs simultaneously."];

separate::usage =
    "separate[criterion][expr_]: separate the elements based on whether they satisfy the criterion.";

stripPattern::usage =
    StringJoin["stripPattern[expr, head]: strip off pattern-related functions from the expression and wrap it with head.", "\n", "Default[head]: Defer."];

vanishing::usage =
    StringJoin["vanishing[expr]: clean up the expression by removing redundant vanishing terms.", "\n", "Sketch: Simplify + Flatten + DeleteDuplicates."];

extractSymbol::usage =
    StringJoin["extractSymbol[expr, exclusionList]: extract user-defined symbols from the expression.", "\n", "Info[exclusionList]: the contexts to exclude."];

extractVariable::usage =
    StringJoin["extractVariable[expr, exclusionList]: extract user-defined variables from the expression.", "\n", "Info[exclusionList]: the contexts to exclude."];