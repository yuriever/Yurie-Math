(* Deprecation.wl *)

relationPowerMono::usage =
    "relation for branch cut of Power at zero.";

collectDerivative::usage =
    "collect by derivatives.";

powerExpandFactor::usage =
    "factor the base of powers and then expand.";


(* Diff.wl *)

PD::usage =
    "head of partial derivative.";

INT::usage =
    "head of integral.";

SUM::usage =
    "head of sum.";

integrate::usage =
    "operator form of Integrate.";

summation::usage =
    "operator form of Sum.";

diffChange::usage =
    "change variables in differential equations.";

integrateChange::usage =
    "change variables in integrals.";

IBP::usage =
    "perform integration by parts.";

jacobianMatrix::usage =
    "Jacobian matrix.";

jacobianDet::usage =
    "Jacobian determinant.";

PDCoefficient::usage =
    "extract the coefficients of PD[__].";

PDCollect::usage =
    "collect the terms with respect to PD[__].";

diffCoefficient::usage =
    "extract the coefficients of Derivative[__][_][__].";

diffCollect::usage =
    "collect the terms with respect to Derivative[__][_][__].";

diffReplace::usage =
    "replace the derivatives of the function.";

diffComm::usage =
    "diffComm[X,Y]=-(X[Y[#]]-Y[X[#]])&.";


(* DLMF.wl *)

DLMF::usage =
    "simplify expressions by the rules in DLMFData.";

DLMFAs::usage =
    "simplify expressions by the rules in DLMFData with the specified conditions.";

DLMFAsTrue::usage =
    "simplify expressions by the rules in DLMFData ignoring the conditions.";

DLMFRule::usage =
    "return the rules in DLMFData.";

DLMFRuleShow::usage =
    "show the rules without context marks in DLMFData.";


(* Dye.wl *)

dye::usage =
    "dye[expr_]: color the elements at the first level of expression.";

dyeIn::usage =
    "dyeIn[levelspec_:1][expr_]: color the elements at the specific levels of expression.";

dyeBy::usage =
    "dyeBy[pattern_,levelspec_,n_][expr_]: color the occurrences of pattern in expression.";

dyeAt::usage =
    "dyeAt[positions_][expr_]: color the expression at the specified positions in expression.";

dyeOff::usage =
    "dyeOff[expr_]: eliminate the colors from dye.";


(* Gamma.wl *)

gammaSimplify::usage =
    StringJoin["simplify Gamma factors in the expression.", "\n", "Developer`GammaSimplify"];

gammaFrom::usage =
    "expand everything to Gamma factors.";

gammaSeparate::usage =
    "split a product into Gamma factors and the rests.";

gammaTakeResidue::usage =
    "take residue of Gamma factors.";

multiGamma::usage =
    "head of multi-Gamma symbol.";

multiGammaFrom::usage =
    "collect Gamma factors into multi-Gamma symbols.";

multiGammaSimplify::usage =
    "simplify the multi-Gamma symbol.";

multiGammaReduceByBarnesLemma::usage =
    "reduce the multi-Gamma symbol by the Barnes lemmas.";


(* Hyper.wl *)

hyper::usage =
    "head used by hyperConvert.";

JacobiPhi::usage =
    "Jacobi Phi function, JacobiPhi[a,b,c,z].";

WilsonPolynomial::usage =
    "Wilson polynomial, WilsonPolynomial[a,b,c,d,n,x].";

hyperSeparate::usage =
    "split a product into hypergeometric functions and the rests.";

hyperUnregularize::usage =
    "convert regularized hypergeometric function to the normal one.";

hyperRegularize::usage =
    "convert hypergeometric function to the regularized one.";

hyperToTaylor::usage =
    "convert hypergeometric function to Taylor series.";

hyperToMellinBarnes::usage =
    "convert hypergeometric function to Mellin-Barnes integral.";

hyperToMellinBarnes2::usage =
    "convert hypergeometric function to Mellin-Barnes integral in terms of (1-z).";

hyperFromAppellF1::usage =
    "convert Appell F1 function to hypergeometric summation.";

JacobiPhiToHyper::usage =
    "convert Jacobi Phi to Hypergeometric2F1.";

JacobiPhiFromHyper::usage =
    "convert Hypergeometric2F1 to Jacobi Phi.";

WilsonPolynomialToHyper::usage =
    "convert Wilson polynomial to Hypergeometric4F3.";

WilsonPolynomialFromHyper::usage =
    "convert Hypergeometric4F3 to Wilson polynomial.";

AppellF1FromIntegral::usage =
    "convert integral to Appell F1.";


(* Label.wl *)

label::usage =
    "join the variable(s) and label(s) into a (sequence of) labeled object(s).";

labelAt::usage =
    "take the specific value(s) of the labeled object(s).";

labelConvert::usage =
    "convert the labeled object(s) according to the two specified label heads.";

labelJoin::usage =
    "labelConvert: _->Symbol.";

labelSplit::usage =
    "labelConvert: Symbol->_.";

labelToZero::usage =
    "x1->0.";

labelToEqual::usage =
    "x1->x2.";

labelToDiff::usage =
    "x1->x12+x2.";

labelToDiffZero::usage =
    "x1->x12, x2->0.";

labelToDiffBack::usage =
    "x12->x1-x2.";


(* Lie.wl *)

lie::usage =
    "simple Lie algebras.";

lieSimpleRoot::usage =
    "orthogonal simple roots of simple Lie algebras.";

lieCartan::usage =
    "Cartan matrix of simple Lie algebras.";

lieCartanInverse::usage =
    "inverse Cartan matrix of simple Lie algebras.";

lieDynkinDiagram::usage =
    "Dynkin diagram of simple Lie algebras.";


(* Matrix.wl *)

matSquareQ::usage =
    "matSquareQ[matrix]: test if the matrix is square.";

matComm::usage =
    StringJoin["matComm[a, b]: compute the commutator of the two matrices.", "\n", "Sketch: a.b - b.a."];

matJordan::usage =
    StringJoin["matJordan[dim, a, b]: construct a Jordan matrix of specified dimension.", "\n", "a: the common diagonal element.", "\n", "b: the common super-diagonal element.", "\n", "Default[b]: 1."];

matAngularMomentum::usage =
    StringJoin["matAngularMomentum[j][direction]: generate angular momentum matrices for the spin-j representation.", "\n", "Value[direction]: {\"x\", \"y\", \"z\"|0, 1, -1}.", "\n", "The column/row indices run from j to -j."];


(* OperatorForm.wl *)

SS::usage =
    "OperatorForm: Simplify.";

FS::usage =
    "OperatorForm: FullSimplify.";

FE::usage =
    "OperatorForm: FunctionExpand.";

FES::usage =
    "OperatorForm: FunctionExpand + Simplify.";

AS::usage =
    "OperatorForm: Assuming.";

SSA::usage =
    "OperatorForm: Simplify + Assuming.";

FSA::usage =
    "OperatorForm: FullSimplify + Assuming.";

FEA::usage =
    "OperatorForm: FunctionExpand + Assuming.";

FESA::usage =
    "OperatorForm: FunctionExpand + Simplify + Assuming.";

modularize::usage =
    "modularize[scope[code, iterators]]: modularize the scoping construction (e.g. Table, Sum, and Integrate) such that the iterators are lexically scoped.";

block::usage =
    "OperatorForm: Block.";

with::usage =
    "OperatorForm: With.";

module::usage =
    "OperatorForm: Module.";

rep::usage =
    "rep[rules][expr]: operator form of ReplaceAll with the rules being flattened.";

repdeep::usage =
    StringJoin["repdeep[rules][level][expr]: operator form of Replace with the rules being flattened.", "\n", "Default[level]: All."];

part::usage =
    "OperatorForm: Part.";

plus::usage =
    "OperatorForm: Plus.";

minus::usage =
    "OperatorForm: Minus.";

times::usage =
    "OperatorForm: Times.";

divide::usage =
    "OperatorForm: Divide.";

series::usage =
    "OperatorForm: Series + Normal.";

limit::usage =
    "OperatorForm: Limit.";

solve::usage =
    "OperatorForm: Solve.";

solve1::usage =
    "OperatorForm: Solve + First.";

collect::usage =
    "OperatorForm: Collect.";


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

presentQ::usage =
    StringJoin["presentQ[pattern][expr]: test whether the pattern occurs in the expression.", "Not + FreeQ."];

linearQ::usage =
    StringJoin["linearQ[expr, var]: test whether the expression is linear in the variable and the variable is present.", "\n", "linearQ[expr, varList]: test whether the expression is linear in all the variables and at least one is present."];

syntacticNegativeQ::usage =
    "syntacticNegativeQ[expr]: test whether the expression is syntactically negative.";

patternPresentQ::usage =
    "patternPresentQ[expr]: test whether any pattern construction occurs in the expression.";

patternFreeQ::usage =
    "patternFreeQ[expr]: test whether no pattern construction occurs in the expression.";


(* Relation.wl *)

relationMellinBarnes::usage =
    StringJoin["relationMellinBarnes[(x+y)^a, x, s]: generate Mellin-Barnes integral representation for the power factor.", "\n", "Example: (x+y)^a -> mg*x^s*y^(a-s)*INT[s]."];

relationFeynman::usage =
    StringJoin["relationFeynman[x^a*y^b, x, s]: generate Feynman-Schwinger integral representation for combining the two power factors.", "\n", "Example: x^a*y^b -> mg*(x+s*y)^(a+b)*s^(-b-1)*INT[s]."];

relationPowerPhase::usage =
    StringJoin["relationPowerPhase[base, expanded, expanded2, sign]: generate transformation rule for separating the power factor.", "\n", "base: specify the power base.", "\n", "expanded: specify the numerator factors to separate.", "\n", "expanded2: specify the denominator factors to separate and can be omitted.", "\n", "sign: specify the phase direction."];


(* SimplifyUnsafe.wl *)

unsafePowerTogether::usage =
    StringJoin["unsafePowerTogether[expr]: try to combine power factors.", "\n", "May produce mathematically invalid result."];

unsafePowerApart::usage =
    StringJoin["unsafePowerApart[expr]: try to separate power factors.", "\n", "May produce mathematically invalid result."];

unsafePowerSimplify::usage =
    StringJoin["unsafePowerSimplify[expr]: try to simplify power factors.", "\n", "May produce mathematically invalid result."];

unsafeExprTogether::usage =
    StringJoin["unsafeExprTogether[expr]: try to combine power factors, logarithms, and absolute values.", "\n", "May produce mathematically invalid result."];

unsafeExprApart::usage =
    StringJoin["unsafeExprApart[expr]: try to separate power factors, logarithms, and absolute values.", "\n", "May produce mathematically invalid result."];

unsafeExprSimplify::usage =
    StringJoin["unsafeExprSimplify[expr]: try to simplify power factors, logarithms, and absolute values.", "\n", "May produce mathematically invalid result."];


(* Simplify.wl *)

freeze::usage =
    StringJoin["freeze[pattern, operation, level][expr]: freeze subexpressions matching the pattern, then perform the operation and unfreeze.", "\n", "freeze[pattern->transform, operation, level][expr]: freeze subexpressions matching the pattern, then perform the operation and unfreeze.", "\n", "Value[pattern->transform]: _->Positive, _->Negative, _->{_,_}.", "\n", "Default[operation]: Simplify.", "\n", "Default[level]: Infinity."];

freezeNegative::usage =
    "freezeNegative[pattern, operation, level][expr]: variant of freeze with Negative as the default transformation.";

focus::usage =
    StringJoin["focus[pattern, operation, level][expr]: apply the operation to the arguments of functions with the specified heads.", "\n", "If level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

fracFocus::usage =
    StringJoin["fracFocus[operation, level][expr]: apply the operation to fractions (expressions containing negative powers).", "\n", "If level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

fracReduce::usage =
    StringJoin["fracReduce[operation, factor][expr]: multiply the factor to the numerator and denominator, then apply the operation separately to them.", "\n", "Default[operation]: Simplify.", "\n", "Default[factor]: 1."];

powerFocus::usage =
    StringJoin["powerFocus[operation, level][expr]: apply the operation to the base and exponent of power factors.", "\n", "If level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

powerBaseFocus::usage =
    StringJoin["powerBaseFocus[operation, level][expr]: apply the operation to the base of power factors only.", "\n", "If level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

powerExponentFocus::usage =
    StringJoin["powerExponentFocus[operation, level][expr]: apply the operation to the exponent of power factors only.", "\n", "If level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used.", "\n", "Default[operation]: Simplify."];

powerSeparate::usage =
    StringJoin["powerSeparate[baseP][expr]: separate the product expression into power factors and non-power factors.", "\n", "baseP: specify the pattern of power bases to match."];

powerBaseTogether::usage =
    StringJoin["powerBaseTogether[baseP, basePreservedP, baseExpandedP][expr]: take together the bases of power factors.", "\n", "baseP: specify the bases to combine.", "\n", "basePreservedP: specify the bases to preserve.", "\n", "baseExpandedP: specify the bases to expand manually.", "\n", "To skip baseP/basePreservedP, use All/None."];

powerExpand::usage =
    StringJoin["powerExpand[baseP, basePreservedP, baseExpandedP][expr]: combine power bases using powerBaseTogether, then expand power factors, and finally simplify power exponents.", "\n", "baseP: specify the bases to combine.", "\n", "basePreservedP: specify the bases to preserve.", "\n", "baseExpandedP: specify the bases to expand manually.", "\n", "To skip baseP/basePreservedP, use All/None."];

powerExponentCollect::usage =
    StringJoin["powerExponentCollect[powers...][expr]: collect and combine power factors with common exponents.", "\n", "Default[]: try to collect all power factors."];

trigPhaseReduce::usage =
    StringJoin["trigPhaseReduce[vars..][expr]: reduce phase factors in trigonometric functions using periodicity.", "\n", "vars: specify the variables to consider for periodicity."];

deltaReduce::usage =
    "deltaReduce[expr]: reduce the Dirac delta function and its derivatives in the expression.";

swap::usage =
    StringJoin["swap[a, b][expr]: swap the two symbols in the expression.", "\n", "swap[{a1, b1}, {a2, b2}, ...][expr]: swap the pairs simultaneously."];

separate::usage =
    "separate[criterion][expr_]: separate the elements based on whether they satisfy the criterion.";

stripPattern::usage =
    StringJoin["stripPattern[expr, head]: strip off pattern-related functions from the expression and wrap it with head.", "\n", "Default[head]: Defer."];

vanishing::usage =
    StringJoin["vanishing[expr]: clean up the expression by removing redundant vanishing terms.", "\n", "Sketch: Simplify + Flatten + DeleteDuplicates."];

extractSymbol::usage =
    StringJoin["extractSymbol[expr, exclusionList]: extract user-defined symbols from the expression.", "\n", "exclusionList: specify the contexts to exclude."];

extractVariable::usage =
    StringJoin["extractVariable[expr, exclusionList]: extract user-defined variables from the expression.", "\n", "exclusionList: specify the contexts to exclude."];