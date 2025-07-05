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
    StringJoin["simplify Gamma factors in the expression.", "\nDeveloper`GammaSimplify"];

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
    "testing if is a square matrix.";

matComm::usage =
    "matComm[a,b]=a.b-b.a.";

matJordan::usage =
    StringJoin["Jordan matrix.", "\nmatJordan[dim_Integer,a_Diagonal,b_OffDiagonal:1]."];

matAngularMomentum::usage =
    StringJoin["spin-j representation of angular momentum in the unit of hbar.", "\nThe column/row indices run from j to -j."];


(* OperatorForm.wl *)

SS::usage =
    "Simplify.";

FS::usage =
    "FullSimplify.";

FE::usage =
    "FunctionExpand.";

FES::usage =
    "FunctionExpand + Simplify.";

AS::usage =
    "operator form of Assuming.";

SSA::usage =
    "Simplify + Assuming.";

FSA::usage =
    "FullSimplify + Assuming.";

FEA::usage =
    "FunctionExpand + Assuming.";

FESA::usage =
    "FunctionExpand + Simplify + Assuming.";

modularize::usage =
    "modularize scoping constructions.";

block::usage =
    "operator form of Block.";

with::usage =
    "operator form of With.";

module::usage =
    "operator form of Module.";

rep::usage =
    "operator form of ReplaceAll.";

part::usage =
    "operator form of Part, GeneralUtilities`Slice.";

plus::usage =
    "operator form of Plus.";

minus::usage =
    "operator form of Minus.";

times::usage =
    "operator form of Times.";

divide::usage =
    "operator form of Divide.";

series::usage =
    "operator form of Series + Normal.";

limit::usage =
    "opeartor form of Limit.";

solve::usage =
    "operator form of Solve.";

solve1::usage =
    "operator form of Solve + First.";

collect::usage =
    "operator form of Collect.";


(* Quest.wl *)

isN::usage =
    "zero or positive integers.";

isZ::usage =
    "integers.";

isZP::usage =
    "positive integers.";

isZN::usage =
    "negative integers.";

isZP0::usage =
    "zero or positive integers.";

isZN0::usage =
    "zero or negative integers.";

isQ::usage =
    "rational numbers.";

isR::usage =
    "real numbers.";

isRP::usage =
    "positive real numbers.";

isRN::usage =
    "negative real numbers.";

isRP0::usage =
    "zero or positive real numbers.";

isRN0::usage =
    "zero or negative real numbers.";

isC::usage =
    "complex numbers.";

presentQ::usage =
    "Not + FreeQ.";

linearQ::usage =
    "linearQ[expr,var|varList]: whether the expression is linear and at least one variable is present.";

syntacticNegativeQ::usage =
    "syntacticNegativeQ[expr]: whether the expression is syntactically negative.";

patternPresentQ::usage =
    "patternPresentQ[expr]: whether any pattern occurs in the expression.";

patternFreeQ::usage =
    "patternFreeQ[expr]: whether no pattern occurs in the expression.";


(* Relation.wl *)

relationMellinBarnes::usage =
    "Mellin-Barnes relation.";

relationFeynman::usage =
    "Feynman-Schwinger relation.";

relationPowerPhase::usage =
    "relation for power phase.";


(* SimplifyUnsafe.wl *)

unsafePowerTogether::usage =
    "take powers together.";

unsafePowerApart::usage =
    "take powers apart, similar to PowerExpand.";

unsafePowerSimplify::usage =
    "simplify powers.";

unsafeExprTogether::usage =
    "take powers, logs and abs together.";

unsafeExprApart::usage =
    "take powers, logs and abs apart.";

unsafeExprSimplify::usage =
    "simplify powers, logs and abs.";

unsafeDeltaSimplify::usage =
    "simplify Delta functions.";


(* Simplify.wl *)

freeze::usage =
    StringJoin["freeze[pattern, operation, level][expr]: freeze subexpressions matching the pattern, then perform the operation and unfreeze.", "\nThe supported transformation rules are: _->Positive, _->Negative, _->{_,_}.", "\nThe default operation is Simplify."];

freezeNegative::usage =
    "freezeNegative[pattern, operation, level][expr]: variant of freeze with Negative as the default transformation.";

focus::usage =
    StringJoin["focus[pattern, operation, level][expr]: apply the operation to the arguments of functions with the specified heads.", "\nThe default operation is Simplify."];

fracFocus::usage =
    StringJoin["fracFocus[operation, level][expr]: apply the operation to fractions (expressions containing negative powers).", "\nThe default operation is Simplify."];

fracReduce::usage =
    StringJoin["fracReduce[operation, factor][expr]: multiply the factor to the numerator and denominator, then apply the operation separately to them.", "\nThe default operation is Simplify.", "\nThe default factor is 1."];

powerFocus::usage =
    StringJoin["powerFocus[operation, level][expr]: apply the operation to the base and exponent of power factors.", "\nThe default operation is Simplify."];

powerBaseFocus::usage =
    StringJoin["powerBaseFocus[operation, level][expr]: apply the operation to the base of power factors only.", "\nThe default operation is Simplify."];

powerExponentFocus::usage =
    StringJoin["powerExponentFocus[operation, level][expr]: apply the operation to the exponent of power factors only.", "\nThe default operation is Simplify."];

powerSeparate::usage =
    StringJoin["powerSeparate[baseP][expr]: separate the product expression into power factors and non-power factors.", "\nbaseP specifies the pattern of power bases to match."];

powerBaseTogether::usage =
    StringJoin["powerBaseTogether[baseP, basePreservedP, baseExpandedP][expr]: take together the bases of power factors.", "\nbaseP specifies which bases to combine.", "\nbasePreservedP specifies which bases to preserve.", "\nbaseExpandedP specifies which bases to expand manually."];

powerExpand::usage =
    StringJoin["powerExpand[baseP, basePreservedP, baseExpandedP][expr]: combine power bases using powerBaseTogether, then expand power factors, and finally simplify power exponents.", "\nbaseP specifies which bases to combine.", "\nbasePreservedP specifies which bases to preserve.", "\nbaseExpandedP specifies which bases to expand manually."];

powerExponentCollect::usage =
    StringJoin["powerExponentCollect[powers...][expr]: collect and combine power factors with common exponents.", "\nThe default is to try to collect all power factors."];

trigPhaseReduce::usage =
    StringJoin["trigPhaseReduce[vars..][expr]: reduce phase factors in trigonometric functions using periodicity.", "\nvars specifies the variables to consider for periodicity."];

deltaReduce::usage =
    "reduce the Dirac delta function.";

swap::usage =
    StringJoin["swap[a, b][expr]: swap the two symbols in the expression.", "\nswap[{a1, b1}, {a2, b2}, ...][expr]: swap the pairs simultaneously."];

separate::usage =
    "separate[criterion][expr_]: separate the elements based on whether they satisfy the criterion.";

stripPattern::usage =
    StringJoin["stripPattern[expr, head]: strip off pattern-related functions from the expression and wrap it with head.", "\nThe default head is Defer."];

vanishing::usage =
    StringJoin["vanishing[expr]: clean up the expression by removing redundant vanishing terms.", "\nThis is equivalent to Simplify + Flatten + DeleteDuplicates."];

extractSymbol::usage =
    StringJoin["extractSymbol[expr, exclusionList]: extract user-defined symbols from the expression.", "\nexclusionList specifies the contexts to exclude."];

extractVariable::usage =
    StringJoin["extractVariable[expr, exclusionList]: extract user-defined variables from the expression.", "\nexclusionList specifies the contexts to exclude."];