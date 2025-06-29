(* Deprecation.wl *)

relationPowerMono::usage =
    "relation for branch cut of Power at zero.";


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
    StringJoin["diffChange[expr,transformations,oldVars,newVars,functions] \n", "diffChange[] gives the example."];

integrateChange::usage =
    StringJoin["integrateChange[expr,transformations,oldVars,newVars] \n", "integrateChange[] gives the example."];

IBP::usage =
    "integration by parts.";

jacobianMatrix::usage =
    "jacobianMatrix.";

jacobianDet::usage =
    "jacobianDet.";

diffComm::usage =
    "diffComm[X,Y]=-(X[Y[#]]-Y[X[#]])&.";

PDCoefficient::usage =
    "collect the coefficients of PD[___].";


(* DLMF.wl *)

DLMF::usage =
    "simplify expressions by the rules in DLMFData.";

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

hyperSeparate::usage =
    "split a product into Hypergeometric2F1 factors and the rests.";

hyperUnregularize::usage =
    "convert Hypergeometric2F1Regularized to Hypergeometric2F1.";

hyperTaylor::usage =
    "head used by hyperToTaylor.";

hyperMellinBarnes::usage =
    "head used by hyperToMellinBarnes and hyperToMellinBarnes2.";

hyperToTaylor::usage =
    "convert Hypergeometric2F1 factors to Taylor terms.";

hyperToMellinBarnes::usage =
    "convert Hypergeometric2F1 factors to Mellin-Barnes integrands.";

hyperToMellinBarnes2::usage =
    "convert Hypergeometric2F1 factors to Mellin-Barnes integrands in terms of (1-z).";

jacobiPhi::usage =
    "head of Jacobi Phi, jacobiPhi[a,b,c,z], DLMF:15.9.11.";

jacobiPhiToHyper::usage =
    "convert Jacobi Phi to Hypergeometric2F1.";

jacobiPhiFromHyper::usage =
    "convert Hypergeometric2F1 to Jacobi Phi.";

wilsonPolynomial::usage =
    "head of Wilson polynomial, wilsonPolynomial[a,b,c,d,n,x].";

wilsonPolynomialToHyper::usage =
    "convert Wilson polynomial to Hypergeometric4F3.";

wilsonPolynomialFromHyper::usage =
    "convert Hypergeometric4F3 to Wilson polynomial.";

AppellF1FromIntegral::usage =
    "integral representation of Appell F1.";

AppellF1ToHyper::usage =
    "convert Appell F1 to summation of Hypergeometric2F1.";

hyperAppellF1::usage =
    "head used by AppellF1ToHyper.";


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
    "freeze subexpressions matching the pattern and then perform the operation.";

freezeNegative::usage =
    StringJoin["variant of freeze.", "\nNegative is used as the default transformation."];

focus::usage =
    "simplify the argument(s) of the specified head(s).";

fracFocus::usage =
    "simplify the numerator and denominator of fractions.";

fracReduce::usage =
    "reduce the fraction by multiplying a common factor onto the numerator and denominator.";

powerFocus::usage =
    "simplify the base and exponent of powers.";

powerBaseFocus::usage =
    "simplify the base of powers.";

powerExponentFocus::usage =
    "simplify the exponent of powers.";

powerSeparate::usage =
    "split a product into powers with specified base(s) and the rests.";

powerBaseTogether::usage =
    "make together the specified base(s) of powers.";

powerExpand::usage =
    "expand the powers with the specified base(s).";

powerExpandFactor::usage =
    "factor the base of powers and then expand.";

powerExponentCollect::usage =
    "collect powers by the specified exponent(s).";

trigPhaseReduce::usage =
    "reduce phase factors in trigonometric functions by the given assumptions.";

deltaReduce::usage =
    "reduce the Dirac delta function.";

collectDerivative::usage =
    "collect by derivatives.";

swap::usage =
    "swap two symbols in an expression.";

separate::usage =
    "separate the elements by whether or not satisfying the criteria.";

stripPattern::usage =
    "strip off pattern-related functions in expressions.";

vanishing::usage =
    "Simplify + Flatten + DeleteDuplicates.";

extractSymbol::usage =
    "extract symbols from the expression.";

extractVariable::usage =
    "extract variables from the expression.";