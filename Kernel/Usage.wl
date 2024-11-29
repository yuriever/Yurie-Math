(* Diff.wl *)

jacobianMatrix::usage =
	"jacobianMatrix.";

jacobianDet::usage =
	"jacobianDet.";

diffComm::usage =
	"diffComm[X,Y]=-(X[Y[#]]-Y[X[#]])&.";

diffChange::usage =
	StringJoin["diffChange[expr,transformations,oldVars,newVars,functions] \n", "diffChange[] gives the example."];

integrateChange::usage =
	StringJoin["integrateChange[expr,transformations,oldVars,newVars] \n", "integrateChange[] gives the example."];

IBP::usage =
	"integration by parts.";


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

gammaFrom::usage =
	"expand everything to Gamma factors.";

gammaSplit::usage =
	"split a product into a list containing Gamma factors and the rests.";

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

hyperSplit::usage =
	"split a product into a list containing Hypergeometric2F1 factors and the rests.";

hyperRegToUnreg::usage =
	"convert Hypergeometric2F1Regularized to Hypergeometric2F1.";

hyperToTaylor::usage =
	"convert Hypergeometric2F1 factors to Taylor terms.";

hyperToMellinBarnes::usage =
	"convert Hypergeometric2F1 factors to Mellin-Barnes integrands.";

hyperToMellinBarnes2::usage =
	"convert Hypergeometric2F1 factors to Mellin-Barnes integrands in terms of (1-z).";

hyperTaylor::usage =
	"head used by hyperToTaylor.";

hyperMellinBarnes::usage =
	"head used by hyperToMellinBarnes.";

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

jordanBlock::usage =
	StringJoin["jordanBlock[dim_Integer,a_OffDiagonal,b_Diagonal:1]\n", "jordanBlock[dim_Integer,a_]."];

sparseBlockMatrix::usage =
	"SparseArray`SparseBlockMatrix.";


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

linearQ::usage =
	"whether the expression is linear with respect to the variables.";

presentQ::usage =
	"Not + FreeQ.";

patternPresentQ::usage =
	"Internal`PatternPresentQ.";

patternFreeQ::usage =
	"Internal`PatternFreeQ.";


(* Simplify.wl *)

SS::usage =
	"Simplify.";

FS::usage =
	"FullSimplify.";

FE::usage =
	"FunctionExpand.";

AS::usage =
	"operator form of Assuming.";

SSA::usage =
	"Simplify + Assuming.";

FSA::usage =
	"FullSimplify + Assuming.";

FEA::usage =
	"FunctionExpand + Assuming.";

modularize::usage =
	"modularize scoping constructions.";

block::usage =
	"operator form of Block.";

with::usage =
	"operator form of With.";

module::usage =
	"operator form of Module.";

times::usage =
	"operator form of Times.";

plus::usage =
	"operator form of Plus.";

series::usage =
	"operator form of Series + Normal.";

limit::usage =
	"opeartor form of Limit.";

solve::usage =
	"operator form of Solve.";

solveFirst::usage =
	"operator form of Solve + First.";

part::usage =
	"operator form of Part, GeneralUtilities`Slice.";

collect::usage =
	"operator form of Collect.";

exprTogether::usage =
	"take powers, logs and abs together.";

exprApart::usage =
	"take powers, logs and abs apart.";

exprSim::usage =
	"simplify powers, logs and abs.";

powerTogether::usage =
	"take powers together.";

powerApart::usage =
	"take powers apart, similar to PowerExpand.";

powerCollect::usage =
	"collect powers by exponents.";

powerSim::usage =
	"simplify powers.";

deltaSim::usage =
	"simplify Delta functions.";

collectDerivative::usage =
	"collect by derivatives.";

fracSimplify::usage =
	"simplify the numerator and denominator.";

powerBaseSimplify::usage =
	"simplify the power bases.";

vanishing::usage =
	"Simplify + Flatten + DeleteDuplicates.";

swap::usage =
	"swap two symbols in an expression.";

stripPattern::usage =
	"strip off pattern-related functions in expressions.";

separateBy::usage =
	"separate the elements by whether or not satisfying the criteria.";

freeze::usage =
	"free subexpressions matching the pattern and perform the operation.";

trigPhaseSimplify::usage =
	"separate the phase factor in trigonometric functions.";