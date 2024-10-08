(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Hyper`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Constant`"];


(* ::Section:: *)
(*Public*)


hyperSplit::usage =
    "split a product into a list containing Hypergeometric2F1 factors and the rests.";

hyperRegToUnreg::usage =
    "convert Hypergeometric2F1Regularized to Hypergeometric2F1.";

hyperSwap::usage =
    "swap the first two arguments of Hypergeometric2F1.";


hyperTo::usage =
    "convert Hypergeometric2F1 factors according to the prototype rule.";

hyperToTaylor::usage =
    "convert Hypergeometric2F1 factors to Taylor terms.";

hyperToMellinBarnes::usage =
    "convert Hypergeometric2F1 factors to Mellin-Barnes integrands.";

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


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Message*)


hyperTo::symbolNotEnough =
    "there are `` more Hypergeometric2F1-s than the number of specified symbols."


(* ::Subsection:: *)
(*hyperSplit*)


hyperSplit[expr_Hypergeometric2F1] :=
    {expr,1};

hyperSplit[expr_Times] :=
    {
        Select[expr,!FreeQ[#,Hypergeometric2F1|HypergeometricPFQ]&],
        Select[expr,FreeQ[Hypergeometric2F1|HypergeometricPFQ]]
    };

hyperSplit[expr_] :=
    {1,expr};


(* ::Subsection:: *)
(*hyperRegToUnreg*)


hyperRegToUnreg[expr_] :=
    expr//ReplaceAll[DLMFData["HyperRegToUnreg"]];


(* ::Subsection:: *)
(*hyperSwap*)


hyperSwap[expr_,head_:Hold] :=
    head[expr]//ReplaceAll[DLMFData["HyperSwapAB"]];


(* ::Subsection:: *)
(*hyperTo*)


(* ::Subsubsection:: *)
(*Main*)


hyperToTaylor[symbols__][expr_] :=
    hyperTo[hyperToTaylorRule,{symbols}][expr];


hyperToMellinBarnes[symbols__][expr_] :=
    hyperTo[hyperToMellinBarnesRule,{symbols}][expr];


hyperTo[which_,symbolList_][expr0_] :=
    Module[ {expr,positionList,numberOfHyper,numberOfSymbol,hyperList,convertedHyperList,result},
        expr =
            expr0//hyperToPreprocess;
        positionList =
            Position[expr,_Hypergeometric2F1];
        numberOfHyper =
            Length[positionList];
        numberOfSymbol =
            Length[symbolList];
        hyperList =
            Extract[expr,positionList];
        Which[
            numberOfHyper===0,
                result = expr,
            numberOfHyper>numberOfSymbol,
                Message[hyperTo::symbolNotEnough,numberOfHyper-numberOfSymbol];
                result = expr,
            numberOfHyper<=numberOfSymbol,
                convertedHyperList =
                    MapThread[which,{Take[symbolList,numberOfHyper],hyperList}];
                result =
                    ReplacePart[expr,MapThread[Rule,{positionList,convertedHyperList}]];
        ];
        result//hyperToPostprocess
    ];


(* ::Subsubsection:: *)
(*Helper*)


hyperToPreprocess[expr_] :=
    ReplaceAll[
        Hold[expr],
        Power[hyper_Hypergeometric2F1,n_]:>
            RuleCondition[Inactive[Times]@@ConstantArray[hyper,n]]
    ];


hyperToPostprocess[expr_] :=
    Activate[ReleaseHold[expr],Times];


hyperToMellinBarnesRule[s_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyperMellinBarnes[((-z)^s Gamma[-s] Gamma[c] Gamma[a+s] Gamma[b+s])/(Gamma[a] Gamma[b] Gamma[c+s])];


hyperToTaylorRule[n_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyperTaylor[(z^n Gamma[c] Gamma[a+n] Gamma[b+n])/(Gamma[1+n] Gamma[a] Gamma[b] Gamma[c+n])];


(* ::Subsection:: *)
(*jacobiPhi*)


jacobiPhiToHyper[expr_] :=
    expr//ReplaceAll[DLMFData["JacobiPhiToHyper"]]//ReplaceAll[head_HypergeometricPFQ:>Simplify[head]];


jacobiPhiFromHyper[expr_] :=
    expr//ReplaceAll[DLMFData["JacobiPhiFromHyper"]]//ReplaceAll[head_jacobiPhi:>Simplify[head]];


(* ::Subsection:: *)
(*wilsonPolynomial*)


wilsonPolynomialToHyper[expr_] :=
    expr//ReplaceAll[DLMFData["WilsonPolynomialToHyper"]]//ReplaceAll[head_HypergeometricPFQ:>Simplify[head]];


wilsonPolynomialFromHyper[expr_] :=
    expr//ReplaceAll[DLMFData["WilsonPolynomialFromHyper"]]//ReplaceAll[head_wilsonPolynomial:>Simplify[head]];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
