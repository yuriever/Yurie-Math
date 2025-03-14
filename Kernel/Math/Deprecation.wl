(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Deprecation`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


(*hyperRegToUnreg::usage =
    "convert Hypergeometric2F1Regularized to Hypergeometric2F1.";*)

(*powerBaseSimplify::usage =
    "simplify the power bases.";*)

(*trigPhaseSimplify::usage =
    "reduce the phase factor in trigonometric functions.";*)

(*separateBy::usage =
    "separate the elements by whether or not satisfying the criteria.";*)

(*solveFirst::usage =
    "operator form of Solve + First.";*)

(*jordanBlock::usage =
    "matJordan[dim_Integer,a_Diagonal,b_OffDiagonal:1].";*)

(*gammaSplit::usage =
    "split a product into a list containing Gamma factors and the rests.";*)

(*hyperSplit::usage =
    "split a product into a list containing Hypergeometric2F1 factors and the rests.";*)


fracSimplify::usage =
    "simplify the numerator and denominator.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


hyperRegToUnreg[expr_] :=
    (
        Message[General::deprecation,"hyperRegToUnreg","hyperUnregularize"];
        expr//hyperUnregularize
    );


powerBaseSimplify[simplify_:Simplify][expr_] :=
    (
        Message[General::deprecation,"powerBaseSimplify","focus"];
        expr//Replace[#,Power[x_,n_]:>Power[simplify@Together[x],n],All]&
    );


trigPhaseSimplify[vars__][expr_] :=
    (
        Message[General::deprecation,"trigPhaseSimplify","trigPhaseReduce"];
        trigPhaseReduce[vars][expr]
    );


separateBy[crit_][expr_] :=
    (
        Message[General::deprecation,"separateBy","separate"];
        separate[crit][expr]
    );


gammaTakeResidue[variable_,index_,gm_Gamma,opts:OptionsPattern[]][expr_] :=
    (
        Message[General::deprecation,
            "gammaTakeResidue[_,_,Gamma[_]]",
            "gammaTakeResidue[_,_,_]"
        ];
        gammaTakeResidue[variable,index,gm[[1]],opts][expr]
    );


solveFirst[args___][expr_] :=
    (
        Message[General::deprecation0,"solveFirst"];
        First@Solve[expr,args]
    );


jordanBlock[args__] :=
    (
        Message[General::deprecation,"jordanBlock","matJordan"];
        matJordan[args]
    );


gammaSplit[expr_] :=
    (
        Message[General::deprecation,"gammaSplit","gammaSeparate"];
        gammaSeparate[expr]
    );


hyperSplit[expr_] :=
    (
        Message[General::deprecation,"hyperSplit","hyperSeparate"];
        hyperSeparate[expr]
    );


fracSimplify[simplify_:Simplify,factor_:1][expr_] :=
    (
        Message[General::deprecation,"fracSimplify","fracReduce"];
        fracReduce[simplify,factor][expr]
    );


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
