(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Deprecation`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


relationPowerMono::usage =
    "relation for branch cut of Power at zero.";


collectDerivative::usage =
    "collect by derivatives.";


powerExpandFactor::usage =
    "factor the base of powers and then expand.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Macro*)


deprecation::usage =
    "macro for deprecation warning.";

deprecation//Attributes = {
    HoldAllComplete
};

deprecation[old_String,new_String] :=
    Function[
        expr,
        (
            Message[General::Deprecation2,old,new];
            expr
        ),
        {HoldAllComplete}
    ];

deprecation[old_String] :=
    Function[
        expr,
        (
            Message[General::Deprecation1,old];
            expr
        ),
        {HoldAllComplete}
    ];


(* ::Subsection:: *)
(*Main*)


(* ::Subsubsection:: *)
(*relationPowerMono*)


relationPowerMono[base_,expanded_List,sign:1|-1:1] :=
    With[ {exponent = Unique[]},
        {
            powerP = Times@@Map[Power[#,exponent]&,expanded]
        },
        HoldComplete[
            Power[base,exponent_],
            Exp[sign*I*π*exponent]*powerP
        ]
    ]//ReplaceAll[HoldComplete->RuleDelayed]//
        deprecation["relationPowerMono","relationPowerPhase"];


relationPowerMono[base_,expanded_List,expanded2_List,sign:1|-1:1] :=
    With[ {exponent = Unique[]},
        {
            powerP = Times@@Map[Power[#,exponent]&,expanded],
            powerM = Times@@Map[Power[#,-exponent]&,expanded2]
        },
        HoldComplete[
            Power[base,exponent_],
            Exp[sign*I*π*exponent]*powerP*powerM
        ]
    ]//ReplaceAll[HoldComplete->RuleDelayed]//
        deprecation["relationPowerMono","relationPowerPhase"];


(* ::Subsubsection:: *)
(*collectDerivative*)


collectDerivative[var:Except[_List],operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][var][___],operation]//
        deprecation["collectDerivative","diffCollect"];

collectDerivative[varList_List,operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][#][___]&/@varList,operation]//
        deprecation["collectDerivative","diffCollect"];


(* ::Subsubsection:: *)
(*powerExpandFactor*)


powerExpandFactor[][expr_] :=
    expr//powerBaseFocus[Factor]//PowerExpand//powerFocus[Simplify]//
        deprecation["powerExpandFactor","powerExpand"];

powerExpandFactor[frozenVar_][expr_] :=
    expr//powerBaseFocus[Factor]//freeze[frozenVar,PowerExpand]//powerFocus[Simplify]//
        deprecation["powerExpandFactor","powerExpand"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
