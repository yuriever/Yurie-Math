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


syntacticNegativeQ::usage =
    "syntacticNegativeQ[expr]: test whether the expression is syntactically negative.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Macro*)



(* ::Subsection:: *)
(*Main*)


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
        Deprecation["relationPowerMono","relationPowerPhase"];

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
        Deprecation["relationPowerMono","relationPowerPhase"];


collectDerivative[var:Except[_List],operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][var][___],operation]//
        Deprecation["collectDerivative","diffCollect"];

collectDerivative[varList_List,operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][#][___]&/@varList,operation]//
        Deprecation["collectDerivative","diffCollect"];


powerExpandFactor[][expr_] :=
    expr//powerBaseFocus[Factor]//PowerExpand//powerFocus[Simplify]//
        Deprecation["powerExpandFactor","powerExpand"];

powerExpandFactor[frozenVar_][expr_] :=
    expr//powerBaseFocus[Factor]//freeze[frozenVar,PowerExpand]//powerFocus[Simplify]//
        Deprecation["powerExpandFactor","powerExpand"];


syntacticNegativeQ[expr_] :=
    Internal`SyntacticNegativeQ[expr]//
        Deprecation["syntacticNegativeQ","minusQ"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
