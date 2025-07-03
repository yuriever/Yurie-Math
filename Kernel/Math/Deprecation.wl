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


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


relationPowerMono[base_,expanded_List,sign:1|-1:1] :=
    With[ {exponent = Unique[]},
        {
            powerP = Times@@Map[Power[#,exponent]&,expanded]
        },
        Message[General::deprecation,"relationPowerMono","relationPowerPhase"];
        HoldComplete[
            Power[base,exponent_],
            Exp[sign*I*π*exponent]*powerP
        ]
    ]//ReplaceAll[HoldComplete->RuleDelayed]


relationPowerMono[base_,expanded_List,expanded2_List,sign:1|-1:1] :=
    With[ {exponent = Unique[]},
        {
            powerP = Times@@Map[Power[#,exponent]&,expanded],
            powerM = Times@@Map[Power[#,-exponent]&,expanded2]
        },
        Message[General::deprecation,"relationPowerMono","relationPowerPhase"];
        HoldComplete[
            Power[base,exponent_],
            Exp[sign*I*π*exponent]*powerP*powerM
        ]
    ]//ReplaceAll[HoldComplete->RuleDelayed]


(* ::Subsubsection:: *)
(*collectDerivative*)


collectDerivative[var:Except[_List],operation_:Identity][expr_] :=
    (
        Message[General::deprecation,"collectDerivative","PDCollect"];
        Collect[expr,Derivative[___][var][___],operation]
    );

collectDerivative[varList_List,operation_:Identity][expr_] :=
    (
        Message[General::deprecation,"collectDerivative","PDCollect"];
        Collect[expr,Derivative[___][#][___]&/@varList,operation]
    );


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
