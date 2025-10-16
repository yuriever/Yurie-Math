(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Deprecation`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


collectDerivative::usage =
    "collect by derivatives.";


syntacticNegativeQ::usage =
    "syntacticNegativeQ[expr]: test whether the expression is syntactically negative.";


relationPowerPhase::usage =
    "relationPowerPhase[base, expanded, expanded2, sign]: generate transformation rule for separating the power factor."<>
    "\n"<>
    "Info[base]: the power base."<>
    "\n"<>
    "Info[expanded]: the numerator factors to separate."<>
    "\n"<>
    "Info[expanded2]: the denominator factors to separate. This argument is optional."<>
    "\n"<>
    "Info[sign]: the phase direction."<>
    "\n"<>
    "Default[sign]: 1.";


label2::usage =
    "label2[var, lab]: variant of label with Symbol as head.";

labelRange::usage =
    "labelRange[var, range, head]: join the variable(s) and labels in the range using the specified head."<>
    "\n"<>
    "Default[head]: Function."<>
    "\n"<>
    "Example: labelRange[x, 3] gives x[1], x[2], x[3].";

labelRange2::usage =
    "labelRange2[var, range]: variant of labelRange with Symbol as head.";


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
        Deprecation["relationPowerMono"->"relationPowerPhase"];

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
        Deprecation["relationPowerMono"->"relationPowerPhase"];


collectDerivative[var:Except[_List],operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][var][___],operation]//
        Deprecation["collectDerivative"->"diffCollect"];

collectDerivative[varList_List,operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][#][___]&/@varList,operation]//
        Deprecation["collectDerivative"->"diffCollect"];


powerExpandFactor[][expr_] :=
    expr//powerBaseFocus[Factor]//PowerExpand//powerFocus[Simplify]//
        Deprecation["powerExpandFactor"->"powerExpand"];

powerExpandFactor[frozenVar_][expr_] :=
    expr//powerBaseFocus[Factor]//freeze[frozenVar,PowerExpand]//powerFocus[Simplify]//
        Deprecation["powerExpandFactor"->"powerExpand"];


syntacticNegativeQ[expr_] :=
    Internal`SyntacticNegativeQ[expr]//
        Deprecation["syntacticNegativeQ"->"minusQ"];


relationPowerPhase[base_,expanded_List,sign:1|-1:1] :=
    With[ {exponent = Unique[]},
        {
            num = Times@@Map[Power[#,exponent]&,expanded],
            rest = Power[Simplify[-base/Times@@expanded],exponent]
        },
        HoldComplete[
            Power[base,exponent_],
            Exp[sign*I*π*exponent]*num*rest
        ]
    ]//ReplaceAll[HoldComplete->RuleDelayed]//Deprecation["relationPowerPhase"->"powerExpandBy","Future"];


relationPowerPhase[base_,expanded_List,expanded2_List,sign:1|-1:1] :=
    With[ {exponent = Unique[]},
        {
            num = Times@@Map[Power[#,exponent]&,expanded],
            denom = Times@@Map[Power[#,-exponent]&,expanded2],
            rest = Power[Simplify[-base*Times@@expanded2/Times@@expanded],exponent]
        },
        HoldComplete[
            Power[base,exponent_],
            Exp[sign*I*π*exponent]*num*denom*rest
        ]
    ]//ReplaceAll[HoldComplete->RuleDelayed]//Deprecation["relationPowerPhase"->"powerExpandBy","Future"];


label2[args___] :=
    labell[args]//Deprecation["label2"->"labell"];

labelRange[args___] :=
    labels[args]//Deprecation["labelRange"->"labels"];

labelRange2[args___] :=
    labells[args]//Deprecation["labelRange2"->"labells"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
