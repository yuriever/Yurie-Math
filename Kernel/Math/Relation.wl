(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Relation`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


relationMellinBarnes::usage =
    "Mellin-Barnes relation.";

relationFeynman::usage =
    "Feynman-Schwinger relation.";

relationPowerPhase::usage =
    "relation for power phase.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


(* ::Subsection:: *)
(*relationMellinBarnes|relationFeynman*)


relationMellinBarnes[expr:Power[y_,a_],x_,s_] :=
    With[ {mg = Simplify@multiGamma[{-a+s,-s},{-a}],rest = Simplify[y-x]},
        expr->mg*x^s*rest^(-s+a)*INT[s]
    ];


relationFeynman[expr:Power[x_,a_]*Power[y_,b_],x_,s_] :=
    With[ {mg = Simplify@multiGamma[{-a-b},{-a,-b}]},
        expr->mg*s^(-b-1)*(x+s*y)^(a+b)*INT[s]
    ];


(* ::Subsection:: *)
(*relationPowerPhase*)


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
    ]//ReplaceAll[HoldComplete->RuleDelayed]


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
    ]//ReplaceAll[HoldComplete->RuleDelayed]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
