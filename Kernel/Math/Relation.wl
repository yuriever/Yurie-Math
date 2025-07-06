(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Relation`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


relationMellinBarnes::usage =
    "relationMellinBarnes[(x+y)^a, x, s]: generate Mellin-Barnes integral representation for the power factor."<>
    "\n"<>
    "Example: (x+y)^a -> mg*x^s*y^(a-s)*INT[s].";


relationFeynman::usage =
    "relationFeynman[x^a*y^b, x, s]: generate Feynman-Schwinger integral representation for combining the two power factors."<>
    "\n"<>
    "Example: x^a*y^b -> mg*(x+s*y)^(a+b)*s^(-b-1)*INT[s].";


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


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


(* ::Subsubsection:: *)
(*Mellin-Barnes*)


relationMellinBarnes[expr:Power[y_,a_],x_,s_] :=
    With[ {mg = Simplify@multiGamma[{-a+s,-s},{-a}],rest = Simplify[y-x]},
        expr->mg*x^s*rest^(-s+a)*INT[s]
    ];


(* ::Subsubsection:: *)
(*Feynman*)


relationFeynman[expr:Power[x_,a_]*Power[y_,b_],x_,s_] :=
    With[ {mg = Simplify@multiGamma[{-a-b},{-a,-b}]},
        expr->mg*s^(-b-1)*(x+s*y)^(a+b)*INT[s]
    ];


(* ::Subsubsection:: *)
(*PowerPhase*)


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
