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


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


relationMellinBarnes[expr:Power[y_,a_],x_,s_,head_:INT] :=
    With[ {mg = Simplify@multiGamma[{-a+s,-s},{-a}],rest = Simplify[y-x]},
        expr->relation["MellinBarnes",s][mg*x^s*rest^(-s+a)]
    ]//handleRelationHead[head];


relationFeynman[expr:Power[x_,a_]*Power[y_,b_],x_,s_,head_:INT] :=
    With[ {mg = Simplify@multiGamma[{-a-b},{-a,-b}]},
        expr->relation["Feynman",s][mg*s^(-b-1)*(x+s*y)^(a+b)]
    ]//handleRelationHead[head];


handleRelationHead[head_][expr_] :=
    expr//ReplaceAll[relation[__]->head];

handleRelationHead[INT][expr_] :=
    expr//ReplaceAll[relation["MellinBarnes"|"Feynman",var_][term_]:>INT[var]*term];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
