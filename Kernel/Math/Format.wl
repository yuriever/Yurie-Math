(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Format`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


interpretableFormat::usage =
    "dye[expr_]: color the elements at the first level of expression.";



(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)



(* ::Subsection:: *)
(*Main*)


interpretableFormat[expr_] :=
    Pass;




(* ::Subsection:: *)
(*Helper*)




(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
