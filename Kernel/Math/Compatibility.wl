(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Unstable`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


deltaOld::usage =
    "deltaOld[z, n]: \[Delta]^n(z) - Dirac delta function."<>
    "\n"<>
    "deltaOld[{z, ...}, {n, ...}]: \[Delta]^n(z) ... - multi-variable Dirac delta function."<>
    "\n"<>
    "deltaOld[z, n, tag]: Dirac delta function with tag.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


deltaOld;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
