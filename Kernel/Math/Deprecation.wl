(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Deprecation`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


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
(*Main*)


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
