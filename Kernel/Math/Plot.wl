(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Plot`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


showComplexMapping::usage =
	"plot the complex mapping.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


showComplexMapping[fun_,var_,th_:0,max_:10] :=
    ContourPlot[Exp[2\[Pi] I th] fun/.{var->re+I im}//ReIm,{re,-max,max},{im,-max,max},ContourShading->None];

showComplexMapping[]:=
    CellPrint@{
        ExpressionCell[
            ToExpression[
                "showComplexMapping[z^2,z,1/8]",
                 StandardForm,
                 Defer
            ],
            "Code"
        ]
    };


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
