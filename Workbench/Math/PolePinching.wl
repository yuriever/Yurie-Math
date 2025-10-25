(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`PolePinching`"];


Needs["Yurie`Math`"];


Needs["Yurie`Base`"];

ClearAll["Yurie`Math`PolePinching`*"];

ClearAll["Yurie`Math`PolePinching`*`*"];


(* ::Section:: *)
(*Public*)


test::usage=
    "test";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Message*)


multiGammaPoleAnalysis::nonlinear =
    "the arguments of Gamma factors should be linear with respect to the variables.";


(* ::Subsection:: *)
(*Main*)


multiGammaPoleAnalysisKernel[varList_List,poleIndexList_List,isolatePoleData:{{_,_}...},assumption_][mg_multiGamma] :=
    Module[{mgdata},
        mgdata =
            getPoleData[isolatePoleData][mg];
        If[!poleStructureLinearQ[varList][mgdata],
            Message[multiGammaPoleAnalysis::nonlinear];
            Throw[mg]
        ];

    ]//Catch;





(* ::Subsection:: *)
(*Helper*)


getPoleData[isolatePoleData_List][mg_multiGamma] :=
    Join[
        Map[<|"Argument"->#,"Type"->"Gamma"|>&,mg[[1]]],
        Map[<|"Argument"->#[[1]],"Type"->{"Isolate",#[[2]]}|>&,isolatePoleData]
    ];


poleStructureLinearQ[varList_List][mgdata_List] :=
    mgdata//Query[All,"Argument",Internal`LinearQ[#,varList]||FreeQ[#,Alternatives@@varList]&]//AllTrue[#,TrueQ]&;

poleStructureLinearQ[___][___]:=
    False;





(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
