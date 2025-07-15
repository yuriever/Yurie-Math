(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Random`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


randomize::usage =
    "randomize[domain, range, excludedList][expr]: randomize the expression by replacing variables with random numbers."<>
    "\n"<>
    "Example: randomize[x+y] -> x1+y1, where x1 and y1 are random numbers.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


randomize[][expr_] :=
    randomizeKernel[Reals,1,{}][expr];

randomize[domain:Reals|Integers|Complexes,range_:1,excludedList_List:{}][expr_] :=
    randomizeKernel[domain,range,excludedList][expr];


randomizeKernel[domain_,range_,excludedList_][expr_] :=
    Module[{varList,varNumber,varSample},
        varList =
            expr//extractVariable//DeleteCases[Alternatives@@excludedList];
        varNumber =
            Length[varList];
        varSample = Switch[domain,
            Reals,
                RandomReal[range,varNumber],
            Integers,
                RandomInteger[range,varNumber],
            Complexes,
                RandomComplex[range,varNumber]
        ];
        expr//ReplaceAll[Thread[varList->varSample]]//Chop
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
