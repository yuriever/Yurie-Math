(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Random`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


randomize::usage =
    "randomize[domain, range][expr]: randomize the expression by replacing variables with random numbers."<>
    "\n"<>
    "Example: randomize[][x+y] -> x1+y1, where x1 and y1 are random numbers.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


randomize[][expr_] :=
    randomizeKernel[Reals,1,{}][expr];

randomize[domain:Reals|Integers|Complexes][expr_] :=
    randomizeKernel[domain,defaultRange[domain],{}][expr];

randomize[domain:Reals|Integers|Complexes,range_][expr_] :=
    randomizeKernel[domain,range,{}][expr];

randomize[domain:Reals|Integers|Complexes,range_,excluded:Except[_Integer]][expr_] :=
    randomizeKernel[domain,range,excluded][expr];

randomize[domain:Reals|Integers|Complexes,range_,repeat_Integer][expr_] :=
    randomizeKernel[domain,range,{},repeat][expr];

randomize[domain:Reals|Integers|Complexes,range_,OrderlessPatternSequence[excluded:Except[_Integer],repeat_Integer]][expr_] :=
    randomizeKernel[domain,range,excluded,repeat][expr];


randomizeKernel[domain_,range_,excluded_][expr_] :=
    Module[{varList,varNumber,varSample},
        varList =
            expr//extractVariable//DeleteCases[excludedPattern@excluded];
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

randomizeKernel[domain_,range_,excluded_,repeat_][expr_] :=
    Module[{varList,varNumber,varSample,ruleList},
        varList =
            expr//extractVariable//DeleteCases[excludedPattern@excluded];
        varNumber =
            Length[varList];
        varSample = Switch[domain,
            Reals,
                RandomReal[range,{repeat,varNumber}],
            Integers,
                RandomInteger[range,{repeat,varNumber}],
            Complexes,
                RandomComplex[range,{repeat,varNumber}]
        ];
        ruleList =
            varSample//Map[Thread[varList->#]&];
        expr//ReplaceAll[ruleList]//Chop
    ];


excludedPattern[excluded_List] :=
    Alternatives@@excluded;

excludedPattern[excluded_] :=
    excluded;


defaultRange[Reals] :=
    1;

defaultRange[Integers] :=
    10;

defaultRange[Complexes] :=
    1+I;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
