(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`DLMFWorkflow`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Info`"];

Needs["Yurie`Math`Constant`"];


(* ::Section:: *)
(*Public*)


DLMFTest::usage =
    "test the rules at random a, b, c, and z.";

DLMFTestAll::usage =
    "test all the rules at random a, b, c, and z.";

DLMFTestPlot::usage =
    "test the rules at random a, b, c and plot by z.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Constant*)


$DLMFDataExceptionList = {
    "16.4.14",
    "WilsonPolynomialToHyper",
    "WilsonPolynomialFromHyper",
    "JacobiPhiToHyper",
    "JacobiPhiFromHyper"
};


$privateConversionList = {
    Yurie`Math`Constant`Private`a->a,
    Yurie`Math`Constant`Private`b->b,
    Yurie`Math`Constant`Private`c->c,
    Yurie`Math`Constant`Private`z->z
};


(* ::Subsection:: *)
(*Option*)


DLMFTest//Options =
    {"Epsilon"->0.01};


DLMFTestAll//Options =
    {"Epsilon"->0.01};


DLMFTestPlot//Options = {
    "Epsilon"->0.01,
    Splice@Options@Plot
};


(* ::Subsection:: *)
(*DLMFTest*)


(* ::Subsubsection:: *)
(*Main*)


DLMFTest[rule1_,{zmin_,zmax_},opts:OptionsPattern[]] :=
    Module[ {rule,condition,randomArg,difference,ep},
        ep = OptionValue["Epsilon"];
        {rule,condition} =
            getRuleCondition[rule1];
        randomArg =
            getRandomArgument[condition,{zmin,zmax}];
        difference =
            (Hypergeometric2F1[a,b,c,z+ep I]/.randomArg)-(Hypergeometric2F1[a,b,c,z+ep I]/.rule/.randomArg);
        Chop[difference,0.1]
    ];


DLMFTestPlot//SetOptions[#,{
    PlotRange->{-20,20},
    AspectRatio->1,
    PlotLabels->{"Before","After","Difference"}
}]&;

DLMFTestPlot[rule1_,{zmin_,zmax_},opts:OptionsPattern[]] :=
    Module[ {rule,condition,randomArg,before,after,z,ep},
        ep = OptionValue["Epsilon"];
        {rule,condition} =
            getRuleCondition[rule1];
        randomArg =
            getRandomArgument[condition,{zmin,zmax}];
        before =
            Hypergeometric2F1[a,b,c,z+ep I]/.randomArg;
        after =
            Hypergeometric2F1[a,b,c,z+ep I]/.rule/.randomArg;
        Plot[
            {before,after,before-after}//Abs//Evaluate,
            {z,zmin,zmax},
            Evaluate@FilterRules[{opts,Options@DLMFTestPlot},Options@Plot]
        ]
    ];


DLMFTestAll[opts:OptionsPattern[]] :=
    Map[
        ToString[#]->testInInterval[#,OptionValue["Epsilon"]]&,
        {{-2,-1},{-1,-0.5},{-0.5,0},{0,0.5},{0.5,1},{1,2}}
    ]//Association//Transpose//Dataset[#,MaxItems->All]&;


(* ::Subsubsection:: *)
(*Helper*)


testInInterval[{zmin_,zmax_},ep_] :=
    DLMFData//KeyDrop[$DLMFDataExceptionList]//KeyValueMap[#1->DLMFTest[#1,{zmin,zmax},"Epsilon"->ep]&]//Association;


getRuleCondition[rule_] :=
    Module[ {rule1,condition1},
        condition1 = True;
        rule1 =
            ReplaceAll[
                DLMFData[rule],
                Verbatim[Condition][arg_,condition_]:>(
                    condition1 = ReplaceAll[condition,$privateConversionList];
                    arg
                )
            ];
        {rule1,condition1}
    ];


getRandomArgument[condition_,{zmin_,zmax_}] :=
    {a,b,c,z}->
        If[ condition===True,
            Join[RandomReal[{0,2},3],RandomReal[{zmin,zmax},1]],
            (*Else*)
            Quiet@RandomPoint@ImplicitRegion[condition,{{a,0,1},{b,0,1},{c,0,2},{z,zmin,zmax}}]
        ]//Thread;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
