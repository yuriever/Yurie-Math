(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Diff`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


PD::usage =
    "head of partial derivative that acts on the rest of the expression."

INT::usage =
    "head of integral that acts on the rest of the expression."

SUM::usage =
    "head of sum that acts on the rest of the expression."

PDCoefficient::usage =
    "collect the coefficients of PD[___].";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Message*)


PDCoefficient::nonlinear =
    "the expression is nonlinear with respect to PD[__]."

INT::duplicate =
    "the original expression contains duplicate integral(s) with respect to ``."

SUM::duplicate =
    "the original expression contains duplicate sum(s) with respect to ``.";


(* ::Subsection:: *)
(*Main*)


(* ::Subsubsection:: *)
(*PD*)


PD//Attributes =
    {Orderless};

head_PD/;System`Private`HoldNotValidQ[head] :=
    (
        System`Private`HoldSetValid[head];
        System`Private`HoldSetNoEntry[head]
    );


PD/:PD[x__]PD[y__]:=
    PD[x,y];

PD/:Power[PD[x__],n_Integer]/;n>=1:=
    PD@@Flatten@ConstantArray[{x},n];

PD/:PD[x__]Power[PD[y_,rest___],-1]/;MemberQ[{x},y]:=
    PD@@DeleteCases[{x},y,{1},1]/PD[rest];

PD[] :=
    1;


(* ::Subsubsection:: *)
(*INT*)


INT//Attributes =
    {Orderless};

head_INT/;System`Private`HoldNotValidQ[head] :=
    (
        Quiet[
            System`Private`HoldSetValid[head],
            {INT::duplicate}
        ];
        System`Private`HoldSetNoEntry[head]
    );


INT/:INT[x__]INT[y__]:=
    INT[x,y];

HoldPattern[INT][x__]/;!DuplicateFreeQ[{x}] :=
    (
        Message[
            INT::duplicate,
            Row[Select[Tally[{x}],Last[#]>=2&][[All,1]],","]
        ];
        INT@@DeleteDuplicates[{x}]
    );

INT/:INT[x__]Power[INT[y_,rest___],-1]/;MemberQ[{x},y]:=
    INT@@DeleteCases[{x},y,{1},1]/INT[rest];

INT[] :=
    1;


(* ::Subsubsection:: *)
(*SUM*)


SUM//Attributes =
    {Orderless};

head_SUM/;System`Private`HoldNotValidQ[head] :=
    (
        Quiet[
            System`Private`HoldSetValid[head],
            {SUM::duplicate}
        ];
        System`Private`HoldSetNoEntry[head]
    );


SUM/:SUM[x__]SUM[y__]:=
    SUM[x,y];

HoldPattern[SUM][x__]/;!DuplicateFreeQ[{x}] :=
    (
        Message[
            SUM::duplicate,
            Row[Select[Tally[{x}],Last[#]>=2&][[All,1]],","]
        ];
        SUM@@DeleteDuplicates[{x}]
    );

SUM/:SUM[x__]Power[SUM[y_,rest___],-1]/;MemberQ[{x},y]:=
    SUM@@DeleteCases[{x},y,{1},1]/SUM[rest];

SUM[] :=
    1;


(* ::Subsubsection:: *)
(*PDCoefficient*)


PDCoefficient[expr_] :=
    Module[ {expr1},
        If[ !PDLinearQ[expr],
            Message[PDCoefficient::nonlinear];
            Throw[expr]
        ];
        expr1 = Expand[expr];
        Join[
            Cases[expr1,PD[x__]*rest_.:>{{x},rest}],
            Cases[expr1,rest_/;FreeQ[rest,_PD]:>{{},rest}]
        ]//GatherBy[#,First]&//Map[Rule[#[[1,1]],Total[#[[All,2]]]]&]
    ]//Catch;


PDLinearQ[expr_] :=
    AllTrue[Cases[expr,_PD,All],Internal`LinearQ[expr,#]&];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
