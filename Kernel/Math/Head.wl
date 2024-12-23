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


(* ::Subsection:: *)
(*Main*)


(* ::Subsubsection:: *)
(*PD|INT*)


PD//Attributes =
    {Orderless};

pd_PD/;System`Private`HoldNotValidQ[pd] :=
    (
        System`Private`HoldSetValid[pd];
        System`Private`HoldSetNoEntry[pd]
    );


PD/:PD[x__]PD[y__]:=
    PD[x,y];

PD/:Power[PD[x__],n_Integer]/;n>=1:=
    PD@@Flatten@ConstantArray[{x},n];

PD/:PD[x__]Power[PD[y_,rest___],-1]/;MemberQ[{x},y]:=
    PD@@DeleteCases[{x},y,{1},1]/PD[rest];

PD[] :=
    1;


INT//Attributes =
    {Orderless};

int_INT/;System`Private`HoldNotValidQ[int] :=
    (
        System`Private`HoldSetValid[int];
        System`Private`HoldSetNoEntry[int]
    );


INT/:INT[x__]INT[y__]:=
    INT[x,y];

INT/:Power[INT[x__],n_Integer]/;n>=2:=
    INT@@Flatten@ConstantArray[{x},n];

INT/:INT[x__]Power[INT[y_,rest___],-1]/;MemberQ[{x},y]:=
    INT@@DeleteCases[{x},y,{1},1]/INT[rest];

INT[] :=
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
