(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Quest`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


isZ::usage =
    "integers.";

isZP::usage =
    "positive integers";

isZN::usage =
    "negative integers";

isZP0::usage =
    "zero or positive integers";

isZN0::usage =
    "zero or negative integers";


isQ::usage =
    "rational numbers.";


isR::usage =
    "real numbers.";

isRP::usage =
    "positive real numbers.";

isRN::usage =
    "negative real numbers.";

isRP0::usage =
    "zero or positive real numbers.";

isRN0::usage =
    "zero or negative real numbers.";


isC::usage =
    "complex numbers.";


linearQ::usage =
    "whether the expression is linear with respect to the variables.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


isZ[x_] :=
    Element[x,Integers];

isZ[x__] :=
    Element[Alternatives[x],Integers];

isZ[] :=
    True;


isZP[x_] :=
    x>0&&Element[x,Integers];

isZP[x__] :=
    Map[#>0&,And[x]]&&Element[Alternatives[x],Integers];

isZP[] :=
    True;


isZN[x_] :=
    x<0&&Element[x,Integers];

isZN[x__] :=
    Map[#<0&,And[x]]&&Element[Alternatives[x],Integers];

isZN[] :=
    True;


isZP0[x_] :=
    x>=0&&Element[x,Integers];

isZP0[x__] :=
    Map[#>=0&,And[x]]&&Element[Alternatives[x],Integers];

isZP0[] :=
    True;


isZN0[x_] :=
    x<=0&&Element[x,Integers];

isZN0[x__] :=
    Map[#<=0&,And[x]]&&Element[Alternatives[x],Integers];

isZN0[] :=
    True;


isQ[x_] :=
    Element[x,Rationals];

isQ[x__] :=
    Element[Alternatives[x],Rationals];

isQ[] :=
    True;


isR[x_] :=
    Element[x,Reals];

isR[x__] :=
    Element[Alternatives[x],Reals];

isR[] :=
    True;


isRP[x_] :=
    x>0;

isRP[x__] :=
    Map[#>0&,And[x]];

isRP[] :=
    True;


isRN[x_] :=
    x<0;

isRN[x__] :=
    Map[#<0&,And[x]];

isRN[] :=
    True;


isRP0[x_] :=
    x>=0;

isRP0[x__] :=
    Map[#>=0&,And[x]];

isRP0[] :=
    True;


isRN0[x_] :=
    x<=0;

isRN0[x__] :=
    Map[#<=0&,And[x]];

isRN0[] :=
    True;


isC[x_] :=
    Element[x,Complexes];

isC[x__] :=
    Element[Alternatives[x],Complexes];

isC[] :=
    True;


(*Internal`LinearQ will return False if the expression does not contain the variables.*)

linearQ[vars__][expr_] :=
    Internal`LinearQ[expr,{vars}];

(*linearQ[vars__][expr_] :=
    PolynomialQ[expr,{vars}]&&
        Max@Total[GroebnerBasis`DistributedTermsList[expr,vars][[1,All,1]],{2}]<=1;*)


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
