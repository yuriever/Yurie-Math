(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Quest`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


isN::usage =
    "isN[x..]: test whether the arguments are natural numbers.";

isZ::usage =
    "isZ[x..]: test whether the arguments are integers.";

isZP::usage =
    "isZP[x..]: test whether the arguments are positive integers.";

isZN::usage =
    "isZN[x..]: test whether the arguments are negative integers.";

isZP0::usage =
    "isZP0[x..]: test whether the arguments are zero or positive integers.";

isZN0::usage =
    "isZN0[x..]: test whether the arguments are zero or negative integers.";


isQ::usage =
    "isQ[x..]: test whether the arguments are rationals.";


isR::usage =
    "isR[x..]: test whether the arguments are reals.";

isRP::usage =
    "isRP[x..]: test whether the arguments are positive reals.";

isRN::usage =
    "isRN[x..]: test whether the arguments are negative reals.";

isRP0::usage =
    "isRP0[x..]: test whether the arguments are zero or positive reals.";

isRN0::usage =
    "isRN0[x..]: test whether the arguments are zero or negative reals.";


isC::usage =
    "isC[x..]: test whether the arguments are complex numbers.";


levelQ::usage =
    "levelQ[level]: test whether the argument is a valid level specification."<>
    "\n"<>
    "Hint: All==={0,Infinity}, Infinity==={1,Infinity}, n_Integer==={1,n}, {_Integer}, {_Integer,_Integer}.";

presentQ::usage =
    "presentQ[pattern][expr]: test whether the pattern occurs in the expression."<>
    "\n"<>
    "Sketch: Not + FreeQ.";

linearQ::usage =
    "linearQ[expr, var]: test whether the expression is linear in the variable and the variable is present."<>
    "\n"<>
    "linearQ[expr, varList]: test linearity for all the variables.";

minusQ::usage =
    "minusQ[expr]: test whether the expression is syntactically negative.";

patternPresentQ::usage =
    "patternPresentQ[expr]: test whether any pattern construction occurs in the expression.";

patternFreeQ::usage =
    "patternFreeQ[expr]: test whether no pattern construction occurs in the expression.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


isN[x_] :=
    x>=0&&Element[x,Integers];

isN[x__] :=
    Map[#>=0&,And[x]]&&Element[Alternatives[x],Integers];

isN[] :=
    True;


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


levelQ[All|Infinity|_Integer|{_Integer}|{_Integer,_Integer}] :=
    True;

levelQ[_] :=
    False;


presentQ[expr_,args__] :=
    Not@FreeQ[expr,args];

presentQ[form_][expr_] :=
    Not@FreeQ[expr,form];


linearQ :=
    Internal`LinearQ;


minusQ :=
    Internal`SyntacticNegativeQ;


patternPresentQ :=
    Internal`PatternPresentQ;

patternFreeQ :=
    Internal`PatternFreeQ;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
