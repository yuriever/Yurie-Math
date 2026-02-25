(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Distribution`"];


Needs["Yurie`Math`"];



(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Symbol*)


deltaD::usage =
    "Dirac delta function.";

deltaC::usage =
    "complex delta function.";

deltaK::usage =
    "Kronecker delta function.";


spower::usage =
    "spower[type][z, λ]: z^λ - signed power."<>
    "\n"<>
    "spower[type][z, λ, n]: z^λ log^n(z) - signed power + log."<>
    "\n"<>
    "Value[type]: Complex, PlusMinus, Abs; Log."<>
    "\n"<>
    "Hint: spower[Log][z, n] has logarithmic behavior.";


rpower::usage =
    "head: regularized power.";


(* ::Subsection:: *)
(*Function*)


spowerReduce::usage =
    "spowerReduce[expr]: reduce spower expressions.";

spowerNormal::usage =
    "spowerNormal[expr]: convert spower expressions to normal power expressions.";

spowerConvert::usage =
    "spowerConvert[type1 -> type2]: convert between different types of power-type distributions with head spower."<>
    "\n"<>
    "Value[type]: Complex, PlusMinus, Abs; {Complex, ε}, HeavisideTheta, RealAbs."<>
    "\n"<>
    "Hint: the types {Complex, PlusMinus, Abs} are invertible, while the others are not."<>
    "\n"<>
    "Hint: the tag Reverse is to reverse the parity.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*spower*)


spower[s_][base_,a_,0] :=
    spower[s][base,a];

spower[s_][base_,0] :=
    1;


Derivative[n_,0][spower["+"]][z_,λ_] :=
    FactorialPower[λ,n]*spower["+"][z,λ-n];

Derivative[n_,0][spower["-"]][z_,λ_] :=
    (-1)^n*FactorialPower[λ,n]*spower["-"][z,λ-n];

Derivative[n_,0][spower[s:I|-I]][z_,λ_] :=
    FactorialPower[λ,n]*spower[s][z,λ-n];

Derivative[n_,0][spower[s:0|1]][z_,λ_] :=
    FactorialPower[λ,n]*spower[Mod[s+n,2]][z,λ-n];


Derivative[0,n_][spower[s:"+"|"-"]][z_,λ_] :=
    spower[s][z,λ,n];

Derivative[1,0][spower[Log]][z_,λ_] :=
    With[{n = -λ},
        -n*spower[Log][z,-n-1]+(-1)^n/n!*deltaD[z,n]
    ];


(* ::Subsection:: *)
(*rpower*)


(*  *)


(* ::Subsection:: *)
(*spowerReduce*)


spowerReduce[expr_] :=
    expr//ReplaceRepeated[{
        spower[s_][base_,n1_]*spower[s_][base_,n2_]:>
            spower[s][base,n1+n2],
        Power[spower[s_][base_,n1_],n2_]:>
            spower[s][base,n1*n2],
        (*  *)
        spower[s1:0|1][base_,n1_]*spower[s2:0|1][base_,n2_]/;s1==s2:>
            spower[0][base,n1+n2],
        spower[s1:0|1][base_,n1_]*spower[s2:0|1][base_,n2_]/;s1+s2==1:>
            spower[1][base,n1+n2],
        (*  *)
        spower[0|1][base_,n1_]*spower["+"][base_,n2_]:>
            spower["+"][base,n1+n2],
        spower[s:0|1][base_,n1_]*spower["-"][base_,n2_]:>
            (-1)^s*spower["-"][base,n1+n2],
        (*  *)
        spower[s1:"+"|"-"][base_,n1_]*spower[s2:"+"|"-"][base_,n2_]/;s1==s2:>
            spower[s1][base,n1+n2],
        spower[s1:"+"|"-"][base_,n1_]*spower[s2:"+"|"-"][base_,n2_]/;s1!=s2:>
            0
    }]//ReplaceAll[{
        spower[_.*I|_.*-I][base_,n_Integer?NonNegative]:>Power[base,n]
    }];


(* ::Subsection:: *)
(*spowerNormal*)


spowerNormal[expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][base_,a_]:>
            Power[base,a],
        spower["+"][base_,a_]:>
            Power[base,a],
        spower["-"][base_,a_]:>
            Power[-base,a]
    }];


(* ::Subsection:: *)
(*spowerConvert*)


(* ::Subsubsection:: *)
(*Main*)


spowerConvert[Verbatim[Rule][type1_,type2_]][expr_] :=
    expr//spowerConvertCore[type1,type2];

spowerConvert[rules__Rule][expr_] :=
    Fold[
        spowerConvertCore[#2[[1]],#2[[2]]][#1]&,
        expr,
        {rules}
    ];


(* ::Subsubsection:: *)
(*Default behavior*)


spowerConvertCore[type_,type_][expr_] :=
    expr;


(* ::Subsubsection:: *)
(*Complex*)


spowerConvertCore[Abs,Complex][expr_] :=
    expr//ReplaceAll[{
        spower[0][base_,a_]:>
            1/2*Exp[-I/2*π*a]*Sec[π/2*a]*spower[I][base,a]+
            1/2*Exp[I/2*π*a]*Sec[π/2*a]*spower[-I][base,a],
        spower[1][base_,a_]:>
            I/2*Exp[-I/2*π*a]*Csc[π/2*a]*spower[I][base,a]+
            -I/2*Exp[I/2*π*a]*Csc[π/2*a]*spower[-I][base,a]
    }];


spowerConvertCore[PlusMinus,Complex][expr_] :=
    expr//ReplaceAll[{
        spower["+"][base_,a_]:>
            -I/2*Exp[I*π*a]*Csc[π*a]*spower[-I][base,a]+
            I/2*Exp[-I*π*a]*Csc[π*a]*spower[I][base,a],
        spower["-"][base_,a_]:>
            I/2*Csc[π*a]*spower[-I][base,a]+
            -I/2*Csc[π*a]*spower[I][base,a]
    }];


(* ::Subsubsection:: *)
(*PlusMinus*)


spowerConvertCore[Abs,PlusMinus][expr_] :=
    expr//ReplaceAll[{
        spower[0][base_,a_]:>
            spower["+"][base,a]+spower["-"][base,a],
        spower[1][base_,a_]:>
            spower["+"][base,a]-spower["-"][base,a]
    }];


spowerConvertCore[Complex,PlusMinus][expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][base_,a_]:>
            spower["+"][base,a]+Exp[s*π*a]*spower["-"][base,a]
    }];


(* ::Subsubsection:: *)
(*Abs*)


spowerConvertCore[PlusMinus,Abs][expr_] :=
    expr//ReplaceAll[{
        spower["+"][base_,a_]:>
            1/2*spower[0][base,a]+1/2*spower[1][base,a],
        spower["-"][base_,a_]:>
            1/2*spower[0][base,a]-1/2*spower[1][base,a]
    }];


spowerConvertCore[Complex,Abs][expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][base_,a_]:>
            Exp[s/2*π*a]*Cos[-I/2*s*π*a]*spower[0][base,a]+
            -I*Exp[s/2*π*a]*Sin[-I/2*s*π*a]*spower[1][base,a]
    }];


(* ::Subsubsection:: *)
(*Type reverse*)


(* Complex, PlusMinus, Abs *)


spowerConvertCore[Complex,{Complex,Reverse}][expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][base_,a_]:>
            Exp[s*π*a]*spower[-s][-base,a]
    }];


spowerConvertCore[PlusMinus,{PlusMinus,Reverse}][expr_] :=
    expr//ReplaceAll[{
        spower["+"][base_,a_]:>
            spower["-"][-base,a],
        spower["-"][base_,a_]:>
            spower["+"][-base,a]
    }];


spowerConvertCore[Abs,{Abs,Reverse}][expr_] :=
    expr//ReplaceAll[{
        spower[0][base_,a_]:>
            spower[0][-base,a],
        spower[1][base_,a_]:>
            -spower[1][-base,a]
    }];


spowerConvertCore[type1:Complex|PlusMinus|Abs,{type2:Complex|PlusMinus|Abs,Reverse}][expr_]/;type1=!=type2 :=
    expr//spowerConvertCore[type1,type2]//spowerConvertCore[type2,{type2,Reverse}];


(* ::Subsubsection:: *)
(*iε prescription*)


spowerConvertCore[Complex,{Complex,ε:Except[Reverse]}][expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][base_,a_]:>
            Power[base+s*ε,a]
    }];


spowerConvertCore[Complex,{Complex,ε:Except[Reverse],Reverse}][expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][base_,a_]:>
            Power[I*ε+s/I*base,a]*Exp[1/2*I*π*a*(-1-I*s)]
    }];


spowerConvertCore[type:PlusMinus|Abs,{Complex,ε:Except[Reverse],reverse___}][expr_] :=
    expr//spowerConvertCore[type,Complex]//spowerConvertCore[Complex,{Complex,ε,reverse}];


(* ::Subsubsection:: *)
(*Step function*)


spowerConvertCore[PlusMinus,HeavisideTheta][expr_] :=
    expr//ReplaceAll[{
        spower["+"][base_,a_]:>
            Power[base,a]*HeavisideTheta[base],
        spower["-"][base_,a_]:>
            Power[-base,a]*HeavisideTheta[-base]
    }];


spowerConvertCore[type:Complex|Abs,HeavisideTheta][expr_] :=
    expr//spowerConvertCore[type,PlusMinus]//spowerConvertCore[PlusMinus,HeavisideTheta];


(* ::Subsubsection:: *)
(*Abs function*)


spowerConvertCore[Abs,RealAbs][expr_] :=
    expr//ReplaceAll[{
        spower[0][base_,a_]:>
            Power[Abs[base],a],
        spower[1][base_,a_]:>
            Power[Abs[base],a]*Sign[base]
    }];


spowerConvertCore[type:Complex|PlusMinus,RealAbs][expr_] :=
    expr//spowerConvertCore[type,Abs]//spowerConvertCore[Abs,RealAbs];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
