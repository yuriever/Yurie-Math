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
    "deltaD[z, n]: δ^n(z) - Dirac delta function.";

deltaC::usage =
    "deltaC[z]: complex delta function.";

deltaK::usage =
    "deltaK[z]: Kronecker delta function.";


spower::usage =
    "spower[s][z, λ]: z_s^λ - signed power of degree λ."<>
    "\n"<>
    "spower[s][z, λ, n]: z_s^λ log^n z - signed power + log of degree λ."<>
    "\n"<>
    "Value[s]: Complex (I, -I), PlusMinus (\"+\", \"-\"), Abs (0, 1); Log (\"L+\", \"L-\", \"L0\", \"L1\")."<>
    "\n"<>
    "Hint: spower[\"L\"][z, n] is defined for negative integer n and has logarithmic behavior.";


rpower::usage =
    "rpower[s][z, λ]: λ-holomorphic signed power.";


(* ::Subsection:: *)
(*Function*)


spowerReduce::usage =
    "spowerReduce[expr]: reduce spower distributions.";

spowerNormal::usage =
    "spowerNormal[expr]: convert spower distributions to the associated function.";

spowerConvert::usage =
    "spowerConvert[type1 -> type2]: convert between different types of spower distributions."<>
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
(*Symbol*)


spower[s_][z_,λ_,0] :=
    spower[s][z,λ];

spower[s_][z_,0] :=
    1;


spower[I|-I][z_,n_Integer?NonNegative] :=
    Power[z,n];

spower[0][z_,n_Integer?NonNegative]/;EvenQ[n] :=
    Power[z,n];

spower[1][z_,n_Integer?NonNegative]/;OddQ[n] :=
    Power[z,n];


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

Derivative[0,n_,0][spower[s:"+"|"-"]][z_,λ_,m_] :=
    spower[s][z,λ,n+m];



Derivative[1,0][spower["L"]][z_,λ_] :=
    λ*spower["L"][z,λ-1]+(-1)^λ/(-λ)!*deltaD[z,-λ];

Derivative[1,0][deltaD][z_,λ_] :=
    deltaD[z,λ+1];


(* ::Subsection:: *)
(*spowerReduce*)


spowerReduce[expr_] :=
    expr//ReplaceRepeated[{
        (* Abs x Abs *)
        spower[s1:0|1][z_,λ1_]*spower[s2:0|1][z_,λ2_]/;s1==s2:>
            spower[0][z,λ1+λ2],
        spower[s1:0|1][z_,λ1_]*spower[s2:0|1][z_,λ2_]/;s1+s2==1:>
            spower[1][z,λ1+λ2],
        (* Abs x PlusMinus *)
        spower[0|1][z_,λ1_]*spower["+"][z_,λ2_]:>
            spower["+"][z,λ1+λ2],
        spower[s:0|1][z_,λ1_]*spower["-"][z_,λ2_]:>
            (-1)^s*spower["-"][z,λ1+λ2],
        (* PlusMinus x PlusMinus *)
        spower[s1:"+"|"-"][z_,λ1_]*spower[s2:"+"|"-"][z_,λ2_]/;s1==s2:>
            spower[s1][z,λ1+λ2],
        spower[s1:"+"|"-"][z_,λ1_]*spower[s2:"+"|"-"][z_,λ2_]/;s1!=s2:>
            0,
        (* Complex x Complex *)
        spower[s:_.*I|_.*-I][z_,λ1_]*spower[s_][z_,λ2_]:>
            spower[s][z,λ1+λ2]
    }];


(* ::Subsection:: *)
(*spowerNormal*)


spowerNormal[expr_] :=
    expr//ReplaceAll[{
        (* Complex *)
        spower[s:_.*I|_.*-I][z_,λ_]:>
            Power[z,λ],
        (* PlusMinus *)
        spower["+"][z_,λ_]:>
            Power[z,λ],
        spower["-"][z_,λ_]:>
            Power[-z,λ],
        (* Abs *)
        spower[0][z_,λ_]:>
            Power[Abs[z],λ],
        spower[1][z_,λ_]:>
            Power[Abs[z],λ]*Sign[z],
        (* Log *)
        spower["L"][z_,n_Integer?Negative]/;EvenQ[n]:>
            Power[Abs[z],n]*Sign[z];
        spower["L"][z_,n_Integer?Negative]/;OddQ[n]:>
            Power[Abs[z],n];
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
        spower[0][z_,λ_]:>
            1/2*Exp[-I/2*π*λ]*Sec[π/2*λ]*spower[I][z,λ]+
            1/2*Exp[I/2*π*λ]*Sec[π/2*λ]*spower[-I][z,λ],
        spower[1][z_,λ_]:>
            I/2*Exp[-I/2*π*λ]*Csc[π/2*λ]*spower[I][z,λ]+
            -I/2*Exp[I/2*π*λ]*Csc[π/2*λ]*spower[-I][z,λ]
    }];


spowerConvertCore[PlusMinus,Complex][expr_] :=
    expr//ReplaceAll[{
        spower["+"][z_,λ_]:>
            -I/2*Exp[I*π*λ]*Csc[π*λ]*spower[-I][z,λ]+
            I/2*Exp[-I*π*λ]*Csc[π*λ]*spower[I][z,λ],
        spower["-"][z_,λ_]:>
            I/2*Csc[π*λ]*spower[-I][z,λ]+
            -I/2*Csc[π*λ]*spower[I][z,λ]
    }];


(* ::Subsubsection:: *)
(*PlusMinus*)


spowerConvertCore[Abs,PlusMinus][expr_] :=
    expr//ReplaceAll[{
        spower[0][z_,λ_]:>
            spower["+"][z,λ]+spower["-"][z,λ],
        spower[1][z_,λ_]:>
            spower["+"][z,λ]-spower["-"][z,λ]
    }];


spowerConvertCore[Complex,PlusMinus][expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][z_,λ_]:>
            spower["+"][z,λ]+Exp[s*π*λ]*spower["-"][z,λ]
    }];


(* ::Subsubsection:: *)
(*Abs*)


spowerConvertCore[PlusMinus,Abs][expr_] :=
    expr//ReplaceAll[{
        spower["+"][z_,λ_]:>
            1/2*spower[0][z,λ]+1/2*spower[1][z,λ],
        spower["-"][z_,λ_]:>
            1/2*spower[0][z,λ]-1/2*spower[1][z,λ]
    }];


spowerConvertCore[Complex,Abs][expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][z_,λ_]:>
            Exp[s/2*π*λ]*Cos[-I/2*s*π*λ]*spower[0][z,λ]+
            -I*Exp[s/2*π*λ]*Sin[-I/2*s*π*λ]*spower[1][z,λ]
    }];


(* ::Subsubsection:: *)
(*Type reverse*)


(* Complex, PlusMinus, Abs *)


spowerConvertCore[Complex,{Complex,Reverse}][expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][z_,λ_]:>
            Exp[s*π*λ]*spower[-s][-z,λ]
    }];


spowerConvertCore[PlusMinus,{PlusMinus,Reverse}][expr_] :=
    expr//ReplaceAll[{
        spower["+"][z_,λ_]:>
            spower["-"][-z,λ],
        spower["-"][z_,λ_]:>
            spower["+"][-z,λ]
    }];


spowerConvertCore[Abs,{Abs,Reverse}][expr_] :=
    expr//ReplaceAll[{
        spower[0][z_,λ_]:>
            spower[0][-z,λ],
        spower[1][z_,λ_]:>
            -spower[1][-z,λ]
    }];


spowerConvertCore[type1:Complex|PlusMinus|Abs,{type2:Complex|PlusMinus|Abs,Reverse}][expr_]/;type1=!=type2 :=
    expr//spowerConvertCore[type1,type2]//spowerConvertCore[type2,{type2,Reverse}];


(* ::Subsubsection:: *)
(*iε prescription*)


spowerConvertCore[Complex,{Complex,ε:Except[Reverse]}][expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][z_,λ_]:>
            Power[z+s*ε,λ]
    }];


spowerConvertCore[Complex,{Complex,ε:Except[Reverse],Reverse}][expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][z_,λ_]:>
            Power[I*ε+s/I*z,λ]*Exp[1/2*I*π*λ*(-1-I*s)]
    }];


spowerConvertCore[type:PlusMinus|Abs,{Complex,ε:Except[Reverse],reverse___}][expr_] :=
    expr//spowerConvertCore[type,Complex]//spowerConvertCore[Complex,{Complex,ε,reverse}];


(* ::Subsubsection:: *)
(*Step function*)


spowerConvertCore[PlusMinus,HeavisideTheta][expr_] :=
    expr//ReplaceAll[{
        spower["+"][z_,λ_]:>
            Power[z,λ]*HeavisideTheta[z],
        spower["-"][z_,λ_]:>
            Power[-z,λ]*HeavisideTheta[-z]
    }];


spowerConvertCore[type:Complex|Abs,HeavisideTheta][expr_] :=
    expr//spowerConvertCore[type,PlusMinus]//spowerConvertCore[PlusMinus,HeavisideTheta];


(* ::Subsubsection:: *)
(*Abs function*)


spowerConvertCore[Abs,RealAbs][expr_] :=
    expr//ReplaceAll[{
        spower[0][z_,λ_]:>
            Power[Abs[z],λ],
        spower[1][z_,λ_]:>
            Power[Abs[z],λ]*Sign[z]
    }];


spowerConvertCore[type:Complex|PlusMinus,RealAbs][expr_] :=
    expr//spowerConvertCore[type,Abs]//spowerConvertCore[Abs,RealAbs];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
