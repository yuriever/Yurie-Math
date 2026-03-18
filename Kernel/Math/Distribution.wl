(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Distribution`"];


Needs["Yurie`Math`"];



(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Symbol*)


dist::usage =
    "dist[type, data][var]: internal representation of distributions/generalized functions."<>
    "\n"<>
    "Value[type]: deltaD, deltaC, deltaK; spower, spowerlog, rpower.";


spower::usage =
    "spower[s][z, λ]: z_s^λ - signed power."<>
    "\n"<>
    "Value[s]: Complex (I, -I), PlusMinus (\"+\", \"-\"), Abs (0, 1).";

spowerlog::usage =
    "spowerlog[s][z, λ, n]: z_s^λ log^n z - signed power + log."<>
    "\n"<>
    "Value[s]: Complex (I, -I), PlusMinus (\"+\", \"-\"), Abs (0, 1)."<>
    "\n"<>
    "Hint: for specific negative integer λ, spowerlog[s][z, λ, 0] is not spower[s][z, λ].";


rpower::usage =
    "rpower[s][z, λ]: λ-holomorphic signed power."<>
    "\n"<>
    "Value[s]: Complex (I, -I), PlusMinus (\"+\", \"-\"), Abs (0, 1).";


deltaD::usage =
    "deltaD[z, n]: δ^n(z) - Dirac delta function."<>
    "\n"<>
    "deltaD[{z, ...}, {n, ...}]: multi-variable Dirac delta function."<>
    "\n"<>
    "deltaD[{z, ...}, {n, ...}, tag]: Dirac delta function with tag.";

deltaC::usage =
    "deltaC[z]: complex delta function.";

deltaK::usage =
    "deltaK[z]: Kronecker delta function.";

step::usage =
    "step[z]: Heaviside step function.";


(* ::Subsection:: *)
(*Function*)


spowerStrip::usage =
    "spowerStrip[expr]: convert spower distributions to the associated function.";

spowerConvert::usage =
    "spowerConvert[type1 -> type2]: convert between different types of spower distributions."<>
    "\n"<>
    "Value[type]: Complex, PlusMinus, Abs; {Complex, ε}, HeavisideTheta, RealAbs."<>
    "\n"<>
    "Hint: the types {Complex, PlusMinus, Abs} are invertible, while the others are not."<>
    "\n"<>
    "Hint: the tag Reverse is to reverse the parity.";


rpowerFrom::usage =
    "rpowerFrom[type]: convert to rpower distributions.";

rpowerTo::usage =
    "rpowerTo[type, assume]: convert from rpower distributions under the assumption.";


distFromSys::usage =
    "distFromSys[expr]: convert the built-in Dirac delta distributions to deltaD.";

distToSys::usage =
    "distToSys[expr]: convert deltaD to the built-in Dirac delta distributions.";

distApart::usage =
    "distApart[expr]: take apart the Dirac delta distributions of several variables.";

distTogether::usage =
    "distTogether[expr]: take together the Dirac delta distributions of several variables.";

distReduce::usage =
    "distReduce[pattern][expr]: reduce the Dirac delta distributions in the expression.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*dist*)


(* ::Subsubsection:: *)
(*Exception*)


spowerlog::InvalidExponent =
    "Invalid exponent `2` and type `1` for rank-0 spowerlog. The exponent should be a specific negative integer according to the type.";

dist::Pole =
    "The distribution `1` has a pole at `2`.";


distExceptionPole//Attributes = {
    HoldAll
};

distExceptionPole[expr_,λ_] :=
    (
        Message[dist::Pole,HoldForm[expr],λ];
        Indeterminate
    );


distExceptionInvalidSPowerLog//Attributes = {
    HoldAll
};

distExceptionInvalidSPowerLog[expr_,s_,λ_] :=
    (
        Message[spowerlog::InvalidExponent,s,λ];
        HoldComplete[expr]
    );


(* ::Subsubsection:: *)
(*Interface*)


spower[s_][z_,λ_] :=
    dist[spower,s][z,λ];


spowerlog[s_][z_,λ_,n_] :=
    dist[spowerlog,s][z,λ,n];


rpower[s_][z_,λ_] :=
    dist[rpower,s][z,λ];


deltaD[z:Except[_List]] :=
    dist[deltaD,{0}][z];

deltaD[z:Except[_List],λ:Except[_List]] :=
    dist[deltaD,{λ}][z];

deltaD[z:Except[_List],λ:Except[_List],tag_] :=
    dist[deltaD,{λ},tag][z];

deltaD[{z__}] :=
    dist[deltaD,ConstantArray[0,Length[{z}]]][z];

deltaD[{z__},{λ__}] :=
    dist[deltaD,{λ}][z];

deltaD[{z__},{λ__},tag_] :=
    dist[deltaD,{λ},tag][z];


deltaC[z_] :=
    dist[deltaC][z];


deltaK[z_] :=
    dist[deltaK][z];


step[z:Except[_List]] :=
    dist[step][z];

step[{z__}] :=
    dist[step][z];


(* ::Subsubsection:: *)
(*Main*)


(* Pole exception. *)

expr:dist[spower,"+"|"-"][z_,λ_Integer?Negative] :=
    distExceptionPole[expr,λ];

expr:dist[spower,0][z_,λ_Integer?Negative]/;OddQ[λ] :=
    distExceptionPole[expr,λ];

expr:dist[spower,1][z_,λ_Integer?Negative]/;EvenQ[λ] :=
    distExceptionPole[expr,λ];

(* expr:dist[spowerlog,"+"|"-"][z_,λ_Integer?Negative,k_] :=
    distExceptionPole[expr,λ];

expr:dist[spowerlog,0][z_,λ_Integer?Negative,k_]/;OddQ[λ] :=
    distExceptionPole[expr,λ];

expr:dist[spowerlog,1][z_,λ_Integer?Negative,k_]/;EvenQ[λ] :=
    distExceptionPole[expr,λ]; *)


(* Reduction from spower to step for exponent 0. *)

dist[spower,s:_.*I|_.*-I][z_,0] :=
    1;

dist[spower,"+"][z_,0] :=
    dist[step][z];

dist[spower,"-"][z_,0] :=
    1-dist[step][z];

dist[spower,0][z_,0] :=
    1;

dist[spower,1][z_,0] :=
    2*dist[step][z]-1;


(* Invalid exponent/rank exception. *)

expr:dist[spowerlog,s:_.*I|_.*-I][z_,λ_,0] :=
    distExceptionInvalidSPowerLog[expr,s,λ];

expr:dist[spowerlog,s:"+"|"-"|0|1][z_,λ_?NumberQ,0]/;!IntegerQ[λ]||NonNegative[λ] :=
    distExceptionInvalidSPowerLog[expr,s,λ];

expr:dist[spowerlog,s:0][z_,λ_Integer?Negative,0]/;EvenQ[λ] :=
    distExceptionInvalidSPowerLog[expr,s,λ];

expr:dist[spowerlog,s:1][z_,λ_Integer?Negative,0]/;OddQ[λ] :=
    distExceptionInvalidSPowerLog[expr,s,λ];


(* ::Subsubsection:: *)
(*Derivative*)


Derivative[n_,0][dist[spower,s:_.*I|_.*-I]][z_,λ_] :=
    FactorialPower[λ,n]*dist[spower,s][z,λ-n];

Derivative[n_,0][dist[spower,"+"]][z_,λ_] :=
    FactorialPower[λ,n]*dist[spower,"+"][z,λ-n];

Derivative[n_,0][dist[spower,"-"]][z_,λ_] :=
    (-1)^n*FactorialPower[λ,n]*dist[spower,"-"][z,λ-n];

Derivative[n_,0][dist[spower,s:0|1]][z_,λ_] :=
    FactorialPower[λ,n]*dist[spower,Mod[s+n,2]][z,λ-n];


Derivative[0,n_][dist[spower,s:"+"|"-"]][z_,λ_] :=
    dist[spowerlog,s][z,λ,n];

Derivative[0,n_,0][dist[spowerlog,s:"+"|"-"]][z_,λ_,m_] :=
    dist[spowerlog,s][z,λ,n+m];


Derivative[1,0,0][dist[spowerlog,"+"]][z_,λ_,0] :=
    λ*dist[spowerlog,"+"][z,λ-1,0]+(-1)^λ/(-λ)!*dist[deltaD,{-λ}][z];

Derivative[1,0,0][dist[spowerlog,"-"]][z_,λ_,0] :=
    -λ*dist[spowerlog,"-"][z,λ-1,0]-1/(-λ)!*dist[deltaD,{-λ}][z];

Derivative[1,0,0][dist[spowerlog,0]][z_,λ_,0] :=
    λ*dist[spowerlog,1][z,λ-1,0]+((-1)^λ-1)/(-λ)!*dist[deltaD,{-λ}][z];

Derivative[1,0,0][dist[spowerlog,1]][z_,λ_,0] :=
    λ*dist[spowerlog,0][z,λ-1,0]+((-1)^λ+1)/(-λ)!*dist[deltaD,{-λ}][z];


Derivative[n__][dist[deltaD,λ_List]][z__] :=
    dist[deltaD,plusSafe[λ,{n}]][z];


Derivative[1][dist[step]][z_] :=
    dist[deltaD,{0}][z];


(* ::Subsection:: *)
(*spowerStrip*)


spowerStrip[expr_] :=
    expr//ReplaceAll[{
        (* Complex *)
        dist[spower,s:_.*I|_.*-I][z_,λ_]:>
            Power[z,λ],

        (* PlusMinus *)
        dist[spower,"+"][z_,λ_]:>
            Power[z,λ],
        dist[spower,"-"][z_,λ_]:>
            Power[-z,λ],

        (* Log, Complex *)
        dist[spowerlog,s:_.*I|_.*-I][z_,λ_,n_]:>
            Power[z,λ]*Log[z]^n,

        (* Log, PlusMinus *)
        dist[spowerlog,"+"][z_,λ_,n_]:>
            Power[z,λ]*Log[z]^n,
        dist[spowerlog,"-"][z_,λ_,n_]:>
            Power[-z,λ]*Log[-z]^n
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
        dist[spower,0][z_,λ_]:>
            1/2*Exp[-I/2*π*λ]*Sec[π/2*λ]*dist[spower,I][z,λ]+
            1/2*Exp[I/2*π*λ]*Sec[π/2*λ]*dist[spower,-I][z,λ],
        dist[spower,1][z_,λ_]:>
            I/2*Exp[-I/2*π*λ]*Csc[π/2*λ]*dist[spower,I][z,λ]+
            -I/2*Exp[I/2*π*λ]*Csc[π/2*λ]*dist[spower,-I][z,λ]
    }];


spowerConvertCore[PlusMinus,Complex][expr_] :=
    expr//ReplaceAll[{
        dist[spower,"+"][z_,λ_]:>
            -I/2*Exp[I*π*λ]*Csc[π*λ]*dist[spower,-I][z,λ]+
            I/2*Exp[-I*π*λ]*Csc[π*λ]*dist[spower,I][z,λ],
        dist[spower,"-"][z_,λ_]:>
            I/2*Csc[π*λ]*dist[spower,-I][z,λ]+
            -I/2*Csc[π*λ]*dist[spower,I][z,λ]
    }];


(* ::Subsubsection:: *)
(*PlusMinus*)


spowerConvertCore[Abs,PlusMinus][expr_] :=
    expr//ReplaceAll[{
        dist[spower,0][z_,λ_]:>
            dist[spower,"+"][z,λ]+dist[spower,"-"][z,λ],
        dist[spower,1][z_,λ_]:>
            dist[spower,"+"][z,λ]-dist[spower,"-"][z,λ]
    }];


spowerConvertCore[Complex,PlusMinus][expr_] :=
    expr//ReplaceAll[{
        dist[spower,s:_.*I|_.*-I][z_,λ_]:>
            dist[spower,"+"][z,λ]+Exp[s*π*λ]*dist[spower,"-"][z,λ]
    }];


(* ::Subsubsection:: *)
(*Abs*)


spowerConvertCore[PlusMinus,Abs][expr_] :=
    expr//ReplaceAll[{
        dist[spower,"+"][z_,λ_]:>
            1/2*dist[spower,0][z,λ]+1/2*dist[spower,1][z,λ],
        dist[spower,"-"][z_,λ_]:>
            1/2*dist[spower,0][z,λ]-1/2*dist[spower,1][z,λ]
    }];


spowerConvertCore[Complex,Abs][expr_] :=
    expr//ReplaceAll[{
        dist[spower,s:_.*I|_.*-I][z_,λ_]:>
            Exp[s/2*π*λ]*Cos[-I/2*s*π*λ]*dist[spower,0][z,λ]+
            -I*Exp[s/2*π*λ]*Sin[-I/2*s*π*λ]*dist[spower,1][z,λ]
    }];


(* ::Subsubsection:: *)
(*Type reverse*)


(* Complex, PlusMinus, Abs *)


spowerConvertCore[Complex,{Complex,Reverse}][expr_] :=
    expr//ReplaceAll[{
        dist[spower,s:_.*I|_.*-I][z_,λ_]:>
            Exp[s*π*λ]*dist[spower,-s][-z,λ]
    }];


spowerConvertCore[PlusMinus,{PlusMinus,Reverse}][expr_] :=
    expr//ReplaceAll[{
        dist[spower,"+"][z_,λ_]:>
            dist[spower,"-"][-z,λ],
        dist[spower,"-"][z_,λ_]:>
            dist[spower,"+"][-z,λ]
    }];


spowerConvertCore[Abs,{Abs,Reverse}][expr_] :=
    expr//ReplaceAll[{
        dist[spower,0][z_,λ_]:>
            dist[spower,0][-z,λ],
        dist[spower,1][z_,λ_]:>
            -dist[spower,1][-z,λ]
    }];


spowerConvertCore[type1:Complex|PlusMinus|Abs,{type2:Complex|PlusMinus|Abs,Reverse}][expr_]/;type1=!=type2 :=
    expr//spowerConvertCore[type1,type2]//spowerConvertCore[type2,{type2,Reverse}];


(* ::Subsubsection:: *)
(*iε prescription*)


spowerConvertCore[Complex,{Complex,ε:Except[Reverse]}][expr_] :=
    expr//ReplaceAll[{
        dist[spower,s:_.*I|_.*-I][z_,λ_]:>
            Power[z+s*ε,λ]
    }];


spowerConvertCore[Complex,{Complex,ε:Except[Reverse],Reverse}][expr_] :=
    expr//ReplaceAll[{
        dist[spower,s:_.*I|_.*-I][z_,λ_]:>
            Power[I*ε+s/I*z,λ]*Exp[1/2*I*π*λ*(-1-I*s)]
    }];


spowerConvertCore[type:PlusMinus|Abs,{Complex,ε:Except[Reverse],reverse___}][expr_] :=
    expr//spowerConvertCore[type,Complex]//spowerConvertCore[Complex,{Complex,ε,reverse}];


(* ::Subsubsection:: *)
(*Step function*)


spowerConvertCore[PlusMinus,HeavisideTheta][expr_] :=
    expr//ReplaceAll[{
        dist[spower,"+"][z_,λ_]:>
            Power[z,λ]*HeavisideTheta[z],
        dist[spower,"-"][z_,λ_]:>
            Power[-z,λ]*HeavisideTheta[-z]
    }];


spowerConvertCore[type:Complex|Abs,HeavisideTheta][expr_] :=
    expr//spowerConvertCore[type,PlusMinus]//spowerConvertCore[PlusMinus,HeavisideTheta];


(* ::Subsubsection:: *)
(*Abs function*)


spowerConvertCore[Abs,RealAbs][expr_] :=
    expr//ReplaceAll[{
        dist[spower,0][z_,λ_]:>
            Power[Abs[z],λ],
        dist[spower,1][z_,λ_]:>
            Power[Abs[z],λ]*Sign[z]
    }];


spowerConvertCore[type:Complex|PlusMinus,RealAbs][expr_] :=
    expr//spowerConvertCore[type,Abs]//spowerConvertCore[Abs,RealAbs];


(* ::Subsection:: *)
(*rpowerFrom|rpowerTo*)


(* ::Subsubsection:: *)
(*Helper*)


$rpowerTypeP =
    Complex|PlusMinus|Abs;


gammaS[arg_] :=
    Gamma@Simplify@arg;


(* ::Subsubsection:: *)
(*Main*)


rpowerFrom[][expr_] :=
    expr//
        rpowerFromKernel[Complex]//
        rpowerFromKernel[PlusMinus]//
        rpowerFromKernel[Abs];

rpowerFrom[type:$rpowerTypeP][expr_] :=
    expr//rpowerFromKernel[type];

rpowerFrom[typeList:{$rpowerTypeP..}][expr_] :=
    Fold[
        rpowerFromKernel[#2][#1]&,
        expr,
        typeList
    ];


rpowerTo[assume_:True][expr_] :=
    expr//
        rpowerToKernel[Complex,assume]//
        rpowerToKernel[PlusMinus,assume]//
        rpowerToKernel[Abs,assume];

rpowerTo[type:$rpowerTypeP,assume_:True][expr_] :=
    expr//rpowerToKernel[type,assume];

rpowerTo[typeList:{$rpowerTypeP..},assume_:True][expr_] :=
    Fold[
        rpowerToKernel[#2,assume][#1]&,
        expr,
        typeList
    ];


(* ::Subsubsection:: *)
(*Kernel*)


rpowerFromKernel[Complex][expr_] :=
    expr//ReplaceAll[{
        spower[s:_.*I|_.*-I][base_,a_]:>
            rpower[s][base,a]
    }];

rpowerToKernel[Complex,assume_][expr_] :=
    expr//ReplaceAll[{
        rpower[I][base_,a_]:>
            With[{n = Simplify[-a-1,assume]},
                If[Simplify[n>=0&&Element[n,Integers],assume]===True,
                    (* Then *)
                    base^(-n-1)-I*π*(-1)^n/n!*deltaD[base,n],
                    (* Else *)
                    spower[I][base,a]
                ]
            ],
        rpower[-I][base_,a_]:>
            With[{n = Simplify[-a-1,assume]},
                If[Simplify[n>=0&&Element[n,Integers],assume]===True,
                    (* Then *)
                    base^(-n-1)+I*π*(-1)^n/n!*deltaD[base,n],
                    (* Else *)
                    spower[-I][base,a]
                ]
            ]
    }];


rpowerFromKernel[PlusMinus][expr_] :=
    expr//ReplaceAll[{
        spower[s:"+"|"-"][base_,a_]:>
            gammaS[a+1]*rpower[s][base,a]
    }];

rpowerToKernel[PlusMinus,assume_][expr_] :=
    expr//ReplaceAll[{
        rpower["+"][base_,a_]:>
            With[{n = Simplify[-a-1,assume]},
                If[Simplify[n>=0&&Element[n,Integers],assume]===True,
                    (* Then *)
                    deltaD[base,n],
                    (* Else *)
                    1/gammaS[a+1]*spower["+"][base,a]
                ]
            ],
        rpower["-"][base_,a_]:>
            With[{n = Simplify[-a-1,assume]},
                If[Simplify[n>=0&&Element[n,Integers],assume]===True,
                    (* Then *)
                    (-1)^n*deltaD[base,n],
                    (* Else *)
                    1/gammaS[a+1]*spower["-"][base,a]
                ]
            ]
    }];


rpowerFromKernel[Abs][expr_] :=
    expr//ReplaceAll[{
        spower[0][base_,a_]:>
            gammaS[(a+1)/2]*rpower[0][base,a],
        spower[1][base_,a_]:>
            gammaS[(a+2)/2]*rpower[1][base,a]
    }];

rpowerToKernel[Abs,assume_][expr_] :=
    expr//ReplaceAll[{
        rpower[0][base_,a_]:>
            With[{n = Simplify[-(a+1)/2,assume]},
                If[Simplify[n>=0&&Element[n,Integers],assume]===True,
                    (* Then *)
                    (-1)^n*n!/(2*n)!*deltaD[base,2*n],
                    (* Else *)
                    1/gammaS[(a+1)/2]*spower[0][base,a]
                ]
            ],
        rpower[1][base_,a_]:>
            With[{n = Simplify[-(a+2)/2,assume]},
                If[Simplify[n>=0&&Element[n,Integers],assume]===True,
                    (* Then *)
                    (-1)^(n+1)*n!/(2*n+1)!*deltaD[base,2*n+1],
                    (* Else *)
                    1/gammaS[(a+2)/2]*spower[1][base,a]
                ]
            ]
    }];


(* ::Subsection:: *)
(*distFromSys|distToSys*)


distFromSys[expr_] :=
    expr//ReplaceAll[{
        DiracDelta[z__]:>
            dist[deltaD,ConstantArray[0,Length[{z}]]][z],
        Derivative[λ__][DiracDelta][z__]:>
            dist[deltaD,{λ}][z],
        HeavisideTheta[z__]:>
            dist[step][z]
    }];


distToSys[expr_] :=
    expr//ReplaceAll[{
        dist[deltaD,{λ__}][z__]:>
            Derivative[λ][DiracDelta][z],
        dist[step][z__]:>
            HeavisideTheta[z]
    }];


(* ::Subsection:: *)
(*distApart*)


distApart[expr_] :=
    expr//distApartKernel;


distApartKernel[expr_] :=
    expr//ReplaceAll[{
        DiracDelta[args__]:>
            Times@@Map[DiracDelta,{args}],
        Derivative[orders__][DiracDelta][args__]:>
            Times@@MapThread[Derivative[#1][DiracDelta][#2]&,{{orders},{args}}],
        HeavisideTheta[args__]:>
            Times@@Map[HeavisideTheta,{args}],

        dist[deltaD,{orders__}][vars__]:>
            Times@@MapThread[dist[deltaD,{#1}][#2]&,{{orders},{vars}}],
        dist[step][args__]:>
            Times@@Map[dist[step],{args}]
    }];


(* ::Subsection:: *)
(*distTogether*)


distTogether[expr_] :=
    expr//
        distTogetherKernel[DiracDelta]//
        distTogetherKernel[deltaD]//
        distTogetherKernel[HeavisideTheta]//
        distTogetherKernel[dist[step]];


distTogetherKernel[DiracDelta][expr_] :=
    expr//ReplaceAll[{
        Verbatim[Times][factors__]/;!FreeQ[{factors},DiracDelta]:>
            With[{
                    deltaList = Cases[{factors},DiracDelta[vars__]:>{Table[0,Length[{vars}]],{vars}}],
                    deltaDList = Cases[{factors},Derivative[orders__][DiracDelta][vars__]:>{{orders},{vars}}],
                    rest = Times@@DeleteCases[{factors},DiracDelta[__]|Derivative[__][DiracDelta][__]]
                },
                {
                    vars = Sequence@@Flatten[{deltaList[[All,2]],deltaDList[[All,2]]},2],
                    orders = Sequence@@Flatten[{deltaList[[All,1]],deltaDList[[All,1]]},2]
                },
                Derivative[orders][DiracDelta][vars]*rest
            ]
    }];

distTogetherKernel[deltaD][expr_] :=
    expr//ReplaceAll[{
        Verbatim[Times][factors__]/;!FreeQ[{factors},dist[deltaD,_List]]:>
            With[{
                    deltaDList = Cases[{factors},dist[deltaD,{orders__}][vars__]:>{{orders},{vars}}],
                    rest = Times@@DeleteCases[{factors},dist[deltaD,_List][__]]
                },
                {
                    vars = Sequence@@Flatten[{deltaDList[[All,2]]},2],
                    orderList = Flatten[{deltaDList[[All,1]]},2]
                },
                dist[deltaD,orderList][vars]*rest
            ]
    }];

distTogetherKernel[funP_][expr_] :=
    expr//ReplaceAll[{
        Verbatim[Times][factors__]/;!FreeQ[{factors},funP]:>
            With[{
                    stepList = Cases[{factors},funP[vars__]:>{vars}],
                    rest = Times@@DeleteCases[{factors},funP[__]]
                },
                {
                    vars = Sequence@@Flatten[stepList,2]
                },
                funP[vars]*rest
            ]
    }];


(* ::Subsection:: *)
(*distReduce*)


distReduce//Options = {
    "DistTogether"->True,
    "RegularTestFunction"->False
};


distReduce[All|PatternSequence[]|Verbatim[Blank[]],opts:OptionsPattern[]][expr_] :=
    expr//distReduceKernel[_,OptionValue["DistTogether"],OptionValue["RegularTestFunction"]];

distReduce[varP_,opts:OptionsPattern[]][expr_] :=
    expr//distReduceKernel[varP,OptionValue["DistTogether"],OptionValue["RegularTestFunction"]];


distReduceKernel[varP_,ifMergeDist_?BooleanQ,ifTestRegular_?BooleanQ][expr_] :=
    expr//
        distApart//
        deltaReduceKernel[varP,ifTestRegular]//
        spowerReduceKernel[varP]//
        distIfTogether[ifMergeDist];


distIfTogether[True][expr_] :=
    distTogether[expr];

distIfTogether[False][expr_] :=
    expr;


(* ::Subsubsection:: *)
(*deltaD*)


deltaReduceKernel[varP_,ifTestRegular_][expr_] :=
    expr//
        deltaRescale[varP]//
        deltaExpandRelevant[varP]//
        ReplaceRepeated[deltaReduceRule[varP,ifTestRegular]];


deltaRescale[p_][expr_] :=
    expr//ReplaceAll[{
        DiracDelta[a_.*x:p]:>
            Sign[a]*a^(-1)*DiracDelta[x],
        Derivative[n_][DiracDelta][a_.*x:p]:>
            Sign[a]*a^(-n-1)*Derivative[n][DiracDelta][x],
        dist[deltaD,{n_}][a_.*x:p]:>
            Sign[a]*a^(-n-1)*dist[deltaD,{n}][x]
    }];


deltaExpandRelevant[varP_][expr_] :=
    With[
        {
            varP1 = Alternatives@@Cases[deltaGetVarListInDelta[expr],varP,{1}]
        },
        Expand[expr,varP1]
    ];

deltaExpandRelevant[Verbatim[Blank][]][expr_] :=
    With[{
            varP = Alternatives@@deltaGetVarListInDelta[expr]
        },
        Expand[expr,varP]
    ];

deltaGetVarListInDelta[expr_] :=
    Cases[
        expr,
        Derivative[__][DiracDelta][vars__]|DiracDelta[vars__]|dist[deltaD,_List][vars__]:>
            {vars},
        All
    ]//Flatten//DeleteDuplicates;


deltaReduceRule[p_,ifTestRegular_] :=
    deltaReduceRule[Verbatim[p],ifTestRegular] =
    {
        Power[x:p,n_.]*(DiracDelta[x:p]|dist[deltaD,{0}][x:p])*rest_./;Simplify[n>=1]&&isTestRegular[ifTestRegular][rest,x]:>
            0,
        Power[x:p,n_.]*Derivative[m_][DiracDelta][x:p]*rest_./;Simplify[n>=1&&m>=1]&&isTestRegular[ifTestRegular][rest,x]:>
            If[Simplify[n<=m],
                (* Then *)
                (-1)^n*FactorialPower[m,n]*Derivative[m-n][DiracDelta][x]*rest,
                (* Else *)
                0,
                (* Final *)
                -m*Power[x,n-1]*Derivative[m-1][DiracDelta][x]*rest
            ],
        Power[x:p,n_.]*dist[deltaD,{m_}][x:p]*rest_./;Simplify[n>=1&&m>=1]&&isTestRegular[ifTestRegular][rest,x]:>
            If[Simplify[n<=m],
                (* Then *)
                (-1)^n*FactorialPower[m,n]*dist[deltaD,{m-n}][x]*rest,
                (* Else *)
                0,
                (* Final *)
                -m*Power[x,n-1]*dist[deltaD,{m-1}][x]*rest
            ]
    };


isTestRegular[True][expr_,var_] :=
    True;

isTestRegular[False][expr_,var_] :=
    FreeQ[expr,var];


(* ::Subsubsection:: *)
(*spower*)


spowerReduceKernel[varP_][expr_] :=
    expr//ReplaceRepeated[spowerTogetherRule[varP]];


spowerTogetherRule[p_] :=
    spowerTogetherRule[Verbatim[p]] =
    {
        (* Complex x Complex *)
        dist[spower,s:_.*I|_.*-I][z:p,λ1_]*dist[spower,s_][z:p,λ2_]:>
            dist[spower,s][z,λ1+λ2],

        (* PlusMinus x PlusMinus *)
        dist[spower,s1:"+"|"-"][z:p,λ1_]*dist[spower,s2:"+"|"-"][z:p,λ2_]/;s1==s2:>
            dist[spower,s1][z,λ1+λ2],
        dist[spower,s1:"+"|"-"][z:p,λ1_]*dist[spower,s2:"+"|"-"][z:p,λ2_]/;s1!=s2:>
            0,

        (* Abs x Abs *)
        dist[spower,s1:0|1][z:p,λ1_]*dist[spower,s2:0|1][z:p,λ2_]/;s1==s2:>
            dist[spower,0][z,λ1+λ2],
        dist[spower,s1:0|1][z:p,λ1_]*dist[spower,s2:0|1][z:p,λ2_]/;s1+s2==1:>
            dist[spower,1][z,λ1+λ2],

        (* Abs x PlusMinus *)
        dist[spower,0|1][z:p,λ1_]*dist[spower,"+"][z:p,λ2_]:>
            dist[spower,"+"][z,λ1+λ2],
        dist[spower,s:0|1][z:p,λ1_]*dist[spower,"-"][z:p,λ2_]:>
            (-1)^s*dist[spower,"-"][z,λ1+λ2]
    };


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
