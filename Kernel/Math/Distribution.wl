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
    "Value[type]: \"DeltaD\", \"DeltaC\", \"DeltaK\", \"PowerS\", \"PowerLogS\", \"PowerR\".";


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
    "rpower[s][z, λ]: λ-holomorphic signed power.";


(* ::Subsection:: *)
(*Function*)


spowerReduce::usage =
    "spowerReduce[expr]: reduce spower distributions.";

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
    "rpowerFrom[pattern]: convert to rpower distribution with the specified base.";


deltaFromDirac::usage =
    "deltaFromDirac[expr]: convert the built-in Dirac delta distributions to deltaD.";

deltaToDirac::usage =
    "deltaToDirac[expr]: convert deltaD to the built-in Dirac delta distributions.";

deltaApart::usage =
    "deltaApart[expr]: take apart the Dirac delta distributions of several variables.";

deltaTogether::usage =
    "deltaTogether[expr]: take together the Dirac delta distributions of several variables.";

deltaReduce::usage =
    "deltaReduce[pattern][expr]: reduce the Dirac delta distributions in the expression.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


Needs["Yurie`Base`"];

ClearAll[dist,deltaD,deltaC,deltaK,spower,spowerlog,rpower,Derivative];


(* ::Subsection:: *)
(*dist*)


(* ::Subsubsection:: *)
(*Message*)


spowerlog::InvalidExponent =
    "For spowerlog of rank 0, the exponent `1` should be a negative integer.";


(* ::Subsubsection:: *)
(*Interface*)


spower[s_][z_,λ_] :=
    dist["PowerS",s][z,λ];

spowerlog[s_][z_,λ_,n_] :=
    dist["PowerLogS",s][z,λ,n];


deltaD[z:Except[_List]] :=
    dist["DeltaD",{0}][z];

deltaD[z:Except[_List],λ:Except[_List]] :=
    dist["DeltaD",{λ}][z];

deltaD[z:Except[_List],λ:Except[_List],tag_] :=
    dist["DeltaD",{λ},tag][z];


deltaD[{z__}] :=
    dist["DeltaD",ConstantArray[0,Length[{z}]]][z];

deltaD[{z__},{λ__}] :=
    dist["DeltaD",{λ}][z];

deltaD[{z__},{λ__},tag_] :=
    dist["DeltaD",{λ},tag][z];


deltaC[z:Except[_List]] :=
    dist["DeltaC",{0}][z];

deltaK[z:Except[_List]] :=
    dist["DeltaK",{0}][z];


(* ::Subsubsection:: *)
(*Main*)


dist["PowerS",s_][z_,0] :=
    1;


dist["PowerLogS",s:_.*I|_.*-I][z_,λ_,0] :=
    dist["PowerS",s][z,λ];

dist["PowerLogS",0][z_,λ_Integer?Negative,0]/;EvenQ[λ] :=
    dist["PowerS",0][z,λ];

dist["PowerLogS",1][z_,λ_Integer?Negative,0]/;OddQ[λ] :=
    dist["PowerS",1][z,λ];

dist["PowerLogS",s_][z_,λ_?NumberQ,0]/;!IntegerQ[λ]||NonNegative[λ] :=
    (
        Message[spowerlog::InvalidExponent,λ];
        HoldComplete[dist["PowerLogS",s][z,λ,0]]
    );


(* ::Subsubsection:: *)
(*Derivative*)


Derivative[n_,0][dist["PowerS","+"]][z_,λ_] :=
    FactorialPower[λ,n]*dist["PowerS","+"][z,λ-n];

Derivative[n_,0][dist["PowerS","-"]][z_,λ_] :=
    (-1)^n*FactorialPower[λ,n]*dist["PowerS","-"][z,λ-n];

Derivative[n_,0][dist["PowerS",s:0|1]][z_,λ_] :=
    FactorialPower[λ,n]*dist["PowerS",Mod[s+n,2]][z,λ-n];

Derivative[n_,0][dist["PowerS",s:_.*I|_.*-I]][z_,λ_] :=
    FactorialPower[λ,n]*dist["PowerS",s][z,λ-n];


Derivative[0,n_][dist["PowerS",s:"+"|"-"]][z_,λ_] :=
    dist["PowerLogS",s][z,λ,n];

Derivative[0,n_,0][dist["PowerLogS",s:"+"|"-"]][z_,λ_,m_] :=
    dist["PowerLogS",s][z,λ,n+m];


Derivative[1,0,0][dist["PowerLogS","+"]][z_,λ_,0] :=
    λ*dist["PowerLogS","+"][z,λ-1,0]+(-1)^λ/(-λ)!*dist["DeltaD",{-λ}][z];

Derivative[1,0,0][dist["PowerLogS","-"]][z_,λ_,0] :=
    -λ*dist["PowerLogS","-"][z,λ-1,0]-1/(-λ)!*dist["DeltaD",{-λ}][z];

Derivative[1,0,0][dist["PowerLogS",0]][z_,λ_,0] :=
    λ*dist["PowerLogS",1][z,λ-1,0]+((-1)^λ-1)/(-λ)!*dist["DeltaD",{-λ}][z];

Derivative[1,0,0][dist["PowerLogS",1]][z_,λ_,0] :=
    λ*dist["PowerLogS",0][z,λ-1,0]+((-1)^λ+1)/(-λ)!*dist["DeltaD",{-λ}][z];


Derivative[n__][dist["DeltaD",λ_List]][z__] :=
    dist["DeltaD",plusSafe[λ,{n}]][z];


(* ::Subsection:: *)
(*spowerReduce*)


spowerReduce[expr_] :=
    expr//
        spowerReduceProduct//
        spowerReduceSpecialValue;


spowerReduceProduct[expr_] :=
    expr//ReplaceRepeated[{
        (* Abs x Abs *)
        dist["PowerS",s1:0|1][z_,λ1_]*dist["PowerS",s2:0|1][z_,λ2_]/;s1==s2:>
            dist["PowerS",0][z,λ1+λ2],
        dist["PowerS",s1:0|1][z_,λ1_]*dist["PowerS",s2:0|1][z_,λ2_]/;s1+s2==1:>
            dist["PowerS",1][z,λ1+λ2],

        (* Abs x PlusMinus *)
        dist["PowerS",0|1][z_,λ1_]*dist["PowerS","+"][z_,λ2_]:>
            dist["PowerS","+"][z,λ1+λ2],
        dist["PowerS",s:0|1][z_,λ1_]*dist["PowerS","-"][z_,λ2_]:>
            (-1)^s*dist["PowerS","-"][z,λ1+λ2],

        (* PlusMinus x PlusMinus *)
        dist["PowerS",s1:"+"|"-"][z_,λ1_]*dist["PowerS",s2:"+"|"-"][z_,λ2_]/;s1==s2:>
            dist["PowerS",s1][z,λ1+λ2],
        dist["PowerS",s1:"+"|"-"][z_,λ1_]*dist["PowerS",s2:"+"|"-"][z_,λ2_]/;s1!=s2:>
            0,

        (* Complex x Complex *)
        dist["PowerS",s:_.*I|_.*-I][z_,λ1_]*dist["PowerS",s_][z_,λ2_]:>
            dist["PowerS",s][z,λ1+λ2]
    }];

spowerReduceSpecialValue[expr_] :=
    expr//ReplaceAll[{
        dist["PowerS",s:_.*I|_.*-I][z_,n_Integer?NonNegative]:>
            Power[z,n],
        dist["PowerS",0][z_,n_Integer?NonNegative]/;EvenQ[n]:>
            Power[z,n],
        dist["PowerS",1][z_,n_Integer?NonNegative]/;OddQ[n]:>
            Power[z,n]
    }];


(* ::Subsection:: *)
(*spowerStrip*)


spowerStrip[expr_] :=
    expr//ReplaceAll[{
        (* PlusMinus *)
        dist["PowerS","+"][z_,λ_]:>
            Power[z,λ],
        dist["PowerS","-"][z_,λ_]:>
            Power[-z,λ],

        (* Abs *)
        dist["PowerS",0][z_,λ_]:>
            Power[Abs[z],λ],
        dist["PowerS",1][z_,λ_]:>
            Power[Abs[z],λ]*Sign[z],

        (* Complex *)
        dist["PowerS",s:_.*I|_.*-I][z_,λ_]:>
            Power[z,λ],

        (* Log, PlusMinus *)
        dist["PowerLogS","+"][z_,λ_,n_]:>
            Power[z,λ]*Log[z]^n,
        dist["PowerLogS","-"][z_,λ_,n_]:>
            Power[-z,λ]*Log[-z]^n,

        (* Log, Abs *)
        dist["PowerLogS",0][z_,λ_,n_]:>
            Power[Abs[z],λ]*Log[z]^n,
        dist["PowerLogS",1][z_,λ_,n_]:>
            Power[Abs[z],λ]*Log[z]^n*Sign[z],

        (* Log, Complex *)
        dist["PowerLogS",s:_.*I|_.*-I][z_,λ_,n_]:>
            Power[z,λ]*Log[z]^n
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
        dist["PowerS",0][z_,λ_]:>
            1/2*Exp[-I/2*π*λ]*Sec[π/2*λ]*dist["PowerS",I][z,λ]+
            1/2*Exp[I/2*π*λ]*Sec[π/2*λ]*dist["PowerS",-I][z,λ],
        dist["PowerS",1][z_,λ_]:>
            I/2*Exp[-I/2*π*λ]*Csc[π/2*λ]*dist["PowerS",I][z,λ]+
            -I/2*Exp[I/2*π*λ]*Csc[π/2*λ]*dist["PowerS",-I][z,λ]
    }];


spowerConvertCore[PlusMinus,Complex][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS","+"][z_,λ_]:>
            -I/2*Exp[I*π*λ]*Csc[π*λ]*dist["PowerS",-I][z,λ]+
            I/2*Exp[-I*π*λ]*Csc[π*λ]*dist["PowerS",I][z,λ],
        dist["PowerS","-"][z_,λ_]:>
            I/2*Csc[π*λ]*dist["PowerS",-I][z,λ]+
            -I/2*Csc[π*λ]*dist["PowerS",I][z,λ]
    }];


(* ::Subsubsection:: *)
(*PlusMinus*)


spowerConvertCore[Abs,PlusMinus][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS",0][z_,λ_]:>
            dist["PowerS","+"][z,λ]+dist["PowerS","-"][z,λ],
        dist["PowerS",1][z_,λ_]:>
            dist["PowerS","+"][z,λ]-dist["PowerS","-"][z,λ]
    }];


spowerConvertCore[Complex,PlusMinus][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS",s:_.*I|_.*-I][z_,λ_]:>
            dist["PowerS","+"][z,λ]+Exp[s*π*λ]*dist["PowerS","-"][z,λ]
    }];


(* ::Subsubsection:: *)
(*Abs*)


spowerConvertCore[PlusMinus,Abs][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS","+"][z_,λ_]:>
            1/2*dist["PowerS",0][z,λ]+1/2*dist["PowerS",1][z,λ],
        dist["PowerS","-"][z_,λ_]:>
            1/2*dist["PowerS",0][z,λ]-1/2*dist["PowerS",1][z,λ]
    }];


spowerConvertCore[Complex,Abs][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS",s:_.*I|_.*-I][z_,λ_]:>
            Exp[s/2*π*λ]*Cos[-I/2*s*π*λ]*dist["PowerS",0][z,λ]+
            -I*Exp[s/2*π*λ]*Sin[-I/2*s*π*λ]*dist["PowerS",1][z,λ]
    }];


(* ::Subsubsection:: *)
(*Type reverse*)


(* Complex, PlusMinus, Abs *)


spowerConvertCore[Complex,{Complex,Reverse}][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS",s:_.*I|_.*-I][z_,λ_]:>
            Exp[s*π*λ]*dist["PowerS",-s][-z,λ]
    }];


spowerConvertCore[PlusMinus,{PlusMinus,Reverse}][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS","+"][z_,λ_]:>
            dist["PowerS","-"][-z,λ],
        dist["PowerS","-"][z_,λ_]:>
            dist["PowerS","+"][-z,λ]
    }];


spowerConvertCore[Abs,{Abs,Reverse}][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS",0][z_,λ_]:>
            dist["PowerS",0][-z,λ],
        dist["PowerS",1][z_,λ_]:>
            -dist["PowerS",1][-z,λ]
    }];


spowerConvertCore[type1:Complex|PlusMinus|Abs,{type2:Complex|PlusMinus|Abs,Reverse}][expr_]/;type1=!=type2 :=
    expr//spowerConvertCore[type1,type2]//spowerConvertCore[type2,{type2,Reverse}];


(* ::Subsubsection:: *)
(*iε prescription*)


spowerConvertCore[Complex,{Complex,ε:Except[Reverse]}][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS",s:_.*I|_.*-I][z_,λ_]:>
            Power[z+s*ε,λ]
    }];


spowerConvertCore[Complex,{Complex,ε:Except[Reverse],Reverse}][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS",s:_.*I|_.*-I][z_,λ_]:>
            Power[I*ε+s/I*z,λ]*Exp[1/2*I*π*λ*(-1-I*s)]
    }];


spowerConvertCore[type:PlusMinus|Abs,{Complex,ε:Except[Reverse],reverse___}][expr_] :=
    expr//spowerConvertCore[type,Complex]//spowerConvertCore[Complex,{Complex,ε,reverse}];


(* ::Subsubsection:: *)
(*Step function*)


spowerConvertCore[PlusMinus,HeavisideTheta][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS","+"][z_,λ_]:>
            Power[z,λ]*HeavisideTheta[z],
        dist["PowerS","-"][z_,λ_]:>
            Power[-z,λ]*HeavisideTheta[-z]
    }];


spowerConvertCore[type:Complex|Abs,HeavisideTheta][expr_] :=
    expr//spowerConvertCore[type,PlusMinus]//spowerConvertCore[PlusMinus,HeavisideTheta];


(* ::Subsubsection:: *)
(*Abs function*)


spowerConvertCore[Abs,RealAbs][expr_] :=
    expr//ReplaceAll[{
        dist["PowerS",0][z_,λ_]:>
            Power[Abs[z],λ],
        dist["PowerS",1][z_,λ_]:>
            Power[Abs[z],λ]*Sign[z]
    }];


spowerConvertCore[type:Complex|PlusMinus,RealAbs][expr_] :=
    expr//spowerConvertCore[type,Abs]//spowerConvertCore[Abs,RealAbs];


(* ::Subsection:: *)
(*rpowerFrom*)


rpowerFrom[pattern_][expr_] :=
    Pass;


(* ::Subsection:: *)
(*deltaFromDirac|deltaToDirac*)


deltaFromDirac[expr_] :=
    expr//ReplaceAll[{
        DiracDelta[z__]:>
            dist["DeltaD",ConstantArray[0,Length[{z}]]][z],
        Derivative[λ__][DiracDelta][z__]:>
            dist["DeltaD",{λ}][z]
    }];


deltaToDirac[expr_] :=
    expr//ReplaceAll[{
        dist["DeltaD",{λ__}][z__]:>
            Derivative[λ][DiracDelta][z]
    }];


(* ::Subsection:: *)
(*deltaApart*)


deltaApart[expr_] :=
    expr//deltaApartKernel;


deltaApartKernel[expr_] :=
    expr//ReplaceAll[{
        DiracDelta[args__]:>
            Times@@Map[DiracDelta,{args}],
        Derivative[orders__][DiracDelta][args__]:>
            Times@@MapThread[Derivative[#1][DiracDelta][#2]&,{{orders},{args}}],

        dist["DeltaD",{orders__}][vars__]:>
            Times@@MapThread[dist["DeltaD",{#1}][#2]&,{{orders},{vars}}]
    }];


(* ::Subsection:: *)
(*deltaTogether*)


deltaTogether[expr_] :=
    expr//deltaTogetherKernel;


deltaTogetherKernel[expr_] :=
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
    }]//ReplaceAll[{
        Verbatim[Times][factors__]/;!FreeQ[{factors},dist["DeltaD",_List]]:>
            With[{
                    deltaDList = Cases[{factors},dist["DeltaD",{orders__}][vars__]:>{{orders},{vars}}],
                    rest = Times@@DeleteCases[{factors},dist["DeltaD",_List][__]]
                },
                {
                    vars = Sequence@@Flatten[{deltaDList[[All,2]]},2],
                    orderList = Flatten[{deltaDList[[All,1]]},2]
                },
                dist["DeltaD",orderList][vars]*rest
            ]
    }];


(* ::Subsection:: *)
(*deltaReduce*)


deltaReduce//Options = {
    "MergeDelta"->True
};


deltaReduce[All|PatternSequence[]|Verbatim[Blank[]],opts:OptionsPattern[]][expr_] :=
    expr//deltaReduceKernel[_,OptionValue["MergeDelta"]];

deltaReduce[varP_,opts:OptionsPattern[]][expr_] :=
    expr//deltaReduceKernel[varP,OptionValue["MergeDelta"]];


deltaReduceKernel[varP_,ifMergeDelta_?BooleanQ][expr_] :=
    expr//
        deltaApartKernel//
        deltaExpandRelevant//
        ReplaceRepeated[deltaReduceRule[varP]]//
        If[ifMergeDelta,
            (* Then *)
            deltaTogetherKernel,
            (* Else *)
            Identity
        ];


deltaReduceRule[p_] :=
    {
        Power[x:p,n_.]*(DiracDelta[x:p]|dist["DeltaD",{0}][x:p])*rest_./;Simplify[n>=1]&&FreeQ[rest,x]:>
            0,
        Power[x:p,n_.]*Derivative[m_][DiracDelta][x:p]*rest_./;Simplify[n>=1&&m>=1]&&FreeQ[rest,x]:>
            If[Simplify[n<=m],
                (* Then *)
                (-1)^n*FactorialPower[m,n]*Derivative[m-n][DiracDelta][x]*rest,
                (* Else *)
                0,
                (* Final *)
                -m*Power[x,n-1]*Derivative[m-1][DiracDelta][x]*rest
            ],
        Power[x:p,n_.]*dist["DeltaD",{m_}][x:p]*rest_./;Simplify[n>=1&&m>=1]&&FreeQ[rest,x]:>
            If[Simplify[n<=m],
                (* Then *)
                (-1)^n*FactorialPower[m,n]*dist["DeltaD",{m-n}][x]*rest,
                (* Else *)
                0,
                (* Final *)
                -m*Power[x,n-1]*dist["DeltaD",{m-1}][x]*rest
            ]
    };


deltaExpandRelevant[expr_] :=
    With[{
            varP =
                Cases[
                    expr,
                    Derivative[__][DiracDelta][vars__]|DiracDelta[vars__]|dist["DeltaD",_List][vars__]:>
                        {vars},
                    Infinity
                ]//
                Flatten//DeleteDuplicates//Apply[Alternatives]
        },
        Expand[expr,varP]
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
