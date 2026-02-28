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
    "deltaD[z, n]: δ^n(z) - Dirac delta function."<>
    "\n"<>
    "deltaD[{z, ...}, {n, ...}]: δ^n(z) ... - product of Dirac delta functions.";

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


deltaReduce::usage =
    "deltaReduce[pattern][expr]: reduce the Dirac delta distributions in the expression.";

(* deltaApart::usage =
    "deltaApart[expr]: take apart the Dirac delta distributions of several variables."; *)

(* deltaTogether::usage =
    "deltaTogether[expr]: take together the Dirac delta distributions of several variables."; *)


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Symbol*)


(* ::Subsubsection:: *)
(*Message*)


(*  *)


(* ::Subsubsection:: *)
(*Main*)


spower[s_][z_,0] :=
    1;


deltaD[z_,0] :=
    deltaD[z];

deltaD[z_List,{(0)..}] :=
    deltaD[z];


deltaC[z_,0] :=
    deltaC[z];


Derivative[n_,0][spower["+"]][z_,λ_] :=
    FactorialPower[λ,n]*spower["+"][z,λ-n];

Derivative[n_,0][spower["-"]][z_,λ_] :=
    (-1)^n*FactorialPower[λ,n]*spower["-"][z,λ-n];

Derivative[n_,0][spower[s:0|1]][z_,λ_] :=
    FactorialPower[λ,n]*spower[Mod[s+n,2]][z,λ-n];

Derivative[n_,0][spower[s:I|-I]][z_,λ_] :=
    FactorialPower[λ,n]*spower[s][z,λ-n];


Derivative[0,n_][spower[s:"+"|"-"]][z_,λ_] :=
    spowerlog[s][z,λ,n];

Derivative[0,n_,0][spowerlog[s:"+"|"-"]][z_,λ_,m_] :=
    spowerlog[s][z,λ,n+m];


Derivative[1,0][spowerlog["+"]][z_,λ_,0] :=
    λ*spowerlog["+"][z,λ-1,0]+(-1)^λ/(-λ)!*deltaD[z,-λ];

Derivative[1,0][spowerlog["-"]][z_,λ_,0] :=
    -λ*spowerlog["-"][z,λ-1,0]-1/(-λ)!*deltaD[z,-λ];

Derivative[1,0][spowerlog[0]][z_,λ_,0] :=
    Pass;

Derivative[1,0][spowerlog[1]][z_,λ_,0] :=
    Pass;


Derivative[n_][deltaD][z_] :=
    deltaD[z,n];

Derivative[n_,_][deltaD][z_,λ_] :=
    deltaD[z,λ+n];


(* ::Subsection:: *)
(*spowerReduce*)


spowerReduce[expr_] :=
    expr//
        spowerReduceProduct//
        spowerReduceSpecialValue;


spowerReduceProduct[expr_] :=
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

spowerReduceSpecialValue[expr_] :=
    expr//ReplaceAll[{
        spower[s:I|-I][z_,n_Integer?NonNegative]:>
            Power[z,n],
        spower[0][z_,n_Integer?NonNegative]/;EvenQ[n]:>
            Power[z,n],
        spower[1][z_,n_Integer?NonNegative]/;OddQ[n]:>
            Power[z,n],

        spowerlog[s:I|-I][z_,λ_,0]:>
            spower[s][z,λ],
        spowerlog[0][z_,λ_Integer?Negative,0]/;EvenQ[λ]:>
            spower[0][z,λ],
        spowerlog[1][z_,λ_Integer?Negative,0]/;OddQ[λ]:>
            spower[1][z,λ]
    }];


(* ::Subsection:: *)
(*spowerStrip*)


spowerStrip[expr_] :=
    expr//ReplaceAll[{
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

        (* Complex *)
        spower[s:_.*I|_.*-I][z_,λ_]:>
            Power[z,λ],

        (* Log, PlusMinus *)
        spowerlog["+"][z_,λ_,n_]:>
            Power[z,λ]*Log[z]^n,
        spowerlog["-"][z_,λ_,n_]:>
            Power[-z,λ]*Log[-z]^n,

        (* Log, Abs *)
        spowerlog[0][z_,λ_,n_]:>
            Power[Abs[z],λ]*Log[z]^n,
        spowerlog[1][z_,λ_,n_]:>
            Power[Abs[z],λ]*Log[z]^n*Sign[z],

        (* Log, Complex *)
        spowerlog[s:_.*I|_.*-I][z_,λ_,n_]:>
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
(*rpowerFrom*)


rpowerFrom[pattern_][expr_] :=
    Pass;


(* ::Subsection:: *)
(*deltaFromDirac|deltaToDirac*)


deltaFromDirac[expr_] :=
    expr//ReplaceAll[{
        DiracDelta[z_]:>
            deltaD[z],
        Derivative[n_][DiracDelta][z_]:>
            deltaD[z,n]
    }]//ReplaceAll[{
        DiracDelta[zs__]:>
            deltaD[{zs}],
        Derivative[ns__][DiracDelta][zs__]:>
            deltaD[{zs},{ns}]
    }];


deltaToDirac[expr_] :=
    expr//ReplaceAll[{
        deltaD[{zs__}]:>
            DiracDelta[zs],
        deltaD[{zs__},{ns__}]:>
            Derivative[ns][DiracDelta][{zs}]
    }]//ReplaceAll[{
        deltaD[z_]:>
            DiracDelta[z],
        deltaD[z_,n_]:>
            Derivative[n][DiracDelta][z]
    }]


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
        Power[x:p,n_.]*(DiracDelta[x:p]|deltaD[x:p])*rest_./;Simplify[n>=1]&&FreeQ[rest,x]:>
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
        Power[x:p,n_.]*deltaD[x:p,m_]*rest_./;Simplify[n>=1&&m>=1]&&FreeQ[rest,x]:>
            If[Simplify[n<=m],
                (* Then *)
                (-1)^n*FactorialPower[m,n]*deltaD[x,m-n]*rest,
                (* Else *)
                0,
                (* Final *)
                -m*Power[x,n-1]*deltaD[x,m-1]*rest
            ]
    };


deltaExpandRelevant[expr_] :=
    With[{
            varP =
                Cases[
                    expr,
                    Derivative[__][DiracDelta][vars__]|DiracDelta[vars__]|deltaD[{vars__},___]|deltaD[var:Except[_List],___]:>
                        {vars,var},
                    Infinity
                ]//
                Flatten//DeleteDuplicates//Apply[Alternatives]
        },
        Expand[expr,varP]
    ];


(* ::Subsection:: *)
(*deltaApart*)


deltaApart[][expr_] :=
    expr//deltaApartKernel;


deltaApartKernel[expr_] :=
    expr//ReplaceAll[{
        DiracDelta[args__]:>
            Times@@Map[DiracDelta,{args}],
        Derivative[orders__][DiracDelta][args__]:>
            Times@@MapThread[Derivative[#1][DiracDelta][#2]&,{{orders},{args}}],

        deltaD[{vars__}]:>
            Times@@Map[deltaD,{vars}],
        deltaD[{vars__},{orders__}]:>
            Times@@MapThread[deltaD[#1,#2]&,{{vars},{orders}}]
    }];


(* ::Subsection:: *)
(*deltaTogether*)


deltaTogether[][expr_] :=
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
    }];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
