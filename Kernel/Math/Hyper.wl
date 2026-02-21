(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Hyper`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Head*)


hyper::usage =
    "hyper[type, var][expr]: head used by hypergeometric conversion functions.";


(* ::Subsection:: *)
(*Hyper utility*)


hyperSeparate::usage =
    "hyperSeparate[expr]: separate a product into hypergeometric functions and the rest.";

hyperUnregularize::usage =
    "hyperUnregularize[expr]: convert regularized hypergeometric function to the normal one.";

hyperRegularize::usage =
    "hyperRegularize[expr]: convert hypergeometric function to the regularized one.";


(* ::Subsection:: *)
(*Hyper conversion*)


hyperToTaylor::usage =
    "hyperToTaylor[symbols, indicator][expr]: convert hypergeometric function to Taylor series."<>
    "\n"<>
    "Hint: HypergeometricPFQ, Hypergeometric2F1, Hypergeometric1F1, Hypergeometric0F1, BesselJ, BesselI.";

hyperToEuler::usage =
    "hyperToEuler[symbols, indicator][expr]: convert hypergeometric function to Euler integral."<>
    "\n"<>
    "Hint: Hypergeometric2F1, Hypergeometric1F1, HypergeometricU.";

hyperToMellinBarnes::usage =
    "hyperToMellinBarnes[symbols, indicator][expr]: convert hypergeometric function to Mellin-Barnes integral with poles at natural numbers."<>
    "\n"<>
    "Hint: HypergeometricPFQ, Hypergeometric2F1, Hypergeometric1F1, Hypergeometric0F1, HypergeometricU, BesselJ, BesselI.";

hyperFromAppellF1::usage =
    "hyperFromAppellF1[symbols, indicator][expr]: convert Appell F1 function to hypergeometric summation."<>
    "\n"<>
    "Hint: AppellF1.";


hyperToEuler2::usage =
    "hyperToEuler2[symbols, indicator][expr]: variant of hyperToEuler."<>
    "\n"<>
    "Hint: Hypergeometric2F1.";

hyperToMellinBarnes2::usage =
    "hyperToMellinBarnes2[symbols, indicator][expr]: variant of hyperToMellinBarnes."<>
    "\n"<>
    "Hint: Hypergeometric2F1, HypergeometricU, BesselK, BesselY, HankelH1, HankelH2.";


hyperFrom::usage =
    "hyperFrom[type][expr]: convert to hypergeometric function."<>
    "\n"<>
    "Value[type]: \"Bessel\".";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Hyper utility*)


(* ::Subsubsection:: *)
(*hyperSeparate*)


hyperSeparate[expr:_Hypergeometric2F1|_Hypergeometric1F1|_Hypergeometric0F1|_HypergeometricPFQ] :=
    {expr,1};

hyperSeparate[expr_Times] :=
    {
        Discard[expr,FreeQ[Hypergeometric2F1|Hypergeometric1F1|Hypergeometric0F1|HypergeometricPFQ]],
        Select[expr,FreeQ[Hypergeometric2F1|Hypergeometric1F1|Hypergeometric0F1|HypergeometricPFQ]]
    };

hyperSeparate[expr_] :=
    {1,expr};


(* ::Subsubsection:: *)
(*hyperUnregularize*)


hyperUnregularize[expr_] :=
    expr//ReplaceAll[{
        Hypergeometric2F1Regularized[a_,b_,c_,z_]:>
            Hypergeometric2F1[a,b,c,z]/Gamma[c],
        Hypergeometric1F1Regularized[a_,b_,z_]:>
            Hypergeometric1F1[a,b,z]/Gamma[b],
        Hypergeometric0F1Regularized[a_,z_]:>
            Hypergeometric0F1[a,z]/Gamma[a],
        HypergeometricPFQRegularized[as_,bs_,z_]:>
            HypergeometricPFQ[as,bs,z]/Apply[Times,Map[Gamma,bs]]
    }];


(* ::Subsubsection:: *)
(*hyperRegularize*)


hyperRegularize[expr_] :=
    expr//ReplaceAll[{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            Hypergeometric2F1Regularized[a,b,c,z]*Gamma[c],
        Hypergeometric1F1[a_,b_,z_]:>
            Hypergeometric1F1Regularized[a,b,z]*Gamma[b],
        Hypergeometric0F1[a_,z_]:>
            Hypergeometric0F1Regularized[a,z]*Gamma[a],
        HypergeometricPFQ[as_,bs_,z_]:>
            HypergeometricPFQRegularized[as,bs,z]*Apply[Times,Map[Gamma,bs]]
    }];


(* ::Subsection:: *)
(*Hyper conversion*)


(* ::Subsubsection:: *)
(*Message*)


hyper::SymbolNotEnough =
    "the number of specified functions is more than that of specified symbols by ``.";


(* ::Subsubsection:: *)
(*Main*)


hyperToTaylor[symbols_,head_:Identity][expr_] :=
    hyperConvert[hyperToTaylorRule][expr,head,symbols];

hyperToEuler[symbols_,head_:Identity][expr_] :=
    hyperConvert[hyperToEulerRule][expr,head,symbols];

hyperToMellinBarnes[symbols_,head_:Identity][expr_] :=
    hyperConvert[hyperToMellinBarnesRule][expr,head,symbols];

hyperFromAppellF1[symbols_,head_:Identity][expr_] :=
    hyperConvert[hyperFromAppellF1Rule][expr,head,symbols];


hyperToEuler2[symbols_,head_:Identity][expr_] :=
    hyperConvert[hyperToEuler2Rule][expr,head,symbols];

hyperToMellinBarnes2[symbols_,head_:Identity][expr_] :=
    hyperConvert[hyperToMellinBarnes2Rule][expr,head,symbols];


hyperConvert[which_][expr0_,head_,symbols_] :=
    Module[{
            pattern,symbolList,expr,
            positionList,numberOfHyper,numberOfSymbol,hyperList,convertedHyperList,result
        },
        pattern =
            hyperValidPattern[which];
        symbolList =
            getSymbolList[symbols];
        expr =
            expr0//expandPower[pattern];

        positionList =
            Position[expr,pattern];
        numberOfHyper =
            Length[positionList];
        numberOfSymbol =
            Length[symbolList];
        hyperList =
            Extract[expr,positionList];

        Which[
            numberOfHyper===0,
                result = expr,
            numberOfHyper>numberOfSymbol,
                Message[hyper::SymbolNotEnough,numberOfHyper-numberOfSymbol];
                result = expr,
            numberOfHyper<=numberOfSymbol,
                convertedHyperList =
                    MapThread[which,{Take[symbolList,numberOfHyper],hyperList}];
                result =
                    ReplacePart[expr,MapThread[Rule,{positionList,convertedHyperList}]];
        ];

        result//activateTimesFromPower//handleHyperHead[head]
    ];


(* ::Subsubsection:: *)
(*Kernel*)


hyperValidPattern[hyperToTaylorRule] =
    _HypergeometricPFQ|
    _Hypergeometric2F1|
    _Hypergeometric1F1|
    _Hypergeometric0F1|
    _BesselJ|
    _BesselI;

hyperValidPattern[hyperToEulerRule] =
    _Hypergeometric2F1|
    _Hypergeometric1F1|
    _HypergeometricU;

hyperValidPattern[hyperToMellinBarnesRule] =
    _HypergeometricPFQ|
    _Hypergeometric2F1|
    _Hypergeometric1F1|
    _Hypergeometric0F1|
    _HypergeometricU|
    _BesselJ|
    _BesselI;

hyperValidPattern[hyperFromAppellF1Rule] =
    _AppellF1;


hyperValidPattern[hyperToEuler2Rule] =
    _Hypergeometric2F1;

hyperValidPattern[hyperToMellinBarnes2Rule] =
    _Hypergeometric2F1|
    _HypergeometricU|
    _BesselK|
    _BesselY|
    _HankelH1|
    _HankelH2;


hyperToTaylorRule[n_,HypergeometricPFQ[as_List,bs_List,z_]] :=
    With[{
            num = Times@@Map[Pochhammer[#,n]&,as],
            denom = Times@@Map[Pochhammer[#,n]&,bs]
        },
        hyper["Taylor",n][
            gammaFrom[num/denom]*
            z^n/n!
        ]
    ];

hyperToTaylorRule[n_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["Taylor",n][
        multiGammaS[{c,a+n,b+n},{a,b,c+n}]*
        z^n/n!
    ];

hyperToTaylorRule[n_,Hypergeometric1F1[a_,b_,z_]] :=
    hyper["Taylor",n][
        multiGammaS[{b,a+n},{a,b+n}]*
        z^n/n!
    ];

hyperToTaylorRule[n_,Hypergeometric0F1[a_,z_]] :=
    hyper["Taylor",n][
        multiGammaS[{a},{a+n}]*
        z^n/n!
    ];

hyperToTaylorRule[n_,BesselJ[a_,z_]] :=
    hyper["Taylor",n][
        multiGammaS[{},{1+a+n}]*
        (-1)^n*2^(-a-2*n)*z^(a+2*n)/n!
    ];

hyperToTaylorRule[n_,BesselI[a_,z_]] :=
    hyper["Taylor",n][
        multiGammaS[{},{1+a+n}]*
        2^(-a-2*n)*z^(a+2*n)/n!
    ];


hyperToEulerRule[t_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["Euler",t][
        multiGammaS[{c},{b,c-b}]*
        t^(b-1)*(1-t)^(c-b-1)*(1-z*t)^(-a)
    ];

hyperToEulerRule[t_,Hypergeometric1F1[a_,b_,z_]] :=
    hyper["Euler",t][
        multiGammaS[{b},{a,b-a}]*
        Exp[z*t]*t^(a-1)*(1-t)^(b-a-1)
    ];

hyperToEulerRule[t_,HypergeometricU[a_,b_,z_]] :=
    hyper["Euler",t][
        multiGammaS[{},{a}]*
        Exp[-z*t]*t^(a-1)*(1+t)^(b-a-1)
    ];


hyperToMellinBarnesRule[s_,HypergeometricPFQ[as_List,bs_List,z_]] :=
    With[{
            num = Times@@Map[Pochhammer[#,s]&,as],
            denom = Times@@Map[Pochhammer[#,s]&,bs]
        },
        hyper["MellinBarnes",s][
            gammaFrom[num/denom]*
            (-z)^s*Gamma[-s]
        ]
    ];

hyperToMellinBarnesRule[s_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{c,a+s,b+s,-s},{a,b,c+s}]*
        (-z)^s
    ];

hyperToMellinBarnesRule[s_,Hypergeometric1F1[a_,b_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{b,a+s,-s},{a,b+s}]*
        (-z)^s
    ];

hyperToMellinBarnesRule[s_,Hypergeometric0F1[a_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{a,-s},{a+s}]*
        (-z)^s
    ];

hyperToMellinBarnesRule[s_,HypergeometricU[a_,b_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{a+s,1+a-b+s,-s},{a,1+a-b}]*
        z^(-a-s)
    ];

hyperToMellinBarnesRule[s_,BesselJ[a_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{-s},{1+a+s}]*
        2^(-a-2*s)*z^(a+2*s)
    ];

hyperToMellinBarnesRule[s_,BesselI[a_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{-s},{1+a+s}]*
        2^(-a-2*s)*z^(a+2*s)*Exp[I*Pi*s]
    ];


hyperFromAppellF1Rule[n_,AppellF1[a_,b1_,b2_,c_,x_,y_]] :=
    hyper["AppellF1",n][
        gammaFrom[
            (Pochhammer[a,n]*Pochhammer[b1,n]*Pochhammer[b2,n]*Pochhammer[c-a,n])/(Pochhammer[c+n-1,n]*Pochhammer[c,2*n])
        ]*
        1/n!*
        x^n*Hypergeometric2F1[a+n,b1+n,c+2*n,x]*
        y^n*Hypergeometric2F1[a+n,b2+n,c+2*n,y]
    ];


hyperToEuler2Rule[t_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["Euler",t][
        multiGammaS[{c},{b,c-b}]*
        t^(b-1)*(t+1)^(a-c)*(t-z*t+1)^(-a)
    ];


hyperToMellinBarnes2Rule[s_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{c,-s,-a-b+c-s,a+s,b+s},{a,b,-a+c,-b+c}]*
        (1-z)^s
    ];

hyperToMellinBarnes2Rule[s_,HypergeometricU[a_,b_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{b-1+s,s},{a+s}]*
        z^(1-b-s)*Exp[z]
    ];

hyperToMellinBarnes2Rule[s_,BesselK[a_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{s,a+s},{}]*
        2^(-1+a+2*s)*z^(-a-2*s)
    ];

hyperToMellinBarnes2Rule[s_,BesselY[a_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{s,a+s},{1/2-s,1/2+s}]*
        (-1)*2^(a+2*s)*z^(-a-2*s)
    ];

hyperToMellinBarnes2Rule[s_,HankelH1[a_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{s,a+s},{}]*
        (-I/π)*2^(a+2*s)*z^(-a-2*s)*Exp[I*π*s]
    ];

hyperToMellinBarnes2Rule[s_,HankelH2[a_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{s,a+s},{}]*
        (I/π)*2^(a+2*s)*z^(-a-2*s)*Exp[-I*π*s]
    ];


(* ::Subsubsection:: *)
(*Helper*)


gammaS[arg_] :=
    Gamma@Simplify@arg;

multiGammaS[args___] :=
    gammaFrom@Simplify@multiGamma[args];


getSymbolList[list_List] :=
    list;

getSymbolList[Verbatim[Alternatives][symbols___]] :=
    {symbols};

getSymbolList[symbol_] :=
    {symbol};


getPatternList[list_List] :=
    Map[Pattern[#,Blank[]]&,list];

getPatternList[Verbatim[Alternatives][symbols___]] :=
    Map[Pattern[#,Blank[]]&,{symbols}];

getPatternList[symbol_] :=
    {Pattern[#,Blank[]]&[symbol]};


expandPower[pattern_][expr_] :=
    ReplaceAll[
        Hold[expr],
        Power[base:pattern,n_]:>
            RuleCondition[Inactive[Times]@@ConstantArray[base,n]]
    ];


activateTimesFromPower[expr_] :=
    Activate[ReleaseHold[expr],Times];


handleHyperHead[INT][expr_] :=
    expr//ReplaceAll[hyper["Euler"|"Euler2"|"MellinBarnes"|"MellinBarnes2",var_][term_]:>INT[var]*term];

handleHyperHead[SUM][expr_] :=
    expr//ReplaceAll[hyper["Taylor"|"AppellF1",var_][term_]:>SUM[var]*term];

handleHyperHead[Full][expr_] :=
    expr;

handleHyperHead[head_][expr_] :=
    expr//ReplaceAll[hyper[_,_][term_]:>head[term]];


(* ::Subsection:: *)
(*Hyper conversion 2*)


(* ::Subsubsection:: *)
(*Message*)


hyper::InvalidType =
    "Invalid type: ``. The valid types include: \"Bessel\".";


(* ::Subsubsection:: *)
(*Main*)


hyperFrom[types__][expr_] :=
    Fold[hyperFromKernel,expr,{types}]//Catch;


hyperFromKernel[expr_,"Bessel"] :=
    expr//ReplaceAll[{
        BesselJ[a_,z_]:>
            2^-a*z^a*1/Gamma[1+a]*Hypergeometric0F1[a+1,-z^2/4],
        BesselI[a_,z_]:>
            2^-a*z^a*1/Gamma[1+a]*Hypergeometric0F1[a+1,z^2/4],
        BesselY[a_,z_]:>
            -1/π*2^-a*z^a*Cos[a*π]*Gamma[-a]*Hypergeometric0F1[1+a,-z^2/4]+
            -1/π*2^a*z^-a*Gamma[a]*Hypergeometric0F1[1-a,-z^2/4],
        BesselK[a_,z_]:>
            2^(-1-a)*z^a*Gamma[-a]*Hypergeometric0F1[1+a,z^2/4]+
            2^(-1+a)*z^-a*Gamma[a]*Hypergeometric0F1[1-a,z^2/4],
        HankelH1[a_,z_]:>
            -I/π*2^-a*Exp[-I*a*π]*z^a*Gamma[-a]*Hypergeometric0F1[1+a,-z^2/4]+
            -I/π*2^a*z^-a*Gamma[a]*Hypergeometric0F1[1-a,-z^2/4],
        HankelH2[a_,z_]:>
            I/π*2^-a*Exp[I*a*π]*z^a*Gamma[-a]*Hypergeometric0F1[1+a,-z^2/4]+
            I/π*2^a*z^-a*Gamma[a]*Hypergeometric0F1[1-a,-z^2/4]
    }];

hyperFromKernel[expr_,type_] :=
    (
        Message[hyper::InvalidType,type];
        Throw[expr]
    );


besselToIJ[expr_] :=
    expr//ReplaceAll[{
        HankelH1[a_,z_]:>
            BesselJ[a,z]+I*BesselY[a,z],
        HankelH2[a_,z_]:>
            BesselJ[a,z]-I*BesselY[a,z]
    }]//ReplaceAll[{
        BesselY[a_,z_]:>
            BesselJ[a,z]*Cot[Pi*a]-BesselJ[-a,z]*Csc[Pi*a],
        BesselK[a_,z_]:>
            π/2*Csc[Pi*a]*(BesselI[-a,z]-BesselI[a,z])
    }];


(* ::Subsubsection:: *)
(*Kernel*)


(* ::Subsubsection:: *)
(*Helper*)


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
