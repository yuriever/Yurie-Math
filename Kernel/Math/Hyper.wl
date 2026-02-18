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
    "Hint: HypergeometricPFQ, Hypergeometric2F1, Hypergeometric1F1, Hypergeometric0F1."<>
    "\n"<>
    "Default[indicator]: SUM.";

hyperToEuler::usage =
    "hyperToEuler[symbols, indicator][expr]: convert hypergeometric function to Euler integral."<>
    "\n"<>
    "Hint: Hypergeometric2F1, Hypergeometric1F1, HypergeometricU."<>
    "\n"<>
    "Default[indicator]: INT.";

hyperToMellinBarnes::usage =
    "hyperToMellinBarnes[symbols, indicator][expr]: convert hypergeometric function to Mellin-Barnes integral with poles at natural numbers."<>
    "\n"<>
    "Hint: HypergeometricPFQ, Hypergeometric2F1, Hypergeometric1F1, Hypergeometric0F1, HypergeometricU, BesselJ, BesselI."<>
    "\n"<>
    "Default[indicator]: INT.";

hyperFromAppellF1::usage =
    "hyperFromAppellF1[symbols, indicator][expr]: convert Appell F1 function to hypergeometric summation."<>
    "\n"<>
    "Hint: AppellF1."<>
    "\n"<>
    "Default[indicator]: SUM.";


hyperToEuler2::usage =
    "hyperToEuler2[symbols, indicator][expr]: variant of hyperToEuler."<>
    "\n"<>
    "Hint: Hypergeometric2F1."<>
    "\n"<>
    "Default[indicator]: INT.";

hyperToMellinBarnes2::usage =
    "hyperToMellinBarnes2[symbols, indicator][expr]: variant of hyperToMellinBarnes."<>
    "\n"<>
    "Hint: Hypergeometric2F1, HypergeometricU, BesselK, BesselY, HankelH1, HankelH2."<>
    "\n"<>
    "Default[indicator]: INT.";


hyperFrom::usage =
    "hyperFrom[pattern][expr]: convert to hypergeometric function.";


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


hyperToTaylor[symbols_,head_:SUM][expr_] :=
    hyperConvert[hyperToTaylorRule][expr,head,symbols];

hyperToEuler[symbols_,head_:INT][expr_] :=
    hyperConvert[hyperToEulerRule][expr,head,symbols];

hyperToMellinBarnes[symbols_,head_:INT][expr_] :=
    hyperConvert[hyperToMellinBarnesRule][expr,head,symbols];

hyperFromAppellF1[symbols_,head_:SUM][expr_] :=
    hyperConvert[hyperFromAppellF1Rule][expr,head,symbols];


hyperToEuler2[symbols_,head_:INT][expr_] :=
    hyperConvert[hyperToEuler2Rule][expr,head,symbols];

hyperToMellinBarnes2[symbols_,head_:INT][expr_] :=
    hyperConvert[hyperToMellinBarnes2Rule][expr,head,symbols];


hyperConvert[which_][expr0_,head_,symbols_] :=
    Module[{
            pattern,symbolList,expr,
            positionList,numberOfHyper,numberOfSymbol,hyperList,convertedHyperList,result
        },
        pattern =
            which[];
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


hyperToTaylorRule[] =
    _Hypergeometric2F1|_Hypergeometric1F1|_Hypergeometric0F1|_HypergeometricPFQ;

hyperToEulerRule[] =
    _Hypergeometric2F1|_Hypergeometric1F1|_Hypergeometric0F1|_HypergeometricPFQ|_HypergeometricU;

hyperToMellinBarnesRule[] =
    _Hypergeometric2F1|_Hypergeometric1F1|_Hypergeometric0F1|_HypergeometricPFQ|_HypergeometricU;

hyperFromAppellF1Rule[] =
    _AppellF1;


hyperToEuler2Rule[] =
    _Hypergeometric2F1;

hyperToMellinBarnes2Rule[] =
    _Hypergeometric2F1|_HypergeometricU;


hyperToTaylorRule[n_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["Taylor",n][
        multiGammaS[{c,a+n,b+n},{a,b,c+n}]*z^n/n!
    ];

hyperToTaylorRule[n_,Hypergeometric1F1[a_,b_,z_]] :=
    hyper["Taylor",n][
        multiGammaS[{b,a+n},{a,b+n}]*z^n/n!
    ];

hyperToTaylorRule[n_,Hypergeometric0F1[a_,z_]] :=
    hyper["Taylor",n][
        multiGammaS[{a},{a+n}]*z^n/n!
    ];

hyperToTaylorRule[n_,HypergeometricPFQ[as_List,bs_List,z_]] :=
    With[{
            num = Times@@Map[Pochhammer[#,n]&,as],
            denom = Times@@Map[Pochhammer[#,n]&,bs]
        },
        hyper["Taylor",n][
            gammaFrom[num/denom]*z^n/n!
        ]
    ];


hyperToEulerRule[t_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["Euler",t][
        multiGammaS[{c},{b,c-b}]*t^(b-1)*(1-t)^(c-b-1)*(1-z*t)^(-a)
    ];

hyperToEulerRule[t_,Hypergeometric1F1[a_,b_,z_]] :=
    hyper["Euler",t][
        multiGammaS[{b},{a,b-a}]*Exp[z*t]*t^(a-1)*(1-t)^(b-a-1)
    ];

hyperToEulerRule[t_,HypergeometricU[a_,b_,z_]] :=
    hyper["Euler",t][
        multiGammaS[{},{a}]*Exp[-z*t]*t^(a-1)*(1+t)^(b-a-1)
    ];


hyperToMellinBarnesRule[s_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{c,a+s,b+s,-s},{a,b,c+s}]*(-z)^s
    ];

hyperToMellinBarnesRule[s_,Hypergeometric1F1[a_,b_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{b,a+s,-s},{a,b+s}]*(-z)^s
    ];

hyperToMellinBarnesRule[s_,Hypergeometric0F1[a_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{a,-s},{a+s}]*(-z)^s
    ];

hyperToMellinBarnesRule[s_,HypergeometricPFQ[as_List,bs_List,z_]] :=
    With[{
            num = Times@@Map[Pochhammer[#,s]&,as],
            denom = Times@@Map[Pochhammer[#,s]&,bs]
        },
        hyper["MellinBarnes",s][
            gammaFrom[num/denom]*(-z)^s*Gamma[-s]
        ]
    ];

hyperToMellinBarnesRule[s_,HypergeometricU[a_,b_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{a+s,1+a-b+s,-s},{a,1+a-b}]*z^(-a-s)
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
        multiGammaS[{c},{b,c-b}]*t^(b-1)*(t+1)^(a-c)*(t-z*t+1)^(-a)
    ];


hyperToMellinBarnes2Rule[s_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{c,-s,-a-b+c-s,a+s,b+s},{a,b,-a+c,-b+c}]*(1-z)^s
    ];

hyperToMellinBarnes2Rule[s_,HypergeometricU[a_,b_,z_]] :=
    hyper["MellinBarnes",s][
        multiGammaS[{b-1+s,s},{a+s}]*z^(1-b-s)*Exp[z]
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


handleHyperHead[Full][expr_] :=
    expr;

handleHyperHead[head_][expr_] :=
    expr//ReplaceAll[hyper[__]->head];

handleHyperHead[INT][expr_] :=
    expr//ReplaceAll[hyper["MellinBarnes"|"Euler",var_][term_]:>INT[var]*term];

handleHyperHead[SUM][expr_] :=
    expr//ReplaceAll[hyper["Taylor"|"AppellF1",var_][term_]:>SUM[var]*term];


(* ::Subsection:: *)
(*Hyper conversion 2*)


(* ::Subsubsection:: *)
(*Message*)


(* ::Subsubsection:: *)
(*Main*)


hyperFrom[pattern_][expr_] :=
    Pass;


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
