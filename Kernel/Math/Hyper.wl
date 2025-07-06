(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Hyper`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Constant`"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Head*)


hyper::usage =
    "hyper[type, var][expr]: head used by hypergeometric conversion functions.";

JacobiPhi::usage =
    "JacobiPhi[a, b, c, z]: Jacobi Phi function.";

WilsonPolynomial::usage =
    "WilsonPolynomial[a, b, c, d, n, x]: Wilson polynomial.";


(* ::Subsection:: *)
(*Function*)


hyperSeparate::usage =
    "hyperSeparate[expr]: separate a product into hypergeometric functions and the rest.";

hyperUnregularize::usage =
    "hyperUnregularize[expr]: convert regularized hypergeometric function to the normal one.";

hyperRegularize::usage =
    "hyperRegularize[expr]: convert hypergeometric function to the regularized one.";


hyperToTaylor::usage =
    "hyperToTaylor[symbols][expr]: convert hypergeometric function to Taylor series."<>
    "\n"<>
    "hyperToTaylor[symbols, indicator][expr]: indicate the summation."<>
    "\n"<>
    "Default[indicator]: SUM.";

hyperToMellinBarnes::usage =
    "hyperToMellinBarnes[symbols][expr]: convert hypergeometric function to Mellin-Barnes integral."<>
    "\n"<>
    "hyperToMellinBarnes[symbols, indicator][expr]: indicate the integration."<>
    "\n"<>
    "Default[indicator]: INT.";

hyperToMellinBarnes2::usage =
    "hyperToMellinBarnes2[symbols][expr]: convert hypergeometric function to Mellin-Barnes integral in terms of (1-z)."<>
    "\n"<>
    "hyperToMellinBarnes2[symbols, indicator][expr]: indicate the integration."<>
    "\n"<>
    "Default[indicator]: INT.";

hyperFromAppellF1::usage =
    "hyperFromAppellF1[symbols][expr]: convert Appell F1 function to hypergeometric summation."<>
    "\n"<>
    "hyperFromAppellF1[symbols, indicator][expr]: indicate the summation."<>
    "\n"<>
    "Default[indicator]: SUM.";


JacobiPhiToHyper::usage =
    "JacobiPhiToHyper[head][expr]: convert Jacobi Phi function to Hypergeometric2F1."<>
    "\n"<>
    "Default[head]: Inactive.";

JacobiPhiFromHyper::usage =
    "JacobiPhiFromHyper[head][expr]: convert Hypergeometric2F1 to Jacobi Phi function."<>
    "\n"<>
    "Default[head]: Inactive.";


WilsonPolynomialToHyper::usage =
    "WilsonPolynomialToHyper[head][expr]: convert Wilson polynomial to Hypergeometric4F3."<>
    "\n"<>
    "Default[head]: Inactive.";

WilsonPolynomialFromHyper::usage =
    "WilsonPolynomialFromHyper[head][expr]: convert Hypergeometric4F3 to Wilson polynomial."<>
    "\n"<>
    "Default[head]: Inactive.";


AppellF1FromIntegral::usage =
    "AppellF1FromIntegral[var, head][expr]: convert integral to Appell F1."<>
    "\n"<>
    "Info[var]: integration variable to match."<>
    "\n"<>
    "Default[var]: All."<>
    "\n"<>
    "Default[head]: Inactive.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
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


(* ::Subsection:: *)
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


(* ::Subsection:: *)
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
(*hyperConvert*)


(* ::Subsubsection:: *)
(*Message*)


hyper::SymbolNotEnough =
    "the number of specified functions is more than that of specified symbols by ``.";


(* ::Subsubsection:: *)
(*Main*)


hyperToTaylor[symbols_,head_:SUM][expr_] :=
    hyperConvert[hyperToTaylorRule][
        expr,head,
        _Hypergeometric2F1|_Hypergeometric1F1|_Hypergeometric0F1|_HypergeometricPFQ,
        getSymbolList[symbols]
    ];

hyperToMellinBarnes[symbols_,head_:INT][expr_] :=
    hyperConvert[hyperToMellinBarnesRule][
        expr,head,
        _Hypergeometric2F1|_Hypergeometric1F1|_Hypergeometric0F1|_HypergeometricPFQ,
        getSymbolList[symbols]
    ];

hyperToMellinBarnes2[symbols_,head_:INT][expr_] :=
    hyperConvert[hyperToMellinBarnesRule2][
        expr,head,
        _Hypergeometric2F1|_Hypergeometric1F1|_Hypergeometric0F1|_HypergeometricPFQ,
        getSymbolList[symbols]
    ];

hyperFromAppellF1[symbols_,head_:SUM][expr_] :=
    hyperConvert[hyperFromAppellF1Rule][
        expr,head,
        _AppellF1,
        getSymbolList[symbols]
    ];


hyperConvert[which_Symbol][expr0_,head_,pattern_,symbolList_List] :=
    Module[ {expr,positionList,numberOfHyper,numberOfSymbol,hyperList,convertedHyperList,result},
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
(*Helper*)


hyperToMellinBarnesRule[s_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper[hyperToMellinBarnes,s][((-z)^s Gamma[-s] Gamma[c] Gamma[a+s] Gamma[b+s])/(Gamma[a] Gamma[b] Gamma[c+s])];

hyperToMellinBarnesRule[s_,Hypergeometric1F1[a_,b_,z_]] :=
    hyper[hyperToMellinBarnes,s][((-z)^s Gamma[-s] Gamma[b] Gamma[a+s])/(Gamma[a] Gamma[b+s])];

hyperToMellinBarnesRule[s_,Hypergeometric0F1[a_,z_]] :=
    hyper[hyperToMellinBarnes,s][((-z)^s Gamma[-s] Gamma[a])/(Gamma[a+s])];

hyperToMellinBarnesRule[s_,HypergeometricPFQ[as_List,bs_List,z_]] :=
    hyper[hyperToMellinBarnes,s][
        Times@@Map[Pochhammer[#,s]&,as]/Times@@Map[Pochhammer[#,s]&,bs] (-z)^s Gamma[-s]//gammaFrom
    ];


hyperToMellinBarnesRule2[s_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper[hyperToMellinBarnes2,s][((1-z)^s Gamma[c] Gamma[-a-b+c-s] Gamma[-s] Gamma[a+s] Gamma[b+s])/(Gamma[a] Gamma[b] Gamma[-a+c] Gamma[-b+c])];


hyperToTaylorRule[n_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper[hyperToTaylor,n][(z^n Gamma[c] Gamma[a+n] Gamma[b+n])/(Gamma[1+n] Gamma[a] Gamma[b] Gamma[c+n])];

hyperToTaylorRule[n_,Hypergeometric1F1[a_,b_,z_]] :=
    hyper[hyperToTaylor,n][(z^n Gamma[b] Gamma[a+n])/(Gamma[a] Gamma[1+n] Gamma[b+n])];

hyperToTaylorRule[n_,Hypergeometric0F1[a_,z_]] :=
    hyper[hyperToTaylor,n][(z^n Gamma[a])/(Gamma[1+n] Gamma[a+n])];

hyperToTaylorRule[n_,HypergeometricPFQ[as_List,bs_List,z_]] :=
    hyper[hyperToTaylor,n][
        Times@@Map[Pochhammer[#,n]&,as]/Times@@Map[Pochhammer[#,n]&,bs] z^n/n!//gammaFrom
    ];


hyperFromAppellF1Rule[n_,AppellF1[a_,b1_,b2_,c_,x_,y_]] :=
    hyper[hyperFromAppellF1,n][
        (
            (Pochhammer[a,n] Pochhammer[b1,n] Pochhammer[b2,n] Pochhammer[c-a,n])/
            (n! Pochhammer[c+n-1,n] Pochhammer[c,2 n])
        )*x^n*y^n*
        Hypergeometric2F1[a+n,b1+n,c+2 n,x]*
        Hypergeometric2F1[a+n,b2+n,c+2 n,y]
    ];


getSymbolList[list_List] :=
    list;

getSymbolList[Verbatim[Alternatives][symbols___]] :=
    {symbols};

getSymbolList[symbol_] :=
    {symbol};


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
    expr//ReplaceAll[hyper[hyperToMellinBarnes|hyperToMellinBarnes2,var_][term_]:>INT[var]*term];

handleHyperHead[SUM][expr_] :=
    expr//ReplaceAll[hyper[hyperToTaylor|hyperFromAppellF1,var_][term_]:>SUM[var]*term];


(* ::Subsection:: *)
(*JacobiPhiToHyper*)


JacobiPhiToHyper[head_:Inactive][expr_] :=
    expr//ReplaceAll[ruleJacobiPhiToHyper[head]];


ruleJacobiPhiToHyper[head_] :=
    (JacobiPhi|_[JacobiPhi])[a_,b_,c_,z_]:>
        Map[
            Simplify,
            head[Hypergeometric2F1][(a+b+1-I*c)/2,(a+b+1+I*c)/2,a+1,-Sinh[z]^2],
            {1,2}
        ];


(* ::Subsection:: *)
(*JacobiPhiFromHyper*)


JacobiPhiFromHyper[head_:Inactive][expr_] :=
    expr//ReplaceAll[ruleJacobiPhiFromHyper[head]];


ruleJacobiPhiFromHyper[head_] :=
    (Hypergeometric2F1|_[Hypergeometric2F1])[a_,b_,c_,z_]:>
        Map[
            Simplify,
            head[JacobiPhi][c-1,a+b-c,I(a-b),ArcSinh[Sqrt[-z]]],
            {1,2}
        ];


(* ::Subsection:: *)
(*WilsonPolynomialToHyper*)


WilsonPolynomialToHyper[head_:Inactive][expr_] :=
    expr//ReplaceAll[ruleWilsonPolynomialToHyper[head]];


ruleWilsonPolynomialToHyper[head_] :=
    (WilsonPolynomial|_[WilsonPolynomial])[a_,b_,c_,d_,n_,x_]:>
        Map[
            Simplify,
            (Gamma[a+b+n]*Gamma[a+c+n]*Gamma[a+d+n])/(Gamma[a+b]*Gamma[a+c]*Gamma[a+d])*
                head[HypergeometricPFQ][{-n,-1+a+b+c+d+n,a-I*Sqrt[x],a+I*Sqrt[x]},{a+b,a+c,a+d},1],
            {1,2}
        ];


(* ::Subsection:: *)
(*WilsonPolynomialFromHyper*)


WilsonPolynomialFromHyper[head_:Inactive][expr_] :=
    expr//ReplaceAll[ruleWilsonPolynomialFromHyper[head]];


ruleWilsonPolynomialFromHyper[head_] :=
    (HypergeometricPFQ|_[HypergeometricPFQ])[{-n_,a_,b_,c_},{d_,e_,f_},1]/;Simplify[a+b+c-d-e-f+1-n==0]:>
        Map[
            Simplify,
            (Gamma[d]*Gamma[e]*Gamma[f])/(Gamma[1+a+b+c-d-e]*Gamma[1+a+b+c-d-f]*Gamma[1+a+b+c-e-f])*
                head[WilsonPolynomial][(b+c)/2,-(b/2)-c/2+d,-(b/2)-c/2+e,-(b/2)-c/2+f,n,-(1/4)*(b-c)^2],
            {1,2}
        ];


(* ::Subsection:: *)
(*AppellF1FromIntegral*)


AppellF1FromIntegral[][expr_] :=
    expr//ReplaceAll[ruleAppellF1FromIntegral[All,Inactive,indexOfINT[expr]]];

AppellF1FromIntegral[var_,head_:Inactive][expr_] :=
    expr//ReplaceAll[ruleAppellF1FromIntegral[var,head,indexOfINT[expr]]];


ruleAppellF1FromIntegral[All,head_,intIndex_] :=
    u_^a_*(1-u_)^b_*(1+u_*x_.)^c_*(1+u_*y_.)^d_:>
        INT[u]^(-intIndex)*Map[
            Simplify,
            (Gamma[1+a]*Gamma[1+b])/Gamma[2+a+b]*head[AppellF1][1+a,-c,-d,2+a+b,-x,-y],
            {1,2}
        ];

ruleAppellF1FromIntegral[u_,head_,intIndex_] :=
    u^a_*(1-u)^b_*(1+u*x_.)^c_*(1+u*y_.)^d_:>
        INT[u]^(-intIndex)*Map[
            Simplify,
            (Gamma[1+a]*Gamma[1+b])/Gamma[2+a+b]*head[AppellF1][1+a,-c,-d,2+a+b,-x,-y],
            {1,2}
        ];


indexOfINT[expr_]/;FreeQ[expr,_INT] :=
    0;

indexOfINT[expr_] :=
    1;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
