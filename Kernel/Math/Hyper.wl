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
    "hyperToTaylor[symbols][expr]: convert hypergeometric function to Taylor series."<>
    "\n"<>
    "hyperToTaylor[symbols, indicator][expr]: indicate the summation."<>
    "\n"<>
    "Default[indicator]: SUM.";

hyperToEuler::usage =
    "hyperToEuler[symbols][expr]: convert hypergeometric function to Euler integral."<>
    "\n"<>
    "hyperToEuler[symbols, indicator][expr]: indicate the integration."<>
    "\n"<>
    "Default[indicator]: INT.";

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


(* ::Subsection:: *)
(*Hyper conversion 2*)


JacobiPhiToHyper::usage =
    "JacobiPhiToHyper[head][expr]: convert Jacobi Phi function to Hypergeometric2F1."<>
    "\n"<>
    "Default[head]: Identity.";

JacobiPhiFromHyper::usage =
    "JacobiPhiFromHyper[head][expr]: convert Hypergeometric2F1 to Jacobi Phi function."<>
    "\n"<>
    "Default[head]: Identity.";


WilsonPolynomialToHyper::usage =
    "WilsonPolynomialToHyper[head][expr]: convert Wilson polynomial to Hypergeometric4F3."<>
    "\n"<>
    "Default[head]: Identity.";

WilsonPolynomialFromHyper::usage =
    "WilsonPolynomialFromHyper[head][expr]: convert Hypergeometric4F3 to Wilson polynomial."<>
    "\n"<>
    "Default[head]: Identity.";


(* ::Subsection:: *)
(*Integral conversion*)


hyperFromIntegral::usage =
    "hyperFromIntegral[var, head][expr]: convert integral to hypergeometric function."<>
    "\n"<>
    "Info[var]: integration variable to match."<>
    "\n"<>
    "Default[var]: All."<>
    "\n"<>
    "Default[head]: Identity.";


AppellF1FromIntegral::usage =
    "AppellF1FromIntegral[var, head][expr]: convert integral to Appell F1."<>
    "\n"<>
    "Info[var]: integration variable to match."<>
    "\n"<>
    "Default[var]: All."<>
    "\n"<>
    "Default[head]: Identity.";


conformalIntegral2::usage =
    "conformalIntegral2[{z1, z2}, {z0}][expr]: perform 1d two-point conformal integral."<>
    "\n"<>
    "conformalIntegral2[{z1, zb1, z2, zb2}, {z0, zb0}][expr]: 2d version with measure d^2z == dxdy."<>
    "\n"<>
    "Hint: see appendix of 1108.6194.";

conformalIntegral3::usage =
    "conformalIntegral3[{z1, z2, z3}, {z0}][expr]: perform 1d three-point conformal integral."<>
    "\n"<>
    "conformalIntegral3[{z1, zb1, z2, zb2, z3, zb3}, {z0, zb0}][expr]: 2d version with measure d^2z == dxdy."<>
    "\n"<>
    "Hint: see appendix of 1108.6194.";

conformalIntegralKLT::usage =
    "conformalIntegralKLT[{z1, z2}, {z0}][expr]: perform 1d three-point conformal integral in the KLT form."<>
    "\n"<>
    "conformalIntegralKLT[{z1, zb1, z2, zb2}, {z0, zb0}][expr]: 2d version with measure d^2z == dxdy."<>
    "\n"<>
    "Hint: see appendix of 1706.05362.";


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
    hyperConvert[hyperToTaylorRule][
        expr,head,
        _Hypergeometric2F1|_Hypergeometric1F1|_Hypergeometric0F1|_HypergeometricPFQ,
        getSymbolList[symbols]
    ];

hyperToEuler[symbols_,head_:INT][expr_] :=
    hyperConvert[hyperToEulerRule][
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


hyperConvert[which_][expr0_,head_,pattern_,symbolList_List] :=
    Module[{expr,positionList,numberOfHyper,numberOfSymbol,hyperList,convertedHyperList,result},
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


hyperToTaylorRule[n_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["Taylor",n][
        (z^n Gamma[c] Gamma[a+n] Gamma[b+n])/(Gamma[1+n] Gamma[a] Gamma[b] Gamma[c+n])
    ];

hyperToTaylorRule[n_,Hypergeometric1F1[a_,b_,z_]] :=
    hyper["Taylor",n][
        (z^n Gamma[b] Gamma[a+n])/(Gamma[a] Gamma[1+n] Gamma[b+n])
    ];

hyperToTaylorRule[n_,Hypergeometric0F1[a_,z_]] :=
    hyper["Taylor",n][
        (z^n Gamma[a])/(Gamma[1+n] Gamma[a+n])
    ];

hyperToTaylorRule[n_,HypergeometricPFQ[as_List,bs_List,z_]] :=
    hyper["Taylor",n][
        Times@@Map[Pochhammer[#,n]&,as]/Times@@Map[Pochhammer[#,n]&,bs] z^n/n!//gammaFrom
    ];


hyperToEulerRule[u_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["Euler",u][Pass];

hyperToEulerRule[u_,Hypergeometric1F1[a_,b_,z_]] :=
    hyper["Euler",u][Pass];

hyperToEulerRule[u_,Hypergeometric0F1[a_,z_]] :=
    hyper["Euler",u][Pass];

hyperToEulerRule[u_,HypergeometricPFQ[as_List,bs_List,z_]] :=
    hyper["Euler",u][Pass];


hyperToMellinBarnesRule[s_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["MellinBarnes",s][
        ((-z)^s Gamma[-s] Gamma[c] Gamma[a+s] Gamma[b+s])/(Gamma[a] Gamma[b] Gamma[c+s])
    ];

hyperToMellinBarnesRule[s_,Hypergeometric1F1[a_,b_,z_]] :=
    hyper["MellinBarnes",s][
        ((-z)^s Gamma[-s] Gamma[b] Gamma[a+s])/(Gamma[a] Gamma[b+s])
    ];

hyperToMellinBarnesRule[s_,Hypergeometric0F1[a_,z_]] :=
    hyper["MellinBarnes",s][
        ((-z)^s Gamma[-s] Gamma[a])/(Gamma[a+s])
    ];

hyperToMellinBarnesRule[s_,HypergeometricPFQ[as_List,bs_List,z_]] :=
    hyper["MellinBarnes",s][
        Times@@Map[Pochhammer[#,s]&,as]/Times@@Map[Pochhammer[#,s]&,bs] (-z)^s Gamma[-s]//gammaFrom
    ];


hyperToMellinBarnesRule2[s_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyper["MellinBarnes",s][
        ((1-z)^s Gamma[c] Gamma[-a-b+c-s] Gamma[-s] Gamma[a+s] Gamma[b+s])/(Gamma[a] Gamma[b] Gamma[-a+c] Gamma[-b+c])
    ];


hyperFromAppellF1Rule[n_,AppellF1[a_,b1_,b2_,c_,x_,y_]] :=
    hyper["AppellF1",n][
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
    expr//ReplaceAll[hyper["MellinBarnes",var_][term_]:>INT[var]*term];

handleHyperHead[SUM][expr_] :=
    expr//ReplaceAll[hyper["Taylor"|"AppellF1",var_][term_]:>SUM[var]*term];


(* ::Subsection:: *)
(*Hyper conversion 2*)


(* ::Subsubsection:: *)
(*JacobiPhiToHyper*)


JacobiPhiToHyper[head_:Identity][expr_] :=
    expr//ReplaceAll[ruleJacobiPhiToHyper[head]];


ruleJacobiPhiToHyper[head_] :=
    (JacobiPhi|_[JacobiPhi])[a_,b_,c_,z_]:>
        Simplify@head[Hypergeometric2F1][(a+b+1-I*c)/2,(a+b+1+I*c)/2,a+1,-Sinh[z]^2];


(* ::Subsubsection:: *)
(*JacobiPhiFromHyper*)


JacobiPhiFromHyper[head_:Identity][expr_] :=
    expr//ReplaceAll[ruleJacobiPhiFromHyper[head]];


ruleJacobiPhiFromHyper[head_] :=
    (Hypergeometric2F1|_[Hypergeometric2F1])[a_,b_,c_,z_]:>
        Simplify@head[JacobiPhi][c-1,a+b-c,I(a-b),ArcSinh[Sqrt[-z]]];


(* ::Subsubsection:: *)
(*WilsonPolynomialToHyper*)


WilsonPolynomialToHyper[head_:Identity][expr_] :=
    expr//ReplaceAll[ruleWilsonPolynomialToHyper[head]];


ruleWilsonPolynomialToHyper[head_] :=
    (WilsonPolynomial|_[WilsonPolynomial])[a_,b_,c_,d_,n_,x_]:>
        (Gamma[a+b+n]*Gamma[a+c+n]*Gamma[a+d+n])/(Gamma[a+b]*Gamma[a+c]*Gamma[a+d])*
        Simplify@head[HypergeometricPFQ][{-n,-1+a+b+c+d+n,a-I*Sqrt[x],a+I*Sqrt[x]},{a+b,a+c,a+d},1];


(* ::Subsubsection:: *)
(*WilsonPolynomialFromHyper*)


WilsonPolynomialFromHyper[head_:Identity][expr_] :=
    expr//ReplaceAll[ruleWilsonPolynomialFromHyper[head]];


ruleWilsonPolynomialFromHyper[head_] :=
    (HypergeometricPFQ|_[HypergeometricPFQ])[{-n_,a_,b_,c_},{d_,e_,f_},1]/;Simplify[a+b+c-d-e-f+1-n==0]:>
        (Gamma[d]*Gamma[e]*Gamma[f])/(Gamma[1+a+b+c-d-e]*Gamma[1+a+b+c-d-f]*Gamma[1+a+b+c-e-f])*
        Simplify@head[WilsonPolynomial][(b+c)/2,-(b/2)-c/2+d,-(b/2)-c/2+e,-(b/2)-c/2+f,n,-(1/4)*(b-c)^2];


(* ::Subsection:: *)
(*Integral conversion*)


(* ::Subsubsection:: *)
(*Helper*)


gammaS[arg_] :=
    Gamma@Simplify@arg;

multiGammaS[args___] :=
    gammaFrom@Simplify@multiGamma[args];


INTCancel[expr_,{vars__}]/;!FreeQ[expr,_INT] :=
    1/INT[vars];

INTCancel[expr_,{vars__}]/;FreeQ[expr,_INT] :=
    1;


(* ::Subsubsection:: *)
(*hyperFromIntegral*)


hyperFromIntegral[All,head_:Identity][expr_] :=
    expr//ReplaceAll[
        u_^a_*(1-u_)^b_*(u_*x_.+y_)^c_/;FreeQ[y,u]:>
            INTCancel[expr,{u}]*hyperFromIntegralKernel[{a,b,c,x,y},head]
    ];

hyperFromIntegral[u_,head_:Identity][expr_] :=
    expr//ReplaceAll[
        u^a_*(1-u)^b_*(u*x_.+y_)^c_/;FreeQ[y,u]:>
            INTCancel[expr,{u}]*hyperFromIntegralKernel[{a,b,c,x,y},head]
    ];


hyperFromIntegralKernel[{a_,b_,c_,x_,y_},head_] :=
    (gammaS[1+a]*gammaS[1+b])/gammaS[2+a+b]*y^c*head[Hypergeometric2F1][1+a,-c,2+a+b,-(x/y)];


(* ::Subsubsection:: *)
(*AppellF1FromIntegral*)


AppellF1FromIntegral[All,head_:Identity][expr_] :=
    expr//ReplaceAll[
        u_^a_*(1-u_)^b_*(u_*x_.+x1_)^c_*(u_*y_.+y1_)^d_/;FreeQ[{x1,y1},u]:>
            INTCancel[expr,{u}]*AppellF1FromIntegralKernel[{a,b,c,d,x,x1,y,y1},head]
    ];

AppellF1FromIntegral[u_,head_:Identity][expr_] :=
    expr//ReplaceAll[
        u^a_*(1-u)^b_*(u*x_.+x1_)^c_*(u*y_.+y1_)^d_/;FreeQ[{x1,y1},u]:>
            INTCancel[expr,{u}]*AppellF1FromIntegralKernel[{a,b,c,d,x,x1,y,y1},head]
    ];


AppellF1FromIntegralKernel[{a_,b_,c_,d_,x_,x1_,y_,y1_},head_] :=
    (gammaS[1+a]*gammaS[1+b])/gammaS[2+a+b]*x1^c*y1^d*head[AppellF1][1+a,-c,-d,2+a+b,-(x/x1),-(y/y1)];


(* ::Subsubsection:: *)
(*conformalIntegral2*)


conformalIntegral2[{z1_,zb1_,z2_,zb2_},All][expr_] :=
    expr//ReplaceAll[
        (z0_-z1)^h1_*(z0_-z2)^h2_*(zb0_-zb1)^hb1_*(zb0_-zb2)^hb2_:>
            INTCancel[expr,{z0}]*conformalIntegral2Kernel[{z1,zb1,z2,zb2},{h1,hb1,h2,hb2}]
    ];

conformalIntegral2[{z1_,zb1_,z2_,zb2_},{z0_,zb0_}][expr_] :=
    expr//ReplaceAll[
        (z0-z1)^h1_*(z0-z2)^h2_*(zb0-zb1)^hb1_*(zb0-zb2)^hb2_:>
            INTCancel[expr,{z0}]*conformalIntegral2Kernel[{z1,zb1,z2,zb2},{h1,hb1,h2,hb2}]
    ];


conformalIntegral2Kernel[{z1_,zb1_,z2_,zb2_},{h1_,hb1_,h2_,hb2_}] :=
    ConditionalExpression[
        (-1)^(h1-hb1)*π^2*multiGammaS[{1+h1,1+h2},{-hb1,-hb2}]*
        DiracDelta[z1-z2,zb1-zb2],
        (* Condition *)
        2+h1+h2==0&&2+hb1+hb2==0&&isZ[h1-hb1,h2-hb2]
    ];


(* ::Subsubsection:: *)
(*conformalIntegral3*)


conformalIntegral3[{z1_,zb1_,z2_,zb2_,z3_,zb3_},All][expr_] :=
    expr//ReplaceAll[
        (z0_-z1)^h1_*(z0_-z2)^h2_*(z0_-z3)^h3_*(zb0_-zb1)^hb1_*(zb0_-zb2)^hb2_*(zb0_-zb3)^hb3_:>
            INTCancel[expr,{z0}]*conformalIntegral3Kernel[{z1,zb1,z2,zb2,z3,zb3},{h1,hb1,h2,hb2,h3,hb3}]
        ];

conformalIntegral3[{z1_,zb1_,z2_,zb2_,z3_,zb3_},{z0_,zb0_}][expr_] :=
    expr//ReplaceAll[
        (z0-z1)^h1_*(z0-z2)^h2_*(z0-z3)^h3_*(zb0-zb1)^hb1_*(zb0-zb2)^hb2_*(zb0-zb3)^hb3_:>
            INTCancel[expr,{z0}]*conformalIntegral3Kernel[{z1,zb1,z2,zb2,z3,zb3},{h1,hb1,h2,hb2,h3,hb3}]
    ];

conformalIntegral3Kernel[{z1_,zb1_,z2_,zb2_,z3_,zb3_},{h1_,hb1_,h2_,hb2_,h3_,hb3_}] :=
    ConditionalExpression[
        π*multiGammaS[{1+h1,1+h2,1+h3},{-hb1,-hb2,-hb3}]*
        (z1-z2)^(-1-h3)*(z2-z3)^(-1-h1)*(-z1+z3)^(-1-h2)*
        (zb1-zb2)^(-1-hb3)*(zb2-zb3)^(-1-hb1)*(-zb1+zb3)^(-1-hb2),
        (* Condition *)
        2+h1+h2+h3==0&&2+hb1+hb2+hb3==0&&isZ[h1-hb1,h2-hb2,h3-hb3]
    ];


(* ::Subsubsection:: *)
(*conformalIntegralKLT*)


conformalIntegralKLT[{z1_,zb1_,z2_,zb2_},All][expr_] :=
    expr//ReplaceAll[
        (z0_-z1)^h1_*(z0_-z2)^h2_*(zb0_-zb1)^hb1_*(zb0_-zb2)^hb2_:>
            INTCancel[expr,{z0}]*conformalIntegralKLTKernel[{z1,zb1,z2,zb2},{h1,hb1,h2,hb2}]
        ];

conformalIntegralKLT[{z1_,zb1_,z2_,zb2_},{z0_,zb0_}][expr_] :=
    expr//ReplaceAll[
        (z0-z1)^h1_*(z0-z2)^h2_*(zb0-zb1)^hb1_*(zb0-zb2)^hb2_:>
            INTCancel[expr,{z0}]*conformalIntegralKLTKernel[{z1,zb1,z2,zb2},{h1,hb1,h2,hb2}]
    ];

conformalIntegralKLTKernel[{z1_,zb1_,z2_,zb2_},{h1_,hb1_,h2_,hb2_}] :=
    ConditionalExpression[
        (-1)^(h1-hb1)*π*multiGammaS[{-1-h1-h2,1+hb1,1+hb2},{-h1,-h2,2+hb1+hb2}]*
        (z1-z2)^(1+h1+h2)*(zb1-zb2)^(1+hb1+hb2),
        (* Condition *)
        isZ[h1-hb1,h2-hb2]
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
