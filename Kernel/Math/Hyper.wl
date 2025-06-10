(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Hyper`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Constant`"];


(* ::Section:: *)
(*Public*)


hyperSeparate::usage =
    "split a product into a list containing Hypergeometric2F1 factors and the rests.";

hyperUnregularize::usage =
    "convert Hypergeometric2F1Regularized to Hypergeometric2F1.";


hyperTaylor::usage =
    "head used by hyperToTaylor.";

hyperMellinBarnes::usage =
    "head used by hyperToMellinBarnes and hyperToMellinBarnes2.";

hyperToTaylor::usage =
    "convert Hypergeometric2F1 factors to Taylor terms.";

hyperToMellinBarnes::usage =
    "convert Hypergeometric2F1 factors to Mellin-Barnes integrands.";

hyperToMellinBarnes2::usage =
    "convert Hypergeometric2F1 factors to Mellin-Barnes integrands in terms of (1-z).";


jacobiPhi::usage =
    "head of Jacobi Phi, jacobiPhi[a,b,c,z], DLMF:15.9.11.";

jacobiPhiToHyper::usage =
    "convert Jacobi Phi to Hypergeometric2F1.";

jacobiPhiFromHyper::usage =
    "convert Hypergeometric2F1 to Jacobi Phi.";


wilsonPolynomial::usage =
    "head of Wilson polynomial, wilsonPolynomial[a,b,c,d,n,x].";

wilsonPolynomialToHyper::usage =
    "convert Wilson polynomial to Hypergeometric4F3.";

wilsonPolynomialFromHyper::usage =
    "convert Hypergeometric4F3 to Wilson polynomial.";


AppellF1FromIntegral::usage =
    "integral representation of Appell F1.";

AppellF1ToHyper::usage =
    "convert Appell F1 to summation of Hypergeometric2F1.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Message*)


hyperTo::usage =
    "convert Hypergeometric functions according to the prototype rule.";

hyperTo::symbolNotEnough =
    "there are `` more Hypergeometric functions than the number of specified symbols."


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
(*hyperTo*)


(* ::Subsubsection:: *)
(*Main*)


hyperToTaylor[symbols__][expr_] :=
    hyperTo[hyperToTaylorRule,{symbols}][expr];

hyperToMellinBarnes[symbols__][expr_] :=
    hyperTo[hyperToMellinBarnesRule,{symbols}][expr];

hyperToMellinBarnes2[symbols__][expr_] :=
    hyperTo[hyperToMellinBarnesRule2,{symbols}][expr];


hyperTo[which_,symbolList_][expr0_] :=
    Module[ {expr,positionList,numberOfHyper,numberOfSymbol,hyperList,convertedHyperList,result},
        expr =
            expr0//hyperToPreprocess;
        positionList =
            Position[expr,_Hypergeometric2F1|_Hypergeometric1F1|_Hypergeometric0F1|_HypergeometricPFQ];
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
                Message[hyperTo::symbolNotEnough,numberOfHyper-numberOfSymbol];
                result = expr,
            numberOfHyper<=numberOfSymbol,
                convertedHyperList =
                    MapThread[which,{Take[symbolList,numberOfHyper],hyperList}];
                result =
                    ReplacePart[expr,MapThread[Rule,{positionList,convertedHyperList}]];
        ];
        result//hyperToPostprocess
    ];


(* ::Subsubsection:: *)
(*Helper*)


hyperToPreprocess[expr_] :=
    ReplaceAll[
        Hold[expr],
        Power[hyper_Hypergeometric2F1,n_]:>
            RuleCondition[Inactive[Times]@@ConstantArray[hyper,n]]
    ];


hyperToPostprocess[expr_] :=
    Activate[ReleaseHold[expr],Times];


hyperToMellinBarnesRule[s_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyperMellinBarnes[((-z)^s Gamma[-s] Gamma[c] Gamma[a+s] Gamma[b+s])/(Gamma[a] Gamma[b] Gamma[c+s])];

hyperToMellinBarnesRule[s_,Hypergeometric1F1[a_,b_,z_]] :=
    hyperMellinBarnes[((-z)^s Gamma[-s] Gamma[b] Gamma[a+s])/(Gamma[a] Gamma[b+s])];

hyperToMellinBarnesRule[s_,Hypergeometric0F1[a_,z_]] :=
    hyperMellinBarnes[((-z)^s Gamma[-s] Gamma[a])/(Gamma[a+s])];

hyperToMellinBarnesRule[s_,HypergeometricPFQ[as_List,bs_List,z_]] :=
    hyperMellinBarnes[
        Times@@Map[Pochhammer[#,s]&,as]/Times@@Map[Pochhammer[#,s]&,bs] (-z)^s Gamma[-s]//gammaFrom
    ];


hyperToMellinBarnesRule2[s_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyperMellinBarnes[((1-z)^s Gamma[c] Gamma[-a-b+c-s] Gamma[-s] Gamma[a+s] Gamma[b+s])/(Gamma[a] Gamma[b] Gamma[-a+c] Gamma[-b+c])];


hyperToTaylorRule[n_,Hypergeometric2F1[a_,b_,c_,z_]] :=
    hyperTaylor[(z^n Gamma[c] Gamma[a+n] Gamma[b+n])/(Gamma[1+n] Gamma[a] Gamma[b] Gamma[c+n])];

hyperToTaylorRule[n_,Hypergeometric1F1[a_,b_,z_]] :=
    hyperTaylor[(z^n Gamma[b] Gamma[a+n])/(Gamma[a] Gamma[1+n] Gamma[b+n])];

hyperToTaylorRule[n_,Hypergeometric0F1[a_,z_]] :=
    hyperTaylor[(z^n Gamma[a])/(Gamma[1+n] Gamma[a+n])];

hyperToTaylorRule[n_,HypergeometricPFQ[as_List,bs_List,z_]] :=
    hyperTaylor[
        Times@@Map[Pochhammer[#,n]&,as]/Times@@Map[Pochhammer[#,n]&,bs] z^n/n!//gammaFrom
    ];


(* ::Subsection:: *)
(*jacobiPhi*)


jacobiPhiToHyper[expr_] :=
    expr//ReplaceAll[DLMFData["JacobiPhiToHyper"]]//ReplaceAll[head_HypergeometricPFQ:>Simplify[head]];


jacobiPhiFromHyper[expr_] :=
    expr//ReplaceAll[DLMFData["JacobiPhiFromHyper"]]//ReplaceAll[head_jacobiPhi:>Simplify[head]];


(* ::Subsection:: *)
(*wilsonPolynomial*)


wilsonPolynomialToHyper[expr_] :=
    expr//ReplaceAll[DLMFData["WilsonToHyper"]]//ReplaceAll[head_HypergeometricPFQ:>Simplify[head]];


wilsonPolynomialFromHyper[expr_] :=
    expr//ReplaceAll[DLMFData["WilsonFromHyper"]]//ReplaceAll[head_wilsonPolynomial:>Simplify[head]];


(* ::Subsection:: *)
(*AppellF1*)


AppellF1FromIntegral[head_:Inactive][expr_] :=
    expr//ReplaceAll[ruleF1[head]];


ruleF1[head_]:=
    u_^a_*(1-u_)^b_*(1-u_*x_)^c_*(1-u_*y_)^d_:>
        Simplify[(Gamma[1+a]*Gamma[1+b])/Gamma[2+a+b]*head[AppellF1][1+a,-c,-d,2+a+b,x,y]];


AppellF1ToHyper[max_:Infinity,head_:Inactive[Sum]][expr_] :=
    expr//ReplaceAll[ruleF1ToF21Sum[max,head]];


ruleF1ToF21Sum[max_,head_]:=
    Module[ {n},
        (AppellF1|_[AppellF1])[a_,b1_,b2_,c_,x_,y_]:>
            head[
                (
                    (Pochhammer[a,n] Pochhammer[b1,n] Pochhammer[b2,n] Pochhammer[c-a,n])/
                    (n! Pochhammer[c+n-1,n] Pochhammer[c,2 n])
                )*x^n*y^n*
                Hypergeometric2F1[a+n,b1+n,c+2 n,x]*
                Hypergeometric2F1[a+n,b2+n,c+2 n,y],
                {n,0,max}
            ]
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
