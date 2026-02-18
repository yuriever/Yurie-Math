(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Unstable`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


JacobiPhi::usage =
    "JacobiPhi[a, b, c, z]: Jacobi Phi function.";

WilsonPolynomial::usage =
    "WilsonPolynomial[a, b, c, d, n, x]: Wilson polynomial.";


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


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*JacobiPhiToHyper*)


JacobiPhiToHyper[head_:Identity][expr_] :=
    expr//ReplaceAll[ruleJacobiPhiToHyper[head]];


ruleJacobiPhiToHyper[head_] :=
    (JacobiPhi|_[JacobiPhi])[a_,b_,c_,z_]:>
        Simplify@head[Hypergeometric2F1][(a+b+1-I*c)/2,(a+b+1+I*c)/2,a+1,-Sinh[z]^2];


(* ::Subsection:: *)
(*JacobiPhiFromHyper*)


JacobiPhiFromHyper[head_:Identity][expr_] :=
    expr//ReplaceAll[ruleJacobiPhiFromHyper[head]];


ruleJacobiPhiFromHyper[head_] :=
    (Hypergeometric2F1|_[Hypergeometric2F1])[a_,b_,c_,z_]:>
        Simplify@head[JacobiPhi][c-1,a+b-c,I(a-b),ArcSinh[Sqrt[-z]]];


(* ::Subsection:: *)
(*WilsonPolynomialToHyper*)


WilsonPolynomialToHyper[head_:Identity][expr_] :=
    expr//ReplaceAll[ruleWilsonPolynomialToHyper[head]];


ruleWilsonPolynomialToHyper[head_] :=
    (WilsonPolynomial|_[WilsonPolynomial])[a_,b_,c_,d_,n_,x_]:>
        multiGamma[{a+b+n,a+c+n,a+d+n},{a+b,a+c,a+d}]*
        Simplify@head[HypergeometricPFQ][{-n,-1+a+b+c+d+n,a-I*Sqrt[x],a+I*Sqrt[x]},{a+b,a+c,a+d},1];


(* ::Subsection:: *)
(*WilsonPolynomialFromHyper*)


WilsonPolynomialFromHyper[head_:Identity][expr_] :=
    expr//ReplaceAll[ruleWilsonPolynomialFromHyper[head]];


ruleWilsonPolynomialFromHyper[head_] :=
    (HypergeometricPFQ|_[HypergeometricPFQ])[{-n_,a_,b_,c_},{d_,e_,f_},1]/;Simplify[a+b+c-d-e-f+1-n==0]:>
        multiGamma[{d,e,f},{1+a+b+c-d-e,1+a+b+c-d-f,1+a+b+c-e-f}]*
        Simplify@head[WilsonPolynomial][(b+c)/2,-(b/2)-c/2+d,-(b/2)-c/2+e,-(b/2)-c/2+f,n,-(1/4)*(b-c)^2];


(* ::Subsection:: *)
(*Helper*)


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
