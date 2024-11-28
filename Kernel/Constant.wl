(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Constant`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Gamma*)


gamma::usage =
    "internal head of Gamma.";

gammaData::usage =
    "rules to convert other functions to Gamma.";

gammaDataKeyList::usage =
    "cache of gammaData keys.";

gammaDataValueList::usage =
    "cache of gammaData values.";


(* ::Subsection:: *)
(*DLMF*)


DLMFData::usage =
    "rules from https://dlmf.nist.gov/.";


(* ::Subsection:: *)
(*Simplify*)


ruleMergePower::usage =
    "rules to merging powers.";

ruleExtractPhase::usage =
    "rules to extract phases from powers.";

ruleSeparatePower::usage =
    "rules to separate powers.";

ruleCombinePower::usage =
    "rules to combine powers.";

ruleCollectPower::usage =
    "rules to collect powers by specified variables.";

ruleSeparateExpr::usage =
    "rules to separate other functions.";

ruleCombineExpr::usage =
    "rules to combine other functions.";

ruleCancelDiracDelta::usage =
    "rules to cancel the Dirac Delta function and its derivatives.";

ruleTrigPhase::usage =
    "rules to separate the phase factor in trigonometric functions.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Gamma*)


gammaData = <|
    "Trig"->{
        Sin[x_]:>
            \[Pi]/(gamma[x/\[Pi]] gamma[1-x/\[Pi]]),
        Cos[x_]:>
            (*Sin[x+\[Pi]/2]*)
            \[Pi]/(gamma[1/2-x/\[Pi]] gamma[1/2+x/\[Pi]]),
        Tan[x_]:>
            (*Sin[x]/Sin[x+\[Pi]/2]*)
            (gamma[1/2-x/\[Pi]] gamma[1/2+x/\[Pi]])/(gamma[x/\[Pi]] gamma[1-x/\[Pi]]),
        Cot[x_]:>
            (*Sin[x+\[Pi]/2]/Sin[x]*)
            (gamma[x/\[Pi]] gamma[1-x/\[Pi]])/(gamma[1/2-x/\[Pi]] gamma[1/2+x/\[Pi]]),
        Csc[x_]:>
            (*1/Sin[x]*)
            (gamma[x/\[Pi]] gamma[1-x/\[Pi]])/\[Pi],
        Sec[x_]:>
            (*1/Sin[x+\[Pi]/2]*)
            (gamma[1/2-x/\[Pi]] gamma[1/2+x/\[Pi]])/\[Pi]
    },
    "Factorial"->{
        Factorial[x_]:>gamma[x+1]
    },
    "Binomial"->{
        Binomial[a_,n_]:>
            gamma[a+1]/(gamma[n+1]*gamma[a-n+1])
    },
    "Beta"->{
        Beta[a_,b_]:>
            (gamma[a]*gamma[b])/gamma[a+b]
    },
    "Pochhammer"->{
        Pochhammer[a_,n_]:>
            gamma[a+n]/gamma[a]
    },
    "MultiGamma"->{
        HoldPattern[multiGamma[num_List,denom_List]]:>
            (Times@@Map[gamma,num])/(Times@@Map[gamma,denom])
    }
|>;


gammaDataKeyList =
    gammaData//Keys;


gammaDataValueList =
    gammaData//Values//Flatten;


(* ::Subsection:: *)
(*DLMF*)


DLMFData = <|
    (*Gamma*)
    "5.5.5"->{
        Gamma[z_]:>
            1/Sqrt[\[Pi]]*2^(z-1)*Gamma[z/2]*Gamma[z/2+1/2]
    },
    "BinomialSwapA"->{
        Binomial[a_,n_]:>
            (-1)^n*Binomial[n-a-1,n]
    },
    "BinomialSwapN"->{
        Binomial[a_,n_]:>
            Binomial[a,a-n]
    },
    "PochhammerSwapA"->{
        Pochhammer[a_,n_]:>
            (-1)^n*Pochhammer[-a-n+1,n]
    },
    "5.2.6"->{
        Pochhammer[a_,n_]:>
            (-1)^n*Pochhammer[-a-n+1,n]
    },
    "5.2.8.1"->{
        Pochhammer[a_,n_]/;Simplify[EvenQ[n]]:>
            2^n*Pochhammer[a/2,n/2]*Pochhammer[(a+1)/2,n/2]
    },
    "5.2.8.2"->{
        Pochhammer[a_,n_]/;Simplify[OddQ[n]]:>
            2^n*Pochhammer[a/2,(n+1)/2]*Pochhammer[(a+1)/2,(n-1)/2]
    },
    (*Hypergeometric2F1*)
    "HyperSwapAB"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            Hypergeometric2F1[b,a,c,z]
    },
    "HyperRegToUnreg"->{
        Hypergeometric2F1Regularized[a_,b_,c_,z_]:>
            Hypergeometric2F1[a,b,c,z]/Gamma[c]
    },
    "15.1.2"->{
        Hypergeometric2F1Regularized[a_,b_,c_,z_]:>
            Hypergeometric2F1[a,b,c,z]/Gamma[c]
    },
    "15.4.20"->{
        Hypergeometric2F1[a_,b_,c_,1]/;Simplify[c-a-b>0]:>
            Gamma[c]*Gamma[c-a-b]/(Gamma[c-a]*Gamma[c-b])
    },
    "15.5.11"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            1/(2*a-c+(-a+b)*z)*(
                (a-c)*Hypergeometric2F1[a-1,b,c,z]-
                a*(z-1)*Hypergeometric2F1[a+1,b,c,z]
            )
    },
    "15.5.12"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            1/(a-b)*(
                a*Hypergeometric2F1[a+1,b,c,z]-
                b*Hypergeometric2F1[a,b+1,c,z]
            )
    },
    "15.5.13"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            1/(a+b-c)*(
                a*(1-z)*Hypergeometric2F1[a+1,b,c,z]+
                (b-c)*Hypergeometric2F1[a,b-1,c,z]
            )
    },
    "15.5.14"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            1/(c*(a+(b-c)*z))*(
                a*c*(1-z)*Hypergeometric2F1[a+1,b,c,z]-
                (c-a)*(c-b)*z*Hypergeometric2F1[a,b,c+1,z]
            )
    },
    "15.5.15"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            1/(a+1-c)*(
                a*Hypergeometric2F1[a+1,b,c,z]-
                (c-1)*Hypergeometric2F1[a,b,c-1,z]
            )
    },
    "15.5.16"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            1/(c*(1-z))*(
                c*Hypergeometric2F1[a-1,b,c,z]+
                (b-c)*z*Hypergeometric2F1[a,b,c+1,z]
            )
    },
    "15.5.17"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            1/(-1+a+(1+b-c)*z)*(
                (a-c)*Hypergeometric2F1[a-1,b,c,z]+
                (c-1)*(1-z)*Hypergeometric2F1[a,b,c-1,z]
            )
    },
    "15.5.18"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            1/(c*(1-c+(-1-a-b+2*c)*z))*(
                c*(c-1)*(z-1)*Hypergeometric2F1[a,b,c-1,z]+
                (c-a)*(c-b)*z*Hypergeometric2F1[a,b,c+1,z]
            )
    },
    "15.8.1.1"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            (1-z)^-a*Hypergeometric2F1[a,c-b,c,z/(z-1)]
    },
    "15.8.1.2"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            (1-z)^-b*Hypergeometric2F1[c-a,b,c,z/(z-1)]
    },
    "15.8.1.3"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            (1-z)^(c-a-b)*Hypergeometric2F1[c-a,c-b,c,z]
    },
    "15.8.2"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>(\[Pi]*Gamma[c])/Sin[\[Pi]*(b-a)]*(
            (-z)^-a/(Gamma[1+a-b]*Gamma[b]*Gamma[-a+c])*Hypergeometric2F1[a,1+a-c,1+a-b,1/z]-
            (-z)^-b/(Gamma[a]*Gamma[1-a+b]*Gamma[-b+c])*Hypergeometric2F1[b,1+b-c,1-a+b,1/z]
        )
    },
    "15.8.3"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>(\[Pi]*Gamma[c])/Sin[\[Pi]*(b-a)]*(
            (1-z)^-a/(Gamma[1+a-b]*Gamma[b]*Gamma[-a+c])*Hypergeometric2F1[a,c-b,1+a-b,1/(1-z)]-
            (1-z)^-b/(Gamma[a]*Gamma[1-a+b]*Gamma[-b+c])*Hypergeometric2F1[b,c-a,1-a+b,1/(1-z)]
        )
    },
    "15.8.4"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>(\[Pi]*Gamma[c])/Sin[\[Pi]*(c-a-b)]*(
            1/(Gamma[a+b-c+1]*Gamma[c-b]*Gamma[-a+c])*Hypergeometric2F1[a,b,a+b-c+1,1-z]-
            (1-z)^(c-a-b)/(Gamma[a]*Gamma[b]*Gamma[c-a-b+1])*Hypergeometric2F1[c-a,c-b,c-a-b+1,1-z]
        )
    },
    "15.8.5"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>(\[Pi]*Gamma[c])/Sin[\[Pi]*(c-a-b)](
            (z)^-a/(Gamma[a+b-c+1]*Gamma[c-b]*Gamma[-a+c])*Hypergeometric2F1[a,1+a-c,a+b-c+1,1-1/z]-
            ((1-z)^(c-a-b)*z^(a-c))/(Gamma[a]*Gamma[b]*Gamma[c-a-b+1])*Hypergeometric2F1[c-a,1-a,c-a-b+1,1-1/z]
        )
    },
    "15.8.13"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[2b-c==0]:>
            (1-z/2)^-a*Hypergeometric2F1[a/2,a/2+1/2,b+1/2,(z/(2-z))^2]
    },
    "15.8.14"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[2b-c==0]:>
            (1-z)^(-a/2)*Hypergeometric2F1[a/2,b-a/2,b+1/2,z^2/(4*z-4)]
    },
    "15.8.15"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a-b+1-c==0]:>
            (1+z)^-a*Hypergeometric2F1[a/2,a/2+1/2,a-b+1,(4*z)/(1+z)^2]
    },
    "15.8.16"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a-b+1-c==0]:>
            (1-z)^-a*Hypergeometric2F1[a/2,a/2-b+1/2,a-b+1,(-4*z)/(1-z)^2]
    },
    "15.8.17"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a+b+1-2c==0]:>
            (1-2*z)^-a*Hypergeometric2F1[a/2,a/2+1/2,(a+b+1)/2,(4*z*(z-1))/(1-2*z)^2]
    },
    "15.8.18"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a+b+1-2c==0]:>
            Hypergeometric2F1[a/2,b/2,(a+b+1)/2,4*z*(1-z)]
    },
    "15.8.19"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a+b-1==0]:>
            (1-2*z)^(1-a-c)*(1-z)^(-1+c)*Hypergeometric2F1[(a+c)/2,(a+c-1)/2,c,(4*z*(z-1))/(1-2*z)^2]
    },
    "15.8.20"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a+b-1==0]:>
            (1-z)^(c-1)*Hypergeometric2F1[(c-a)/2,(a+c-1)/2,c,4*z*(1-z)]
    },
    "15.8.21"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a-b+1-c==0]:>
            (1+Sqrt[z])^(-2*a)*Hypergeometric2F1[a,1/2+a-b,1+2*a-2*b,(4*Sqrt[z])/(1+Sqrt[z])^2]
    },
    "15.8.22"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a+b+1-2c==0]:>
            ((-1+Sqrt[1-1/z])/(1+Sqrt[1-1/z]))^a*Hypergeometric2F1[a,(a+b)/2,a+b,(4*Sqrt[1-1/z])/(1+Sqrt[1-1/z])^2]
    },
    "15.8.23"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a+b-1==0]:>
            (-1+Sqrt[1-1/z])^(1-a)*(1+Sqrt[1-1/z])^(1+a-2*c)*(1-1/z)^(-1+c)*Hypergeometric2F1[-(1/2)+c,-a+c,-1+2*c,(4*Sqrt[1-1/z])/(1+Sqrt[1-1/z])^2]
    },
    "15.8.24"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a-b+1-c==0]:>
            ((Sqrt[\[Pi]]*(1-z)^-a*Gamma[1+a-b])/(Gamma[1/2+a/2]*Gamma[1+a/2-b]))*Hypergeometric2F1[a/2,a/2-b+1/2,1/2,((z+1)/(z-1))^2]-
            (2*Sqrt[\[Pi]]*(1-z)^(-1-a)*(1+z)*Gamma[1+a-b])/(Gamma[a/2]*Gamma[1/2+a/2-b])*Hypergeometric2F1[a/2+1/2,a/2-b+1,3/2,((z+1)/(z-1))^2]
    },
    "15.8.25"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a+b+1-2c==0]:>
            (Sqrt[\[Pi]]*Gamma[1/2*(1+a+b)])/(Gamma[1/2+a/2]*Gamma[1/2+b/2])*Hypergeometric2F1[a/2,b/2,1/2,(1-2*z)^2]-
            (2*Sqrt[\[Pi]]*(1-2*z)*Gamma[1/2*(1+a+b)])/(Gamma[a/2]*Gamma[b/2])*Hypergeometric2F1[a/2+1/2,b/2+1/2,3/2,(1-2*z)^2]
    },
    "15.8.26"->{
        Hypergeometric2F1[a_,b_,c_,z_]/;Simplify[a+b-1==0]:>
            (Sqrt[\[Pi]]*(1-z)^(-1+c)*Gamma[c])/(Gamma[a/2+c/2]*Gamma[1/2*(1-a+c)])*Hypergeometric2F1[c/2-a/2,c/2+a/2-1/2,1/2,(1-2*z)^2]-
            (2*Sqrt[\[Pi]]*(1-2*z)(1-z)^(-1+c)*Gamma[c])/(Gamma[-(a/2)+c/2]*Gamma[1/2*(-1+a+c)])*Hypergeometric2F1[c/2-a/2+1/2,c/2+a/2,3/2,(1-2*z)^2]
    },
    "15.8.27"->{
        Hypergeometric2F1[a_,b_,1/2,z_]:>
            (Gamma[1/2+a]*Gamma[1/2+b])/(2*Sqrt[\[Pi]]*Gamma[1/2+a+b])*(
                Hypergeometric2F1[2*a,2*b,1/2+a+b,1/2*(1-Sqrt[z])]+
                Hypergeometric2F1[2*a,2*b,1/2+a+b,1/2*(1+Sqrt[z])]
            )
    },
    "15.8.28"->{
        Hypergeometric2F1[a_,b_,3/2,z_]:>
            -((Gamma[-(1/2)+a]*Gamma[-(1/2)+b])/(4*Sqrt[\[Pi]]*Sqrt[z]*Gamma[-(1/2)+a+b]))*(
                Hypergeometric2F1[-1+2*a,-1+2*b,-(1/2)+a+b,1/2*(1-Sqrt[z])]-
                Hypergeometric2F1[-1+2*a,-1+2*b,-(1/2)+a+b,1/2*(1+Sqrt[z])]
            )
    },
    (*HypergeometricPFQ*)
    "16.4.14"->{
        HoldPattern[HypergeometricPFQ[{-n_,a_,b_,c_},{d_,e_,f_},1]]/;Simplify[a+b+c-d-e-f+1-n==0]:>
            ((Pochhammer[e-a,n] Pochhammer[f-a,n]) HypergeometricPFQ[{-n,a,d-b,d-c},{d,a-e-n+1,a-f-n+1},1])/(Pochhammer[e,n] Pochhammer[f,n])
    },
    (*LegendreToHypergeometric2F1*)
    "14.3.1"->{
        LegendreP[a_,b_,z_]:>
            ((1+z)/(1-z))^(b/2)/Gamma[1-b]*Hypergeometric2F1[a+1,-a,1-b,1/2-z/2]
    },
    "14.3.2"->{
        LegendreQ[a_,b_,z_]:>
            (\[Pi]*((1+z)/(1-z))^(b/2)*Cot[b \[Pi]])/(2*Gamma[1-b])*Hypergeometric2F1[a+1,-a,1-b,1/2-z/2]-
            (\[Pi]*((1-z)/(1+z))^(b/2)*Csc[b \[Pi]]*Gamma[1+a+b])/(2*Gamma[1+a-b]*Gamma[1+b])*Hypergeometric2F1[a+1,-a,1+b,1/2-z/2]
    },
    "14.3.6"->{
        LegendreP[a_,b_,3,z_]:>
            ((z+1)/(z-1))^(b/2)/Gamma[1-b]*Hypergeometric2F1[a+1,-a,1-b,1/2-z/2]
    },
    "14.3.7"->{
        LegendreQ[a_,b_,3,z_]:>
            Exp[I*\[Pi] b](Sqrt[\[Pi]]*Gamma[a+b+1]*(z^2-1)^(b/2))/(2^(a+1)*z^(a+b+1)*Gamma[a+3/2])*Hypergeometric2F1[a/2+b/2+1,a/2+b/2+1/2,a+3/2,1/z^2]
    },
    "LegendreAtZ=0"->{
        LegendreP[a_,0]:>
            Sqrt[\[Pi]]/(Gamma[1/2-a/2]*Gamma[1+a/2]),
        LegendreQ[a_,0]:>
            -((Sqrt[\[Pi]]*Gamma[(1+a)/2]*Sin[(a*\[Pi])/2])/(2*Gamma[(2+a)/2])),
        LegendreP[a_,-1,0]:>
            Sqrt[\[Pi]]/(Gamma[3/2+a/2]*Gamma[1-a/2]),
        LegendreQ[a_,-1,0]:>
            (Sqrt[\[Pi]]*Cos[(a*\[Pi])/2]*Gamma[a/2])/(4*Gamma[(3+a)/2])
    },
    "LegendreAtB=1/2"->{
        LegendreP[a_,1/2,z_]:>
            ((z+I*Sqrt[1-z^2])^(-(1/2)-a)+(z+I*Sqrt[1-z^2])^(1/2+a))/(Sqrt[2*\[Pi]]*(1-z^2)^(1/4)),
        LegendreQ[a_,1/2,z_]:>
            -(1/2)*I*(-((Sqrt[\[Pi]]*(z-I*Sqrt[1-z^2])^(-(1/2)-a))/(Sqrt[2]*(1-z^2)^(1/4)))+(Sqrt[\[Pi]/2]*(z+I*Sqrt[1-z^2])^(-(1/2)-a))/(1-z^2)^(1/4)),
        LegendreP[a_,1/2,3,z_]:>
            ((z+Sqrt[-1+z^2])^(1/2+a)+(z+Sqrt[-1+z^2])^(-(1/2)-a))/(Sqrt[2\[Pi]]*(-1+z^2)^(1/4)),
        LegendreQ[a_,1/2,3,z_]:>
            (I*Sqrt[\[Pi]]*(z+Sqrt[-1+z^2])^(-(1/2)-a))/(Sqrt[2]*(-1+z^2)^(1/4))
    },
    (*Wilson polynomial*)
    "WilsonPolynomialToHyper"->{
        wilsonPolynomial[a_,b_,c_,d_,n_,x_]:>
            (Gamma[a+b+n]*Gamma[a+c+n]*Gamma[a+d+n])/(Gamma[a+b]*Gamma[a+c]*Gamma[a+d])*HypergeometricPFQ[{-n,-1+a+b+c+d+n,a-I*Sqrt[x],a+I*Sqrt[x]},{a+b,a+c,a+d},1]
    },
    "WilsonPolynomialFromHyper"->{
        HoldPattern[HypergeometricPFQ[{-n_,a_,b_,c_},{d_,e_,f_},1]]/;Simplify[a+b+c-d-e-f+1-n==0]:>
            (Gamma[d]*Gamma[e]*Gamma[f])/(Gamma[1+a+b+c-d-e]*Gamma[1+a+b+c-d-f]*Gamma[1+a+b+c-e-f])*wilsonPolynomial[(b+c)/2,-(b/2)-c/2+d,-(b/2)-c/2+e,-(b/2)-c/2+f,n,-(1/4)*(b-c)^2]
    },
    (*Jacobi Phi function*)
    "JacobiPhiToHyper"->{
        jacobiPhi[a_,b_,c_,z_]:>
            Hypergeometric2F1[(a+b+1-I*c)/2,(a+b+1+I*c)/2,a+1,-Sinh[z]^2]
    },
    "JacobiPhiFromHyper"->{
        Hypergeometric2F1[a_,b_,c_,z_]:>
            jacobiPhi[c-1,a+b-c,I(a-b),ArcSinh[Sqrt[-z]]]
    }
|>;




(* ::Subsection:: *)
(*Simplify*)


ruleMergePower = {
    IgnoringInactive[(x_^a_)^b_]:>
        x^(a*b)
};


ruleExtractPhase = {
    IgnoringInactive[(-x_)^a_]:>
        (-1)^a*x^a,
    IgnoringInactive[(I*x_.)^a_]:>
        Exp[I*Pi/2*a]*x^a,
    IgnoringInactive[(-I*x_.)^a_]:>
        Exp[-I*Pi/2*a]*x^a,
    IgnoringInactive[(-1)^(k_Integer*a_.+b_.)]/;EvenQ@k:>
        (-1)^b,
    IgnoringInactive[(-1)^(-a_)]/;!IntegerQ[a]:>
        (-1)^a
};


ruleSeparatePower = {
    Splice@ruleMergePower,
    Splice@ruleExtractPhase,
    IgnoringInactive[(x_*y_)^a_]:>
        x^a*y^a,
    IgnoringInactive[x_^(a_+b_)]:>
        Inactivate[x^a*x^b,Power|Sqrt]
};


(*These rules can cause loops.*)

ruleCombinePower = {
    Splice@ruleMergePower,
    Splice@ruleExtractPhase,
    IgnoringInactive[(x_*rest1_.)^a_*(y_*rest2_.)^b_]/;Simplify[x-y===0]:>
        x^(a+b)*rest1^a*rest2^b,
    IgnoringInactive[(x_*rest1_.)^a_*(y_*rest2_.)^b_]/;Simplify[x+y===0]:>
        (-1)^(b)*x^(a+b)*rest1^a*rest2^b,
    IgnoringInactive[x_^a_*y_^b_]/;x=!=-1&&y=!=-1&&a===b:>
        Inactivate[(x*y)^a,Power|Sqrt]
};


ruleCollectPower[power_] :=
    ruleCollectPower[power] =
        {
            Splice@ruleMergePower,
            IgnoringInactive[x_^(power*k1_.+rest1_.)*y_^(power*k2_.+rest2_.)]:>
                With[ {var = Simplify[x^k1*y^k2]},
                    If[ IntegerQ[x]||IntegerQ[y],
                        Inactivate[var^power,Power|Sqrt],
                        var^power
                    ]
                ]*x^rest1*y^rest2
        };

ruleCollectPower[] = {
    Splice@ruleMergePower,
    IgnoringInactive[x_^(power_*k1_.+rest1_.)*y_^(power_*k2_.+rest2_.)]:>
        With[ {var = Simplify[x^k1*y^k2]},
            If[ IntegerQ[x]||IntegerQ[y],
                Inactivate[var^power,Power|Sqrt],
                var^power
            ]
        ]*x^rest1*y^rest2
};


ruleSeparateExpr = {
    Log[x_*y_]:>
        Log[x]+Log[y],
    Log[x_^a_]:>
        a*Log[x],
    Abs[x_*y_]:>
        Abs[x]*Abs[y],
    Abs[x_^a_]:>
        Abs[x]^a,
    Log[Exp[x_]]:>
        x
};


ruleCombineExpr =
    {
        Log[x_]+Log[y_]:>
            Log[x*y//Simplify],
        a_*Log[x_]:>
            Log[x^a],
        Abs[x_]*Abs[y_]:>
            Abs[x*y//Simplify],
        Abs[x_]^a_:>
            Abs[x^a],
        Log[Exp[x_]]:>
            x,
        f_[g_[x_]]/;Simplify[g[f[_]]===_]:>
            x
    };


ruleCancelDiracDelta = {
    x_*DiracDelta[x_]:>
        0,
    x_*Derivative[n_][DiracDelta][x_]:>
        -n*Derivative[n-1][DiracDelta][x],
    Power[x_,n_]*DiracDelta[x_]:>
        0/;n>=1,
    Power[x_,n_]*Derivative[m_][DiracDelta][x_]:>
        -m*Power[x,n-1]*Derivative[m-1][DiracDelta][x]/;n>=1&&m>=1
};


ruleTrigPhase[var_] :=
    ruleTrigPhase[var] =
        {
            (h:Sin|Cos|Csc|Sec)[\[Pi] var+rest_.]:>(-1)^var*h[rest],
            (h:Tan|Cot)[\[Pi] var+rest_.]:>h[rest]
        };


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
