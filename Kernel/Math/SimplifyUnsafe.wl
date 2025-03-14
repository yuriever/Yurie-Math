(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Simplify`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


unsafePowerTogether::usage =
    "take powers together.";

unsafePowerApart::usage =
    "take powers apart, similar to PowerExpand.";

unsafePowerSimplify::usage =
    "simplify powers.";


unsafeExprTogether::usage =
    "take powers, logs and abs together.";

unsafeExprApart::usage =
    "take powers, logs and abs apart.";

unsafeExprSimplify::usage =
    "simplify powers, logs and abs.";


unsafeDeltaSimplify::usage =
    "simplify Delta functions.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Constant*)


$exprSimLoop::usage =
     "control the Nest in unsafeExprSimplify and unsafePowerSimplify.";

$exprSimLoop = 4;


(* ::Subsection:: *)
(*Main*)


unsafePowerApart[expr_] :=
    powerPreprocess[expr]//ReplaceRepeated[ruleSeparatePower]//Activate;


unsafePowerTogether[expr_] :=
    powerPreprocess[expr]//ReplaceRepeated[ruleSeparatePower]//ReplaceRepeated[ruleCombinePower]//Activate;


unsafePowerSimplify[expr_] :=
    Nest[unsafePowerTogether/*Simplify,expr,$exprSimLoop];


unsafeExprApart[expr_] :=
    expr//ReplaceAll[subexpr_Times:>unsafePowerApart@subexpr]//ReplaceRepeated[ruleSeparateExpr]//Activate;


unsafeExprTogether[expr_] :=
    expr//ReplaceAll[subexpr_Times:>unsafePowerTogether@subexpr]//ReplaceRepeated[ruleCombineExpr]//Activate;


unsafeExprSimplify[expr_] :=
    Nest[unsafeExprApart/*unsafeExprTogether/*Simplify,expr,$exprSimLoop];


unsafeDeltaSimplify[expr_] :=
    expr//ReplaceRepeated[ruleCancelDiracDelta]//Simplify;


(* ::Subsection:: *)
(*Helper*)


powerPreprocess[expr_] :=
    expr//ReplaceAll[{subexpr_Plus:>Together@subexpr}];


(* ::Subsection:: *)
(*Rule*)


ruleMergePower::usage =
    "rules to merging powers.";

ruleExtractPhase::usage =
    "rules to extract phases from powers.";

ruleSeparatePower::usage =
    "rules to separate powers.";

ruleCombinePower::usage =
    "rules to combine powers.";

ruleSeparateExpr::usage =
    "rules to separate other functions.";

ruleCombineExpr::usage =
    "rules to combine other functions.";

ruleCancelDiracDelta::usage =
    "rules to cancel the Dirac Delta function and its derivatives.";


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


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
