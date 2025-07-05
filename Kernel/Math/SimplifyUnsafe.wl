(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Simplify`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


unsafePowerTogether::usage =
    "unsafePowerTogether[expr]: try to combine power factors."<>
    "\n"<>
    "May produce mathematically invalid result.";

unsafePowerApart::usage =
    "unsafePowerApart[expr]: try to separate power factors."<>
    "\n"<>
    "May produce mathematically invalid result.";

unsafePowerSimplify::usage =
    "unsafePowerSimplify[expr]: try to simplify power factors."<>
    "\n"<>
    "May produce mathematically invalid result.";


unsafeExprTogether::usage =
    "unsafeExprTogether[expr]: try to combine power factors, logarithms, and absolute values."<>
    "\n"<>
    "May produce mathematically invalid result.";

unsafeExprApart::usage =
    "unsafeExprApart[expr]: try to separate power factors, logarithms, and absolute values."<>
    "\n"<>
    "May produce mathematically invalid result.";

unsafeExprSimplify::usage =
    "unsafeExprSimplify[expr]: try to simplify power factors, logarithms, and absolute values."<>
    "\n"<>
    "May produce mathematically invalid result.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Constant*)


$exprSimLoop::usage =
    "$exprSimLoop: control the number of iterations in unsafeExprSimplify and unsafePowerSimplify."<>
    "\n"<>
    "The default value is 4.";

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


(* ::Subsection:: *)
(*Helper*)


powerPreprocess[expr_] :=
    expr//ReplaceAll[{subexpr_Plus:>Together@subexpr}];


(* ::Subsection:: *)
(*Rule*)


ruleMergePower::usage =
    "ruleMergePower: transformation rules for merging nested powers."<>
    "\n"<>
    "Example: (x^a)^b -> x^(a*b).";

ruleExtractPhase::usage =
    "ruleExtractPhase: transformation rules for extracting phase factors from powers."<>
    "\n"<>
    "Example: (-x)^a -> (-1)^a*x^a, (I*x)^a -> Exp[I*Pi/2*a]*x^a.";

ruleSeparatePower::usage =
    "ruleSeparatePower: transformation rules for separating powers."<>
    "\n"<>
    "Example: (x*y)^a -> x^a*y^a, x^(a+b) -> x^a*x^b.";

ruleCombinePower::usage =
    "ruleCombinePower: transformation rules for combining powers."<>
    "\n"<>
    "May cause infinite loops.";

ruleSeparateExpr::usage =
    "ruleSeparateExpr: transformation rules for separating logarithms and absolute values."<>
    "\n"<>
    "Example: Log[x*y] -> Log[x]+Log[y], Abs[x*y] -> Abs[x]*Abs[y].";

ruleCombineExpr::usage =
    "ruleCombineExpr: transformation rules for combining logarithms and absolute values."<>
    "\n"<>
    "Example: Log[x]+Log[y] -> Log[x*y], Abs[x]+Abs[y] -> Abs[x*y].";


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
    IgnoringInactive[(-1)^(k_Integer*a_.+b_.)]/;Simplify[EvenQ[k]]:>
        (-1)^b,
    IgnoringInactive[(-1)^(-a_)]/;Simplify[IntegerQ[a]]:>
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


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
