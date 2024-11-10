(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Simplify`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Constant`"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Operator form*)


SS::usage =
    "Simplify.";

FS::usage =
    "FullSimplify.";

FE::usage =
    "FunctionExpand.";

AS::usage =
    "operator form of Assuming.";

SSA::usage =
    "Simplify + Assuming.";

FSA::usage =
    "FullSimplify + Assuming.";

FEA::usage =
    "FunctionExpand + Assuming.";


modularize::usage =
    "modularize scoping constructions.";

block::usage =
    "operator form of Block.";

with::usage =
    "operator form of With.";

module::usage =
    "operator form of Module.";


times::usage =
    "operator form of Times.";

plus::usage =
    "operator form of Plus.";

series::usage =
    "operator form of Series + Normal.";

limit::usage =
    "opeartor form of Limit.";

solve::usage =
    "operator form of Solve.";

solveFirst::usage =
    "operator form of Solve + First.";

part::usage =
    "operator form of Part, GeneralUtilities`Slice.";

collect::usage =
    "operator form of Collect.";

separateBy::usage =
    "separate the elements by whether or not satisfying the criteria.";



(* ::Subsection:: *)
(*Simplification*)


exprTogether::usage =
    "take powers, logs and abs together.";

exprApart::usage =
    "take powers, logs and abs apart.";

exprSim::usage =
    "simplify powers, logs and abs.";


powerTogether::usage =
    "take powers together.";

powerApart::usage =
    "take powers apart, similar to PowerExpand.";

powerCollect::usage =
    "collect powers by exponents.";

powerSim::usage =
    "simplify powers.";

deltaSim::usage =
    "simplify Delta functions.";


(* ::Subsection:: *)
(*Misc*)


collectDerivative::usage =
    "collect by derivatives.";

fracSimplify::usage =
    "simplify the Numerator and Denominator by multiplying a factor.";

vanishing::usage =
    "Simplify + Flatten + DeleteDuplicates.";

swap::usage =
    "swap two symbols in an expression.";

stripPattern::usage =
    "strip off pattern-related functions in expressions.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


SSA//Options =
    Options@Simplify;

FSA//Options =
    Options@FullSimplify;

FEA//Options =
    Options@FunctionExpand;

series//Options =
    Options@Series;

limit//Options =
    Options@Limit;

solve//Options =
    Options@Solve;

solveFirst//Options =
    Options@Solve;

collect//Options =
    Options@Collect;


(* ::Subsection:: *)
(*Operator form*)


SS :=
    Simplify;


FS :=
    FullSimplify;


FE :=
    FunctionExpand;


AS[as_] :=
    Function[expr,Assuming[as,expr],{HoldAll}];


SSA[assumption_,opts:OptionsPattern[]][expr_] :=
    Simplify[expr,assumption,FilterRules[{opts,Options@SSA},Options@Simplify]];


FSA[assumption_,opts:OptionsPattern[]][expr_] :=
    FullSimplify[expr,assumption,FilterRules[{opts,Options@FSA},Options@FullSimplify]];


FEA[assumption_,opts:OptionsPattern[]][expr_] :=
    FunctionExpand[expr,assumption,FilterRules[{opts,Options@FEA},Options@FunctionExpand]];


modularize//Attributes =
    {HoldAllComplete};

modularize[head_[code_,iterators__]] :=
    Replace[
        DeleteCases[
            HoldComplete[iterators][[All,1]],
            Except[_Symbol]
        ],
        HoldComplete[args___]:>
            Module[ {args},
                head[code,iterators]
            ]
    ];


block//Attributes =
    {HoldAllComplete};

block[localVarList_List] :=
    Function[
        expr,
        Block[ localVarList,
            expr
        ],
        {HoldAllComplete}
    ];


with//Attributes =
    {HoldAllComplete};

with[localVarLists___List] :=
    Function[
        expr,
        Replace[
            HoldComplete[localVarLists],
            HoldComplete[args___]:>
                With[ args,
                    expr
                ]
        ],
        {HoldAllComplete}
    ];


module//Attributes =
    {HoldAllComplete};

module[localVarList_List] :=
    Function[
        expr,
        Replace[
            HoldComplete[localVarList],
            HoldComplete[arg_]:>
                Module[ arg,
                    expr
                ]
        ],
        {HoldAllComplete}
    ];


times[args___][expr_] :=
    Times[expr,args];


plus[args___][expr_] :=
    Plus[expr,args];


series[args___,opts:OptionsPattern[]][expr_] :=
    Normal@Series[expr,args,FilterRules[{opts,Options@series},Options@Series]];


limit[direction_,opts:OptionsPattern[]][expr_] :=
    Limit[expr,direction,FilterRules[{opts,Options@limit},Options@Limit]];


solve[args___,opts:OptionsPattern[]][expr_] :=
    Solve[expr,args,FilterRules[{opts,Options@solve},Options@Solve]];


solveFirst[args___,opts:OptionsPattern[]][expr_] :=
    First@Solve[expr,args,FilterRules[{opts,Options@solveFirst},Options@Solve]];


part :=
    GeneralUtilities`Slice;


collect[var_,head_:Identity,opts:OptionsPattern[]][expr_] :=
    Collect[expr,var,head,FilterRules[{opts,Options@collect},Options@Collect]];


separateBy[crit_][expr_] :=
    {
        Select[expr,crit[#]&],
        Select[expr,!crit[#]&]
    };


(* ::Subsection:: *)
(*Simplification*)


(* ::Subsubsection:: *)
(*Constant*)


$exprSimLoop::usage =
     "control the Nest in exprSim and powerSim.";

$exprSimLoop = 4;


(* ::Subsubsection:: *)
(*Helper*)


powerPreprocess[expr_] :=
    expr//ReplaceAll[{subexpr_Plus:>Together@subexpr}];


powerCollectKernel[][expr_] :=
    expr//ReplaceRepeated[ruleCollectPower[]];

powerCollectKernel[power_][expr_] :=
    expr//ReplaceRepeated[ruleCollectPower[power]];

powerCollectKernel[power1_,powerRest__][expr_] :=
    powerCollectKernel[powerRest][
        powerCollectKernel[power1][expr]
    ];


(* ::Subsubsection:: *)
(*Main*)


powerApart[expr_] :=
    powerPreprocess[expr]//ReplaceRepeated[ruleSeparatePower]//Activate;


powerTogether[expr_] :=
    powerPreprocess[expr]//ReplaceRepeated[ruleSeparatePower]//ReplaceRepeated[ruleCombinePower]//Activate;


powerSim[expr_] :=
    Nest[powerTogether/*Simplify,expr,$exprSimLoop];


powerCollect[powers___][expr_] :=
    powerCollectKernel[powers][expr]//Activate;


exprApart[expr_] :=
    expr//ReplaceAll[subexpr_Times:>powerApart@subexpr]//ReplaceRepeated[ruleSeparateExpr]//Activate;


exprTogether[expr_] :=
    expr//ReplaceAll[subexpr_Times:>powerTogether@subexpr]//ReplaceRepeated[ruleCombineExpr]//Activate;


exprSim[expr_] :=
    Nest[exprApart/*exprTogether/*Simplify,expr,$exprSimLoop];


deltaSim[expr_] :=
    expr//ReplaceRepeated[ruleCancelDiracDelta]//Simplify;


(* ::Subsection:: *)
(*Misc*)


collectDerivative[var_Symbol,post_:Identity][expr_] :=
    Collect[expr,Derivative[___][var][___],post];

collectDerivative[varList_List,post_:Identity][expr_] :=
    Collect[expr,Derivative[___][#][___]&/@varList,post];


fracSimplify[simplify_:Simplify,factor_:1][expr_] :=
    simplify[factor*Numerator[expr]]/simplify[factor*Denominator[expr]];


vanishing[expr_] :=
    expr//Simplify//Flatten//DeleteDuplicates;


swap[a_Symbol,b_Symbol][expr_] :=
    expr//ReplaceAll[{a->b,b->a}];

swap[pairs:{_Symbol,_Symbol}...][expr_] :=
    expr//ReplaceAll[Flatten@Map[{#[[1]]->#[[2]],#[[2]]->#[[1]]}&,{pairs}]];


stripPattern//Attributes =
    {HoldAll};

stripPattern[expr_,head_:Defer] :=
    head[expr]//ReplaceRepeated[(Verbatim[Pattern]|Verbatim[Optional]|Verbatim[PatternTest]|Verbatim[Condition])[pattern_,_]:>pattern];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
