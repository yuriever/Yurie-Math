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


fracSimplify::usage =
    "simplify the numerator and denominator.";

trigPhaseReduce::usage =
    "reduce the phase factor in trigonometric functions.";

collectDerivative::usage =
    "collect by derivatives.";

stripPattern::usage =
    "strip off pattern-related functions in expressions.";

vanishing::usage =
    "Simplify + Flatten + DeleteDuplicates.";

swap::usage =
    "swap two symbols in an expression.";

separate::usage =
    "separate the elements by whether or not satisfying the criteria.";

freeze::usage =
    "screen subexpressions matching the pattern and then perform the operation.";

focus::usage =
    "simplify the arguments of the specified heads.";


(* ::Subsection:: *)
(*Deprecation*)


powerBaseSimplify::usage =
    "simplify the power bases.";

trigPhaseSimplify::usage =
    "reduce the phase factor in trigonometric functions.";

separateBy::usage =
    "separate the elements by whether or not satisfying the criteria.";


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


AS[assumption_] :=
    Function[expr,Assuming[assumption,expr],{HoldAll}];


SSA[assumption_][expr_] :=
    Simplify[expr,assumption];


FSA[assumption_][expr_] :=
    FullSimplify[expr,assumption];


FEA[assumption_][expr_] :=
    FunctionExpand[expr,assumption];


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


series[args___][expr_] :=
    Normal@Series[expr,args];


limit[direction_][expr_] :=
    Limit[expr,direction];


solve[args___][expr_] :=
    Solve[expr,args];


solveFirst[args___][expr_] :=
    First@Solve[expr,args];


part :=
    GeneralUtilities`Slice;


collect[var_,head_:Identity][expr_] :=
    Collect[expr,var,head];


(* ::Subsection::Closed:: *)
(*Simplification*)


(* ::Subsubsection:: *)
(*Constant*)


$exprSimLoop::usage =
     "control the Nest in exprSim and powerSim.";

$exprSimLoop = 4;


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


(* ::Subsubsection:: *)
(*Helper*)


powerPreprocess[expr_] :=
    expr//ReplaceAll[{subexpr_Plus:>Together@subexpr}];


powerCollectKernel[][expr_] :=
    expr//ReplaceRepeated[ruleCollectPower[]];

powerCollectKernel[power_][expr_] :=
    expr//ReplaceRepeated[ruleCollectPower[power]];

powerCollectKernel[power_,rest__][expr_] :=
    powerCollectKernel[rest][
        powerCollectKernel[power][expr]
    ];


(* ::Subsection:: *)
(*Misc*)


(* ::Subsubsection:: *)
(*fracSimplify*)


fracSimplify[simplify_:Simplify,factor_:1][expr_] :=
    simplify[factor*Numerator[expr]]/simplify[factor*Denominator[expr]];


(* ::Subsubsection:: *)
(*trigPhaseReduce*)


trigPhaseReduce[vars__][expr_] :=
    expr//ReplaceAll[(trig:Sin|Cos|Tan|Cot|Csc|Sec)[arg_]:>trig@Expand@arg]//
        trigPhaseReduceKernel[vars]//
            ReplaceAll[(trig:Sin|Cos|Tan|Cot|Csc|Sec)[arg_]:>trig@Simplify@arg];


trigPhaseReduceKernel[var_][expr_] :=
    expr//ReplaceAll[ruleTrigPhase[var]]//ReplaceAll[(-1)^(k_.*var)/;IntegerQ[k]:>(-1)^(Mod[k,2]*var)];

trigPhaseReduceKernel[var_,rest__][expr_] :=
    trigPhaseReduceKernel[rest][
        trigPhaseReduceKernel[var][expr]
    ];


(* ::Subsubsection:: *)
(*collectDerivative*)


collectDerivative[var_Symbol,operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][var][___],operation];

collectDerivative[varList_List,operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][#][___]&/@varList,operation];


(* ::Subsubsection:: *)
(*stripPattern*)


stripPattern//Attributes =
    {HoldAll};

stripPattern[expr_,head_:Defer] :=
    head[expr]//ReplaceRepeated[(Verbatim[Pattern]|Verbatim[Optional]|Verbatim[PatternTest]|Verbatim[Condition])[pattern_,_]:>pattern];


(* ::Subsubsection:: *)
(*vanishing*)


vanishing[expr_] :=
    expr//Simplify//Flatten//DeleteDuplicates;


(* ::Subsubsection:: *)
(*swap*)


swap[a_Symbol,b_Symbol][expr_] :=
    expr//ReplaceAll[{a->b,b->a}];

swap[pairs:{_Symbol,_Symbol}...][expr_] :=
    expr//ReplaceAll[Flatten@Map[{#[[1]]->#[[2]],#[[2]]->#[[1]]}&,{pairs}]];


(* ::Subsubsection:: *)
(*separate*)


separate[crit_][expr_] :=
    {
        Select[expr,crit[#]&],
        Select[expr,!crit[#]&]
    };


(* ::Subsubsection:: *)
(*freeze*)


freeze//Options = {
    "Transformation"->{Identity,Identity}
};

freeze//Attributes =
    {HoldAll};

freeze[args___][expr_] :=
    With[ {sep = ArgumentsOptions[freeze[args],{1,3},<|"Head"->Hold,"OptionsMode"->"Shortest"|>]},
        freezeKernel[sep][expr]/;!FailureQ[sep]
    ];


freezeKernel[{Hold[pattern_],Hold[opts___]}][expr_] :=
    freezeKernel[{Hold[pattern,Simplify,Infinity],Hold[opts]}][expr];

freezeKernel[{Hold[pattern_,operation_],Hold[opts___]}][expr_] :=
    freezeKernel[{Hold[pattern,operation,Infinity],Hold[opts]}][expr];

freezeKernel[{Hold[pattern_,operation_,level_],Hold[opts___]}][expr_] :=
    Module[ {frozenExpr,subExprList,tempList,ruleList,inverseRuleList,trans,inverseTrans},
        {trans,inverseTrans} = OptionValue[freeze,{opts},"Transformation"];
        subExprList =
            DeleteDuplicates@Cases[expr,pattern,level];
        tempList =
            Table[Unique["sub$",{Temporary}],Length@subExprList];
        ruleList =
            MapThread[Rule[#1,trans[#2]]&,{subExprList,tempList}];
        inverseRuleList =
            MapThread[Rule[#1,inverseTrans[#2]]&,{tempList,subExprList}];
        frozenExpr =
            Replace[expr,ruleList,level];
        If[ freeze`Debug,
            Return@{frozenExpr,ruleList,inverseRuleList}
        ];
        frozenExpr//operation//ReplaceAll[inverseRuleList]
    ];


(* ::Subsubsection:: *)
(*focus*)


focus//Attributes =
    {HoldAll};


focus[pattern_,operation_:Simplify][expr_] :=
    expr//Replace[#,{
        (head:pattern):>operation@head,
        (head:pattern)[arg_]:>head@operation@arg,
        (head:pattern)[args__]:>head@@Map[operation,{args}]
    },All]&;


(* ::Subsection:: *)
(*Deprecation*)


powerBaseSimplify::deprecation =
    "this function has been superseded by focus.";

powerBaseSimplify[simplify_:Simplify][expr_] :=
    (
        Message[powerBaseSimplify::deprecation];
        expr//Replace[#,Power[x_,n_]:>Power[simplify@Together[x],n],All]&
    );


trigPhaseSimplify::deprecation =
    "this function has been superseded by trigPhaseReduce.";

trigPhaseSimplify[vars__][expr_] :=
    (
        Message[trigPhaseSimplify::deprecation];
        trigPhaseReduce[vars][expr]
    );


separateBy::deprecation =
    "this function has been superseded by separate.";

separateBy[crit_][expr_] :=
    (
        Message[separateBy::deprecation];
        separate[crit][expr]
    );


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
