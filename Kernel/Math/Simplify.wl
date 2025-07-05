(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Simplify`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*freeze*)


freeze::usage =
    "freeze subexpressions matching the pattern and then perform the operation.";

freezeNegative::usage =
    "variant of freeze."<>
    "\nNegative is used as the default transformation."


(* ::Subsection:: *)
(*focus*)


focus::usage =
    "simplify the argument(s) of the specified head(s).";


(* ::Subsection:: *)
(*Frac*)


fracFocus::usage =
    "simplify the numerator and denominator of fractions.";

fracReduce::usage =
    "reduce the fraction by multiplying a common factor onto the numerator and denominator.";


(* ::Subsection:: *)
(*Power*)


powerFocus::usage =
    "apply the operation to the base and exponent of power factors."<>
    "\nThe default operation is Simplify.";

powerBaseFocus::usage =
    "apply the operation to the base of power factors."<>
    "\nThe default operation is Simplify.";

powerExponentFocus::usage =
    "apply the operation to the exponent of power factors."<>
    "\nThe default operation is Simplify.";

powerSeparate::usage =
    "separate the product expression into power factors and the rests."<>
    "\nThe first argument specifies the pattern of power base."

powerBaseTogether::usage =
    "take together the bases of power factors."<>
    "\nThe first argument specifies the pattern of power base."<>
    "\nThe second argument specifies the pattern of preserved power base.";

powerExpand::usage =
    "expand power factors after taking together of power bases, and then simplify power exponents.";

powerExponentCollect::usage =
    "collect and combine power factors with common exponent factors.";


(* ::Subsection:: *)
(*Trig*)


trigPhaseReduce::usage =
    "reduce phase factors in trigonometric functions by the given assumptions.";


(* ::Subsection:: *)
(*DiracDelta*)


deltaReduce::usage =
    "reduce the Dirac delta function."


(* ::Subsection:: *)
(*Misc*)


swap::usage =
    "swap two symbols in an expression.";


separate::usage =
    "separate the elements by whether or not satisfying the criteria.";


stripPattern::usage =
    "strip off pattern-related functions in expressions.";


vanishing::usage =
    "Simplify + Flatten + DeleteDuplicates.";


extractSymbol::usage =
    "extract symbols from the expression.";

extractVariable::usage =
    "extract variables from the expression.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*freeze*)


(* ::Subsubsection:: *)
(*Main*)


freeze::badInput =
    "The input `1` or `2` is invalid."<>
    "\nHint: to match _Rule|_List, Verbatim should be adopted."<>
    "\nHint: the default transformation should be a pair of functions.";


freeze//Options = {
    "DefaultTransformation"->{Identity,Identity}
};


freeze[
    patternOrItsList_,
    operation:_Symbol|_Symbol[___]|_Function|_Composition|_RightComposition:Simplify,
    level:_Integer|_List|Infinity|All:Infinity,
    opts:OptionsPattern[]
][expr_] :=
    Catch[
        freezeKernel[patternOrItsList,operation,OptionValue["DefaultTransformation"],level,expr],
        "badInput",
        HoldComplete[expr]&
    ];


freezeNegative[
    patternOrItsList_,
    operation:_Symbol|_Symbol[___]|_Function|_Composition|_RightComposition:Simplify,
    level:_Integer|_List|Infinity|All:Infinity
][expr_] :=
    Catch[
        freezeKernel[patternOrItsList,operation,{-#&,-#&},level,expr],
        "badInput",
        HoldComplete[expr]&
    ];


freezeKernel[pattern_,operation_,default_,level_,expr_] :=
    Module[ {ruleList,ruleInvList},
        {ruleList,ruleInvList} =
            prepareFrozenRuleList[pattern,default,level,expr];
        Replace[expr,ruleList,level]//operation//ReplaceAll[ruleInvList]
    ];

freezeKernel[{patterns__},operation_,default_,level_,expr_] :=
    Module[ {ruleList,ruleInvList},
        {ruleList,ruleInvList} =
            Map[prepareFrozenRuleList[#,default,level,expr]&,{patterns}]//Transpose//MapApply[Join];
        Replace[expr,ruleList,level]//operation//ReplaceAll[ruleInvList]
    ];


(* ::Subsubsection:: *)
(*Helper*)


prepareFrozenRuleList[pattern1_,default_,level_,expr_] :=
    Module[ {pattern,fun,funInv,subExprList,tempList},
        {pattern,fun,funInv} =
            patternAndTransformation[pattern1,default];
        subExprList =
            DeleteDuplicates@Cases[expr,pattern,level];
        tempList =
            Table[Unique["sub$",{Temporary}],Length@subExprList];
        {
            MapThread[Rule[#1,fun[#2]]&,{subExprList,tempList}],
            MapThread[Rule[#1,funInv[#2]]&,{tempList,subExprList}]
        }
    ];


patternAndTransformation[pattern:Except[_Rule|_RuleDelayed],default:{_,_}] :=
    {pattern,Sequence@@default};

patternAndTransformation[(Rule|RuleDelayed)[pattern_,Positive],{_,_}] :=
    {pattern,Identity,Identity};

patternAndTransformation[(Rule|RuleDelayed)[pattern_,Negative],{_,_}] :=
    {pattern,-#&,-#&};

patternAndTransformation[(Rule|RuleDelayed)[pattern_,{fun_,funInv_}],{_,_}] :=
    {pattern,fun,funInv};

patternAndTransformation[pattern_,default_] :=
    (
        Message[freeze::badInput,pattern,default];
        Throw[Null,"badInput"]
    );


(* ::Subsection:: *)
(*focus*)


focus[pattern_,operation_:Simplify][expr_] :=
    expr//ReplaceAll[{
        (head:pattern):>operation@head,
        (head:pattern)[arg_]:>head@operation@arg,
        (head:pattern)[args__]:>head@@Map[operation,{args}]
    }];

focus[pattern_,operation_,level_][expr_] :=
    expr//Replace[#,{
        (head:pattern):>operation@head,
        (head:pattern)[arg_]:>head@operation@arg,
        (head:pattern)[args__]:>head@@Map[operation,{args}]
    },level]&;


(* ::Subsection:: *)
(*Frac*)


fracReduce[operation_:Simplify][expr_] :=
    operation[Numerator[expr]]/operation[Denominator[expr]];

fracReduce[operation_,factor_][expr_] :=
    operation[factor*Numerator[expr]]/operation[factor*Denominator[expr]];


fracFocus[operation_:Simplify][expr_] :=
    expr//ReplaceAll[{
        subexpr:Verbatim[Times][___,Power[_,_?Internal`SyntacticNegativeQ],___]:>operation@subexpr
    }];

fracFocus[operation_,level_][expr_] :=
    expr//Replace[#,{
        subexpr:Verbatim[Times][___,Power[_,_?Internal`SyntacticNegativeQ],___]:>operation@subexpr
    },level]&;


(* ::Subsection:: *)
(*Power*)


powerFocus[operation_:Simplify][expr_] :=
    expr//ReplaceAll[{
        Power[base_,exponent_]:>Power[operation@base,operation@exponent]
    }];

powerFocus[operation_,level_][expr_] :=
    expr//Replace[#,{
        Power[base_,exponent_]:>Power[operation@base,operation@exponent]
    },level]&;


powerBaseFocus[operation_:Simplify][expr_] :=
    expr//ReplaceAll[{
        Power[base_,exponent_]:>Power[operation@base,exponent]
    }];

powerBaseFocus[operation_,level_][expr_] :=
    expr//Replace[#,{
        Power[base_,exponent_]:>Power[operation@base,exponent]
    },level]&;


powerExponentFocus[operation_:Simplify][expr_] :=
    expr//ReplaceAll[{
        Power[base_,exponent_]:>Power[base,operation@exponent]
    }];

powerExponentFocus[operation_,level_][expr_] :=
    expr//Replace[#,{
        Power[base_,exponent_]:>Power[base,operation@exponent]
    },level]&;


(* ::Subsubsection:: *)
(*powerSeparate*)


powerSeparate[][expr_Power] :=
    {expr,1};

powerSeparate[][expr_Times] :=
    {
        Discard[expr,FreeQ[_Power]],
        Select[expr,FreeQ[_Power]]
    };

powerSeparate[][expr_] :=
    {1,expr};


powerSeparate[var_][expr:Power[base_,_]] :=
    If[ MatchQ[base,basePattern[var]],
        {expr,1},
        (*Else*)
        {1,expr}
    ];

powerSeparate[var_][expr_Times] :=
    With[ { baseP = basePattern[var]},
        {
            Discard[expr,FreeQ[Power[baseP,_]]],
            Select[expr,FreeQ[Power[baseP,_]]]
        }
    ];

powerSeparate[var_][expr_] :=
    {1,expr};


basePattern[All] :=
    _;

basePattern[var_List] :=
    Alternatives@@var;

basePattern[var_] :=
    var;


(* ::Subsubsection:: *)
(*powerBaseTogether*)


(* ::Text:: *)
(*BeginDeveloper*)


Needs["Yurie`Base`"];

ClearAll["powerBaseTogether"];


(* ::Text:: *)
(*EndDeveloper*)


powerBaseTogether[][expr_] :=
    powerBaseTogether[All][expr];

powerBaseTogether[base1_][expr_] :=
    With[ {baseP = basePattern[base1]},
        expr//ReplaceAll[{
            Power[base:baseP,exponent_]:>Power[togetherAndSimplify[base],exponent]
        }]
    ];

powerBaseTogether[base1_,None][expr_] :=
    powerBaseTogether[base1][expr];

powerBaseTogether[base1_,base2_][expr_] :=
    With[ {
            baseP = basePattern[base1],
            baseFrozenP = basePattern[base2]
        },
        expr//ReplaceAll[{
            subexpr:Power[baseFrozenP,_]:>subexpr,
            Power[base:baseP,exponent_]:>Power[togetherAndSimplify[base],exponent]
        }]
    ];

powerBaseTogether[base1_,base2_,base3_][expr_] :=
    expr//togetherSpecifiedBase[base3]//powerBaseTogether[base1,base2];


togetherAndSimplify[expr_] :=
    Together[expr]//Simplify[Numerator[#]]/Simplify[Denominator[#]]&;


togetherSpecifiedBase[rule_Rule|(List|Alternatives)[rules__Rule]][expr_] :=
    expr//ReplaceAll[Map[togetherRuleForSpecifiedBase,{rule,rules}]];


togetherRuleForSpecifiedBase[Rule[base_,(List|Alternatives)[factors__]]] :=
    Power[base,exponent_]:>Times@@Map[Power[#,exponent]&,{factors}];


(* ::Subsubsection:: *)
(*powerExpand*)


powerExpand[args___][expr_] :=
    expr//powerBaseTogether[args]//PowerExpand//powerExponentFocus[Simplify];


(* ::Subsubsection:: *)
(*powerExponentCollect*)


powerExponentCollect[powers___][expr_] :=
    powerExponentCollectKernel[powers][expr]//Activate;


powerExponentCollectKernel[][expr_] :=
    expr//ReplaceRepeated[ruleCollectPower[]];

powerExponentCollectKernel[power_][expr_] :=
    expr//ReplaceRepeated[ruleCollectPower[power]];

powerExponentCollectKernel[power_,rest__][expr_] :=
    expr//powerExponentCollectKernel[power]//powerExponentCollectKernel[rest];


ruleCollectPower[power_] :=
    ruleCollectPower[power] =
        {
            IgnoringInactive[(x_^a_)^b_]:>
                x^(a*b),
            IgnoringInactive[x_^(power*k1_.+rest1_.)*y_^(power*k2_.+rest2_.)]:>
                With[ {var = Simplify[x^k1*y^k2]},
                    If[ IntegerQ[x]||IntegerQ[y],
                        Inactivate[var^power,Power|Sqrt],
                        var^power
                    ]
                ]*x^rest1*y^rest2
        };

ruleCollectPower[] =
    {
        IgnoringInactive[(x_^a_)^b_]:>
            x^(a*b),
        IgnoringInactive[x_^(power_*k1_.+rest1_.)*y_^(power_*k2_.+rest2_.)]:>
            With[ {var = Simplify[x^k1*y^k2]},
                If[ IntegerQ[x]||IntegerQ[y],
                    Inactivate[var^power,Power|Sqrt],
                    var^power
                ]
            ]*x^rest1*y^rest2
    };


(* ::Subsection:: *)
(*Trig*)


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


ruleTrigPhase[var_] :=
    ruleTrigPhase[var] =
        {
            (h:Sin|Cos|Csc|Sec)[k_.*π*var+rest_.]/;IntegerQ[k]:>(-1)^(Mod[k,2] var)*h[rest],
            (h:Tan|Cot)[k_.*π*var+rest_.]/;IntegerQ[k]:>h[rest]
        };


(* ::Subsection:: *)
(*DiracDelta*)


deltaReduce[expr_] :=
    expr//ReplaceRepeated[{
        x_*DiracDelta[x_]:>
            0,
        x_*Derivative[m_][DiracDelta][x_]:>
            -m*Derivative[m-1][DiracDelta][x],
        Power[x_,n_]*DiracDelta[x_]/;Simplify[n>=1]:>
            0,
        Power[x_,n_]*Derivative[m_][DiracDelta][x_]/;Simplify[n>=1&&m>=1]:>
            -m*Power[x,n-1]*Derivative[m-1][DiracDelta][x]
    }];


(* ::Subsection:: *)
(*Misc*)


(* ::Subsubsection:: *)
(*swap*)


swap[a:Except[{_,_}],b:Except[{_,_}]][expr_] :=
    expr//ReplaceAll[{a->b,b->a}];

swap[pairs:{_,_}...][expr_] :=
    expr//ReplaceAll[Flatten@Map[{#[[1]]->#[[2]],#[[2]]->#[[1]]}&,{pairs}]];


(* ::Subsubsection:: *)
(*separate*)


separate[crit_][expr_] :=
    {
        Select[expr,crit[#]&],
        Select[expr,!crit[#]&]
    };


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
(*extractSymbol*)


extractSymbol[expr_,exclusionList_:{}] :=
    System`Utilities`SymbolList[expr,Identity,Join[{"System`"},exclusionList]];


(* ::Subsubsection:: *)
(*extractVariable*)


extractVariable[expr_,exclusionList_:{}] :=
    With[ {
            res=Reduce`FreeVariables[expr],
            excludedContext=Join[{"System`"},exclusionList]
        },
        Select[res,variableQ[excludedContext]]
    ];


variableQ[excludedContext_][var_Symbol] :=
    !MemberQ[excludedContext,Context[var]];

variableQ[_][var_?AtomQ] :=
    !MemberQ[{PD,INT,SUM,Association},Head[var]];

variableQ[_][var_] :=
    True;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
