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
    "freeze[pattern, operation, level][expr]: freeze subexpressions matching the pattern, then perform the operation and unfreeze."<>
    "\n"<>
    "freeze[pattern->transform, operation, level][expr]: additionally perform the transform to the frozen subexpressions."<>
    "\n"<>
    "Value[pattern->transform]: _->Positive, _->Negative, _->{_,_}."<>
    "\n"<>
    "Default[operation]: Simplify."<>
    "\n"<>
    "Default[level]: Infinity.";


freezeNegative::usage =
    "freezeNegative[pattern, operation, level][expr]: variant of freeze with Negative as the default transformation.";


(* ::Subsection:: *)
(*focus*)


focus::usage =
    "focus[pattern, operation, level][expr]: apply the operation to the arguments of functions with the specified heads."<>
    "\n"<>
    "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used."<>
    "\n"<>
    "Default[operation]: Simplify.";


(* ::Subsection:: *)
(*Frac*)


fracFocus::usage =
    "fracFocus[operation, level][expr]: apply the operation to fractions (expressions containing negative powers)."<>
    "\n"<>
    "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used."<>
    "\n"<>
    "Default[operation]: Simplify.";

fracReduce::usage =
    "fracReduce[operation, factor][expr]: multiply the factor to the numerator and denominator, then apply the operation separately to them."<>
    "\n"<>
    "Default[operation]: Simplify."<>
    "\n"<>
    "Default[factor]: 1.";


(* ::Subsection:: *)
(*Power*)


powerFocus::usage =
    "powerFocus[operation, level][expr]: apply the operation to the base and exponent of power factors."<>
    "\n"<>
    "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used."<>
    "\n"<>
    "Default[operation]: Simplify.";

powerBaseFocus::usage =
    "powerBaseFocus[operation, level][expr]: apply the operation to the base of power factors only."<>
    "\n"<>
    "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used."<>
    "\n"<>
    "Default[operation]: Simplify.";

powerExponentFocus::usage =
    "powerExponentFocus[operation, level][expr]: apply the operation to the exponent of power factors only."<>
    "\n"<>
    "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used."<>
    "\n"<>
    "Default[operation]: Simplify.";

powerBaseTogether::usage =
    "powerBaseTogether[operation, level][expr]: take together the bases of power factors and then apply the operation to the combined base."<>
    "\n"<>
    "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used."<>
    "\n"<>
    "Default[operation]: Simplify.";

powerExpand::usage =
    "powerExpand[operation, level][expr]: expand the power factors after combining power bases."<>
    "\n"<>
    "Hint: if level is not specified, ReplaceAll is used to match the pattern, otherwise Replace is used."<>
    "\n"<>
    "Default[operation]: Simplify.";

powerExpandBy::usage =
    "powerExpandBy[rules..][expr]: expand the power factors according to the rules."<>
    "\n"<>
    "Info[rules]: rules of the form base->{factor1, factor2, ...}.";

powerSeparate::usage =
    "powerSeparate[baseP][expr]: separate the product expression into power factors and non-power factors."<>
    "\n"<>
    "Info[baseP]: the pattern of power bases to match.";

powerExponentCollect::usage =
    "powerExponentCollect[exponents...][expr]: collect and combine power factors with common exponents."<>
    "\n"<>
    "Hint: if no exponent is specified, try to collect all power factors.";


(* ::Subsection:: *)
(*Trig*)


trigPhaseReduce::usage =
    "trigPhaseReduce[vars..][expr]: reduce phase factors in trigonometric functions using periodicity."<>
    "\n"<>
    "Info[vars]: the variables to consider for periodicity.";


(* ::Subsection:: *)
(*DiracDelta*)


deltaReduce::usage =
    "deltaReduce[expr]: reduce the Dirac delta function and its derivatives in the expression.";


(* ::Subsection:: *)
(*Misc*)


swap::usage =
    "swap[a, b][expr]: swap the two symbols in the expression."<>
    "\n"<>
    "swap[{a, b}..][expr]: swap the pairs simultaneously.";


separate::usage =
    "separate[criterion][expr_]: separate the elements based on whether they satisfy the criterion.";


stripPattern::usage =
    "stripPattern[expr, head]: strip off pattern-related functions from the expression and wrap it with head."<>
    "\n"<>
    "Default[head]: Defer.";


vanishing::usage =
    "vanishing[expr]: clean up the expression by removing redundant vanishing terms."<>
    "\n"<>
    "Sketch: Simplify + Flatten + DeleteDuplicates.";


extractSymbol::usage =
    "extractSymbol[expr, exclusionList]: extract user-defined symbols from the expression."<>
    "\n"<>
    "Info[exclusionList]: the contexts to exclude.";

extractVariable::usage =
    "extractVariable[expr, exclusionList]: extract user-defined variables from the expression."<>
    "\n"<>
    "Info[exclusionList]: the contexts to exclude.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*freeze*)


(* ::Subsubsection:: *)
(*Main*)


freeze::BadInput =
    "The input `1` or `2` is invalid."<>
    "\n"<>
    "Hint: to match _Rule|_List, Verbatim should be adopted."<>
    "\n"<>
    "Hint: the default transformation should be a pair of functions.";


freeze//Options = {
    "DefaultTransformation"->{Identity,Identity}
};


freeze[
    patternOrItsList_,
    operation:_Symbol|_Symbol[___]|_Function|_Composition|_RightComposition:Simplify,
    level:_?levelQ:Infinity,
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
    level:_?levelQ:Infinity
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
        Message[freeze::BadInput,pattern,default];
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

focus[pattern_,operation_,level_?levelQ][expr_] :=
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

fracFocus[operation_,level_?levelQ][expr_] :=
    expr//Replace[#,{
        subexpr:Verbatim[Times][___,Power[_,_?Internal`SyntacticNegativeQ],___]:>operation@subexpr
    },level]&;


(* ::Subsection:: *)
(*Power*)


powerFocus[operation_:Simplify][expr_] :=
    expr//ReplaceAll[{
        Power[base_,exponent_]:>Power[operation@base,operation@exponent]
    }];

powerFocus[operation_,level_?levelQ][expr_] :=
    expr//Replace[#,{
        Power[base_,exponent_]:>Power[operation@base,operation@exponent]
    },level]&;


powerBaseFocus[operation_:Simplify][expr_] :=
    expr//ReplaceAll[{
        Power[base_,exponent_]:>Power[operation@base,exponent]
    }];

powerBaseFocus[operation_,level_?levelQ][expr_] :=
    expr//Replace[#,{
        Power[base_,exponent_]:>Power[operation@base,exponent]
    },level]&;


powerExponentFocus[operation_:Simplify][expr_] :=
    expr//ReplaceAll[{
        Power[base_,exponent_]:>Power[base,operation@exponent]
    }];

powerExponentFocus[operation_,level_?levelQ][expr_] :=
    expr//Replace[#,{
        Power[base_,exponent_]:>Power[base,operation@exponent]
    },level]&;


(* ::Subsubsection:: *)
(*powerBaseTogether*)


powerBaseTogether[operation_:Simplify][expr_] :=
    expr//ReplaceAll[{
        Power[base_,exponent_]:>Power[togetherAnd[operation][base],exponent]
    }];

powerBaseTogether[operation_,level_?levelQ][expr_] :=
    expr//Replace[#,{
        Power[base_,exponent_]:>Power[togetherAnd[operation][base],exponent]
    },level]&;


togetherAnd[operation_][expr_] :=
    Together[expr]//operation[Numerator[#]]/operation[Denominator[#]]&;


(* ::Subsubsection:: *)
(*powerExpand*)


powerExpand//Options = {
    "Assumptions"->Automatic
};

powerExpand[operation_:Simplify,opts:OptionsPattern[]][expr_] :=
    expr//
        powerBaseTogether[operation]//
        PowerExpand[#,Assumptions->OptionValue["Assumptions"]]&//
        powerExponentFocus[Simplify];

powerExpand[operation_,level_?levelQ,opts:OptionsPattern[]][expr_] :=
    expr//
        powerBaseTogether[operation,level]//
        PowerExpand[#,Assumptions->OptionValue["Assumptions"]]&//
        powerExponentFocus[Simplify];


(* ::Subsubsection:: *)
(*powerExpandBy*)


powerExpandBy[rule:_Rule|_RuleDelayed][expr_] :=
    expr//ReplaceAll[expandRuleForSpecifiedBase[rule]];

powerExpandBy[rules:(_Rule|_RuleDelayed)..][expr_] :=
    expr//ReplaceAll[Map[expandRuleForSpecifiedBase,{rules}]];


expandRuleForSpecifiedBase[_[base_,(List|Alternatives)[factors__]]] :=
    Power[base,exponent_]:>Times@@Map[Power[#,exponent]&,{factors}];


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


powerSeparate[base1_][expr:Power[base_,_]] :=
    If[ MatchQ[base,basePattern[base1]],
        {expr,1},
        (*Else*)
        {1,expr}
    ];

powerSeparate[base1_][expr_Times] :=
    With[ { baseP = basePattern[base1]},
        {
            Discard[expr,FreeQ[Power[baseP,_]]],
            Select[expr,FreeQ[Power[baseP,_]]]
        }
    ];

powerSeparate[base1_][expr_] :=
    {1,expr};


basePattern[All] :=
    _;

basePattern[base_List] :=
    Alternatives@@base;

basePattern[base_] :=
    base;


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

swap[pairs:{_,_}..][expr_] :=
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
