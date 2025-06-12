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
    "simplify the base and exponent of powers.";

powerBaseFocus::usage =
    "simplify the base of powers.";

powerExponentFocus::usage =
    "simplify the exponent of powers.";

powerSeparate::usage =
    "split a product into powers with specified base(s) and the rests.";

powerBaseTogether::usage =
    "make together the specified base(s) of powers.";

powerExpand::usage =
    "expand the powers with the specified base(s).";

powerExponentCollect::usage =
    "collect powers by the specified exponent(s).";

powerPhaseReduce::usage =
    "reduce the phase factor in powers according to the assumptions and/or the specified holomorphic/antiholomorphic variables.";


(* ::Subsection:: *)
(*Trig*)


trigPhaseReduce::usage =
    "reduce phase factors in trigonometric functions by the given assumptions.";


(* ::Subsection:: *)
(*DiracDelta*)


deltaReduce::usage =
    "reduce the Dirac delta function."


(* ::Subsection:: *)
(*Derivative*)


collectDerivative::usage =
    "collect by derivatives.";


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


powerBaseTogether[][expr_] :=
    expr//ReplaceAll[{
        Power[base_,exponent_]:>Power[togetherAndSimplify[base],exponent]
    }];

powerBaseTogether[var_][expr_] :=
    With[ {baseP = basePattern[var]},
        expr//ReplaceAll[{
            Power[base:baseP,exponent_]:>Power[togetherAndSimplify[base],exponent]
        }]
    ];

powerBaseTogether[var_,level_][expr_] :=
    With[ {baseP = basePattern[var]},
        expr//Replace[#,{
            Power[base:baseP,exponent_]:>Power[togetherAndSimplify[base],exponent]
        },level]&
    ];


togetherAndSimplify[expr_] :=
    Together[expr]//Simplify[Numerator[#]]/Simplify[Denominator[#]]&;


(* ::Subsubsection:: *)
(*powerExpand*)


powerExpand[][expr_] :=
    expr//powerBaseTogether[]//PowerExpand//powerExponentFocus[Simplify];

powerExpand[var_][expr_] :=
    expr//powerBaseTogether[var]//PowerExpand//powerExponentFocus[Simplify];

powerExpand[var_,level_][expr_] :=
    expr//powerBaseTogether[var,level]//PowerExpand//powerExponentFocus[Simplify];


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


(* ::Subsubsection:: *)
(*powerPhaseReduce*)


powerPhaseReduce//Options = {
    "ShowIndeterminate"->False
};


powerPhaseReduce[assume_,opts:OptionsPattern[]][expr_] :=
    expr//reducePhaseBy[assume]//powerExponentFocus[Simplify];

powerPhaseReduce[assume_,antiholo_,opts:OptionsPattern[]][expr_] :=
    expr//reducePhaseBy[assume,listToPattern@antiholo]//powerExponentFocus[Simplify];

powerPhaseReduce[assume_,holo_,antiholo_,opts:OptionsPattern[]][expr_] :=
    Module[ {res,indet,show = OptionValue["ShowIndeterminate"]},
        If[ TrueQ@show,
            {res,indet} =
                expr//reducePhaseBy[assume,listToPattern@holo,listToPattern@antiholo,show];
            If[ indet=!={},
                indet[[1]]//Print
            ],
            (*Else*)
            res =
                expr//reducePhaseBy[assume,listToPattern@holo,listToPattern@antiholo,show]
        ];
        res//powerExponentFocus[Simplify]
    ];


reducePhaseBy[assume_][expr_] :=
    expr//ReplaceAll[
        Power[base_,exponent_]/;Simplify[base<0,assume]:>
            Exp[I*π*exponent]*Power[-base,exponent]
    ];

reducePhaseBy[assume_,antiholoP_][expr_] :=
    expr//ReplaceAll[
        Power[base_,exponent_]/;Simplify[base<0,assume]:>
            If[ FreeQ[base,antiholoP],
                Exp[I*π*exponent]*Power[-base,exponent],
                (*Else*)
                Exp[-I*π*exponent]*Power[-base,exponent]
            ]
    ];

reducePhaseBy[assume_,holoP_,antiholoP_,False][expr_] :=
    expr//ReplaceAll[
        Power[base_,exponent_]/;Simplify[base<0,assume]:>
            Which[
                !FreeQ[base,holoP]&&FreeQ[base,antiholoP],
                    Exp[I*π*exponent]*Power[-base,exponent],
                !FreeQ[base,antiholoP]&&FreeQ[base,holoP],
                    Exp[-I*π*exponent]*Power[-base,exponent],
                True,
                    Power[base,exponent]
            ]
    ];

reducePhaseBy[assume_,holoP_,antiholoP_,True][expr_] :=
    Reap[
        expr//ReplaceAll[
            Power[base_,exponent_]/;Simplify[base<0,assume]:>
                Which[
                    !FreeQ[base,holoP]&&FreeQ[base,antiholoP],
                        Power[-base,exponent]*Exp[I*π*exponent],
                    !FreeQ[base,antiholoP]&&FreeQ[base,holoP],
                        Power[-base,exponent]*Exp[-I*π*exponent],
                    True,
                        Sow[Power[base,exponent],"indet"]
                ]
        ],
        "indet"
    ];


listToPattern[list_List] :=
    Alternatives@@list;

listToPattern[other_] :=
    other;


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
(*Derivative*)


(* ::Subsubsection:: *)
(*collectDerivative*)


collectDerivative[var:Except[_List],operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][var][___],operation];

collectDerivative[varList_List,operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][#][___]&/@varList,operation];


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


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
