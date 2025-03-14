(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Simplify`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Constant`"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Simplification*)


swap::usage =
    "swap two symbols in an expression.";


separate::usage =
    "separate the elements by whether or not satisfying the criteria.";


freeze::usage =
    "freeze subexpressions matching the pattern and then perform the operation.";

freezeNegative::usage =
    "variant of freeze. Negative is used as the default transformation.";


focus::usage =
    "simplify the arguments of the specified heads."<>
    "\nReplaceAll";

focusDeep::usage =
    "simplify the arguments of the specified heads recursively."<>
    "\nReplace[#1,#2,All]&";


focusPower::usage =
    "simplify the arguments of Power."<>
    "\nReplace[#1,#2,All]&";

focusPowerBase::usage =
    "simplify the base of Power."<>
    "\nReplace[#1,#2,All]&";

focusPowerExponent::usage =
    "simplify the exponent of Power."<>
    "\nReplace[#1,#2,All]&";

focusFrac::usage =
    "simplify the numerator and denominator of fractions."<>
    "\nReplaceAll";


powerPhaseReduce::usage =
    "reduce the phase factor in power function according to the assumptions and/or the specified holomorphic/antiholomorphic variables.";

trigPhaseReduce::usage =
    "reduce the phase factor in trigonometric functions.";


(* ::Subsection:: *)
(*Misc*)


collectDerivative::usage =
    "collect by derivatives.";

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
(*Simplification*)


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
(*freeze*)


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


(* ::Subsubsection:: *)
(*focus*)


focus[pattern_,operation_:Simplify][expr_] :=
    expr//ReplaceAll[{
        (head:pattern):>operation@head,
        (head:pattern)[arg_]:>head@operation@arg,
        (head:pattern)[args__]:>head@@Map[operation,{args}]
    }];


focusDeep[pattern_,operation_:Simplify][expr_] :=
    expr//Replace[#,{
        (head:pattern):>operation@head,
        (head:pattern)[arg_]:>head@operation@arg,
        (head:pattern)[args__]:>head@@Map[operation,{args}]
    },All]&;


focusPower[operation_:Simplify][expr_] :=
    expr//Replace[#,{
        Power[base_,exponent_]:>Power[operation@base,operation@exponent]
    },All]&;


focusPowerBase[operation_:Simplify][expr_] :=
    expr//Replace[#,{
        Power[base_,exponent_]:>Power[operation@base,exponent]
    },All]&;


focusPowerExponent[operation_:Simplify][expr_] :=
    expr//Replace[#,{
        Power[base_,exponent_]:>Power[base,operation@exponent]
    },All]&;


focusFrac[simplify_:Simplify,factor_:1][expr_] :=
    expr//ReplaceAll[{
        subexpr:Verbatim[Times][___,Power[_,_?Internal`SyntacticNegativeQ],___]:>simplify[factor*Numerator[subexpr]]/simplify[factor*Denominator[subexpr]]
    }];


(* ::Subsubsection:: *)
(*powerPhaseReduce*)


powerPhaseReduce//Options = {
    "ExtraCondition"->(True&),
    "ShowIndeterminate"->False
};


powerPhaseReduce[assume_,opts:OptionsPattern[]][expr_] :=
    expr//reducePhaseBy[assume,OptionValue["ExtraCondition"]]//
        focusPowerExponent[Simplify];

powerPhaseReduce[assume_,antiholo_,opts:OptionsPattern[]][expr_] :=
    expr//reducePhaseBy[assume,listToPattern@antiholo,OptionValue["ExtraCondition"]]//
        focusPowerExponent[Simplify];

powerPhaseReduce[assume_,holo_,antiholo_,opts:OptionsPattern[]][expr_] :=
    Module[ {res,indet,extra = OptionValue["ExtraCondition"],show = OptionValue["ShowIndeterminate"]},
        If[ TrueQ@show,
            {res,indet} =
                expr//reducePhaseBy[assume,listToPattern@holo,listToPattern@antiholo,extra,show];
            If[ indet=!={},
                indet[[1]]//Echo
            ],
            (*Else*)
            res =
                expr//reducePhaseBy[assume,listToPattern@holo,listToPattern@antiholo,extra,show]
        ];
        res//focusPowerExponent[Simplify]
    ];


reducePhaseBy[assume_,extra_][expr_] :=
    expr//ReplaceAll[
        power:Power[base_,exponent_]/;Simplify[base<0,assume]&&extra[power]:>
            Power[-base,exponent]*Exp[I*\[Pi]*exponent]
    ];

reducePhaseBy[assume_,antiholoP_,extra_][expr_] :=
    expr//ReplaceAll[
        power:Power[base_,exponent_]/;Simplify[base<0,assume]&&extra[power]:>
            If[ FreeQ[base,antiholoP],
                Power[-base,exponent]*Exp[I*\[Pi]*exponent],
                (*Else*)
                Power[-base,exponent]*Exp[-I*\[Pi]*exponent]
            ]
    ];

reducePhaseBy[assume_,holoP_,antiholoP_,extra_,False][expr_] :=
    expr//ReplaceAll[
        power:Power[base_,exponent_]/;Simplify[base<0,assume]&&extra[power]:>
            Which[
                !FreeQ[base,holoP]&&FreeQ[base,antiholoP],
                    Power[-base,exponent]*Exp[I*\[Pi]*exponent],
                !FreeQ[base,antiholoP]&&FreeQ[base,holoP],
                    Power[-base,exponent]*Exp[-I*\[Pi]*exponent],
                True,
                    Power[base,exponent]
            ]
    ];

reducePhaseBy[assume_,holoP_,antiholoP_,extra_,True][expr_] :=
    Reap[
        expr//ReplaceAll[
            power:Power[base_,exponent_]/;Simplify[base<0,assume]&&extra[power]:>
                Which[
                    !FreeQ[base,holoP]&&FreeQ[base,antiholoP],
                        Power[-base,exponent]*Exp[I*\[Pi]*exponent],
                    !FreeQ[base,antiholoP]&&FreeQ[base,holoP],
                        Power[-base,exponent]*Exp[-I*\[Pi]*exponent],
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


(* ::Subsection:: *)
(*Misc*)


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


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
