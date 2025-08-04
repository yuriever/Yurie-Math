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
    "Info[rules]: base->{factor1, factor2, ...}."<>
    "\n"<>
    "Hint: To specify the phase direction, include Positive or Negative in the factor list."<>
    "\n"<>
    "Hint: To match powers with exponent 1, include Optional in the factor list.";

powerSeparate::usage =
    "powerSeparate[baseP][expr]: separate the product expression into power factors and non-power factors."<>
    "\n"<>
    "Info[baseP]: the pattern of power bases to match.";

powerExponentCollect::usage =
    "powerExponentCollect[exponents...][expr]: collect and combine power factors with common exponents."<>
    "\n"<>
    "Hint: if no exponent is specified, try to collect all power factors.";


(* ::Subsection:: *)
(*Phase*)


phaseIgnore::usage =
    "phaseIgnore[expr]: ignore the phase factor in the product.";


(* ::Subsection:: *)
(*Polynomial*)


togetherBy::usage =
    "togetherBy[base][expr]: take together the terms with the specified base pattern in the polynomial expression."<>
    "\n"<>
    "Info[base]: the pattern of base to match.";


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

powerExpand[Shortest[operation_:Simplify],OptionsPattern[]][expr_] :=
    expr//
        powerBaseTogether[operation]//
        PowerExpand[#,Assumptions->OptionValue["Assumptions"]]&//
        powerExponentFocus[Simplify];

powerExpand[operation_,level_?levelQ,OptionsPattern[]][expr_] :=
    expr//
        powerBaseTogether[operation,level]//
        PowerExpand[#,Assumptions->OptionValue["Assumptions"]]&//
        powerExponentFocus[Simplify];


(* ::Subsubsection:: *)
(*powerExpandBy*)


powerExpandBy::SuspiciousRule =
    "The base `1` does not match the product of the factors, with the left factor being `2`.";

powerExpandBy::SuspiciousRule2 =
    "For the delayed rule, the base `1` may not match the product of the factors, with the left factor being `2`.";

powerExpandBy::InvalidPhase =
    "To specify the direction of the phase, use only one of {Positive, Negative}.";


powerExpandBy[rule:_Rule|_RuleDelayed,factorHead:Except[_Rule|_RuleDelayed]:Identity][expr_] :=
    expr//ReplaceAll[expandRuleForSpecifiedBase[rule]]//ReplaceAll[factorHeadOfExpandRule->factorHead];

powerExpandBy[rules:(_Rule|_RuleDelayed)..,factorHead:Except[_Rule|_RuleDelayed]:Identity][expr_] :=
    expr//ReplaceAll[Map[expandRuleForSpecifiedBase,{rules}]]//ReplaceAll[factorHeadOfExpandRule->factorHead];


factorHeadOfExpandRule//Attributes = {
    HoldAllComplete
};

expandRuleForSpecifiedBase[head_[base_,factorList1_List]] :=
    With[ {
            factorList = DeleteCases[factorList1,Positive|Negative|Optional],
            phaseSign = getPhaseSign[factorList1],
            ifOptional = MemberQ[factorList1,Optional],
            exponent = Unique[]
        },
        {
            powerFactorProduct = Times@@Map[Power[factorHeadOfExpandRule[#],exponent]&,factorList]
        },
        expandRuleCheckEquality[head,phaseSign][base,factorList];
        If[ ifOptional,
            HoldComplete[
                Power[base,exponent_.],
                Exp[phaseSign*I*π*exponent]*powerFactorProduct
            ],
            (*Else*)
            HoldComplete[
                Power[base,exponent_],
                Exp[phaseSign*I*π*exponent]*powerFactorProduct
            ]
        ]
    ]//ReplaceAll[HoldComplete[args__]:>RuleDelayed[args]];


getPhaseSign[factorList_List]/;FreeQ[factorList,Positive|Negative] :=
    0;

getPhaseSign[factorList_List]/;!FreeQ[factorList,Positive]&&FreeQ[factorList,Negative] :=
    1;

getPhaseSign[factorList_List]/;FreeQ[factorList,Positive]&&!FreeQ[factorList,Negative] :=
    -1;

getPhaseSign[factorList_List]/;!FreeQ[factorList,Positive]&&!FreeQ[factorList,Negative] :=
    (
        Message[powerExpandBy::InvalidPhase];
        0
    );


expandRuleCheckEquality[Rule,phaseSign_][base_,List[factors___]] :=
    With[ {
            ratio = Simplify[Exp[phaseSign*I*π]*base/Times[factors]]
        },
        If[ ratio =!= 1,
            Message[powerExpandBy::SuspiciousRule,base,ratio];
        ]
    ];

expandRuleCheckEquality[RuleDelayed,phaseSign_][base_,List[factors___]] :=
    With[ {
            ratio = Simplify[Exp[phaseSign*I*π]*stripPattern[base,Identity]/Times[factors]]
        },
        If[ ratio =!= 1,
            Message[powerExpandBy::SuspiciousRule2,base,ratio];
        ]
    ];


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
(*Phase*)


(* ::Subsubsection:: *)
(*phaseIgnore*)


phaseIgnore[expr:_Times|_Power] :=
    expr//ReplaceAll[{
        (-1)^_->1,
        Power[E,π*_Complex*_.]->1,
        Power[Complex[0,1|-1],_]->1
    }]//dropMinusSign;

phaseIgnore[(-1)^_] :=
    1;

phaseIgnore[Power[E,π*_Complex*_.]] :=
    1;

phaseIgnore[Power[Complex[0,1|-1],_]] :=
    1;


dropMinusSign[expr_?minusQ] :=
    -expr;

dropMinusSign[expr_] :=
    expr;


(* ::Subsection:: *)
(*Polynomial*)


togetherBy[baseP_][expr1_]:=
    FixedPoint[
        Replace[#,expr:Verbatim[Plus][___,Power[baseP,_?minusQ]*_.,___]:>Together[expr],All]&,
        expr1
    ];


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
    head[expr]//ReplaceRepeated[(Verbatim[Pattern]|Verbatim[Optional]|Verbatim[PatternTest]|Verbatim[Condition])[pattern_,___]:>pattern];


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
            res = Reduce`FreeVariables[expr],
            excludedContext = Join[{"System`"},exclusionList]
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
