(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`DLMF`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Constant`"];


(* ::Section:: *)
(*Public*)


DLMF::usage =
    "simplify expressions by the rules in DLMFData.";

DLMFAs::usage =
    "simplify expressions by the rules in DLMFData with the specified conditions.";

DLMFAsTrue::usage =
    "simplify expressions by the rules in DLMFData ignoring the conditions.";


DLMFRule::usage =
    "return the rules in DLMFData.";

DLMFRuleShow::usage =
    "show the rules without context marks in DLMFData.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


DLMF//Options = {
    "IgnoreCondition"->False
};

DLMFRule//Options = {
    "IgnoreCondition"->False
};

DLMFRuleShow//Options = {
    "IgnoreCondition"->False
};


(* ::Subsection:: *)
(*Main*)


DLMF[rules_,OptionsPattern[]][expr_] :=
    expr//DLMFPreprocess//DLMFKernel[getRuleList[rules],OptionValue["IgnoreCondition"]];

DLMF[rules_,as_,OptionsPattern[]][expr_] :=
    Pass;


DLMFAs[rules_,as_][expr_] :=
    Pass;


DLMFAsTrue[rules_][expr_] :=
    expr//DLMFPreprocess//DLMFKernel[getRuleList[rules],True];


DLMFRule[rules_,OptionsPattern[]] :=
    getRuleIgnoringCondition[OptionValue["IgnoreCondition"],getRuleList[rules]];


DLMFRuleShow/:MakeBoxes[DLMFRuleShow[rules_,OptionsPattern[]],form_] :=
    Block[ {Internal`$ContextMarks = False},
        (*here the function associated to the OptionValue should be specified.*)
        With[ {expr = getRuleIgnoringCondition[OptionValue[DLMFRuleShow,"IgnoreCondition"],getRuleList[rules]]},
            MakeBoxes[expr,form]
        ]
    ];


(* ::Subsection:: *)
(*Helper*)


(*convert Hypergeometric2F1Regularized to Hypergeometric2F1.*)

DLMFPreprocess[expr_] :=
    expr//ReplaceRepeated[DLMFData["15.1.2"]];


DLMFKernel[{rule_String},ifIgnoreCondition_][expr_] :=
    expr//ReplaceAll[getRuleIgnoringCondition[ifIgnoreCondition,rule]];

DLMFKernel[{rule_String,rest__String},ifIgnoreCondition_][expr_] :=
    expr//DLMFKernel[{rule},ifIgnoreCondition]//DLMFKernel[{rest},ifIgnoreCondition];


getRuleIgnoringCondition[True,rules_] :=
    DLMFData//Lookup[rules]//ReplaceAll[
        Verbatim[RuleDelayed][Verbatim[Condition][left_,condition_],right_]:>(left:>right)
    ];

getRuleIgnoringCondition[False,rules_] :=
    DLMFData//Lookup[rules];


getRuleList[rule_String] :=
    {rule};

getRuleList[(List|Alternatives)[rules___String]] :=
    {rules};


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
