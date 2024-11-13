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


DLMF[ruleOrItsList_,OptionsPattern[]][expr_] :=
    expr//DLMFPreprocess//DLMFKernel[ruleOrItsList,OptionValue["IgnoreCondition"]];


DLMFRule[ruleOrItsList_,OptionsPattern[]] :=
    getRuleIgnoringCondition[OptionValue["IgnoreCondition"],ruleOrItsList];


DLMFRuleShow/:MakeBoxes[DLMFRuleShow[ruleOrItsList_,OptionsPattern[]],form_] :=
    Block[ {Internal`$ContextMarks = False},
        (*here the function associated to the OptionValue should be specified.*)
        With[ {expr = getRuleIgnoringCondition[OptionValue[DLMFRuleShow,"IgnoreCondition"],ruleOrItsList]},
            MakeBoxes[expr,form]
        ]
    ];


(* ::Subsection:: *)
(*Helper*)


(*convert Hypergeometric2F1Regularized to Hypergeometric2F1.*)

DLMFPreprocess[expr_] :=
    expr//ReplaceRepeated[DLMFData["15.1.2"]];


DLMFKernel[rule_String,ifIgnoreCondition_][expr_] :=
    expr//ReplaceAll[getRuleIgnoringCondition[ifIgnoreCondition,rule]];

DLMFKernel[{rule_String},ifIgnoreCondition_][expr_] :=
    expr//ReplaceAll[getRuleIgnoringCondition[ifIgnoreCondition,rule]];

DLMFKernel[{rule_String,ruleRest__String},ifIgnoreCondition_][expr_] :=
    expr//DLMFKernel[{rule},ifIgnoreCondition]//DLMFKernel[{ruleRest},ifIgnoreCondition];


getRuleIgnoringCondition[True,ruleOrItsList_] :=
    DLMFData//Lookup[ruleOrItsList]//ReplaceAll[
        Verbatim[RuleDelayed][Verbatim[Condition][left_,condition_],right_]:>(left:>right)
    ];

getRuleIgnoringCondition[False,ruleOrItsList_] :=
    DLMFData//Lookup[ruleOrItsList];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
