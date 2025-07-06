(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`DLMF`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Constant`"];


(* ::Section:: *)
(*Public*)


DLMF::usage =
    "DLMF[rules, opts][expr]: simplify the expression by the DLMF rules."<>
    "\n"<>
    "Default[\"IgnoreCondition\"]: False.";

DLMFAs::usage =
    "DLMFAs[rules, as][expr]: simplify the expression by the DLMF rules under the assumption."<>
    "\n"<>
    "Def[as]: the assumption.";

DLMFAsTrue::usage =
    "DLMFAsTrue[rules][expr]: simplify the expression by the DLMF rules ignoring all the conditions."<>
    "\n"<>
    "Hint: this is equivalent to DLMF with \"IgnoreCondition\"->True.";


DLMFRule::usage =
    "DLMFRule[rules, opts]: return the DLMF rules."<>
    "\n"<>
    "Default[\"IgnoreCondition\"]: False.";

DLMFRuleShow::usage =
    "DLMFRuleShow[rules, opts]: show the DLMF rules without context marker."<>
    "\n"<>
    "Default[\"IgnoreCondition\"]: False.";


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
