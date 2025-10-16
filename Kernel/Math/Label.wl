(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Label`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


label::usage =
    "label[var, lab, head]: join the variable(s) and label into labeled objects using the specified head."<>
    "\n"<>
    "Default[head]: Function.";

labell::usage =
    "labell[var, lab]: variant of label with Symbol as head.";

labels::usage =
    "labels[var, range, head]: join the variable(s) and labels in the range using the specified head."<>
    "\n"<>
    "Default[head]: Function."<>
    "\n"<>
    "Example: labels[x, 3] gives x[1], x[2], x[3].";

labells::usage =
    "labells[var, range]: variant of labels with Symbol as head.";

labelAt::usage =
    "labelAt[var, rules, head]: take the specific values of the labeled objects according to rules."<>
    "\n"<>
    "Default[head]: Function.";


labelConvert::usage =
    "labelConvert[var, head1->head2]: convert the labeled objects according to the two specified label heads.";

labelJoin::usage =
    "labelJoin[var, head]: convert labeled objects from any head to Symbol."<>
    "\n"<>
    "Default[head]: Function."<>
    "\n"<>
    "Sketch: labelConvert with _->Symbol.";

labelSplit::usage =
    "labelSplit[var, head]: convert labeled objects from Symbol to any head."<>
    "\n"<>
    "Default[head]: Function."<>
    "\n"<>
    "Sketch: labelConvert with Symbol->_.";


labelToZero::usage =
    "labelToZero[var, labs, head]: shift to zero."<>
    "\n"<>
    "Default[head]: Function."<>
    "\n"<>
    "Example: x1->0.";

labelToEqual::usage =
    "labelToEqual[var, rules, head]: shift the first to the second."<>
    "\n"<>
    "Default[head]: Function."<>
    "\n"<>
    "Example: x1->x2.";

labelToDiff::usage =
    "labelToDiff[var, rules, head]: shift the first to the difference plus the second."<>
    "\n"<>
    "Default[head]: Function."<>
    "\n"<>
    "Example: x1->x12+x2.";

labelToDiffZero::usage =
    "labelToDiffZero[var, rules, head]: shift the first to the difference and the second to zero."<>
    "\n"<>
    "Default[head]: Function."<>
    "\n"<>
    "Example: x1->x12, x2->0.";

labelToDiffBack::usage =
    "labelToDiffBack[var, rules, head]: shift the difference back to the original two."<>
    "\n"<>
    "Default[head]: Function."<>
    "\n"<>
    "Example: x12->x1-x2.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Constant*)


$labelTypeList = {
    "PositiveInteger",
    "PositiveIntegerOrSingleLetter",
    "PositiveIntegerOrGreekLetter",
    "NaturalNumber",
    "NaturalNumberOrSingleLetter",
    "NaturalNumberOrGreekLetter"
};


(* ::Subsection:: *)
(*Option*)


labelConvert//Options = {
    "LabelType"->All
};

labelJoin//Options =
    Options@labelConvert;

labelSplit//Options =
    Options@labelConvert;


(* ::Subsection:: *)
(*Message*)


label::InvalidSymbol =
    "```` is not a valid labeled symbol."

labelInvalidSymbolF[var_,lab_] :=
    Failure["InvalidSymbol",<|
        "MessageTemplate":>label::InvalidSymbol,
        "MessageParameters"->{var,lab},
        "Var"->var,
        "Label"->lab
    |>];


label::UndefinedType =
    "The label type `` is undefined, and should be one of the followings:\n``."


(* ::Subsection:: *)
(*label*)


(* ::Subsubsection:: *)
(*Main*)


label[var_,lab_,head_Symbol:Function] :=
    labelKernel[head,var,lab];

label[(List|Alternatives)[vars__],lab_,head_Symbol:Function] :=
    Map[labelKernel[head,#,lab]&,Unevaluated@Sequence[vars]];

labell[var_,lab_] :=
    label[var,lab,Symbol];


labels[var_,n_Integer?NonNegative,head_Symbol:Function] :=
    Sequence@@Map[label[var,#,head]&,Range[n]];

labels[var_,n1_Integer?NonNegative,n2_Integer?NonNegative,head_Symbol:Function] :=
    Sequence@@Map[label[var,#,head]&,Range[n1,n2]];

labels[var_,n1_Integer?NonNegative,n2_Integer?NonNegative,step_Integer?Positive,head_Symbol:Function] :=
    Sequence@@Map[label[var,#,head]&,Range[n1,n2,step]];

labels[var_,n1_String,n2_String,head_Symbol:Function] :=
    Sequence@@Map[label[var,#,head]&,CharacterRange[n1,n2]];

labels[var_,list_List,head_Symbol:Function] :=
    Sequence@@Map[label[var,#,head]&,list];

labells[var_,args__] :=
    labels[var,args,Symbol];


(* ::Subsubsection:: *)
(*Helper*)


labelKernel[head_,var_,lab_] :=
    head[var,lab];


labelKernel[Function,var_,lab_] :=
    var[lab];


labelKernel[Symbol,var_,lab_] :=
    With[ {symbolname = varToString[var]<>ToString[lab]},
        If[ Internal`SymbolNameQ[symbolname],
            ToExpression[symbolname],
            (*Else*)
            labelInvalidSymbolF[var,lab]
        ]
    ];

labelKernel[Symbol,Verbatim[Pattern][var_Symbol,pat_],lab_] :=
    With[ {symbolname = varToString[var]<>ToString[lab]},
        If[ Internal`SymbolNameQ[symbolname],
            Pattern[Evaluate@ToExpression[symbolname],pat],
            (*Else*)
            labelInvalidSymbolF[var,lab]
        ]
    ];


(* This option is to ensure symbols with format definitions will be converted correctly. *)

varToString[expr_] :=
    ToString[expr,FormatType->InputForm];


(* ::Subsection:: *)
(*labelAt*)


(* ::Subsubsection:: *)
(*Main*)


labelAt[(List|Alternatives)[vars__]|var_,rules__Rule|{rules__Rule},head_Symbol:Function] :=
    labelAtKernel[head,{vars,var},{rules}]//ReplaceAll;


(* ::Subsubsection:: *)
(*Helper*)


labelAtKernel[head_,{var_},ruleList_] :=
    ruleList//expandRule//MapAt[labelKernel[head,var,#]&,{All,1}];

labelAtKernel[head_,varList_List,ruleList_] :=
    varList//Map[labelAtKernel[head,{#},ruleList]&]//Flatten;


expandRule[ruleList_] :=
    ruleList//
        ReplaceAll[Verbatim[Alternatives][args__]:>List[args]]//
            Map[Thread]//Flatten;


(* ::Subsection:: *)
(*labelConvert*)


(* ::Subsubsection:: *)
(*Main*)


labelConvert[(List|Alternatives)[vars__]|var_,Rule[head1_Symbol,head2_Symbol],opts:OptionsPattern[]][expr_] :=
    With[ {type = OptionValue["LabelType"]},
        If[ Head[type]=!=String||Head[type]===String&&MemberQ[$labelTypeList,type],
            labelConvertKernel[head1,head2,type][Alternatives[var,vars],expr],
            (*Else*)
            Message[label::UndefinedType,type,Column@$labelTypeList];
            expr
        ]
    ];


(* ::Subsubsection:: *)
(*Helper*)


labelConvertKernel[Function,head2:Except[Function],type_][varP_,expr_] :=
    expr//ReplaceAll[
        (* RuleCondition is to ensure the conversion can be preformed inside held expressions. *)
        (var:varP)[lab_]/;labelQ[type,lab]:>RuleCondition@labelKernel[head2,var,lab]
    ];

labelConvertKernel[head1:Except[Function|Symbol],head2:Function|Symbol,type_][varP_,expr_] :=
    expr//ReplaceAll[
        head1[var:varP,lab_]/;labelQ[type,lab]:>RuleCondition@labelKernel[head2,var,lab]
    ];

labelConvertKernel[Symbol,head2:Except[Symbol],type_][varP_,expr_] :=
    With[ {varStringP = Map[varToString,varP]},
        expr//ReplaceAll[
            symbol_Symbol/;Context[symbol]=!="System`":>
                RuleCondition@symbolPrepare[stringSplit[varToString[symbol],varStringP,type],symbol,head2]
        ]
    ];

labelConvertKernel[head_,head_,type_][varP_,expr_] :=
    expr;


labelQ[All,_] :=
    True;

labelQ[type_String,lab_String] :=
    namedLabelQ[type,lab];

labelQ[type_String,lab_] :=
    namedLabelQ[type,ToString[lab]];

labelQ[fun_,lab_] :=
    fun[lab];


namedLabelQ["PositiveInteger",lab_String] :=
    Quiet@StringMatchQ[lab,RegularExpression["^$|[1-9]\\d*"]];

namedLabelQ["PositiveIntegerOrSingleLetter",lab_String] :=
    Quiet@StringMatchQ[lab,RegularExpression["^$|[1-9]\\d*|[^\\W_]"]];

namedLabelQ["PositiveIntegerOrGreekLetter",lab_String] :=
    Quiet@StringMatchQ[lab,RegularExpression["^$|[1-9]\\d*|[αβγδεζηθικλμνξοπρστυφχψω]"]];

namedLabelQ["NaturalNumber",lab_String] :=
    Quiet@StringMatchQ[lab,RegularExpression["^$|0|[1-9]\\d*"]];

namedLabelQ["NaturalNumberOrSingleLetter",lab_String] :=
    Quiet@StringMatchQ[lab,RegularExpression["^$|0|[1-9]\\d*|[^\\W_]"]];

namedLabelQ["NaturalNumberOrGreekLetter",lab_String] :=
    Quiet@StringMatchQ[lab,RegularExpression["^$|0|[1-9]\\d*|[αβγδεζηθικλμνξοπρστυφχψω]"]];


stringSplit[str_,varP_,type_]/;StringStartsQ[str,varP] :=
    str//StringReplace[
        StartOfString~~Shortest[var__]~~Longest[lab__]~~EndOfString/;StringMatchQ[var,varP]&&labelQ[type,lab]:>{var,lab}
    ];

stringSplit[str_,varP_,type_] :=
    str;


symbolPrepare[res_String,symbol_,head_] :=
    symbol;

symbolPrepare[StringExpression[{var_,lab_}],symbol_,head_] :=
    labelKernel[head,ToExpression[var],ToExpression[lab]];


(* ::Subsection:: *)
(*labelJoin|labelSplit*)


labelJoin[vars_,head_Symbol:Function,opts:OptionsPattern[]][expr_] :=
    labelConvert[vars,head->Symbol,FilterRules[{opts,Options@labelJoin},Options@labelConvert]][expr];


labelSplit[vars_,head_Symbol:Function,opts:OptionsPattern[]][expr_] :=
    labelConvert[vars,Symbol->head,FilterRules[{opts,Options@labelSplit},Options@labelConvert]][expr];


(* ::Subsection:: *)
(*labelTo*)


(* ::Subsubsection:: *)
(*Main*)


labelToZero[(List|Alternatives)[vars__]|var_,{labs__}|lab_,head_Symbol:Function] :=
    ReplaceAll[
        labelRulePrototype[(#[[1]]->0)&,head,{vars,var},Map[#->#&,{labs,lab}]]
    ];

labelToEqual[(List|Alternatives)[vars__]|var_,rules__Rule|{rules__Rule},head_Symbol:Function] :=
    ReplaceAll[
        labelRulePrototype[(#[[1]]->#[[2]])&,head,{vars,var},{rules}]
    ];

labelToDiff[(List|Alternatives)[vars__]|var_,rules__Rule|{rules__Rule},head_Symbol:Function] :=
    ReplaceAll[
        labelRulePrototype[(#[[1]]->#[[2]]+#[[3]])&,head,{vars,var},{rules}]
    ];

labelToDiffZero[(List|Alternatives)[vars__]|var_,rules__Rule|{rules__Rule},head_Symbol:Function] :=
    ReplaceAll[
        labelRulePrototype[{#[[1]]->#[[3]],#[[2]]->0}&,head,{vars,var},{rules}]
    ];

labelToDiffBack[(List|Alternatives)[vars__]|var_,rules__Rule|{rules__Rule},head_Symbol:Function] :=
    ReplaceAll[
        labelRulePrototype[(#[[3]]->#[[1]]-#[[2]])&,head,{vars,var},{rules}]
    ];


(* ::Subsubsection:: *)
(*Helper*)


labelRulePrototype::usage =
    "generate rules from prototype function.";

labelRulePrototype[proto_,head_,{var_},{rule_}] :=
    proto@Map[
        labelKernel[head,var,#]&,extractLabelFromRule[rule]
    ];

labelRulePrototype[proto_,head_,{var_},{rules__}] :=
    Flatten@Map[labelRulePrototype[proto,head,{var},{#}]&,{rules}];

labelRulePrototype[proto_,head_,{vars__},{rule_}] :=
    Flatten@Map[labelRulePrototype[proto,head,{#},{rule}]&,{vars}];

labelRulePrototype[proto_,head_,{vars__},{rules__}] :=
    Flatten@Outer[labelRulePrototype[proto,head,{#1},{#2}]&,{vars},{rules}];


extractLabelFromRule::usage =
    "extract labels and difference of labels from rule.";

extractLabelFromRule[Rule[lab1_,lab2_]] :=
    (* 1->2 returns {1,2,12} *)
    {lab1,lab2,ToString[lab1]<>ToString[lab2]};


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
