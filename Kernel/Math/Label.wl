(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Label`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


label::usage =
    "join the variable(s) and label(s) into a (sequence of) labeled object(s).";

labelAt::usage =
    "take the specific value(s) of the labeled object(s)."


labelConvert::usage =
    "convert the labeled object(s) according to the two specified label heads.";

labelJoin::usage =
    "labelConvert: _->Symbol.";

labelSplit::usage =
    "labelConvert: Symbol->_.";


labelToZero::usage =
    "x1->0.";

labelToEqual::usage =
    "x1->x2.";

labelToDiff::usage =
    "x1->x12+x2.";

labelToDiffZero::usage =
    "x1->x12, x2->0.";

labelToDiffBack::usage =
    "x12->x1-x2.";


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

labelInvalidSymbolF[var_,lab_]:=
    Failure["InvalidSymbol",<|
        "MessageTemplate":>label::InvalidSymbol,
        "MessageParameters"->{var,lab},
        "Var"->var,
        "Label"->lab
    |>];


label::InvalidLabel =
    "`` is not a valid label.";

labelInvalidLabelF[var_,lab_]:=
    Failure["InvalidLabel",<|
        "MessageTemplate":>label::InvalidLabel,
        "MessageParameters"->lab,
        "Var"->var,
        "Label"->lab
    |>];


label::UndefinedType =
    "the label type `` is undefined, and should be one of the followings:\n``."


(* ::Subsection:: *)
(*label*)


(* ::Subsubsection:: *)
(*Main*)


label[var_,lab_,head_Symbol:Function] :=
    labelKernel[head,var,lab];


label[var_,(List|Alternatives)[labs__],head_Symbol:Function] :=
    Map[labelKernel[head,var,#]&,Unevaluated@Sequence[labs]];

label[(List|Alternatives)[vars__],lab_,head_Symbol:Function] :=
    Map[labelKernel[head,#,lab]&,Unevaluated@Sequence[vars]];

label[(List|Alternatives)[vars__],(List|Alternatives)[labs__],head_Symbol:Function] :=
    Outer[labelKernel[head,#1,#2]&,Unevaluated@Sequence[vars],Unevaluated@Sequence[labs]];


(* ::Subsubsection:: *)
(*Helper*)


labelKernel[head_,var_,lab_] :=
    head[var,lab];


labelKernel[Function,var_,lab_] :=
    var[lab];


labelKernel[Symbol,var_Symbol,lab_] :=
    Catch[
        ToExpression[ToString[var]<>labelToString[lab]],
        "InvalidLabel",
        labelInvalidLabelF[var,#]&
    ];

labelKernel[Symbol,Verbatim[Pattern][var_Symbol,pat_],lab_] :=
    Catch[
        Pattern[
            Evaluate@ToExpression[ToString[var]<>labelToString[lab]],
            pat
        ],
        "InvalidLabel",
        labelInvalidLabelF[var,#]&
    ]

labelKernel[Symbol,var_String,lab_] :=
    Catch[
        ToExpression[var<>labelToString[lab]],
        "InvalidLabel",
        labelInvalidLabelF[var,#]&
    ];

labelKernel[Symbol,var_,lab_] :=
    labelInvalidSymbolF[var,lab];


labelToString[lab_Integer?NonNegative] :=
    ToString[lab];

labelToString[lab_Symbol] :=
    SymbolName[lab];

labelToString[lab_String] :=
    lab;

labelToString[lab_] :=
    Throw[lab,"InvalidLabel"];


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
        (var:varP)[lab_]/;labelQ[type,lab]:>labelKernel[head2,var,lab]
    ];

labelConvertKernel[head1:Except[Function|Symbol],head2:Function|Symbol,type_][varP_,expr_] :=
    expr//ReplaceAll[
        head1[var:varP,lab_]/;labelQ[type,lab]:>labelKernel[head2,var,lab]
    ];

labelConvertKernel[Symbol,head2:Except[Symbol],type_][varP_,expr_] :=
    With[ {varStringP = Map[ToString,varP]},
        expr//ReplaceAll[
            symbol_Symbol/;Context[symbol]=!="System`":>
                symbolPrepare[stringSplit[ToString[symbol],varStringP,type],symbol,head2]
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


labelToZero[(List|Alternatives)[vars__]|var_,labelList_List,head_Symbol:Function] :=
    ReplaceAll[
        labelRulePrototype[(#[[1]]->0)&,head,{vars,var},Map[#->#&,labelList]]
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
