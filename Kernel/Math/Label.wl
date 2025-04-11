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
    "take special values of the labeled object(s)."


labelConvert::usage =
    "convert the labeled object(s) according to the two specified label positions.";

labelJoin::usage =
    "labelConvert: Function|Subscript|Superscript->Symbol.";

labelSplit::usage =
    "labelConvert: Symbol->Function|Subscript|Superscript.";


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


$labelPositionP::usage =
    "pattern of label positions.";

$labelPositionP =
    Symbol|Function|Subscript|Superscript;


$labelTypeP::usage =
    "pattern of label types.";

$labelTypeP =
    All|
    "PositiveInteger"|"PositiveIntegerOrSingleLetter"|"PositiveIntegerOrGreekLetter"|
    "NaturalNumber"|"NaturalNumberOrSingleLetter"|"NaturalNumberOrGreekLetter"|
    _Symbol|_Function|_RightComposition|_Composition;


$emptyLabelP::usage =
    "pattern of empty label.";

$emptyLabelP =
    Null|\[FormalO];


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


label::typenotmatch =
    "the label type `` should be one of the followings:\n``."

label::posnotmatch =
    "the label position `` should be one of the followings:\n``."

label::badlab =
    "the label `` should be symbol, string, or nonnegative integer when the label position is Symbol."

labelConvert::posequal =
    "the two label positions `` and `` should be different.";


checkAndThrowLabelType[type:$labelTypeP] :=
    type;

checkAndThrowLabelType[type:Except[$labelTypeP]] :=
    (
        Message[label::typenotmatch,type,$labelTypeP];
        Throw[Null,"typenotmatch"]
    );


throwBadLabel[lab_] :=
    (
        Message[label::badlab,lab];
        Throw[Null,"badlab"]
    );


returnWrongLabelPosition[pos_,return_] :=
    (
        Message[label::posnotmatch,pos,$labelPositionP];
        HoldComplete[return]
    );

returnEqualLabelPosition[pos1_,pos2_,return_] :=
    (
        Message[labelConvert::posequal,pos1,pos2];
        HoldComplete[return]
    );


(* ::Subsection:: *)
(*label*)


(* ::Subsubsection:: *)
(*Main*)


label[var:_Symbol,lab_,pos:$labelPositionP:Function] :=
    Catch[
        labelKernel[pos,var,lab],
        _,
        HoldComplete[var]&
    ];

label[var:_Symbol,labList_List,pos:$labelPositionP:Function] :=
    Catch[
        Map[labelKernel[pos,var,#]&,labList]//Apply[Sequence],
        _,
        HoldComplete[var]&
    ];

label[varList:{__Symbol},lab_,pos:$labelPositionP:Function] :=
    Catch[
        Map[labelKernel[pos,#,lab]&,varList]//Apply[Sequence],
        _,
        HoldComplete[varList]&
    ];

label[varList:{__Symbol},labList_List,pos:$labelPositionP:Function] :=
    Catch[
        Outer[labelKernel[pos,#1,#2]&,varList,labList]//Transpose//Flatten//Apply[Sequence],
        _,
        HoldComplete[varList]&
    ];

label[var_,_,pos:Except[$labelPositionP]] :=
    returnWrongLabelPosition[pos,var];


labelAt[var_Symbol,rules__Rule,pos:$labelPositionP:Function] :=
    Catch[
        Map[Thread,{rules}]//Flatten//MapAt[labelKernel[pos,var,#]&,{All,1}]//ReplaceAll,
        _,
        HoldComplete[{rules}]&
    ];

labelAt[var_,Longest[__],pos:Except[$labelPositionP]] :=
    returnWrongLabelPosition[pos,var];


(* ::Subsubsection:: *)
(*Helper*)


labelKernel[position:$labelPositionP,var_,lab:$emptyLabelP] :=
    var;

labelKernel[position:Function,var_,lab_] :=
    var[lab];

labelKernel[position:Subscript|Superscript,var_,lab_] :=
    position[var,lab];

labelKernel[Symbol,var_,lab_] :=
    ToExpression[ToString[var,FormatType->InputForm]<>labelToString[lab]];


labelToString[lab_String] :=
    lab;

labelToString[lab_Symbol] :=
    SymbolName@lab;

labelToString[lab_Integer?NonNegative] :=
    ToString@lab;

labelToString[lab_] :=
    throwBadLabel[lab];


(* ::Subsection:: *)
(*labelConvert*)


(* ::Subsubsection:: *)
(*Main*)


labelConvert[var_Symbol|{vars__Symbol},Rule[pos1:$labelPositionP,pos2:$labelPositionP],opts:OptionsPattern[]][expr_]/;pos1=!=pos2 :=
    Catch[
        With[ {type = checkAndThrowLabelType@OptionValue["LabelType"]},
            labelConvertKernel[{var,vars},pos1,pos2,type][expr]
        ],
        _,
        HoldComplete[expr]&
    ];

labelConvert[_,Rule[pos1_,pos2_],OptionsPattern[]][expr_]/;pos1===pos2 :=
    returnEqualLabelPosition[pos1,pos2,expr];

labelConvert[_,Rule[pos1_,pos2_],OptionsPattern[]][expr_]/;!MatchQ[pos1,$labelPositionP]||!MatchQ[pos2,$labelPositionP] :=
    returnWrongLabelPosition[{pos1,pos2},expr];


labelJoin[var_,pos:$labelPositionP:Function,opts:OptionsPattern[]][expr_] :=
    labelConvert[var,pos->Symbol,FilterRules[{opts,Options@labelJoin},Options@labelConvert]][expr];


labelSplit[var_,pos:$labelPositionP:Function,opts:OptionsPattern[]][expr_] :=
    labelConvert[var,Symbol->pos,FilterRules[{opts,Options@labelSplit},Options@labelConvert]][expr];


(* ::Subsubsection:: *)
(*Helper*)


labelConvertKernel[varList_List,pos1_,pos2_,type_][expr_] :=
    With[ {varP = Alternatives@@varList},
        Switch[pos1,
            Function,
                expr//ReplaceAll[
                    (var:varP)[lab_]/;AtomQ[lab]&&labelQ[type,labelToString[lab]]:>
                        RuleCondition@labelKernel2[pos2,var,lab]
                ],
            Subscript|Superscript,
                expr//ReplaceAll[
                    pos1[var:varP,lab_]/;AtomQ[lab]&&labelQ[type,labelToString[lab]]:>
                        RuleCondition@labelKernel2[pos2,var,lab]
                ],
            Symbol,
                With[ {varStringP = Map[ToString[#,FormatType->InputForm]&,varP]},
                    expr//ReplaceAll[
                        symbol_Symbol:>
                            RuleCondition@symbolFromStringOrStringExpression@StringReplace[
                                ToString[symbol,FormatType->InputForm],
                                StartOfString~~Shortest[var__]~~Longest[lab__]~~EndOfString/;StringMatchQ[var,varStringP]&&labelQ[type,lab]:>
                                    labelKernel2[pos2,ToExpression@var,ToExpression@lab]
                            ]
                    ]
                ]
        ]
    ];


(* Unlike labelKernel, labelKernel2 does not handle empty label. *)

labelKernel2[position:Function,var_,lab_] :=
    var[lab];

labelKernel2[position:Subscript|Superscript,var_,lab_] :=
    position[var,lab];

labelKernel2[Symbol,var_,lab_] :=
    ToExpression[ToString[var,FormatType->InputForm]<>labelToString[lab]];


labelQ[All,_] :=
    True;

labelQ["PositiveInteger",str_] :=
    StringMatchQ[str,RegularExpression["^$|[1-9]\\d*"]];

labelQ["PositiveIntegerOrSingleLetter",str_] :=
    StringMatchQ[str,RegularExpression["^$|[1-9]\\d*|[^\\W_]"]];

labelQ["PositiveIntegerOrGreekLetter",str_] :=
    StringMatchQ[str,RegularExpression["^$|0|[1-9]\\d*|[\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Omicron]\[Pi]\[Rho]\[Sigma]\[Tau]\[Upsilon]\[CurlyPhi]\[Chi]\[Psi]\[Omega]]"]];

labelQ["NaturalNumber",str_] :=
    StringMatchQ[str,RegularExpression["^$|0|[1-9]\\d*"]];

labelQ["NaturalNumberOrSingleLetter",str_] :=
    StringMatchQ[str,RegularExpression["^$|0|[1-9]\\d*|[^\\W_]"]];

labelQ["NaturalNumberOrGreekLetter",str_] :=
    StringMatchQ[str,RegularExpression["^$|0|[1-9]\\d*|[\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Omicron]\[Pi]\[Rho]\[Sigma]\[Tau]\[Upsilon]\[CurlyPhi]\[Chi]\[Psi]\[Omega]]"]];

labelQ[fun_Symbol,str_] :=
    fun[str];


symbolFromStringOrStringExpression[expr_String] :=
    ToExpression@expr;

symbolFromStringOrStringExpression[expr_StringExpression] :=
    First@expr;


(* ::Subsection:: *)
(*labelTo*)


(* ::Subsubsection:: *)
(*Main*)


labelToZero[var_Symbol|{vars__Symbol},(label:Except[_List])|{labels__},pos:$labelPositionP:Function] :=
    Catch[
        ReplaceAll[
            labelRulePrototype[(#[[1]]->0)&,pos,{var,vars},Map[#->#&,{label,labels}]]
        ],
        _,
        HoldComplete[Identity]&
    ];


labelToEqual[var_Symbol|{vars__Symbol},rule_Rule|{rules__Rule},pos:$labelPositionP:Function] :=
    Catch[
        ReplaceAll[
            labelRulePrototype[(#[[1]]->#[[2]])&,pos,{var,vars},{rule,rules}]
        ],
        _,
        HoldComplete[Identity]&
    ];


labelToDiff[var_Symbol|{vars__Symbol},rule_Rule|{rules__Rule},pos:$labelPositionP:Function] :=
    Catch[
        ReplaceAll[
            labelRulePrototype[(#[[1]]->#[[2]]+#[[3]])&,pos,{var,vars},{rule,rules}]
        ],
        _,
        HoldComplete[Identity]&
    ];


labelToDiffZero[var_Symbol|{vars__Symbol},rule_Rule|{rules__Rule},pos:$labelPositionP:Function] :=
    Catch[
        ReplaceAll[
            labelRulePrototype[{#[[1]]->#[[3]],#[[2]]->0}&,pos,{var,vars},{rule,rules}]
        ],
        _,
        HoldComplete[Identity]&
    ];


labelToDiffBack[var_Symbol|{vars__Symbol},rule_Rule|{rules__Rule},pos:$labelPositionP:Function] :=
    Catch[
        ReplaceAll[
            labelRulePrototype[(#[[3]]->#[[1]]-#[[2]])&,pos,{var,vars},{rule,rules}]
        ],
        _,
        HoldComplete[Identity]&
    ];


labelToZero[_,_,pos:Except[$labelPositionP]] :=
    returnWrongLabelPosition[pos,Identity];

labelToEqual[_,_,pos:Except[$labelPositionP]] :=
    returnWrongLabelPosition[pos,Identity];

labelToDiff[_,_,pos:Except[$labelPositionP]] :=
    returnWrongLabelPosition[pos,Identity];

labelToDiffZero[_,_,pos:Except[$labelPositionP]] :=
    returnWrongLabelPosition[pos,Identity];

labelToDiffBack[_,_,pos:Except[$labelPositionP]] :=
    returnWrongLabelPosition[pos,Identity];


(* ::Subsubsection:: *)
(*Helper*)


labelRulePrototype::usage =
    "generate rules from prototype function.";


labelRulePrototype[proto_,pos_,{var_Symbol},{rule_Rule}] :=
    proto@Map[
        labelKernel[pos,var,#]&,extractLabelFromRule[rule]
    ];

labelRulePrototype[proto_,pos_,{var_Symbol},{rules__Rule}] :=
    Flatten@Map[labelRulePrototype[proto,pos,{var},{#}]&,{rules}];

labelRulePrototype[proto_,pos_,{vars__Symbol},{rule_Rule}] :=
    Flatten@Map[labelRulePrototype[proto,pos,{#},{rule}]&,{vars}];

labelRulePrototype[proto_,pos_,{vars__Symbol},{rules__Rule}] :=
    Flatten@Outer[labelRulePrototype[proto,pos,{#1},{#2}]&,{vars},{rules}];


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
