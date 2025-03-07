(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Label`"];


Needs["Yurie`Math`"];

Needs["Yurie`Base`"];

ClearAll["Yurie`Math`Label`*"];
ClearAll["Yurie`Math`Label`Private`*"];



(* ::Section:: *)
(*Public*)


label::usage =
    "join the variable(s) and label(s) into a (sequence of) labeled object(s).";

labelConvert::usage =
    "convert the labeled object(s) according to the two specified label positions.";


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
    Symbol|Construct|Subscript|Superscript;


$labelPositionP2::usage =
    "pattern of label positions except for Symbol.";

$labelPositionP2 =
    Construct|Subscript|Superscript;


$labelTypeP::usage =
    "pattern of label types.";

$labelTypeP =
    "PositiveInteger"|"PositiveIntegerOrSingleLetter"|"PositiveIntegerOrGreekLetter"|
    "NaturalNumber"|"NaturalNumberOrSingleLetter"|"NaturalNumberOrGreekLetter"|
    All|_Symbol;


$emptyLabelP::usage =
    "pattern of empty label.";

$emptyLabelP =
    Null|\[FormalO];


(* ::Subsection:: *)
(*Option*)


label//Options = {
    "LabelPosition"->Symbol
};


labelConvert//Options = {
    "LabelType"->All
};


(* ::Subsection:: *)
(*Message*)


label::posnotmatch =
    "the label position `` should be one of the followings:\n``."

label::typenotmatch =
    "the label type `` should be one of the followings:\n``."

label::badlab =
    "the label `` should be symbol, string, or nonnegative integer when the label position is Symbol."

labelConvert::posequal =
    "the two label positions `` and `` should be different.";


(* ::Subsection:: *)
(*label*)


(* ::Subsubsection:: *)
(*Main*)


label[var:_Symbol,lab_,opts:OptionsPattern[]] :=
    Catch[
        labelKernel[checkLabelPosition@OptionValue["LabelPosition"],var,lab],
        _,
        var&
    ];

label[var:_Symbol,labList_List,opts:OptionsPattern[]] :=
    Catch[
        With[ {position = checkLabelPosition@OptionValue["LabelPosition"]},
            Map[labelKernel[position,var,#]&,labList]//Apply[Sequence]
        ],
        _,
        var&
    ];

label[varList:{__Symbol},lab_,opts:OptionsPattern[]] :=
    Catch[
        With[ {position = checkLabelPosition@OptionValue["LabelPosition"]},
            Map[labelKernel[position,#,lab]&,varList]//Apply[Sequence]
        ],
        _,
        varList&
    ];

label[varList:{__Symbol},labList_List,opts:OptionsPattern[]] :=
    Catch[
        With[ {position = checkLabelPosition@OptionValue["LabelPosition"]},
            Outer[labelKernel[position,#1,#2]&,varList,labList]//Transpose//Flatten//Apply[Sequence]
        ],
        _,
        varList&
    ];


(* ::Subsubsection:: *)
(*Helper*)


checkLabelPosition[opt_] :=
    If[ !MatchQ[opt,$labelPositionP],
        Message[label::posnotmatch,opt,$labelPositionP];
        Throw[Null,"posnotmatch"],
        (*Else*)
        opt
    ];


checkLabelType[opt_] :=
    If[ !MatchQ[opt,$labelTypeP],
        Message[label::typenotmatch,opt,$labelTypeP];
        Throw[Null,"typenotmatch"],
        (*Else*)
        opt
    ];


labelKernel[position:$labelPositionP,var_,lab:$emptyLabelP] :=
    var;

labelKernel[position:$labelPositionP2,var_,lab_] :=
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
    (
        Message[label::badlab,lab];
        Throw[Null,"badlab"]
    );


(* ::Subsection:: *)
(*labelConvert*)


(* ::Subsubsection:: *)
(*Main*)


labelConvert[var_Symbol|{vars__Symbol},Rule[pos1:$labelPositionP,pos2:$labelPositionP],opts:OptionsPattern[]][expr_]/;pos1=!=pos2 :=
    Catch[
        With[ {type = checkLabelType@OptionValue["LabelType"]},
            labelConvertKernel[{var,vars},checkLabelPosition@pos1,checkLabelPosition@pos2,type][expr]
        ],
        _,
        expr&
    ];

labelConvert[_,Rule[pos1_,pos2_],OptionsPattern[]][expr_]/;pos1===pos2 :=
    Catch[
        Message[labelConvert::posequal,pos1,pos2];
        Throw[expr]
    ];


labelConvertKernel[varList_List,pos1_,pos2_,type_][expr_] :=
    With[ {varP = Alternatives@@varList},
        Switch[pos1,
            Construct,
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
                                    pos2[ToExpression@var,ToExpression@lab]
                            ]
                    ]
                ]
        ]
    ];


(* ::Subsubsection:: *)
(*Helper*)


labelKernel2[position:$labelPositionP2,var_,lab_] :=
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
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
