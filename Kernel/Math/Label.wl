(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Label`"];


Needs["Yurie`Math`"];


(* ::Text:: *)
(*BeginDeveloper*)


Needs["Yurie`Base`"];

ClearAll["Yurie`Math`Label`*`*"];
ClearAll["labelConvert"];
ClearAll["label"];
ClearAll["labelAt"];


(* ::Text:: *)
(*EndDeveloper*)


(* ::Section:: *)
(*Public*)


label::usage =
    "join the variable(s) and label(s) into a (sequence of) labeled object(s).";

labelAt::usage =
    "take the specific value(s) of the labeled object(s)."


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


(* labelP =
    _String|_Symbol|_Integer?NonNegative; *)

typeP =
    All|
    "PositiveInteger"|"PositiveIntegerOrSingleLetter"|"PositiveIntegerOrGreekLetter"|
    "NaturalNumber"|"NaturalNumberOrSingleLetter"|"NaturalNumberOrGreekLetter"|
    _Symbol|_Function|_RightComposition|_Composition;


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


label::badsymbol =
    "```` is not a valid labeled symbol."

label::badlabel =
    "`` is not a valid label.";

label::typenotmatch =
    "the label type `` should be one of the followings:\n``."


(* ::Subsection:: *)
(*label*)


(* ::Subsubsection:: *)
(*Main*)


label[var_,lab_,pos_] :=
    labelKernel[pos,var,lab];


label[var_,(List|Alternatives)[labs___],pos_] :=
    Map[labelKernel[pos,var,#]&,Unevaluated@Sequence[labs]];

label[(List|Alternatives)[vars___],lab_,pos_] :=
    Map[labelKernel[pos,#,lab]&,Unevaluated@Sequence[vars]];

label[(List|Alternatives)[vars___],(List|Alternatives)[labs___],pos_] :=
    Outer[labelKernel[pos,#1,#2]&,Unevaluated@Sequence[vars],Unevaluated@Sequence[labs]];


label[var_,lab_] :=
    label[var,lab,Function];


(* ::Subsubsection:: *)
(*Helper*)


labelKernel[head_,var_,lab_] :=
    head[var,lab];


labelKernel[Function,var_,lab_] :=
    var[lab];


labelKernel[Symbol,var_Symbol,lab_] :=
    Catch[
        ToExpression[ToString[var,FormatType->InputForm]<>labelToString@lab],
        "badlabel",
        HoldComplete[var,#]&
    ];

labelKernel[Symbol,Verbatim[Pattern][var_Symbol,pat_],lab_] :=
    Catch[
        Pattern[
            Evaluate@ToExpression[ToString[var,FormatType->InputForm]<>labelToString@lab],
            pat
        ],
        "badlabel",
        HoldComplete[var,#]&
    ]

labelKernel[Symbol,var_String,lab_] :=
    Catch[
        ToExpression[var<>labelToString@lab],
        "badlabel",
        HoldComplete[var,#]&
    ];

labelKernel[Symbol,var_,lab_] :=
    (
        Message[label::badsymbol,var,lab];
        HoldComplete[var,lab]
    );


labelToString[lab_Integer?NonNegative] :=
    ToString@lab;

labelToString[lab_Symbol] :=
    SymbolName@lab;

labelToString[lab_String] :=
    lab;

labelToString[lab_] :=
    (
        Message[label::badlabel,lab];
        Throw[lab,"badlabel"]
    );


(* ::Subsection:: *)
(*labelAt*)


(* ::Subsubsection:: *)
(*Main*)


labelAt[var_,rules__Rule,Symbol] :=
    (* Expand the head Alternatives into List when the position is Symbol. *)
    labelAtKernel[Symbol,var,ReplaceAll[{rules},Verbatim[Alternatives][args__]:>List[args]]];

labelAt[var_,rules__Rule,pos_] :=
    labelAtKernel[pos,var,{rules}];


labelAt[var_,rules__Rule] :=
    labelAtKernel[Function,var,{rules}];


(* ::Subsubsection:: *)
(*Helper*)


labelAtKernel[pos_,var_,ruleList_] :=
    Map[Thread,ruleList]//Flatten//MapAt[labelKernel[pos,var,#]&,{All,1}]//ReplaceAll;


(* ::Subsection:: *)
(*labelConvert*)


(* ::Subsubsection:: *)
(*Main*)


labelConvert[(List|Alternatives)[vars__]|var_,Rule[pos1_,pos2_],opts:OptionsPattern[]][expr_] :=
    With[ {type = OptionValue["LabelType"]},
        If[ MatchQ[type,typeP],
            labelConvertKernel[Alternatives[var,vars],pos1,pos2,type][expr],
            (*Else*)
            Message[label::typenotmatch,type,typeP];
            HoldComplete[expr]
        ]
    ];


(* ::Subsubsection:: *)
(*Helper*)


labelConvertKernel[varP_,Function,pos:Except[Symbol],type_][expr_] :=
    expr//ReplaceAll[
        (var:varP)[lab_]/;labelQ[type,lab]:>labelKernel[pos,var,lab]
    ];

labelConvertKernel[varP_,pos:Except[Symbol],Function,type_][expr_] :=
    expr//ReplaceAll[
        pos[var:varP,lab_]/;labelQ[type,lab]:>var[lab]
    ];

labelConvertKernel[varP_,Superscript,pos:Subscript|Function,type_][expr_] :=
    expr//ReplaceAll[
        Superscript[var:varP,lab_]/;labelQ[type,lab]:>labelKernel[pos,var,lab]
    ];


labelConvertKernel[varP_,Function,Symbol,type_][expr_] :=
    expr//ReplaceAll[
        (var:varP)[lab_]/;AtomQ[lab]&&labelQ[type,lab]:>labelKernel[Symbol,var,lab]
    ];

labelConvertKernel[varP_,pos:Subscript|Superscript,Symbol,type_][expr_] :=
    expr//ReplaceAll[
        pos[var:varP,lab_]/;AtomQ[lab]&&labelQ[type,lab]:>labelKernel[Symbol,var,lab]
    ];

labelConvertKernel[varP_,Symbol,pos2:Function|Subscript|Superscript,type_][expr_] :=
    With[ {varStringP = Map[ToString[#,FormatType->InputForm]&,varP]},
        expr//ReplaceAll[
            symbol_Symbol:>
                symbolFromStringOrStringExpression@StringReplace[
                    ToString[symbol,FormatType->InputForm],
                    StartOfString~~Shortest[var__]~~Longest[lab__]~~EndOfString/;StringMatchQ[var,varStringP]&&labelQ[type,lab]:>
                        labelKernel[pos2,ToExpression@var,ToExpression@lab]
                ]
        ]
    ];


labelQ[All,_] :=
    True;

labelQ[fun:_Symbol|_Function|_RightComposition|_Composition,lab_] :=
    fun[lab];

labelQ["PositiveInteger",lab_] :=
    Quiet@StringMatchQ[labelToString[lab],RegularExpression["^$|[1-9]\\d*"]];

labelQ["PositiveIntegerOrSingleLetter",lab_] :=
    Quiet@StringMatchQ[labelToString[lab],RegularExpression["^$|[1-9]\\d*|[^\\W_]"]];

labelQ["PositiveIntegerOrGreekLetter",lab_] :=
    Quiet@StringMatchQ[labelToString[lab],RegularExpression["^$|0|[1-9]\\d*|[\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Omicron]\[Pi]\[Rho]\[Sigma]\[Tau]\[Upsilon]\[CurlyPhi]\[Chi]\[Psi]\[Omega]]"]];

labelQ["NaturalNumber",lab_] :=
    Quiet@StringMatchQ[labelToString[lab],RegularExpression["^$|0|[1-9]\\d*"]];

labelQ["NaturalNumberOrSingleLetter",lab_] :=
    Quiet@StringMatchQ[labelToString[lab],RegularExpression["^$|0|[1-9]\\d*|[^\\W_]"]];

labelQ["NaturalNumberOrGreekLetter",lab_] :=
    Quiet@StringMatchQ[labelToString[lab],RegularExpression["^$|0|[1-9]\\d*|[\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Omicron]\[Pi]\[Rho]\[Sigma]\[Tau]\[Upsilon]\[CurlyPhi]\[Chi]\[Psi]\[Omega]]"]];


symbolFromStringOrStringExpression[expr_String] :=
    ToExpression@expr;

symbolFromStringOrStringExpression[expr_StringExpression] :=
    First@expr;


(* ::Subsection:: *)
(*labelJoin|labelSplit*)


labelJoin[vars_,pos:posP:Function,opts:OptionsPattern[]][expr_] :=
    labelConvert[{vars},pos->Symbol,FilterRules[{opts,Options@labelJoin},Options@labelConvert]][expr];

labelSplit[vars_,pos:posP:Function,opts:OptionsPattern[]][expr_] :=
    labelConvert[{vars},Symbol->pos,FilterRules[{opts,Options@labelSplit},Options@labelConvert]][expr];


(* ::Subsection:: *)
(*labelTo*)


(* ::Subsubsection:: *)
(*Main*)


labelToZero[vars__Symbol|{vars__Symbol}|Verbatim[Alternatives][vars__Symbol],labelList_List,pos:allPosP:Function] :=
    Catch[
        ReplaceAll[
            labelRulePrototype[(#[[1]]->0)&,pos,{vars},Map[#->#&,labelList]]
        ],
        _,
        HoldComplete[Identity]&
    ];


labelToEqual[vars__Symbol|{vars__Symbol}|Verbatim[Alternatives][vars__Symbol],rules__Rule|{rules__Rule},pos:allPosP:Function] :=
    Catch[
        ReplaceAll[
            labelRulePrototype[(#[[1]]->#[[2]])&,pos,{vars},{rules}]
        ],
        _,
        HoldComplete[Identity]&
    ];


labelToDiff[vars__Symbol|{vars__Symbol}|Verbatim[Alternatives][vars__Symbol],rules__Rule|{rules__Rule},pos:allPosP:Function] :=
    Catch[
        ReplaceAll[
            labelRulePrototype[(#[[1]]->#[[2]]+#[[3]])&,pos,{vars},{rules}]
        ],
        _,
        HoldComplete[Identity]&
    ];


labelToDiffZero[vars__Symbol|{vars__Symbol}|Verbatim[Alternatives][vars__Symbol],rules__Rule|{rules__Rule},pos:allPosP:Function] :=
    Catch[
        ReplaceAll[
            labelRulePrototype[{#[[1]]->#[[3]],#[[2]]->0}&,pos,{vars},{rules}]
        ],
        _,
        HoldComplete[Identity]&
    ];


labelToDiffBack[vars__Symbol|{vars__Symbol}|Verbatim[Alternatives][vars__Symbol],rules__Rule|{rules__Rule},pos:allPosP:Function] :=
    Catch[
        ReplaceAll[
            labelRulePrototype[(#[[3]]->#[[1]]-#[[2]])&,pos,{vars},{rules}]
        ],
        _,
        HoldComplete[Identity]&
    ];


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
