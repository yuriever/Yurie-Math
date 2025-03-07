(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Deprecation`Index`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


indexize::usage =
    "join the variable and index(s) into a symbol.";

indexify::usage =
    "join the variable(s) and index(s) into a sequence of symbols.";


indexJoin::usage =
    "join indexed variables into symbols in the expression.";

indexSplit::usage =
    "split symbols into indexed variables in the expression.";


indexToZero::usage =
    "x1->0.";

indexToEqual::usage =
    "x1->x2.";

indexToDiff::usage =
    "x1->x12+x2.";

indexToDiffZero::usage =
    "x1->x12,x2->0.";

indexToDiffBack::usage =
    "x12->x1-x2.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];



(* ::Subsection:: *)
(*Constant*)


$indexPositionP::usage =
    "pattern of index positions, used by indexJoin|indexSplit.";

$indexPositionP =
    Construct|Subscript|Superscript;


$indexPositionP2::usage =
    "pattern of index positions, used by indexTo* functions.";

$indexPositionP2 =
    Symbol|Construct|Subscript|Superscript;


$indexTypeP::usage =
    "pattern of index types.";

$indexTypeP =
    "PositiveInteger"|"PositiveIntegerOrSingleLetter"|"PositiveIntegerOrGreekLetter"|
    "NaturalNumber"|"NaturalNumberOrSingleLetter"|"NaturalNumberOrGreekLetter"|
    All|_Symbol;


(* ::Subsection:: *)
(*indexize|indexify*)


(* ::Subsubsection:: *)
(*Main*)


indexize[var:_Symbol|_String,indices__] :=
    (
        Message[General::deprecation0,"indexize"];
        indexizeKernel[var,indices]
    );


indexify[var:_Symbol|_String,indices__] :=
    (
        Message[General::deprecation0,"indexify"];
        Map[indexizeKernel[var,#]&,{indices}]//Apply[Sequence]
    );

indexify[varList:{(_Symbol|_String)..},indices__] :=
    (
        Message[General::deprecation0,"indexify"];
        Outer[indexizeKernel,varList,{indices}]//Transpose//Flatten//Apply[Sequence]
    );


(* ::Subsubsection:: *)
(*Helper*)


indexizeKernel[var_String,index_] :=
    ToExpression[var<>indexToString[index]];

indexizeKernel[var_String,indices__] :=
    ToExpression@StringJoin[var,Map[indexToString,{indices}]];

indexizeKernel[var_Symbol,index__] :=
    indexizeKernel[ToString[var,FormatType->InputForm],index];


indexToString[Null] :=
    "";

indexToString[index_String] :=
    index;

indexToString[index_Symbol] :=
    SymbolName@index;

indexToString[index_Integer] :=
    ToString@index;


(* ::Subsection:: *)
(*indexJoin|indexSplit*)


(* ::Subsubsection:: *)
(*Option*)


indexJoinKernel//Options = {
    "IndexPosition"->Construct,
    "IndexType"->All
};

indexJoin//Options =
    Options@indexJoinKernel;


indexSplitKernel//Options = {
    "IndexPosition"->Construct,
    "IndexType"->All
};

indexSplit//Options =
    Options@indexSplitKernel;


(* ::Subsubsection:: *)
(*Message*)


indexJoin::optnotmatch =
    "the input options are not supported."

indexSplit::optnotmatch =
    "the input options are not supported."


(* ::Subsubsection:: *)
(*Main*)


indexJoin[vars__Symbol,opts:OptionsPattern[]][expr_] :=
    (
        Message[General::deprecation0,"indexJoin"];
        indexJoinKernel[{vars},FilterRules[{opts,Options@indexJoin},Options@indexJoinKernel]][expr]
    );

indexJoin[varList:{__Symbol},opts:OptionsPattern[]][expr_] :=
    (
        Message[General::deprecation0,"indexJoin"];
        indexJoinKernel[varList,FilterRules[{opts,Options@indexJoin},Options@indexJoinKernel]][expr]
    );


indexJoinKernel[varList_List,OptionsPattern[]][expr_] :=
    Module[ {varP,formatFunction,indexQFunction},
        If[ !MatchQ[OptionValue["IndexPosition"],$indexPositionP]||
            !MatchQ[OptionValue["IndexType"],$indexTypeP],
            Message[indexJoin::optnotmatch];
            Throw[expr]
        ];
        varP = Alternatives@@varList;
        formatFunction = OptionValue["IndexPosition"];
        indexQFunction = OptionValue["IndexType"];
        (*kernel*)
        Switch[formatFunction,
            Construct,
                expr//ReplaceAll[
                    var_Symbol[index_]/;MatchQ[var,varP]&&AtomQ[index]&&indexQ[indexQFunction][indexToString[index]]:>
                        RuleCondition@indexizeKernel[var,index]
                ],
            Subscript,
                expr//ReplaceAll[
                    Subscript[var_,index_]/;MatchQ[var,varP]&&AtomQ[index]&&indexQ[indexQFunction][indexToString[index]]:>
                        RuleCondition@indexizeKernel[var,index]
                ],
            Superscript,
                expr//ReplaceAll[
                    Superscript[var_,index_]/;MatchQ[var,varP]&&AtomQ[index]&&indexQ[indexQFunction][indexToString[index]]:>
                        RuleCondition@indexizeKernel[var,index]
                ]
        ]
    ]//Catch;


indexSplit[vars__Symbol,opts:OptionsPattern[]][expr_] :=
    (
        Message[General::deprecation0,"indexSplit"];
        indexSplitKernel[{vars},FilterRules[{opts,Options@indexSplit},Options@indexSplitKernel]][expr]
    );

indexSplit[varList:{__Symbol},opts:OptionsPattern[]][expr_] :=
    (
        Message[General::deprecation0,"indexSplit"];
        indexSplitKernel[varList,FilterRules[{opts,Options@indexSplit},Options@indexSplitKernel]][expr]
    );


indexSplitKernel[varList_List,OptionsPattern[]][expr_] :=
    Module[ {varP,formatFunction,indexQFunction},
        If[ !MatchQ[OptionValue["IndexPosition"],$indexPositionP]||
            !MatchQ[OptionValue["IndexType"],$indexTypeP],
            Message[indexSplit::optnotmatch];
            Throw[expr]
        ];
        varP = Alternatives@@Map[ToString[#,FormatType->InputForm]&,varList];
        formatFunction = OptionValue["IndexPosition"];
        indexQFunction = OptionValue["IndexType"];
        (*kernel*)
        expr//ReplaceAll[
            symbol_Symbol:>
                RuleCondition@symbolFromStringOrStringExpression@StringReplace[
                    ToString[symbol,FormatType->InputForm],
                    StartOfString~~Shortest[var__]~~Longest[index__]~~EndOfString/;StringMatchQ[var,varP]&&indexQ[indexQFunction][index]:>
                        formatFunction[ToExpression@var,ToExpression@index]
                ]
        ]
    ]//Catch;


(* ::Subsubsection:: *)
(*Helper*)


symbolFromStringOrStringExpression[expr_] :=
    If[ Head[expr]===String,
        ToExpression@expr,
        (*Else*)
        First@expr
    ];


indexQ[All][_] :=
    True;

indexQ["PositiveInteger"][str_] :=
    StringMatchQ[str,RegularExpression["^$|[1-9]\\d*"]];

indexQ["PositiveIntegerOrSingleLetter"][str_] :=
    StringMatchQ[str,RegularExpression["^$|[1-9]\\d*|[^\\W_]"]];

indexQ["PositiveIntegerOrGreekLetter"][str_] :=
    StringMatchQ[str,RegularExpression["^$|0|[1-9]\\d*|[\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Omicron]\[Pi]\[Rho]\[Sigma]\[Tau]\[Upsilon]\[CurlyPhi]\[Chi]\[Psi]\[Omega]]"]];

indexQ["NaturalNumber"][str_] :=
    StringMatchQ[str,RegularExpression["^$|0|[1-9]\\d*"]];

indexQ["NaturalNumberOrSingleLetter"][str_] :=
    StringMatchQ[str,RegularExpression["^$|0|[1-9]\\d*|[^\\W_]"]];

indexQ["NaturalNumberOrGreekLetter"][str_] :=
    StringMatchQ[str,RegularExpression["^$|0|[1-9]\\d*|[\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Kappa]\[Lambda]\[Mu]\[Nu]\[Xi]\[Omicron]\[Pi]\[Rho]\[Sigma]\[Tau]\[Upsilon]\[CurlyPhi]\[Chi]\[Psi]\[Omega]]"]];

indexQ[fun_Symbol][str_] :=
    fun[str];


(* ::Subsection:: *)
(*indexTo*)


(* ::Subsubsection:: *)
(*Main*)


indexToZero[heads__][indexs__] :=
    (
        Message[General::deprecation0,"indexToZero"];
        ReplaceAll[indexRulePrototype[(#[[1]]->0)&,padSymbolToRule[indexs],{heads}]]
    );


indexToEqual[heads__][rules__Rule] :=
    (
        Message[General::deprecation0,"indexToEqual"];
        ReplaceAll[indexRulePrototype[(#[[1]]->#[[2]])&,{rules},{heads}]]
    );


indexToDiff[heads__][rules__Rule] :=
    (
        Message[General::deprecation0,"indexToDiff"];
        ReplaceAll[indexRulePrototype[(#[[1]]->#[[2]]+#[[3]])&,{rules},{heads}]]
    );


indexToDiffZero[heads__][rules__Rule] :=
    (
        Message[General::deprecation0,"indexToDiffZero"];
        ReplaceAll[indexRulePrototype[{#[[1]]->#[[3]],#[[2]]->0}&,{rules},{heads}]]
    );


indexToDiffBack[heads__][rules__Rule] :=
    (
        Message[General::deprecation0,"indexToDiffBack"];
        ReplaceAll[indexRulePrototype[(#[[3]]->#[[1]]-#[[2]])&,{rules},{heads}]]
    );


(* ::Subsubsection:: *)
(*Helper*)


indexRulePrototype::usage =
    "generate rules from prototype function.";

indexRulePrototype[proto_,position_,ruleList_List,headList_List] :=
    Flatten@Outer[indexRulePrototypeSingle[proto,position,#1,#2]&,ruleList,headList];


indexRulePrototypeSingle[proto_,position_,rule_Rule,head_Symbol] :=
    proto@indexize2[position,head,extractIndexFromRule[rule]];


(*extract indices and difference of indices from rule.*)
(*e.g. 1->2 returns {1,2,12}.*)

extractIndexFromRule[rule_Rule] :=
    {Sequence@@rule,StringExpression@@ToString/@rule};


(*pad symbols to identical rules, used in indexToZero.*)

padSymbolToRule[heads__] :=
    Map[#->#&,{heads}];


(*indexize symbols with indices.*)

indexize2[type_][head_,indexList_List] :=
    Map[ToExpression[ToString[head,FormatType->InputForm]<>indexToString@#]&,indexList];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
