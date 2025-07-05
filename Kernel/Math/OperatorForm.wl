(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Simplify`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Operator form*)


SS::usage =
    "OperatorForm: Simplify.";

FS::usage =
    "OperatorForm: FullSimplify.";

FE::usage =
    "OperatorForm: FunctionExpand.";

FES::usage =
    "OperatorForm: FunctionExpand + Simplify.";


AS::usage =
    "OperatorForm: Assuming.";

SSA::usage =
    "OperatorForm: Simplify + Assuming.";

FSA::usage =
    "OperatorForm: FullSimplify + Assuming.";

FEA::usage =
    "OperatorForm: FunctionExpand + Assuming.";

FESA::usage =
    "OperatorForm: FunctionExpand + Simplify + Assuming.";


modularize::usage =
    "modularize[scope[code, iterators]]: modularize the scoping construction (e.g. Table, Sum, and Integrate) such that the iterators are lexically scoped.";

block::usage =
    "OperatorForm: Block.";

with::usage =
    "OperatorForm: With.";

module::usage =
    "OperatorForm: Module.";


rep::usage =
    "rep[rules][expr]: operator form of ReplaceAll with the rules being flattened.";

repdeep::usage =
    "repdeep[rules][level][expr]: operator form of Replace with the rules being flattened."<>
    "\n"<>
    "Default[level]: All.";


part::usage =
    "OperatorForm: Part.";

plus::usage =
    "OperatorForm: Plus.";

minus::usage =
    "OperatorForm: Minus.";

times::usage =
    "OperatorForm: Times.";

divide::usage =
    "OperatorForm: Divide.";

series::usage =
    "OperatorForm: Series + Normal.";

limit::usage =
    "OperatorForm: Limit.";

solve::usage =
    "OperatorForm: Solve.";

solve1::usage =
    "OperatorForm: Solve + First.";

collect::usage =
    "OperatorForm: Collect.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Operator form*)


SS :=
    Simplify;


FS :=
    FullSimplify;


FE :=
    FunctionExpand;


FES :=
    FunctionExpand/*Simplify;


AS[assumption_] :=
    Function[expr,Assuming[assumption,expr],{HoldAll}];


SSA[assumption_][expr_] :=
    Simplify[expr,assumption];

SSA[assumption_,excludedFormList_List][expr_] :=
    Simplify[expr,assumption,ExcludedForms->excludedFormList];


FSA[assumption_][expr_] :=
    FullSimplify[expr,assumption];

FSA[assumption_,excludedFormList_List][expr_] :=
    FullSimplify[expr,assumption,ExcludedForms->excludedFormList];


FEA[assumption_][expr_] :=
    FunctionExpand[expr,assumption];


FESA[assumption_][expr_] :=
    Simplify[FunctionExpand[expr,assumption],assumption];

FESA[assumption_,excludedFormList_List][expr_] :=
    Simplify[FunctionExpand[expr,assumption],assumption,ExcludedForms->excludedFormList];


modularize//Attributes =
    {HoldAllComplete};

modularize[scope_[code_,iterators__]] :=
    Replace[
        DeleteCases[
            HoldComplete[iterators][[All,1]],
            Except[_Symbol]
        ],
        HoldComplete[args___]:>
            Module[ {args},
                scope[code,iterators]
            ]
    ];


block//Attributes =
    {HoldAllComplete};

block[localVarList_List] :=
    Function[
        expr,
        Block[ localVarList,
            expr
        ],
        {HoldAllComplete}
    ];


with//Attributes =
    {HoldAllComplete};

with[localVarLists___List] :=
    Function[
        expr,
        Replace[
            HoldComplete[localVarLists],
            HoldComplete[args___]:>
                With[ args,
                    expr
                ]
        ],
        {HoldAllComplete}
    ];


module//Attributes =
    {HoldAllComplete};

module[localVarList_List] :=
    Function[
        expr,
        Replace[
            HoldComplete[localVarList],
            HoldComplete[arg_]:>
                Module[ arg,
                    expr
                ]
        ],
        {HoldAllComplete}
    ];


rep[rules___][expr_] :=
    ReplaceAll[expr,Flatten[{rules}]];


repdeep[rules___][level_:All][expr_] :=
    Replace[expr,Flatten[{rules}],level];


part :=
    GeneralUtilities`Slice;


plus[args___][expr_] :=
    Plus[expr,args];


minus[args___][expr_] :=
    Subtract[expr,Plus[args]];


times[args___][expr_] :=
    Times[expr,args];


divide[args___][expr_] :=
    Divide[expr,Times[args]];


series[args___][expr_] :=
    Normal@Series[expr,args];


limit[direction_][expr_] :=
    Limit[expr,direction];


solve[args___][expr_] :=
    Solve[expr,args];

solve1[args___][expr_] :=
    First[Solve[expr,args],{}];


collect[var_][expr_] :=
    Collect[expr,var];

collect[var_,head_][expr_] :=
    Collect[expr,var,head];

collect[var_,head_,head2_][expr_] :=
    Collect[expr,var,head,head2];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
