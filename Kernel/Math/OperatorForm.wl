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
    "Sketch: Simplify.";

FS::usage =
    "Sketch: FullSimplify.";

FE::usage =
    "Sketch: FunctionExpand.";

FES::usage =
    "Sketch: FunctionExpand + Simplify.";


AS::usage =
    "Sketch: Assuming.";

SSA::usage =
    "Sketch: Simplify + Assuming.";

FSA::usage =
    "Sketch: FullSimplify + Assuming.";

FEA::usage =
    "Sketch: FunctionExpand + Assuming.";

FESA::usage =
    "Sketch: FunctionExpand + Simplify + Assuming.";


modularize::usage =
    "modularize[scope[code, iterators]]: modularize the scoping construction (e.g. Table, Sum, and Integrate) such that the iterators are lexically scoped.";

block::usage =
    "Sketch: Block.";

with::usage =
    "Sketch: With.";

module::usage =
    "Sketch: Module.";


rep::usage =
    "rep[rules][expr]: operator form of ReplaceAll with the rules being flattened.";

repdeep::usage =
    "repdeep[rules][level][expr]: operator form of Replace with the rules being flattened."<>
    "\n"<>
    "Default[level]: All.";


part::usage =
    "Sketch: Part.";

plus::usage =
    "Sketch: Plus.";

minus::usage =
    "Sketch: Minus.";

times::usage =
    "Sketch: Times.";

divide::usage =
    "Sketch: Divide.";

series::usage =
    "Sketch: Series + Normal.";

limit::usage =
    "Sketch: Limit.";

solve::usage =
    "Sketch: Solve.";

solve1::usage =
    "Sketch: Solve + First.";

collect::usage =
    "Sketch: Collect.";


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
