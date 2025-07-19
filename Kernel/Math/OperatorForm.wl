(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Simplify`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Simplify*)


SS::usage =
    "Sketch: Simplify.";

FS::usage =
    "Sketch: FullSimplify.";

FE::usage =
    "Sketch: FunctionExpand.";

FES::usage =
    "Sketch: FunctionExpand + Simplify.";


(* ::Subsection:: *)
(*Assuming*)


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


(* ::Subsection:: *)
(*Module*)


modularize::usage =
    "modularize[scope[code, iterators]]: modularize the scoping construction (e.g. Table, Sum, and Integrate) such that the iterators are lexically scoped.";

block::usage =
    "Sketch: Block.";

with::usage =
    "Sketch: With.";

module::usage =
    "Sketch: Module.";


(* ::Subsection:: *)
(*ReplaceAll*)


rep::usage =
    "rep[rules][expr]: operator form of ReplaceAll with the rules being flattened.";

repdeep::usage =
    "repdeep[rules][level][expr]: operator form of Replace with the rules being flattened."<>
    "\n"<>
    "Default[level]: All.";


(* ::Subsection:: *)
(*Part*)


part::usage =
    "Sketch: Part.";


(* ::Subsection:: *)
(*Plus|Subtract|Times|Divide*)


plus::usage =
    "Sketch: Plus.";

minus::usage =
    "Sketch: Minus.";

times::usage =
    "Sketch: Times.";

divide::usage =
    "Sketch: Divide.";

timesOverPlus::usage =
    "timesOverPlus[args][expr]: operator form of Times that automatically threads over Plus.";

divideOverPlus::usage =
    "divideOverPlus[args][expr]: operator form of Divide that automatically threads over Plus.";


(* ::Subsection:: *)
(*Series|Limit*)


series::usage =
    "Sketch: Series + Normal.";

limit::usage =
    "Sketch: Limit.";


(* ::Subsection:: *)
(*Solve*)


solve::usage =
    "Sketch: Solve + Part.";

solve1::usage =
    "Sketch: Solve + First.";

solveKernel;


(* ::Subsection:: *)
(*Collect*)


collect::usage =
    "Sketch: Collect.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Simplify*)


SS :=
    Simplify;


FS :=
    FullSimplify;


FE :=
    FunctionExpand;


FES :=
    FunctionExpand/*Simplify;


(* ::Subsection:: *)
(*Assuming*)


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


(* ::Subsection:: *)
(*Module*)


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


(* ::Subsection:: *)
(*ReplaceAll*)


rep[rules___][expr_] :=
    ReplaceAll[expr,Flatten[{rules}]];


repdeep[rules___][level_:All][expr_] :=
    Replace[expr,Flatten[{rules}],level];


(* ::Subsection:: *)
(*Part*)


part :=
    GeneralUtilities`Slice;


(* ::Subsection:: *)
(*Plus|Subtract|Times|Divide*)


plus[args___][expr_] :=
    Plus[expr,args];


minus[args___][expr_] :=
    Subtract[expr,Plus[args]];


times[args___][expr_] :=
    Times[expr,args];


divide[args___][expr_] :=
    Divide[expr,Times[args]];


timesOverPlus[args__][expr_Plus] :=
    Map[Times[#,args]&,expr];

timesOverPlus[args__][expr_] :=
    Times[expr,args];


divideOverPlus[args__][expr_Plus] :=
    Map[Divide[#,Times[args]]&,expr];

divideOverPlus[args__][expr_] :=
    Divide[expr,Times[args]];


(* ::Subsection:: *)
(*Series|Limit*)


series[args___][expr_] :=
    Normal@Series[expr,args];


limit[direction_][expr_] :=
    Limit[expr,direction];


(* ::Subsection:: *)
(*Solve*)


solve::InvalidSolutionChoice =
    "The choice of the solution is invalid."

solve::InvalidEquation =
    "The equations should be a list of expressions with the following heads: Equal|Rule|RuleDelayed."

solve::NoSolution =
    "No solution is found.";

solve::UnknownError =
    "An unknown error occurred while solving the equations.";


solve[vars_][equations_] :=
    solveKernel[prepareList[vars],All,False][prepareList[equations]]//Catch;

solve[vars_,whichSolution_][equations_] :=
    solveKernel[prepareList[vars],whichSolution,False][prepareList[equations]]//Catch;

solve1[vars_][equations_] :=
    solveKernel[prepareList[vars],1,False][prepareList[equations]]//Catch;


solveKernel[varList_,whichSolution_,ifListize_][eqList1_] :=
    Module[ {eqList,solution},
        eqList =
            (* Throw exception if the equation list is invalid. *)
            If[ MatchQ[eqList1,{(_Equal|_Rule|_RuleDelayed)..}],
                eqList1//ReplaceAll[Rule|RuleDelayed->Equal],
                (*Else*)
                Message[solve::InvalidEquation];
                Throw[eqList1]
            ];
        solution =
            Solve[eqList,varList]//Normal//ReplaceAll[C[_]->0];
        (* Throw exception if solution is empty. *)
        If[ solution==={},
            Message[solve::NoSolution];
            Throw[{}]
        ];
        solution =
            (* Throw exception if Part operation is invalid. *)
            Quiet[
                Check[
                    solution[[whichSolution]],
                    Message[solve::InvalidSolutionChoice];
                    Throw[solution],
                    {Part::pkspec1,Part::partw}
                ],
                {Part::pkspec1,Part::partw}
            ];
        If[ ifListize===True&&MatchQ[solution,{__Rule}],
            {solution},
            (*Else*)
            solution
        ]
    ];


prepareList[list_List] :=
    Flatten[list];

prepareList[assoc_Association] :=
    Normal[assoc];

prepareList[other_] :=
    Flatten[{other}];


(* ::Subsection:: *)
(*Collect*)


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
