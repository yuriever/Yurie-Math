(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Diff`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


jacobianMatrix::usage =
    "jacobianMatrix.";

jacobianDet::usage =
    "jacobianDet.";


diffComm::usage =
    "diffComm[X,Y]=-(X[Y[#]]-Y[X[#]])&.";


diffChange::usage =
    "diffChange[expr,transformations,oldVars,newVars,functions] \n"<>
    "diffChange[] gives the example.";


integrateChange::usage =
    "integrateChange[expr,transformations,oldVars,newVars] \n"<>
    "integrateChange[] gives the example.";


IBP::usage =
    "integration by parts.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


cleanSolve//Options = {
    "Solution"->1,
    "ShowSolution"->False
};

getFunctionRuleList//Options =
    Options@cleanSolve;

diffChange//Options =
    Options@getFunctionRuleList;

integrateChange//Options =
    Options@cleanSolve;


(* ::Subsubsection:: *)
(*Jacobian*)


jacobianMatrix[f_List,x_List] :=
    Outer[D,f,x];


jacobianDet[f_List,x_List]/;Length[f]==Length[x] :=
    Det@Outer[D,f,x];


(* ::Subsubsection:: *)
(*diffComm*)


diffComm[x_,y_] :=
    -(x[y[#]]-y[x[#]])&;


(* ::Subsubsection:: *)
(*diffChange*)


diffChange[eqList_List,oldList_List,newList_List,funList_List,opts:OptionsPattern[]][expr_] :=
    diffChangeKernel[expr,eqList,oldList,newList,funList,opts];

diffChange[expr_,eqList_List,oldList_List,newList_List,funList_List,opts:OptionsPattern[]] :=
    diffChangeKernel[expr,eqList,oldList,newList,funList,opts];


diffChangeKernel[expr_,eqList1:{(_Equal|_Rule|_RuleDelayed)..},oldList_List,newList_List,funList_List,opts:OptionsPattern[diffChange]] :=
    Module[ {eqList},
        eqList =
            eqList1//ReplaceAll[Rule|RuleDelayed->Equal];
        expr//ReplaceAll[getFunctionRuleList[eqList,oldList,newList,funList,FilterRules[{opts,Options@diffChange},Options@getFunctionRuleList]]]//
            (*convert old variables to new ones.*)
            ReplaceAll[cleanSolve[eqList,oldList,FilterRules[{opts,Options@diffChange},Options@cleanSolve]]]//
                diffChangeStripList
    ]//Catch;


diffChange[] :=
    CellPrint@{
        ExpressionCell[
            ToExpression[
                "diffChange[D[f[x,t],{t,2}]==c^2*D[f[x,t],{x,2}],{u==x+c*t,v==x-c*t},{x,t},{u,v},{f[x,t]}]//Simplify",
                 StandardForm,
                 Defer
            ],
            "Code"
        ],
        ExpressionCell[
            ToExpression[
                "c Derivative[1,1][f][u,v]==0",
                StandardForm,
                Defer
            ],
            "Output"
        ]
    };


(* ::Subsubsection:: *)
(*integrateChange*)


integrateChange[eqList_List,oldList_List,newList_List,opts:OptionsPattern[]][expr_] :=
    integrateChangeKernel[expr,eqList,oldList,newList,opts];

integrateChange[expr_,eqList_List,oldList_List,newList_List,opts:OptionsPattern[]] :=
    integrateChangeKernel[expr,eqList,oldList,newList,opts];

integrateChange[eqList_List,oldList_List,newList_List,signOfJacobian:-1|1,opts:OptionsPattern[]][expr_] :=
    signOfJacobian*integrateChangeKernel[expr,eqList,oldList,newList,opts];

integrateChange[expr_,eqList_List,oldList_List,newList_List,signOfJacobian:-1|1,opts:OptionsPattern[]] :=
    signOfJacobian*integrateChangeKernel[expr,eqList,oldList,newList,opts];


integrateChangeKernel[expr_,eqList1:{(_Equal|_Rule|_RuleDelayed)..},oldList_List,newList_List,opts:OptionsPattern[integrateChange]] :=
    Module[ {oldToNew,oldToNewList,eqList},
        eqList =
            eqList1//ReplaceAll[Rule|RuleDelayed->Equal];
        oldToNewList =
            cleanSolve[eqList,oldList,FilterRules[{opts,Options@integrateChange},Options@cleanSolve]];
        Table[
            ReplaceAll[expr,oldToNew]*Det@Outer[D,ReplaceAll[oldList,oldToNew],newList],
            {oldToNew,oldToNewList}
        ]//dealPDAndINT[INT,oldList,newList]//integrateChangeStripList
    ]//Catch;


integrateChange[] :=
    CellPrint@{
        ExpressionCell[
            ToExpression[
                "integrateChange[t^a,{t==1-x},{t},{x}]",
                 StandardForm,
                 Defer
            ],
            "Code"
        ],
        ExpressionCell[
            ToExpression[
                "-(1-x)^a",
                StandardForm,
                Defer
            ],
            "Output"
        ]
    };


(* ::Subsubsection:: *)
(*IBP*)


IBP[fun_Symbol][expr_] :=
    expr//Expand//ReplaceAll[IBPRule[fun]];

IBP[fun_Symbol,variables__Symbol][expr_] :=
    expr//Expand//ReplaceAll[IBPRule[fun,variables]];


(* ::Subsection:: *)
(*Helper*)


(* ::Subsubsection:: *)
(*diffChange*)


getFunctionRuleList[eqList_List,oldList_List,newList_List,funList_List,opts:OptionsPattern[]] :=
    Module[ {newByOld,newByOldList,fun,head},
        newByOldList =
            newList//ReplaceAll[cleanSolve[eqList,newList,FilterRules[{opts,Options@getFunctionRuleList},Options@cleanSolve]]];
        Table[
            Table[
                Head[fun]->head[
                    Apply[List,fun],
                    ReplacePart[fun,Thread[getVarPositionList[fun,oldList]->newByOld]]
                ]
                ,
                {fun,funList}
            ],
            {newByOld,newByOldList}
        ]//ReplaceAll[head->Function]
    ];


getVarPositionList[fun_,variableList_] :=
    Flatten[Map[Position[fun,#]&,variableList],{{1},{2,3}}];


cleanSolve::argx =
    "the option \"Solution\" accepts a valid Part operation on the solution list."

cleanSolve::nosoln =
    "no solution found.";

cleanSolve[eqList_,varList_,opts:OptionsPattern[]] :=
    Module[ {solutionList,solution},
        solutionList =
            Solve[eqList,varList]//Normal//ReplaceAll[C[_]->0];
        solution =
            (* Throw exception if Part operation is invalid. *)
            Quiet[
                Check[
                    solutionList[[OptionValue["Solution"]]],
                    Message[cleanSolve::argx];
                    Throw[solutionList],
                    {Part::pkspec1,Part::partw}
                ],
                {Part::pkspec1,Part::partw}
            ];
        solution =
            (* If solution is a list of rules, convert it to a list of lists. *)
            (* Throw exception if solution is empty. *)
            Which[
                MatchQ[solution,{__Rule}],
                    {solution},
                MatchQ[solution,{__List}],
                    solution,
                solution==={},
                    Message[cleanSolve::nosoln];
                    Throw[solution]
            ];
        If[ OptionValue["ShowSolution"]===True,
            Echo@Column@solution
        ];
        solution
    ];


dealPDAndINT[head_,oldList_,newList_][expr_] :=
    With[ {rule = MapThread[Rule,{oldList,newList}]},
        expr//ReplaceAll[{
            head[args__]:>head@@ReplaceAll[{args},rule]
        }]
    ];


diffChangeStripList[list_] :=
    If[ Length[list]===1,
        list[[1,1]],
        (*Else*)
        Flatten[list,1]
    ];


(* ::Subsubsection:: *)
(*integrateChange*)


integrateChangeStripList[list_] :=
    If[ Length[list]===1,
        list[[1]],
        (*Else*)
        list
    ];


(* ::Subsubsection:: *)
(*IBP*)


IBPRule[fun_] :=
    Longest[exprs___]*Derivative[orders__][fun][vars__]:>
        (-1)^Total[{orders}]*
            D[Times[exprs],Sequence@@cleanNumPairs@Transpose@{{vars},{orders}}]*
                fun[vars];

IBPRule[fun_,var_] :=
    Longest[exprs___]*Derivative[orders__][fun][vars__]:>
        With[ {orderOfVar = getOrderOfVar[{vars},{orders},var]},
            (-1)^orderOfVar*
                D[Times[exprs],{var,orderOfVar}]*
                    Derivative[dropOrderByVar[{vars},{orders},var]][fun][vars]
        ];

IBPRule[fun_,vars1__] :=
    Longest[exprs___]*Derivative[orders__][fun][vars__]:>
        With[ {orderList1 = getOrderOfVar[{vars},{orders},{vars1}]},
            (-1)^Total@getOrderOfVar[{vars},{orders},{vars1}]*
                D[Times[exprs],Sequence@@cleanNumPairs@Transpose@{{vars1},orderList1}]*
                    Derivative[dropOrderByVar[{vars},{orders},{vars1}]][fun][vars]
        ];


cleanNumPairs[varOrderPairList_] :=
    DeleteCases[varOrderPairList,{_Integer,_Integer}];


getOrderOfVar[varList_,orderList_,varOrItsList_] :=
    Lookup[
        AssociationThread[varList->orderList],
        varOrItsList
    ];


dropOrderByVar[varList_,orderList_,varOrItsList_] :=
    Sequence@@Lookup[
        Association[Thread[varList->orderList],Thread[varOrItsList->0]],
        varList
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
