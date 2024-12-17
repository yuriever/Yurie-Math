(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Diff`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


PD::usage =
    "head of partial derivative that acts on the rest of the expression."

INT::usage =
    "head of integral that acts on the rest of the expression."


PDCoefficient::usage =
    "collect the coefficients of PD[___].";


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


cleanSolve//Options =
    {"FirstSolution"->True};

getFunctionRuleList//Options =
    Options@cleanSolve;

diffChange//Options =
    Options@getFunctionRuleList;

integrateChange//Options =
    Options@cleanSolve;


(* ::Subsection:: *)
(*Message*)


PDCoefficient::nonlinear =
    "the expression is nonlinear with respect to PD[__]."


(* ::Subsection:: *)
(*Main*)


(* ::Subsubsection:: *)
(*PD|INT*)


PD//Attributes =
    {Orderless};

pd_PD/;System`Private`HoldNotValidQ[pd] :=
    (
        System`Private`HoldSetValid[pd];
        System`Private`HoldSetNoEntry[pd]
    );


PD/:PD[x__]PD[y__]:=
    PD[x,y];

PD/:Power[PD[x__],n_Integer]/;n>=2:=
    PD@@Flatten@ConstantArray[{x},n];

PD[] :=
    1;


INT//Attributes =
    {Orderless};

int_INT/;System`Private`HoldNotValidQ[int] :=
    (
        System`Private`HoldSetValid[int];
        System`Private`HoldSetNoEntry[int]
    );


INT/:INT[x__]INT[y__]:=
    INT[x,y];

INT/:Power[INT[x__],n_Integer]/;n>=2:=
    INT@@Flatten@ConstantArray[{x},n];

INT[] :=
    1;


(* ::Subsubsection:: *)
(*PDCoefficient*)


PDCoefficient[expr_] :=
    Module[ {expr1},
        If[ !PDLinearQ[expr],
            Message[PDCoefficient::nonlinear];
            Throw[expr]
        ];
        expr1 = Expand[expr];
        Join[
            Cases[expr1,PD[x__]*rest_.:>{{x},rest}],
            Cases[expr1,rest_/;FreeQ[rest,_PD]:>{{},rest}]
        ]//GatherBy[#,First]&//Map[Rule[#[[1,1]],Total[#[[All,2]]]]&]
    ]//Catch;


PDLinearQ[expr_] :=
    AllTrue[Cases[expr,_PD,All],Internal`LinearQ[expr,#]&];


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


diffChange[expr_,eqList:{__Equal},oldList_List,newList_List,funList_List,opts:OptionsPattern[]] :=
    expr//ReplaceAll[getFunctionRuleList[eqList,oldList,newList,funList,FilterRules[{opts,Options@diffChange},Options@getFunctionRuleList]]]//
        (*convert old variables to new ones.*)
        ReplaceAll[cleanSolve[eqList,oldList,FilterRules[{opts,Options@diffChange},Options@cleanSolve]]]//
            diffChangeStripList;

diffChange[expr_,eqList:{__Rule},oldList_List,newList_List,funList_List,opts:OptionsPattern[]] :=
    diffChange[expr,ReplaceAll[eqList,Rule->Equal],oldList,newList,funList,opts];

diffChange[eqList_List,oldList_List,newList_List,funList_List,opts:OptionsPattern[]][expr_] :=
    diffChange[expr,eqList,oldList,newList,funList,opts];


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


integrateChange[expr_,eqList:{__Equal},oldList_List,newList_List,opts:OptionsPattern[]] :=
    Module[ {oldToNew,oldToNewList},
        oldToNewList =
            cleanSolve[eqList,oldList,FilterRules[{opts,Options@integrateChange},Options@cleanSolve]];
        Table[
            ReplaceAll[expr,oldToNew]*Det@Outer[D,ReplaceAll[oldList,oldToNew],newList],
            {oldToNew,oldToNewList}
        ]//dealPDAndINT[INT,oldList,newList]//integrateChangeStripList
    ];

integrateChange[expr_,eqList:{__Rule},oldList_List,newList_List,opts:OptionsPattern[]] :=
    integrateChange[expr,ReplaceAll[eqList,Rule->Equal],oldList,newList,opts];

integrateChange[eqList_List,oldList_List,newList_List,opts:OptionsPattern[]][expr_] :=
    integrateChange[expr,eqList,oldList,newList,opts];


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


cleanSolve[eqList_,varList_,opts:OptionsPattern[]] :=
    If[ OptionValue["FirstSolution"]===True,
        Solve[eqList,varList]//Take[#,1]&//Normal//ReplaceAll[C[_]->0],
        (*Else*)
        Solve[eqList,varList]//Normal//ReplaceAll[C[_]->0]
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
