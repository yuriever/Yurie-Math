(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Diff`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Atomic head*)


PD::usage =
    "head of partial derivative."

INT::usage =
    "head of integral."

SUM::usage =
    "head of sum."


(* ::Subsection:: *)
(*Operator form*)


integrate::usage =
    "operator form of Integrate.";

summation::usage =
    "operator form of Sum.";


(* ::Subsection:: *)
(*Variable change*)


diffChange::usage =
    "change variables in differential equations.";

integrateChange::usage =
    "change variables in integrals.";


(* ::Subsection:: *)
(*IBP*)


IBP::usage =
    "perform integration by parts.";


(* ::Subsection:: *)
(*Utility*)


jacobianMatrix::usage =
    "Jacobian matrix.";

jacobianDet::usage =
    "Jacobian determinant.";


PDCoefficient::usage =
    "extract the coefficients of PD[__].";

PDCollect::usage =
    "collect the terms with respect to PD[__].";


diffComm::usage =
    "diffComm[X,Y]=-(X[Y[#]]-Y[X[#]])&.";

diffCoefficient::usage =
    "extract the coefficients of Derivative[__][_][__].";

diffCollect::usage =
    "collect the terms with respect to Derivative[__][_][__].";

diffReplace::usage =
    "replace the derivatives of the function.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Atomic head*)


(* ::Subsubsection:: *)
(*Message*)


INT::Duplicate =
    "the original expression contains duplicate integral(s) with respect to ``."

SUM::Duplicate =
    "the original expression contains duplicate sum(s) with respect to ``.";


(* ::Subsubsection:: *)
(*PD*)


PD//Attributes =
    {Orderless};

head_PD/;System`Private`HoldNotValidQ[head] :=
    (
        System`Private`HoldSetValid[head];
        System`Private`HoldSetNoEntry[head]
    );


PD/:PD[x__]PD[y__]:=
    PD[x,y];

PD/:Power[PD[x__],n_Integer]/;n>=1:=
    PD@@Flatten@ConstantArray[{x},n];

PD/:PD[x__]Power[PD[y_,rest___],-1]/;MemberQ[{x},y]:=
    PD@@DeleteCases[{x},y,{1},1]/PD[rest];

PD[] :=
    1;


(* ::Subsubsection:: *)
(*INT*)


INT//Attributes =
    {Orderless};

head_INT/;System`Private`HoldNotValidQ[head] :=
    (
        Quiet[
            System`Private`HoldSetValid[head],
            {INT::Duplicate}
        ];
        System`Private`HoldSetNoEntry[head]
    );


INT/:INT[x__]INT[y__]:=
    INT[x,y];

HoldPattern[INT][x__]/;!DuplicateFreeQ[{x}] :=
    (
        Message[
            INT::Duplicate,
            Row[Select[Tally[{x}],Last[#]>=2&][[All,1]],","]
        ];
        INT@@DeleteDuplicates[{x}]
    );

INT/:INT[x__]Power[INT[y_,rest___],-1]/;MemberQ[{x},y]:=
    INT@@DeleteCases[{x},y,{1},1]/INT[rest];

INT[] :=
    1;


(* ::Subsubsection:: *)
(*SUM*)


SUM//Attributes =
    {Orderless};

head_SUM/;System`Private`HoldNotValidQ[head] :=
    (
        Quiet[
            System`Private`HoldSetValid[head],
            {SUM::Duplicate}
        ];
        System`Private`HoldSetNoEntry[head]
    );


SUM/:SUM[x__]SUM[y__]:=
    SUM[x,y];

HoldPattern[SUM][x__]/;!DuplicateFreeQ[{x}] :=
    (
        Message[
            SUM::Duplicate,
            Row[Select[Tally[{x}],Last[#]>=2&][[All,1]],","]
        ];
        SUM@@DeleteDuplicates[{x}]
    );

SUM/:SUM[x__]Power[SUM[y_,rest___],-1]/;MemberQ[{x},y]:=
    SUM@@DeleteCases[{x},y,{1},1]/SUM[rest];

SUM[] :=
    1;


(* ::Subsection:: *)
(*Operator form*)


(* ::Subsubsection:: *)
(*Option*)


integrate//Options =
    Options@Integrate;

SetOptions[integrate,GenerateConditions->False];


summation//Options =
    Options@Sum;

SetOptions[summation,GenerateConditions->False];


(* ::Subsubsection:: *)
(*Main*)


integrate[args__List,opts:OptionsPattern[]][expr_]/;FreeQ[expr,_INT] :=
    Integrate[expr,args,FilterRules[{opts,Options@integrate},Options@Integrate]];

integrate[args__List,opts:OptionsPattern[]][expr_Times]/;!FreeQ[expr,_INT] :=
    With[ {
        expr1 = Cases[expr,Except[_INT],1]//Apply[Times],
        int = DeleteCases[FirstCase[expr,INT[vars__]:>{vars},{}],Alternatives@@Cases[{args},{var_,__}:>var]],
        fopts = Sequence@@FilterRules[{opts,Options@integrate},Options@Integrate]
    },
        Integrate[expr1,args,fopts]*INT@@int
    ];


summation[args__List,opts:OptionsPattern[]][expr_]/;FreeQ[expr,_SUM] :=
    With[ {
        fopts = Sequence@@FilterRules[{opts,Options@summation},Options@Sum]
    },
        Sum[expr,args,fopts]
    ];

summation[args__List,opts:OptionsPattern[]][expr_Times]/;!FreeQ[expr,_SUM] :=
    With[ {
        expr1 = Cases[expr,Except[_SUM],1]//Apply[Times],
        sum = DeleteCases[FirstCase[expr,SUM[vars__]:>{vars},{}],Alternatives@@Cases[{args},{var_,__}:>var]],
        fopts = Sequence@@FilterRules[{opts,Options@summation},Options@Sum]
    },
        Sum[expr1,args,fopts]*SUM@@sum
    ];


(* ::Subsection:: *)
(*Variable change*)


(* ::Subsubsection:: *)
(*Option*)


diffChange//Options = {
    "Solution"->1,
    "ShowSolution"->False
};

integrateChange//Options = {
    "Solution"->1,
    "ShowSolution"->False
};


(* ::Subsubsection:: *)
(*Main*)


diffChange[eqList_List,oldList_List,newList_List,funList_List,opts:OptionsPattern[]][expr_] :=
    diffChangeKernel[expr,eqList,oldList,newList,funList,opts];

diffChange[expr_,eqList_List,oldList_List,newList_List,funList_List,opts:OptionsPattern[]] :=
    diffChangeKernel[expr,eqList,oldList,newList,funList,opts];


integrateChange[eqList_List,oldList_List,newList_List,opts:OptionsPattern[]][expr_] :=
    integrateChangeKernel[expr,eqList,oldList,newList,opts];

integrateChange[expr_,eqList_List,oldList_List,newList_List,opts:OptionsPattern[]] :=
    integrateChangeKernel[expr,eqList,oldList,newList,opts];

integrateChange[eqList_List,oldList_List,newList_List,signOfJacobian:-1|1,opts:OptionsPattern[]][expr_] :=
    signOfJacobian*integrateChangeKernel[expr,eqList,oldList,newList,opts];

integrateChange[expr_,eqList_List,oldList_List,newList_List,signOfJacobian:-1|1,opts:OptionsPattern[]] :=
    signOfJacobian*integrateChangeKernel[expr,eqList,oldList,newList,opts];


(* ::Subsubsection:: *)
(*Helper*)


diffChangeKernel[expr_,eqList:{(_Equal|_Rule|_RuleDelayed)..},oldList_List,newList_List,funList_List,opts:OptionsPattern[diffChange]] :=
    With[ {
            eqList1 = eqList//ReplaceAll[Rule|RuleDelayed->Equal],
            whichSolution = OptionValue[diffChange,opts,"Solution"],
            ifShowSolution = OptionValue[diffChange,opts,"ShowSolution"]
        },
        expr//
            ReplaceAll[getFunctionRuleList[whichSolution][eqList1,oldList,newList,funList]]//
            (*convert old variables to new ones.*)
            ReplaceAll[cleanSolve[whichSolution,ifShowSolution][eqList1,oldList]]//
            diffChangeStripList
    ]//Catch;


integrateChangeKernel[expr_,eqList:{(_Equal|_Rule|_RuleDelayed)..},oldList_List,newList_List,opts:OptionsPattern[integrateChange]] :=
    Module[ {
            oldToNew,oldToNewList,res,
            eqList1 = eqList//ReplaceAll[Rule|RuleDelayed->Equal],
            whichSolution = OptionValue[integrateChange,opts,"Solution"],
            ifShowSolution = OptionValue[integrateChange,opts,"ShowSolution"]
        },
        oldToNewList =
            cleanSolve[whichSolution,ifShowSolution][eqList1,oldList];
        res =
        Table[
            ReplaceAll[expr,oldToNew]*Det@Outer[D,ReplaceAll[oldList,oldToNew],newList],
            {oldToNew,oldToNewList}
        ];
        res//integrateChangeConvertINT[oldList,newList]//integrateChangeStripList
    ]//Catch;


cleanSolve::argx =
    "the option \"Solution\" accepts a valid Part operation on the solution list."

cleanSolve::nosoln =
    "no solution found.";

cleanSolve[whichSolution_,ifShowSolution_][eqList_,varList_] :=
    Module[ {solutionList,solution},
        solutionList =
            Solve[eqList,varList]//Normal//ReplaceAll[C[_]->0];
        solution =
            (* Throw exception if Part operation is invalid. *)
            Quiet[
                Check[
                    solutionList[[whichSolution]],
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
        If[ ifShowSolution===True,
            Print@Column@solution
        ];
        solution
    ];


getFunctionRuleList[whichSolution_][eqList_List,oldList_List,newList_List,funList_List] :=
    Module[ {newByOld,newByOldList,fun,head},
        newByOldList =
            newList//ReplaceAll[cleanSolve[whichSolution,False][eqList,newList]];
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


integrateChangeConvertINT[oldList_,newList_][expr_] :=
    With[ {rule = MapThread[Rule,{oldList,newList}]},
        expr//ReplaceAll[{
            INT[args__]:>INT@@ReplaceAll[{args},rule]
        }]
    ];


diffChangeStripList[list_] :=
    If[ Length[list]===1,
        list[[1,1]],
        (*Else*)
        Flatten[list,1]
    ];


integrateChangeStripList[list_] :=
    If[ Length[list]===1,
        list[[1]],
        (*Else*)
        list
    ];


(* ::Subsubsection:: *)
(*Demo*)


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


(* ::Subsection:: *)
(*IBP*)


(* ::Subsubsection:: *)
(*Main*)


IBP[fun_][expr_] :=
    expr//Expand//ReplaceAll[IBPRule[fun]];

IBP[fun_,vars__][expr_] :=
    expr//Expand//ReplaceAll[IBPRule[fun,vars]];


(* ::Subsubsection:: *)
(*Helper*)


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
        varOrItsList,
        (* if the variable is not present in the variable list, return 0. *)
        0
    ];


dropOrderByVar[varList_,orderList_,varOrItsList_] :=
    Sequence@@Lookup[
        Association[Thread[varList->orderList],Thread[varOrItsList->0]],
        varList
    ];


(* ::Subsection:: *)
(*Utility*)


(* ::Subsubsection:: *)
(*jacobianMatrix|jacobianDet*)


jacobianMatrix[f_List,x_List] :=
    Outer[D,f,x];


jacobianDet[f_List,x_List]/;Length[f]==Length[x] :=
    Det@Outer[D,f,x];


(* ::Subsubsection:: *)
(*PDCoefficient*)


PDCoefficient::nonlinear =
    "the expression is nonlinear with respect to PD[__]."

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
(*diffComm*)


diffComm[x_,y_] :=
    -(x[y[#]]-y[x[#]])&;


(* ::Subsubsection:: *)
(*diffCollect*)


diffCollect[var:Except[_List],operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][var][___],operation];

diffCollect[varList_List,operation_:Identity][expr_] :=
    Collect[expr,Derivative[___][#][___]&/@varList,operation];


(* ::Subsubsection:: *)
(*diffReplace*)


diffReplace[rules_] :=
    ReplaceAll[getDiffReplaceRule[rules]];


getDiffReplaceRule[Rule[f_,rhs_]] :=
    {
        f[___]:>rhs,
        Derivative[orders__][f][vars__]:>
            D[rhs,Sequence@@cleanNumPairs@Transpose@{{vars},{orders}}]
    };

getDiffReplaceRule[ruleList_List] :=
    ruleList//Map[getDiffReplaceRule]//Flatten;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
