(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Diff`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Simplify`"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Atomic head*)


PD::usage =
    "PD[vars]: head of partial derivative.";

INT::usage =
    "INT[vars]: head of integration.";

SUM::usage =
    "SUM[vars]: head of summation.";


(* ::Subsection:: *)
(*Operator form*)


integration::usage =
    "integration[args][expr]: operator form of Integrate."<>
    "\n"<>
    "Default[GenerateConditions]: False.";

summation::usage =
    "summation[args][expr]: operator form of Sum."<>
    "\n"<>
    "Default[GenerateConditions]: False.";


(* ::Subsection:: *)
(*Variable change*)


integrationChange::usage =
    "integrationChange[equations, oldVars, newVars, signs][expr]: change variables in integrals."<>
    "\n"<>
    "Info[signs]: Jacobian signs.";


diffChange::usage =
    "diffChange[equations, oldVars, newVars, funs][expr]: change variables in differential equations."<>
    "\n"<>
    "Info[funs]: list of functions to transform.";


(* ::Subsection:: *)
(*IBP*)


IBP::usage =
    "IBP[fun][expr]: perform integration by parts."<>
    "\n"<>
    "IBP[fun, vars][expr]: perform integration by parts with respect to specific variables.";


(* ::Subsection:: *)
(*Utility*)


jacobianMatrix::usage =
    "jacobianMatrix[funList, varList]: Jacobian matrix.";

jacobianDet::usage =
    "jacobianDet[funList, varList]: Jacobian determinant.";


PDCoefficient::usage =
    "PDCoefficient[post, opts][expr]: extract the coefficients of PD[__]."<>
    "\n"<>
    "Info[post]: post-operation applied to the coefficients."<>
    "\n"<>
    "Default[post]: Identity.";

PDCollect::usage =
    "PDCollect[args][expr]: collect the terms with respect to PD[__]."<>
    "\n"<>
    "Info[args]: inherited from Collect.";


diffCoefficient::usage =
    "diffCoefficient[fun, post, opts][expr]: extract the coefficients of Derivative[__][_][__]."<>
    "\n"<>
    "Info[post]: post-operation applied to the coefficients."<>
    "\n"<>
    "Default[post]: Identity.";

diffCollect::usage =
    "diffCollect[fun, args][expr]: collect the terms with respect to Derivative[__][_][__]."<>
    "\n"<>
    "diffCollect[funList, args][expr]: collect terms for multiple functions."<>
    "\n"<>
    "Info[args]: inherited from Collect.";

diffReplace::usage =
    "diffReplace[fun->res...]: replace the derivatives of the function."<>
    "\n"<>
    "diffReplace[fun->res..., head]: prevent the evaluation of symbolic derivatives.";

diffComm::usage =
    "diffComm[X, Y]: compute the commutator of differential operators."<>
    "\n"<>
    "Sketch: sign*(X[Y[#]]-Y[X[#]])&.";

diffSymbolicOrder::usage =
    "diffSymbolicOrder[x, n][fun]: take the derivative with symbolic order.";


INTCancel::usage =
    "INTCancel[vars][expr]: cancel the possible INT head in the expression.";

SUMCancel::usage =
    "SUMCancel[vars][expr]: cancel the possible SUM head in the expression.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Helper*)


complementList[list1_List,list2_List]/;Length[list1]<=32 :=
    Fold[
        Function[{list,tallied},DeleteCases[list,tallied[[1]],{1},tallied[[2]]]],
        list1,
        Tally@list2
    ];

complementList[list1_List,list2_List]/;Length[list1]>32 :=
    With[{t1 = 2Tally[list1],t2 = Tally@Join[list1,list2]},
        {
            t2[[;;Length@t1,1]],
            t1[[All,2]]-t2[[;;Length@t1,2]]
        }//Transpose//Pick[#,Sign@#[[All,2]],1]&//MapApply[ConstantArray]//Apply[Join]//Sort
    ];


argumentD[varList_List,orderList_List] :=
    Sequence@@DeleteCases[Transpose@{varList,orderList},{_Integer,_Integer}];


(* ::Subsection:: *)
(*Atomic head*)


(* ::Subsubsection:: *)
(*Message*)


INT::Duplicate =
    "The original expression contains duplicate integral(s) with respect to ``."

SUM::Duplicate =
    "The original expression contains duplicate sum(s) with respect to ``.";


(* ::Subsubsection:: *)
(*PD*)


PD//Attributes =
    {Orderless};

head_PD/;System`Private`HoldNotValidQ[head] :=
    (
        System`Private`HoldSetValid[head];
        System`Private`HoldSetNoEntry[head]
    );


PD[] :=
    1;

PD/:PD[x__]PD[y__] :=
    PD[x,y];

PD/:Power[PD[x__],n_Integer]/;n>=1 :=
    PD@@Flatten@ConstantArray[{x},n];

PD/:PD[x__]Power[PD[y__],-1]/;!Language`EmptyIntersectionQ[{x},{y}] :=
    PD@@complementList[{x},{y}]/PD@@complementList[{y},{x}];


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


INT[] :=
    1;

INT/:INT[x__]INT[y__] :=
    INT[x,y];

INT/:INT[x__]Power[INT[y__],-1]/;!Language`EmptyIntersectionQ[{x},{y}] :=
    INT@@complementList[{x},{y}]/INT@@complementList[{y},{x}];

HoldPattern[INT][x__]/;!DuplicateFreeQ[{x}] :=
    (
        Message[
            INT::Duplicate,
            Row[Select[Tally[{x}],Last[#]>=2&][[All,1]],","]
        ];
        INT@@DeleteDuplicates[{x}]
    );


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


SUM[] :=
    1;

SUM/:SUM[x__]SUM[y__] :=
    SUM[x,y];

SUM/:SUM[x__]Power[SUM[y__],-1]/;!Language`EmptyIntersectionQ[{x},{y}] :=
    SUM@@complementList[{x},{y}]/SUM@@complementList[{y},{x}];

HoldPattern[SUM][x__]/;!DuplicateFreeQ[{x}] :=
    (
        Message[
            SUM::Duplicate,
            Row[Select[Tally[{x}],Last[#]>=2&][[All,1]],","]
        ];
        SUM@@DeleteDuplicates[{x}]
    );


(* ::Subsection:: *)
(*Operator form*)


(* ::Subsubsection:: *)
(*Option*)


integration//Options =
    Options@Integrate;

SetOptions[integration,GenerateConditions->False];


summation//Options =
    Options@Sum;

SetOptions[summation,GenerateConditions->False];


(* ::Subsubsection:: *)
(*Main*)


integration[iterator:{dummy_,__},opts:OptionsPattern[]][expr_Times] :=
    With[{
            fopts = Sequence@@FilterRules[{opts,Options@integration},Options@Integrate],
            expr1 = expr//separate[FreeQ[dummy]]
        },
        Integrate[expr1[[2]],iterator,fopts]*expr1[[1]]//INTCancel[dummy]
    ];

integration[args__List,opts:OptionsPattern[]][expr_] :=
    With[{
            fopts = Sequence@@FilterRules[{opts,Options@integration},Options@Integrate],
            dummies = Sequence@@Cases[{args},{var_,__}:>var]
        },
        Integrate[expr,args,fopts]//INTCancel[dummies]
    ];


summation[iterator:{dummy_,__},opts:OptionsPattern[]][expr_Times] :=
    With[{
            fopts = Sequence@@FilterRules[{opts,Options@summation},Options@Sum],
            expr1 = expr//separate[FreeQ[dummy]]
        },
        Sum[expr1[[2]],iterator,fopts]*expr1[[1]]//SUMCancel[dummy]
    ];

summation[args__List,opts:OptionsPattern[]][expr_] :=
    With[{
            fopts = Sequence@@FilterRules[{opts,Options@summation},Options@Sum],
            dummies = Sequence@@Cases[{args},{var_,__}:>var]
        },
        Sum[expr,args,fopts]//SUMCancel[dummies]
    ];


(* ::Subsection:: *)
(*Variable change*)


(* ::Subsubsection:: *)
(*Option*)


integrationChange//Options = {
    "Solution"->1,
    "ShowSolution"->False,
    "ShowJacobian"->False
};


diffChange//Options = {
    "Solution"->{1,1},
    "ShowSolution"->{False,False}
};


(* ::Subsubsection:: *)
(*Message*)


integrationChange::MismatchNumberOfJacobianSign =
    "The number of the Jacobian sign does not match the number of solutions.";


(* ::Subsubsection:: *)
(*Main*)


integrationChange[eqs_,oldVars_,newVars_,opts:OptionsPattern[]][expr_] :=
    integrateChangeKernel[expr,prepareList[eqs],prepareList[oldVars],prepareList[newVars],{1},opts];

integrationChange[eqs_,oldVars_,newVars_,signs_,opts:OptionsPattern[]][expr_] :=
    integrateChangeKernel[expr,prepareList[eqs],prepareList[oldVars],prepareList[newVars],prepareList[signs],opts];


integrateChangeKernel[expr_,eqList_List,oldList_List,newList_List,jacSignList_List,opts:OptionsPattern[integrationChange]] :=
    Module[{
            res,
            oldToNewList,jacSignList1,jacobianList,newExprList,
            whichSol = OptionValue[integrationChange,{opts},"Solution"],
            ifShowSol = OptionValue[integrationChange,{opts},"ShowSolution"],
            ifShowJac = OptionValue[integrationChange,{opts},"ShowJacobian"]
        },
        oldToNewList =
            solveKernel[oldList,whichSol,True][eqList];
        jacSignList1 =
            intcJacobianSignList[jacSignList,Length[oldToNewList]];
        jacobianList =
            intcJacobianList[oldList,newList,oldToNewList];
        newExprList =
            oldToNewList//Map[ReplaceAll[expr,#]&];
        res =
            MapThread[Times,{newExprList,jacobianList,jacSignList1}]//
                intcConvertINT[oldList,newList]//
                intcStripList;
        intcShowSolutionJacobian[ifShowSol,ifShowJac][oldToNewList,jacobianList];
        res
    ]//Catch;


diffChange[eqs_,oldVars_,newVars_,funs_,opts:OptionsPattern[]][expr_] :=
    diffChangeKernel[expr,prepareList[eqs],prepareList[oldVars],prepareList[newVars],prepareList[funs],opts];


diffChangeKernel[expr_,eqList_List,oldList_List,newList_List,funList_List,opts:OptionsPattern[diffChange]] :=
    Module[{
            res,
            eqList1,oldList1,funList1,expr1,oldToNewList,oldSymToOldList,
            whichSol = OptionValue[diffChange,{opts},"Solution"]//diffcDuplicateOption,
            ifShowSol = OptionValue[diffChange,{opts},"ShowSolution"]//diffcDuplicateOption
        },
        {eqList1,oldList1,funList1,expr1,oldSymToOldList} =
            {eqList,oldList,funList,expr}//diffcSymbolizeVar[oldList];
        oldToNewList =
            eqList1//solveKernel[oldList1,whichSol[[1]],True]//
                diffcShowSolution[ifShowSol[[1]],oldSymToOldList];
        res =
            expr1//
                ReplaceAll[diffcFunRuleList[whichSol[[2]],ifShowSol[[2]],oldSymToOldList][eqList1,oldList1,newList,funList1]]//
                (*convert old variables to new ones.*)
                ReplaceAll[oldToNewList]//
                diffcStripList;
        res
    ]//Catch;


(* ::Subsubsection:: *)
(*Helper*)


prepareList[list_List] :=
    Flatten[list];

prepareList[assoc_Association] :=
    Normal[assoc];

prepareList[other_] :=
    Flatten[{other}];


intcJacobianSignList[jacSignList_,oldToNewLength_] :=
    Which[
        (* If the sign of Jacobian is given as a single value, repeat it. *)
        Length[jacSignList]===1,
            Table[jacSignList[[1]],{oldToNewLength}],
        Length[jacSignList]===oldToNewLength,
            jacSignList,
        True,
            Message[integrationChange::MismatchNumberOfJacobianSign];
            Throw[jacSignList]
    ];


intcJacobianList[oldList_,newList_,oldToNewList_] :=
    Module[{res,oldToNew},
        res =
            Table[
                Det@Outer[D,ReplaceAll[oldList,oldToNew],newList],
                {oldToNew,oldToNewList}
            ];
        (* Try to simplify the result *)
        TimeConstrained[Simplify[res],2,res]
    ];


intcConvertINT[oldList_,newList_][expr_] :=
    With[{rule = MapThread[Rule,{oldList,newList}]},
        expr//ReplaceAll[{
            INT[args__]:>INT@@ReplaceAll[{args},rule]
        }]
    ];


intcStripList[list_] :=
    If[Length[list]===1,
        list[[1]],
        (*Else*)
        list
    ];


intcShowSolutionJacobian[True,False][solList_,jacobianList_] :=
    Echo@Grid[
        Transpose[{solList}],
        Spacings->{1,1/2},
        Alignment->{Left,Right}
    ];

intcShowSolutionJacobian[False,True][solList_,jacobianList_] :=
    Echo@Grid[
        Transpose[{jacobianList}],
        Spacings->{1,1/2},
        Alignment->{Left,Right}
    ];

intcShowSolutionJacobian[False,False][solList_,jacobianList_] :=
    Null;

intcShowSolutionJacobian[True,True][solList_,jacobianList_] :=
    Echo@Grid[
        Transpose[{solList,jacobianList}],
        Spacings->{1,1/2},
        Alignment->{Left,Right}
    ];


diffcDuplicateOption[opt_] :=
    If[MatchQ[opt,{_,_}],
        (* Then *)
        opt,
        (* Else *)
        {opt,opt}
    ];


diffcSymbolizeVar[varList:{__Symbol}][{exprs__}] :=
    {
        exprs,
        {}
    };

diffcSymbolizeVar[varList_][{exprs__}] :=
    Module[{symList,ruleList},
        symList =
            Table[Unique["$",Temporary],Length[varList]];
        ruleList =
            varList->symList//Thread;
        {
            Splice@{exprs}//ReplaceAll[ruleList],
            ruleList//Map[Reverse]
        }
    ];


diffcFunRuleList[whichSol_,ifShowSol_,oldSymToOldList_][eqList_,oldList_,newList_,funList_] :=
    Module[{newByOld,newByOldList,newToOldList,fun,head},
        newToOldList =
            eqList//solveKernel[newList,whichSol,True]//
                diffcShowSolution[ifShowSol,oldSymToOldList];
        newByOldList =
            newList//ReplaceAll[newToOldList];
        Table[
            Table[
                Head[fun]->head[
                    Apply[List,fun],
                    ReplacePart[fun,Thread[diffcGetVarPositionList[fun,oldList]->newByOld]]
                ]
                ,
                {fun,funList}
            ],
            {newByOld,newByOldList}
        ]//ReplaceAll[head->Function]
    ];


diffcGetVarPositionList[fun_,variableList_] :=
    Flatten[Map[Position[fun,#]&,variableList],{{1},{2,3}}];


diffcStripList[list_] :=
    If[Length[list]===1,
        list[[1,1]],
        (*Else*)
        Flatten[list,1]
    ];


diffcShowSolution[True,oldSymToOldList_][solList_] :=
    (
        Echo@Grid[
            {solList}//ReplaceAll[oldSymToOldList]//Transpose,
            Spacings->{1,1/2},
            Alignment->{Left,Right}
        ];
        solList
    )

diffcShowSolution[False,_][solList_] :=
    solList;


(* ::Subsubsection:: *)
(*Demo*)


diffChange[] :=
    CellPrint@{
        ExpressionCell[
            ToExpression[
                "D[f[x,t],{t,2}]==c^2*D[f[x,t],{x,2}]//diffChange[{u==x+c*t,v==x-c*t},{x,t},{u,v},{f[x,t]}]//Simplify",
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


integrationChange[] :=
    CellPrint@{
        ExpressionCell[
            ToExpression[
                "t^a//integrationChange[t==1-x,t,x,-1]",
                 StandardForm,
                 Defer
            ],
            "Code"
        ],
        ExpressionCell[
            ToExpression[
                "(1-x)^a",
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
            D[Times[exprs],argumentD[{vars},{orders}]]*
                fun[vars];

IBPRule[fun_,var_] :=
    Longest[exprs___]*Derivative[orders__][fun][vars__]:>
        With[{orderOfVar = getOrderOfVar[{vars},{orders},var]},
            (-1)^orderOfVar*
                D[Times[exprs],{var,orderOfVar}]*
                    Derivative[dropOrderByVar[{vars},{orders},var]][fun][vars]
        ];

IBPRule[fun_,vars1__] :=
    Longest[exprs___]*Derivative[orders__][fun][vars__]:>
        With[{orderList1 = getOrderOfVar[{vars},{orders},{vars1}]},
            (-1)^Total@getOrderOfVar[{vars},{orders},{vars1}]*
                D[Times[exprs],argumentD[{vars1},orderList1]]*
                    Derivative[dropOrderByVar[{vars},{orders},{vars1}]][fun][vars]
        ];


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

PDCoefficient//Options = {
    "CheckLinearity"->True
};

PDCoefficient[post:Except[_Rule]:Identity,opts:OptionsPattern[]][expr_] :=
    Catch[
        PDCheckLinearity[OptionValue["CheckLinearity"]][expr];
        expr//Expand//PDCoefficientKernel//MapAt[post,{All,2}]
    ];


PDCoefficientKernel[expr_Plus] :=
    Join[
        Cases[expr,PD[x__]*rest_.:>{{x},rest}],
        Cases[expr,rest_/;FreeQ[rest,_PD]:>{{},rest}]
    ]//GatherBy[#,First]&//Map[Rule[#[[1,1]],Total[#[[All,2]]]]&];

PDCoefficientKernel[expr_Times] :=
    expr//separate[FreeQ[_PD]]//
        ReplaceAll[{lhs_,PD[args__]|1}:>{{args}->lhs}];

PDCoefficientKernel[PD[args__]] :=
    {{args}->1};

PDCoefficientKernel[expr_] :=
    {{}->expr};


PDCheckLinearity[True][expr_] :=
    If[AnyTrue[Cases[expr,_PD,All],!linearQ[expr,#]&],
        Message[PDCoefficient::nonlinear];
        Throw[expr]
    ];

PDCheckLinearity[False][expr_] :=
    Null;


(* ::Subsubsection:: *)
(*PDCollect*)


PDCollect[args___][expr_] :=
    Collect[expr,PD[__],args];


(* ::Subsubsection:: *)
(*diffCoefficient*)


diffCoefficient::nonlinear =
    "the expression is nonlinear with respect to Derivative[__][_][__]."

diffCoefficient//Options = {
    "CheckLinearity"->True
};

diffCoefficient[funP:Except[_List],post:Except[_Rule]:Identity,opts:OptionsPattern[]][expr_] :=
    Catch[
        diffCheckLinearity[OptionValue["CheckLinearity"]][expr,funP];
        expr//Expand//diffCoefficientKernel[funP]//convertDerivative//MapAt[post,{All,2}]
    ];


diffCoefficientKernel[funP_][expr_Plus] :=
    Join[
        Cases[expr,(fun:funP[___]|Derivative[___][funP][___])*rest_.:>{fun,rest}],
        Cases[expr,rest_/;FreeQ[rest,funP]:>{{},rest}]
    ]//GatherBy[#,First]&//Map[Rule[#[[1,1]],Total[#[[All,2]]]]&];

diffCoefficientKernel[funP_][expr_Times] :=
    expr//separate[FreeQ[funP]]//
        ReplaceAll[{
            {lhs_,fun:funP[___]|Derivative[___][funP][___]}:>{fun->lhs},
            {lhs_,1}:>{{}->lhs}
        }];

diffCoefficientKernel[funP_][expr_] :=
    If[MatchQ[expr,funP[___]|Derivative[___][funP][___]],
        {expr->1},
        (*Else*)
        {{}->expr}
    ];


convertDerivative[list_List] :=
    list//ReplaceAt[{
        Derivative[orders__][fun_][vars__]:>{fun[vars],argumentD[{vars},{orders}]},
        fun_[vars__]:>{fun[vars]}
    },{All,1}];


diffCheckLinearity[True][expr_,fun_] :=
    If[AnyTrue[Cases[expr,Derivative[__][fun][__],All],!linearQ[expr,#]&],
        Message[diffCoefficient::nonlinear];
        Throw[expr]
    ];

diffCheckLinearity[False][expr_,fun_] :=
    Null;


(* ::Subsubsection:: *)
(*diffCollect*)


diffCollect[fun:Except[_List],args___][expr_] :=
    Collect[expr,Derivative[__][fun][__]|fun[__],args];

diffCollect[funList_List,args___][expr_] :=
    Collect[expr,Map[(Derivative[__][#][__]|#[__])&,funList],args];


(* ::Subsubsection:: *)
(*diffReplace*)


diffReplace[rules_,head_:Identity] :=
    ReplaceAll[getDiffReplaceRule[head][rules]];


getDiffReplaceRule//Attributes = {
    HoldAllComplete
};

getDiffReplaceRule[head_] :=
    Function[rule,getDiffReplaceRule[head,rule],HoldAllComplete];

getDiffReplaceRule[head_,Verbatim[Rule][f_Symbol,rhs_]] :=
    {
        f[___]->rhs,
        Derivative[orders__][f][vars___]:>
            head[D][rhs,argumentD[{vars},{orders}]]
    };

getDiffReplaceRule[head_,(rule:Rule|RuleDelayed)[lhs:f_[args___],rhs_]] :=
    With[{
            varList = stripPattern[{args}]
        },
        {
            rule[lhs,rhs],
            HoldComplete[
                Derivative[orders__][f][args],
                head[D][rhs,argumentD[varList,{orders}]]
            ]//ReplaceAll[HoldComplete->RuleDelayed]
        }
    ];

getDiffReplaceRule[head_,ruleList_List] :=
    ruleList//Map[getDiffReplaceRule[head]]//Flatten;


(* ::Subsubsection:: *)
(*diffComm*)


diffComm//Options = {
    "Sign"->Automatic
};


diffComm[x_,y_,opts:OptionsPattern[]] :=
    diffCommKernel[x,y,OptionValue["Sign"]];


diffCommKernel[x_,y_,sign_] :=
    sign*(x[y[#]]-y[x[#]])&;

diffCommKernel[x_,y_,Automatic] :=
    (x[y[#]]-y[x[#]])&;


(* ::Subsubsection:: *)
(*diffSymbolicOrder*)


diffSymbolicOrder[x_,n_][expr_] :=
    PiecewiseExpand[
        System`Private`SymbolicD[expr,x,n],
        n>=1
    ];


(* ::Subsubsection:: *)
(*INTCancel|SUMCancel*)


INTCancel[vars___][expr_] :=
    headCancel[INT,{vars}][expr];

SUMCancel[vars___][expr_] :=
    headCancel[SUM,{vars}][expr];


headCancel[head_,varList_List][expr_]/;FreeQ[expr,_head] :=
    expr;

headCancel[head_,varList_List][head_[vars__]] :=
    head@@complementList[{vars},varList];

headCancel[head_,varList_List][Verbatim[Times][prec___,head_[vars2___],succ___]] :=
    prec*head@@complementList[{vars2},varList]*succ;

headCancel[head_,varList_List][expr_Plus] :=
    expr//Map[headCancel[head,varList]];

headCancel[head_,varList_List][expr_List] :=
    expr//Map[headCancel[head,varList]];

headCancel[head_,varList_List][HoldPattern[ConditionalExpression[ce_,condition_]]] :=
    ConditionalExpression[headCancel[head,varList][ce],condition];

headCancel[head_,varList_List][HoldPattern[Piecewise[pw_,default_]]] :=
    Piecewise[
        MapAt[headCancel[head,varList],pw,{All,1}],
        headCancel[head,varList][default]
    ];

headCancel[head_,varList_List][expr_] :=
    expr;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
