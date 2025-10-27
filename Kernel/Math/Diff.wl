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


integrate::usage =
    "integrate[args][expr]: operator form of Integrate."<>
    "\n"<>
    "Default[GenerateConditions]: False.";

summation::usage =
    "summation[args][expr]: operator form of Sum."<>
    "\n"<>
    "Default[GenerateConditions]: False.";


(* ::Subsection:: *)
(*Variable change*)


integrateChange::usage =
    "integrateChange[equations, oldVars, newVars, signs][expr]: change variables in integrals."<>
    "\n"<>
    "Info[signs]: Jacobian signs."<>
    "\n"<>
    "Default[\"Solution\"]: 1."<>
    "\n"<>
    "Default[\"ShowSolution\"]: False."<>
    "\n"<>
    "Default[\"ShowJacobian\"]: False.";


diffChange::usage =
    "diffChange[equations, oldVars, newVars, funs][expr]: change variables in differential equations."<>
    "\n"<>
    "Info[funs]: list of functions to transform."<>
    "\n"<>
    "Default[\"Solution\"]: 1."<>
    "\n"<>
    "Default[\"ShowSolution\"]: False.";


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
    "Default[post]: Identity."<>
    "\n"<>
    "Default[\"CheckLinearity\"]: True.";

PDCollect::usage =
    "PDCollect[args][expr]: collect the terms with respect to PD[__]."<>
    "\n"<>
    "Info[args]: inherited from Collect.";


diffCoefficient::usage =
    "diffCoefficient[fun, post, opts][expr]: extract the coefficients of Derivative[__][_][__]."<>
    "\n"<>
    "Info[post]: post-operation applied to the coefficients."<>
    "\n"<>
    "Default[post]: Identity."<>
    "\n"<>
    "Default[\"CheckLinearity\"]: True.";

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
    "Sketch: -(X[Y[#]]-Y[X[#]])&.";


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
    With[{
            expr1 = Cases[expr,Except[_INT],1]//Apply[Times],
            int = DeleteCases[FirstCase[expr,INT[vars__]:>{vars},{}],Alternatives@@Cases[{args},{var_,__}:>var]],
            fopts = Sequence@@FilterRules[{opts,Options@integrate},Options@Integrate]
        },
        Integrate[expr1,args,fopts]*INT@@int
    ];


summation[args__List,opts:OptionsPattern[]][expr_]/;FreeQ[expr,_SUM] :=
    With[{
            fopts = Sequence@@FilterRules[{opts,Options@summation},Options@Sum]
        },
        Sum[expr,args,fopts]
    ];

summation[args__List,opts:OptionsPattern[]][expr_Times]/;!FreeQ[expr,_SUM] :=
    With[{
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


integrateChange//Options = {
    "Solution"->1,
    "ShowSolution"->False,
    "ShowJacobian"->False
};


diffChange//Options = {
    "Solution"->1,
    "ShowSolution"->False
};


(* ::Subsubsection:: *)
(*Message*)


integrateChange::MismatchNumberOfJacobianSign =
    "The number of the Jacobian sign does not match the number of solutions.";


(* ::Subsubsection:: *)
(*Main*)


integrateChange[eqs_,oldVars_,newVars_,opts:OptionsPattern[]][expr_] :=
    integrateChangeKernel[expr,prepareList[eqs],prepareList[oldVars],prepareList[newVars],{1},opts];

integrateChange[eqs_,oldVars_,newVars_,signs_,opts:OptionsPattern[]][expr_] :=
    integrateChangeKernel[expr,prepareList[eqs],prepareList[oldVars],prepareList[newVars],prepareList[signs],opts];


integrateChangeKernel[expr_,eqList_List,oldList_List,newList_List,signOfJacobianList1_List,opts:OptionsPattern[integrateChange]] :=
    Module[{
            res,
            oldToNewList,jacobianList,newExprList,signOfJacobianList,
            whichSolution = OptionValue[integrateChange,{opts},"Solution"],
            ifShowSolution = OptionValue[integrateChange,{opts},"ShowSolution"],
            ifShowJacobian = OptionValue[integrateChange,{opts},"ShowJacobian"]
        },
        oldToNewList =
            solveKernel[oldList,whichSolution,True][eqList];
        signOfJacobianList =
            getSignOfJacobianList[signOfJacobianList1,Length[oldToNewList]];
        jacobianList =
            getJacobianList[oldList,newList,oldToNewList];
        newExprList =
            oldToNewList//Map[ReplaceAll[expr,#]&];
        res =
            MapThread[Times,{newExprList,jacobianList,signOfJacobianList}]//
                integrateChangeConvertINT[oldList,newList]//
                integrateChangeStripList;
        showSolutionJacobian[ifShowSolution,ifShowJacobian][oldToNewList,jacobianList];
        res
    ]//Catch;


diffChange[eqList_,oldList_,newList_,funList_,opts:OptionsPattern[]][expr_] :=
    diffChangeKernel[expr,prepareList[eqList],prepareList[oldList],prepareList[newList],prepareList[funList],opts];


diffChangeKernel[expr_,eqList_List,oldList_List,newList_List,funList_List,opts:OptionsPattern[diffChange]] :=
    Module[{
            res,oldToNewList,
            whichSolution = OptionValue[diffChange,{opts},"Solution"],
            ifShowSolution = OptionValue[diffChange,{opts},"ShowSolution"]
        },
        oldToNewList =
            solveKernel[oldList,whichSolution,True][eqList];
        res =
            expr//
                ReplaceAll[getFunctionRuleList[whichSolution][eqList,oldList,newList,funList]]//
                (*convert old variables to new ones.*)
                ReplaceAll[oldToNewList]//
                diffChangeStripList;
        showSolution[ifShowSolution][oldToNewList];
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


getSignOfJacobianList[signOfJacobianList_,oldToNewLength_] :=
    Which[
        (* If the sign of Jacobian is given as a single value, repeat it. *)
        Length[signOfJacobianList]===1,
            Table[signOfJacobianList[[1]],{oldToNewLength}],
        Length[signOfJacobianList]===oldToNewLength,
            signOfJacobianList,
        True,
            Message[integrateChange::MismatchNumberOfJacobianSign];
            Throw[signOfJacobianList]
    ];


getJacobianList[oldList_,newList_,oldToNewList_] :=
    Module[{res,oldToNew},
        res =
            Table[
                Det@Outer[D,ReplaceAll[oldList,oldToNew],newList],
                {oldToNew,oldToNewList}
            ];
        (* Try to simplify the result *)
        TimeConstrained[Simplify[res],2,res]
    ];


integrateChangeConvertINT[oldList_,newList_][expr_] :=
    With[{rule = MapThread[Rule,{oldList,newList}]},
        expr//ReplaceAll[{
            INT[args__]:>INT@@ReplaceAll[{args},rule]
        }]
    ];


integrateChangeStripList[list_] :=
    If[Length[list]===1,
        list[[1]],
        (*Else*)
        list
    ];


showSolutionJacobian[True,False][solList_,jacobianList_] :=
    Print@Grid[
        Transpose[{solList}],
        Spacings->{1,1/2},
        Alignment->{Left,Right}
    ];

showSolutionJacobian[False,True][solList_,jacobianList_] :=
    Print@Grid[
        Transpose[{jacobianList}],
        Spacings->{1,1/2},
        Alignment->{Left,Right}
    ];

showSolutionJacobian[False,False][solList_,jacobianList_] :=
    Null;

showSolutionJacobian[True,True][solList_,jacobianList_] :=
    Print@Grid[
        Transpose[{solList,jacobianList}],
        Spacings->{1,1/2},
        Alignment->{Left,Right}
    ];


getFunctionRuleList[whichSolution_][eqList_,oldList_,newList_,funList_] :=
    Module[{newByOld,newByOldList,fun,head},
        newByOldList =
            newList//ReplaceAll[solveKernel[newList,whichSolution,True][eqList]];
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


diffChangeStripList[list_] :=
    If[Length[list]===1,
        list[[1,1]],
        (*Else*)
        Flatten[list,1]
    ];


showSolution[True][solList_] :=
    Print@Grid[
        Transpose[{solList}],
        Spacings->{1,1/2},
        Alignment->{Left,Right}
    ];

showSolution[False][_] :=
    Null;


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


integrateChange[] :=
    CellPrint@{
        ExpressionCell[
            ToExpression[
                "t^a//integrateChange[t==1-x,t,x,-1]",
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


argumentD[varList_List,orderList_List] :=
    Sequence@@DeleteCases[Transpose@{varList,orderList},{_Integer,_Integer}];


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

PDCoefficient[post_:Identity,opts:OptionsPattern[]][expr_] :=
    Module[{},
        PDCheckLinearity[OptionValue["CheckLinearity"]][expr];
        expr//Expand//PDCoefficientKernel//MapAt[post,{All,2}]
    ]//Catch;


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
    If[AnyTrue[Cases[expr,_PD,All],!Internal`LinearQ[expr,#]&],
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

diffCoefficient[funP:Except[_List],post_:Identity,opts:OptionsPattern[]][expr_] :=
    Module[{},
        diffCheckLinearity[OptionValue["CheckLinearity"]][expr,funP];
        expr//Expand//diffCoefficientKernel[funP]//convertDerivative//MapAt[post,{All,2}]
    ]//Catch;


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
    If[AnyTrue[Cases[expr,Derivative[__][fun][__],All],!Internal`LinearQ[expr,#]&],
        Message[diffCoefficient::nonlinear];
        Throw[expr]
    ];

diffCheckLinearity[False][expr_,fun_] :=
    Null;


(* ::Subsubsection:: *)
(*diffCollect*)


diffCollect[fun:Except[_List],args___][expr_] :=
    Collect[expr,Derivative[___][fun][___],args];

diffCollect[funList_List,args___][expr_] :=
    Collect[expr,Map[Derivative[___][#][___]&,funList],args];


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

getDiffReplaceRule[head_,(rule:Rule|RuleDelayed)[lhs:f_Symbol[args___],rhs_]] :=
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


diffComm[x_,y_] :=
    -(x[y[#]]-y[x[#]])&;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
