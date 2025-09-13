(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Gamma`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Constant`"];


(* ::Section:: *)
(*Public*)


gammaSimplify::usage =
    "gammaSimplify[expr]: simplify Gamma functions in the expression."<>
    "\n"<>
    "Sketch: Developer`GammaSimplify.";

gammaFrom::usage =
    "gammaFrom[expr, opts]: expand everything to Gamma functions."<>
    "\n"<>
    "Default[\"Transformation\"]: Automatic."<>
    "\n"<>
    "Default[\"ActivateGamma\"]: True.";

gammaSeparate::usage =
    "gammaSeparate[expr]: separate a product into Gamma functions and the rest.";

gammaTakeResidue::usage =
    "gammaTakeResidue[variable, index, gamma, sign, opts][expr]: take residue of a series of poles from the Gamma factor."<>
    "\n"<>
    "gammaTakeResidue[variable, index->n, gamma, sign, opts][expr]: specify one pole in the series."<>
    "\n"<>
    "Info[index]: the index of the poles."<>
    "\n"<>
    "Info[gamma]: the argument of the Gamma function."<>
    "\n"<>
    "Info[sign]: the direction of contour."<>
    "\n"<>
    "Value[sign]: {1, -1, Left, Right}."<>
    "\n"<>
    "Default[sign]: 1."<>
    "\n"<>
    "Default[\"SimplePole\"]: True."<>
    "\n"<>
    "Default[\"ShowPole\"]: True.";


multiGamma::usage =
    "multiGamma[num, denom]: represent a product of Gamma functions in numerator and denominator."<>
    "\n"<>
    "Info[num]: list of arguments for Gamma functions in the numerator."<>
    "\n"<>
    "Info[denom]: list of arguments for Gamma functions in the denominator.";

multiGammaFrom::usage =
    "multiGammaFrom[expr]: convert Gamma functions into multi-Gamma symbols.";

multiGammaSimplify::usage =
    "multiGammaSimplify[expr]: simplify the multi-Gamma symbol with the assumption."<>
    "\n"<>
    "Default[assume]: True.";

multiGammaReduceByBarnesLemma::usage =
    "multiGammaReduceByBarnesLemma[s][expr]: reduce the multi-Gamma symbol by the first and second Barnes lemmas."<>
    "\n"<>
    "Info[s]: the variable parameter in the Barnes lemma reduction.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


gammaFrom//Options = {
    "Transformation"->Automatic,
    "ActivateGamma"->True
};


gammaTakeResidue//Options = {
    "SimplePole"->True,
    "ShowPole"->True
};


(* ::Subsection:: *)
(*Message*)


gammaFrom::noSuchKey =
    "the transformations are expected as a subset of ``.";


gammaTakeResidue::NotProduct =
    "the expression is expected to be a product involving Gamma functions.";

gammaTakeResidue::IndexConflict =
    "the index `` conflicts with the expression.";

gammaTakeResidue::NotMatchVar =
    "the argument `` should be a linear function of the variable ``.";

gammaTakeResidue::NotInExpr =
    "the factor `` does not appear in the expression.";


multiGammaReduceByBarnesLemma::NotMatch =
    "the multi-Gamma symbol cannot be reduced by the Barnes lemmas."

multiGammaReduceByBarnesLemma::NotProduct =
    "the expression is expected to be a product involving Gamma functions.";


(* ::Subsection:: *)
(*gammaSimplify*)


gammaSimplify :=
    Developer`GammaSimplify;


(* ::Subsection:: *)
(*gammaFrom*)


(* ::Subsubsection:: *)
(*Main*)


gammaFrom[expr_,OptionsPattern[]] :=
    Module[ {opt = OptionValue["Transformation"],ruleList},
        ruleList =
            Which[
                opt===Automatic,
                    gammaDataValueList,
                Head[opt]===List&&SubsetQ[gammaDataKeyList,opt],
                    gammaData//Lookup[opt]//Flatten,
                True,
                    Message[gammaFrom::noSuchKey,gammaDataKeyList];
                    expr//Throw
            ];
        expr//ReplaceAll[ruleList]//
            ReplaceAll[gm_gamma:>Simplify@gm]//
                gammaActivateHead[OptionValue["ActivateGamma"]]
    ]//Catch;


(* ::Subsubsection:: *)
(*Helper*)


gammaActivateHead[True][expr_] :=
    expr//ReplaceAll[gamma->Gamma];

gammaActivateHead[False][expr_] :=
    expr//ReplaceAll[gamma->Inactive[Gamma]];


(* ::Subsection:: *)
(*gammaSeparate*)


gammaSeparate[expr_Gamma] :=
    {expr,1};

gammaSeparate[expr:Power[_Gamma,_]] :=
    {expr,1};

gammaSeparate[expr_multiGamma] :=
    {gammaFrom[expr,"Transformation"->{"MultiGamma"}],1};

gammaSeparate[expr_Times] :=
    Module[ {expr1},
        expr1 =
            If[ FreeQ[expr,_multiGamma],
                expr,
                (*Else*)
                gammaFrom[expr,"Transformation"->{"MultiGamma"}]
            ];
        {
            Discard[expr1,FreeQ[Gamma]],
            Select[expr1,FreeQ[Gamma]]
        }
    ];

gammaSeparate[expr_] :=
    {1,expr};


(* ::Subsection:: *)
(*gammaResidue*)


(* ::Subsubsection:: *)
(*Main*)


gammaTakeResidue[variable_,index1_,gmarg_,sign:1|-1|Left|Right:1,OptionsPattern[]][expr1_] :=
    Module[ {expr,isSpecificPole,index,pos,solution,residue},
        expr =
            expr1//gammaTakeResidueHandleMultiGamma;
        {isSpecificPole,index,pos} =
            index1//gammaTakeResidueGetIndex;
        gammaTakeResidueCheck[variable,index,gmarg][expr];
        solution =
            Part[Solve[gmarg==-index,{variable}],1,1];
        residue =
            If[ OptionValue["SimplePole"]===True,
                Residue[Gamma[gmarg],{variable,solution[[2]]},Assumptions->index>=0&&Element[index,Integers]]*
                    ReplaceAll[expr/Gamma[gmarg],solution],
                (*Else*)
                Residue[expr,{variable,solution[[2]]},Assumptions->index>=0&&Element[index,Integers]]
            ];
        gammaTakeResidueShowPoleData[OptionValue["ShowPole"]][solution,variable,index,expr];
        residueSign[sign]*residue//
            takeSpecificPole[isSpecificPole][index,pos]//
            ReplaceAll[gm_Gamma:>Simplify[gm]]//
            handleResidueWithINT[expr,variable]
    ]//Catch;


gammaTakeResidue[] :=
    CellPrint@{
        ExpressionCell[
            ToExpression[
                "gammaTakeResidue[x,n,2x+1,\"ShowPole\"->True]@Gamma[2x+1]",
                 StandardForm,
                 Defer
            ],
            "Code"
        ],
        ExpressionCell[
            ToExpression[
                "{(-1)^n/(2 n!),x->1/2 (-1-n)}",
                StandardForm,
                Defer
            ],
            "Output"
        ]
    };


(* ::Subsubsection:: *)
(*Helper*)


gammaTakeResidueHandleMultiGamma[expr_] :=
    If[ FreeQ[expr,_multiGamma],
        expr,
        (*Else*)
        gammaFrom[expr,"Transformation"->{"MultiGamma"}]
    ];


gammaTakeResidueGetIndex[(Rule|List)[index_,pos_]] :=
    {True,index,pos};

gammaTakeResidueGetIndex[index_] :=
    {False,index,Null};


gammaTakeResidueCheck[variable_,index_,gmarg_][expr_] :=
    Which[
        !MatchQ[expr,_Gamma|_multiGamma|_Times|_Power|_List],
            Message[gammaTakeResidue::NotProduct];
            expr//Throw,
        !FreeQ[expr,index],
            Message[gammaTakeResidue::IndexConflict,index];
            expr//Throw,
        !Internal`LinearQ[gmarg,{variable}],
            Message[gammaTakeResidue::NotMatchVar,gmarg,variable];
            expr//Throw,
        FreeQ[expr,Gamma[gmarg]],
            Message[gammaTakeResidue::NotInExpr,HoldForm[Gamma][gmarg]];
            expr//Throw
    ];


gammaTakeResidueShowPoleData[True][solution_,___] :=
    Echo[solution];

gammaTakeResidueShowPoleData[Full][solution_,variable_,index_,expr1_] :=
    Module[ {sign,gammaList},
        sign =
            Simplify[Sign@Coefficient[solution[[2]],index]];
        gammaList =
            Cases[expr1,Gamma[arg_]/;!FreeQ[arg,variable]:>arg,All]//Map[{#,Simplify@ReplaceAll[#,solution],Exponent[expr1,Gamma[#]]}&];

        gammaList =
            Switch[sign,
                1,
                    separate[Simplify[Coefficient[#[[1]],variable]>0]&][gammaList],
                -1,
                    separate[Simplify[Coefficient[#[[1]],variable]<0]&][gammaList],
                _,
                    gammaList
            ];

        gammaList =
            gammaList//MapAt[
                Switch[ #[[3]],
                    1,
                        {#[[1]],#[[2]]},
                    -1,
                        {Style[#[[1]],StandardBlue],Style[#[[2]],StandardBlue]},
                    _,
                        {Style[#[[1]],StandardRed],Style[#[[2]],StandardRed]}
                ]&,
                {All,All}
            ];

        Echo[solution];
        Print@Grid[
            {Map[gammaListGrid,gammaList]},
            Spacings->{1,0},
            Alignment->Top
        ];
    ];

gammaTakeResidueShowPoleData[False,___][_] :=
    Null;


gammaListGrid[list_] :=
    Grid[list,Alignment->{Left,Center},Spacings->{1,0.5},Dividers->{True,{{True}}},FrameStyle->LightGray]


residueSign[Right|-1] :=
    -1;

residueSign[Left|1] :=
    1;


takeSpecificPole[True][index_,pos_] :=
    ReplaceAll[index->pos];

takeSpecificPole[False][index_,pos_] :=
    Identity;


handleResidueWithINT[expr_,variable_][residue_] :=
    If[ FreeQ[expr,_INT],
        residue,
        (*Else*)
        residue/INT[variable]
    ];


(* ::Subsection:: *)
(*multiGammaFrom*)


(* ::Subsubsection:: *)
(*Main*)


multiGamma[{},{}] :=
    1;

HoldPattern[multiGamma[num_List,denom_List]]/;!Language`EmptyIntersectionQ[num,denom] :=
    multiGamma[listComplement[num,denom],listComplement[denom,num]];

HoldPattern[multiGamma[num_List,denom_List]]/;!OrderedQ[num] :=
    multiGamma[Sort@num,denom];

HoldPattern[multiGamma[num_List,denom_List]]/;!OrderedQ[denom] :=
    multiGamma[num,Sort@denom];

multiGamma/:Power[HoldPattern[multiGamma[num_List,denom_List]],-1]:=
    multiGamma[denom,num];

multiGamma/:Power[HoldPattern[multiGamma[num_List,denom_List]],n_Integer]/;n>=2:=
    multiGamma[Catenate@ConstantArray[num,n],Catenate@ConstantArray[denom,n]];

multiGamma/:Power[HoldPattern[multiGamma[num_List,denom_List]],n_Integer]/;n<=-2:=
    multiGamma[Catenate@ConstantArray[denom,-n],Catenate@ConstantArray[num,-n]];

(* Verbatim is necessary here to prevent the attributes of Times. *)
multiGamma/:prod:HoldPattern[Verbatim[Times][___,_multiGamma,___,_multiGamma,___]]:=
    With[ {
            mgList = Cases[Unevaluated@prod,_multiGamma],
            rest = Cases[Unevaluated@prod,Except[_multiGamma]]
        },
        Apply[Times,rest]*multiGamma[Catenate@Part[mgList,All,1],Catenate@Part[mgList,All,2]]
    ];


multiGammaFrom[expr_] :=
    expr//ReplaceAll[prod_Times:>multiGammaFromProduct[prod]]//
        ReplaceAll[Gamma[x_]:>multiGamma[{x},{}]];


(* ::Subsubsection:: *)
(*Helper*)


multiGammaFromProduct[expr_Times] :=
    With[ {
            num = Catenate@Cases[expr,Power[Gamma[x_],n_.]/;n>=1:>ConstantArray[x,n]],
            denom = Catenate@Cases[expr,Power[Gamma[x_],n_]/;n<=-1:>ConstantArray[x,-n]],
            rest = Cases[expr,Except[Power[_Gamma,_.]]]
        },
        Apply[Times,rest]*multiGamma[num,denom]
    ];


listComplement[list1_List,list2_List]/;Length[list1]<=32 :=
    Fold[
        Function[{list,tallied},DeleteCases[list,tallied[[1]],{1},tallied[[2]]]],
        list1,
        Tally@list2
    ];

listComplement[list1_List,list2_List]/;Length[list1]>32 :=
    With[ {t1 = 2Tally[list1],t2 = Tally@Join[list1,list2]},
        {
            t2[[;;Length@t1,1]],
            t1[[All,2]]-t2[[;;Length@t1,2]]
        }//Transpose//Pick[#,Sign@#[[All,2]],1]&//MapApply[ConstantArray]//Apply[Join]//Sort
    ];


(* ::Subsection:: *)
(*multiGammaSimplify*)


(* ::Subsubsection:: *)
(*Main*)


multiGammaSimplify[expr_,assume_:True] :=
    expr//ReplaceAll[mg_multiGamma:>multiGammaFunctionExpand[assume][mg]];


(* ::Subsubsection:: *)
(*Helper*)


multiGammaFunctionExpand[assume_][mg_] :=
    mg//gammaFrom//FunctionExpand//Simplify[#,assume]&//
        gammaFrom//multiGammaFrom//Simplify;


(* ::Subsection:: *)
(*multiGammaReduceByBarnesLemma*)


(* ::Subsubsection:: *)
(*Main*)


multiGammaReduceByBarnesLemma[s_][k_*mg_multiGamma]/;FreeQ[k,s] :=
    k*multiGammaReduceByBarnesLemma[s][mg];

multiGammaReduceByBarnesLemma[s_][expr:k_*_multiGamma]/;!FreeQ[k,s] :=
    (
        Message[multiGammaReduceByBarnesLemma::NotMatch];
        expr
    );

multiGammaReduceByBarnesLemma[_][expr:Except[_Times|_multiGamma]] :=
    (
        Message[multiGammaReduceByBarnesLemma::NotProduct];
        expr
    );

multiGammaReduceByBarnesLemma[s_][mg_multiGamma] :=
    Module[ {num,denom,numPlus,numMinus,denomPlus,numPlusMinusSum,numRest,denomRest},
        {num,denom} =
            Collect[{mg[[1]],mg[[2]]},s];
        numPlus =
            Cases[num,s+a_.:>a,{1}];
        numMinus =
            Cases[num,-s+b_.:>b,{1}];
        Which[
            (*first Barnes lemma.*)
            Length[numPlus]===Length[numMinus]===2,
                numRest =
                    num//DeleteCases[s+_.|-s+_.];
                denomRest =
                    denom;
                multiGammaReduceByFirstBarnesLemma[numPlus,numMinus,numRest,denomRest,mg,s],
            (*second Barnes lemma.*)
            Length[numPlus]===3&&Length[numMinus]===2,
                denomPlus =
                    Cases[denom,s+a_.:>a,{1}];
                numRest =
                    num//DeleteCases[s+_.|-s+_.];
                denomRest =
                    denom//DeleteCases[s+_.];
                numPlusMinusSum =
                    Total[{numPlus,numMinus},2];
                multiGammaReduceBySecondBarnesLemma[numPlus,numMinus,denomPlus,numPlusMinusSum,numRest,denomRest,mg,s],
            True,
                Message[multiGammaReduceByBarnesLemma::NotMatch];
                mg//Throw
        ]//multiGammaFactorSimplify
    ]//Catch;


(* ::Subsubsection:: *)
(*Helper*)


multiGammaReduceByFirstBarnesLemma[numPlus_,numMinus_,numRest_,denomRest_,mg_,s_] :=
    If[ FreeQ[numRest,s]&&FreeQ[denomRest,s],
        multiGamma[
            Join[numRest,Flatten@Outer[Plus,numPlus,numMinus]],
            Join[denomRest,{Total[{numPlus,numMinus},2]}]
        ],
        (*Else*)
        Message[multiGammaReduceByBarnesLemma::NotMatch];
        mg//Throw
    ];


multiGammaReduceBySecondBarnesLemma[numPlus_,numMinus_,denomPlus_,numPlusMinusSum_,numRest_,denomRest_,mg_,s_] :=
    If[ FreeQ[numRest,s]&&FreeQ[denomRest,s]&&Length[denomPlus]===1&&Simplify[denomPlus[[1]]-numPlusMinusSum]===0,
        multiGamma[
            Join[numRest,Flatten@Outer[Plus,numPlus,numMinus]],
            Join[denomRest,numPlusMinusSum-numPlus]
        ],
        (*Else*)
        Message[multiGammaReduceByBarnesLemma::NotMatch];
        mg//Throw
    ];


multiGammaFactorSimplify[expr_] :=
    expr//ReplaceAll[HoldPattern[multiGamma[num_List,denom_List]]:>multiGamma[Simplify@num,Simplify@denom]];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
