(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Gamma`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Constant`"];


(* ::Section:: *)
(*Public*)


gammaSimplify::usage =
    "simplify Gamma factors in the expression.\n Developer`GammaSimplify";

gammaFrom::usage =
    "expand everything to Gamma factors.";

gammaSeparate::usage =
    "split a product into a list containing Gamma factors and the rests.";

gammaTakeResidue::usage =
    "take residue of Gamma factors.";


multiGamma::usage =
    "head of multi-Gamma symbol.";

multiGammaFrom::usage =
    "collect Gamma factors into multi-Gamma symbols.";

multiGammaSimplify::usage =
    "simplify the multi-Gamma symbol.";

multiGammaReduceByBarnesLemma::usage =
    "reduce the multi-Gamma symbol by the Barnes lemmas.";


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


gammaTakeResidue::notProduct =
    "the expression is expected to be a product involving Gamma functions.";

gammaTakeResidue::indexConflict =
    "the index `` conflicts with the expression.";

gammaTakeResidue::gammaNotMatchVar =
    "the argument `` should be a linear function of the variable ``.";

gammaTakeResidue::gammaNotInExpr =
    "the factor `` does not appear in the expression.";


multiGammaReduceByBarnesLemma::notMatch =
    "the multi-Gamma symbol cannot be reduced by the Barnes lemmas."

multiGammaReduceByBarnesLemma::notProduct =
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
                    HoldComplete[expr]//Throw
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
            Discard[expr,FreeQ[Gamma]],
            Select[expr,FreeQ[Gamma]]
        }
    ];

gammaSeparate[expr_] :=
    {1,expr};


(* ::Subsection:: *)
(*gammaResidue*)


(* ::Subsubsection:: *)
(*Main*)


gammaTakeResidue[variable_,index_,gmarg_,sign:1|-1|Left|Right:1,OptionsPattern[]][expr_] :=
    Module[ {expr1,solution,residue},
        expr1 =
            If[ FreeQ[expr,_multiGamma],
                expr,
                (*Else*)
                gammaFrom[expr,"Transformation"->{"MultiGamma"}]
            ];
        gammaTakeResidueCheck[variable,index,gmarg][expr1];
        solution =
            Part[Solve[gmarg==-index,{variable}],1,1];
        residue =
            If[ OptionValue["SimplePole"]===True,
                Residue[Gamma[gmarg],{variable,solution[[2]]},Assumptions->index>=0&&Element[index,Integers]]*
                    ReplaceAll[expr1/Gamma[gmarg],solution],
                (*Else*)
                Residue[expr1,{variable,solution[[2]]},Assumptions->index>=0&&Element[index,Integers]]
            ];
        ifShowPoleData[OptionValue["ShowPole"]][solution,variable,index,expr1];
        residueSign[sign]*residue//ReplaceAll[gm_Gamma:>Simplify@gm]//
            handleResidueWithINT[expr1,variable]
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


gammaTakeResidueCheck[variable_,index_,gmarg_][expr_] :=
    Which[
        !MatchQ[expr,_Gamma|_multiGamma|_Times|_Power|_List],
            Message[gammaTakeResidue::notProduct];
            HoldComplete[expr]//Throw,
        !FreeQ[expr,index],
            Message[gammaTakeResidue::indexConflict,index];
            HoldComplete[expr]//Throw,
        !Internal`LinearQ[gmarg,{variable}],
            Message[gammaTakeResidue::gammaNotMatchVar,gmarg,variable];
            HoldComplete[expr]//Throw,
        FreeQ[expr,Gamma[gmarg]],
            Message[gammaTakeResidue::gammaNotInExpr,HoldForm[Gamma][gmarg]];
            HoldComplete[expr]//Throw
    ];


ifShowPoleData[True][solution_,___] :=
    Echo[solution];

ifShowPoleData[Full][solution_,variable_,index_,expr1_] :=
    Module[ {sign,gammaList,gammaListNew},
        sign =
            Simplify[Sign@Coefficient[solution[[2]],index]];
        gammaList =
            Cases[expr1,Gamma[arg_]/;!FreeQ[arg,variable]:>arg,All]//Map[{#,Simplify@ReplaceAll[#,solution]}&];
        gammaListNew =
            Switch[sign,
                1,
                    separate[Simplify[Coefficient[#[[1]],variable]>0]&][gammaList],
                -1,
                    separate[Simplify[Coefficient[#[[1]],variable]<0]&][gammaList],
                _,
                    gammaList
            ];
        Echo[solution];
        Print@Grid[
            {Map[gammaListGrid,gammaListNew]},
            Spacings->{1,0},
            Alignment->Top
        ];
    ];

ifShowPoleData[False,___][_] :=
    Null;


gammaListGrid[list_] :=
    Grid[list,Alignment->{Left,Center},Spacings->{1,0.5},Dividers->{True,{{True}}},FrameStyle->LightGray]


handleResidueWithINT[expr_,variable_][residue_] :=
    If[ FreeQ[expr,_INT],
        residue,
        (*Else*)
        residue/INT[variable]
    ];


residueSign[Right|-1] :=
    -1;

residueSign[Left|1] :=
    1;


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
        Message[multiGammaReduceByBarnesLemma::notMatch];
        expr
    );

multiGammaReduceByBarnesLemma[_][expr:Except[_Times|_multiGamma]] :=
    (
        Message[multiGammaReduceByBarnesLemma::notProduct];
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
                Message[multiGammaReduceByBarnesLemma::notMatch];
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
        Message[multiGammaReduceByBarnesLemma::notMatch];
        mg//Throw
    ];


multiGammaReduceBySecondBarnesLemma[numPlus_,numMinus_,denomPlus_,numPlusMinusSum_,numRest_,denomRest_,mg_,s_] :=
    If[ FreeQ[numRest,s]&&FreeQ[denomRest,s]&&Length[denomPlus]===1&&Simplify[denomPlus[[1]]-numPlusMinusSum]===0,
        multiGamma[
            Join[numRest,Flatten@Outer[Plus,numPlus,numMinus]],
            Join[denomRest,numPlusMinusSum-numPlus]
        ],
        (*Else*)
        Message[multiGammaReduceByBarnesLemma::notMatch];
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
