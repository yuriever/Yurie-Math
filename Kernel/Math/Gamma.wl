(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Gamma`"];


Needs["Yurie`Math`"];

Needs["Yurie`Math`Constant`"];


(* ::Section:: *)
(*Public*)


gammaFrom::usage =
    "expand everything to Gamma factors.";

gammaSplit::usage =
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
    "ShowPole"->False
};


multiGammaSimplify//Options = {
    "Assumptions":>$Assumptions
};


(* ::Subsection:: *)
(*Message*)


gammaFrom::noSuchKey =
    "the transformations are expected as a subset of ``.";


gammaTakeResidue::indexConflict =
    "the index `` conflicts with the expression.";

gammaTakeResidue::gammaNotMatchVar =
    "the argument of the factor `` should be a linear function of the variable ``.";

gammaTakeResidue::gammaNotInExpr =
    "the factor `` does not appear in the expression, and the residue vanishes.";


multiGammaReduceByBarnesLemma::notMatch =
    "the multi-Gamma symbol cannot be reduced by the Barnes lemmas."

multiGammaReduceByBarnesLemma::notProduct =
    "this function only handles product-type expression."


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
        expr//ReplaceAll[ruleList]//gammaFactorSimplify2//gammaActivateHead[OptionValue["ActivateGamma"]]
    ]//Catch;


(* ::Subsubsection:: *)
(*Helper*)


gammaFactorSimplify2[expr_] :=
    expr//ReplaceAll[gm_gamma:>Simplify@gm];


gammaActivateHead[True][expr_] :=
    expr//ReplaceAll[gamma->Gamma];

gammaActivateHead[False][expr_] :=
    expr//ReplaceAll[gamma->Inactive[Gamma]];


(* ::Subsection:: *)
(*gammaSplit*)


gammaSplit[expr_Gamma] :=
    {expr,1};

gammaSplit[expr_Times] :=
    {
        Select[expr,!FreeQ[#,Gamma]&],
        Select[expr,FreeQ[Gamma]]
    };

gammaSplit[expr_] :=
    {1,expr};


(* ::Subsection:: *)
(*gammaResidue*)


(* ::Subsubsection:: *)
(*Main*)


gammaTakeResidue[variable_,index_,gm_Gamma,OptionsPattern[]][expr_] :=
    Module[ {solution,residue},
        gammaTakeResidueCheckAndThrow[variable,index,gm][expr];
        solution =
            Part[Solve[gm[[1]]==-index,{variable}],1,1];
        residue =
            If[ OptionValue["SimplePole"]===True,
                Residue[gm,{variable,solution[[2]]},Assumptions->index>=0&&Element[index,Integers]]*
                    ReplaceAll[expr/gm,solution],
                (*Else*)
                Residue[expr,{variable,solution[[2]]},Assumptions->index>=0&&Element[index,Integers]]
            ]//gammaFactorSimplify;
        gammaTakeResidueIfShowPole[OptionValue["ShowPole"]][residue,solution]
    ]//Catch;


gammaTakeResidue[] :=
    CellPrint@{
        ExpressionCell[
            ToExpression[
                "gammaTakeResidue[x,n,Gamma[2x+1],\"ShowPole\"->True]@Gamma[2x+1]",
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


gammaTakeResidueCheckAndThrow[variable_,index_,gm_Gamma][expr_] :=
    Which[
        !FreeQ[expr,index],
            Message[gammaTakeResidue::indexConflict,index];
            Throw[expr],
        !Internal`LinearQ[gm[[1]],{variable}],
            Message[gammaTakeResidue::gammaNotMatchVar,gm,variable];
            Throw[expr],
        FreeQ[expr,gm],
            Message[gammaTakeResidue::gammaNotInExpr,gm];
            Throw[0]
    ];


gammaFactorSimplify[expr_] :=
    expr//ReplaceAll[gm_Gamma:>Simplify@gm];


gammaTakeResidueIfShowPole[True][residue_,solution_] :=
    {residue,solution};

gammaTakeResidueIfShowPole[False][residue_,solution_] :=
    residue;

gammaTakeResidueIfShowPole[fun_Symbol][residue_,solution_] :=
    (
        fun[solution];
        residue
    );


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
(*multiGammaReduce*)


(* ::Subsubsection:: *)
(*Main*)


multiGammaSimplify[expr_,OptionsPattern[]] :=
    ReplaceAll[expr,
        mg_multiGamma:>multiGammaFunctionExpand[OptionValue["Assumptions"]][mg]
    ];


(* ::Subsubsection:: *)
(*Helper*)


multiGammaFunctionExpand[assumption_][mg_] :=
    mg//gammaFrom//Simplify//FunctionExpand//Simplify[#,Assumptions->assumption]&//gammaFrom//multiGammaFrom;


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
