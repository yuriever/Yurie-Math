(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Gamma`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


gammaSimplify::usage =
    "gammaSimplify[expr]: simplify Gamma functions in the expression."<>
    "\n"<>
    "Sketch: Developer`GammaSimplify.";

gammaShift::usage =
    "gammaShift[var, shift][expr]: shift the argument of Gamma functions by the specified integer.";

gammaFrom::usage =
    "gammaFrom[expr, opts]: expand everything to Gamma functions.";

gammaSeparate::usage =
    "gammaSeparate[expr]: separate a product into Gamma functions and the rest.";

gammaTakeResidue::usage =
    "gammaTakeResidue[var, index, gamma, sign, opts][expr]: take residue of a series of poles from the Gamma factor."<>
    "\n"<>
    "gammaTakeResidue[var, index->n, gamma, sign, opts][expr]: specify one pole in the series."<>
    "\n"<>
    "Info[index]: the index of the poles."<>
    "\n"<>
    "Info[gamma]: the argument of the Gamma function."<>
    "\n"<>
    "Info[sign]: the direction of contour."<>
    "\n"<>
    "Value[\"ShowPole\"]: {True, False, Full}."<>
    "\n"<>
    "Default[sign]: 1.";


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
    "ShowPole"->True,
    "PlusListable"->False
};


(* ::Subsection:: *)
(*Message*)


gammaFrom::KeyNotFound =
    "The transformations are expected as a subset of ``.";


gammaTakeResidue::Listable =
    "Turn on the option \"Listable\" to handle nested list.";

gammaTakeResidue::PlusListable =
    "Turn on the option \"PlusListable\" to handle sum.";

gammaTakeResidue::InvalidExpr =
    "A product involving Gamma functions is expected for the expression ``.";

gammaTakeResidue::IndexConflict =
    "The index `` conflicts with the expression ``.";

gammaTakeResidue::NonlinearInVar =
    "The argument `` should be linear to the variable ``.";

gammaTakeResidue::GammaNotInExpr =
    "The factor `` does not appear in the expression ``.";

gammaTakeResidue::InvalidSign =
    "The sign `` should be either 1|Left or -1|Right.";


multiGammaReduceByBarnesLemma::NotMatch =
    "The multi-Gamma symbol cannot be reduced by the Barnes lemmas."

multiGammaReduceByBarnesLemma::NotProduct =
    "The expression is expected to be a product involving Gamma functions.";


(* ::Subsection:: *)
(*gammaSimplify*)


gammaSimplify :=
    Developer`GammaSimplify;


(* ::Subsection:: *)
(*gammaShift*)


gammaShift[Rule[gm_,shift_]][expr_] :=
    expr//ReplaceAll[{
        Gamma[arg:gm]:>Simplify@Gamma[arg+shift]/Simplify@Pochhammer[arg,shift]
    }];


(* ::Subsection:: *)
(*gammaFrom*)


(* ::Subsubsection:: *)
(*Main*)


gammaFrom[expr_,OptionsPattern[]] :=
    Module[{opt = OptionValue["Transformation"],ruleList},
        ruleList =
            Which[
                opt===Automatic,
                    gammaDataValueList,
                Head[opt]===List&&SubsetQ[gammaDataKeyList,opt],
                    gammaData//Lookup[opt]//Flatten,
                True,
                    Message[gammaFrom::KeyNotFound,gammaDataKeyList];
                    expr//Throw
            ];
        expr//ReplaceAll[ruleList]//
            ReplaceAll[gm_gamma:>Simplify@gm]//
                gammaActivateHead[OptionValue["ActivateGamma"]]
    ]//Catch;


(* ::Subsubsection:: *)
(*Kernel*)


gamma::usage =
    "internal head of Gamma.";


gammaData = <|
    "Trig"->{
        Sin[x_]:>
            π/(gamma[x/π] gamma[1-x/π]),
        Cos[x_]:>
            (*Sin[x+π/2]*)
            π/(gamma[1/2-x/π] gamma[1/2+x/π]),
        Tan[x_]:>
            (*Sin[x]/Sin[x+π/2]*)
            (gamma[1/2-x/π] gamma[1/2+x/π])/(gamma[x/π] gamma[1-x/π]),
        Cot[x_]:>
            (*Sin[x+π/2]/Sin[x]*)
            (gamma[x/π] gamma[1-x/π])/(gamma[1/2-x/π] gamma[1/2+x/π]),
        Csc[x_]:>
            (*1/Sin[x]*)
            (gamma[x/π] gamma[1-x/π])/π,
        Sec[x_]:>
            (*1/Sin[x+π/2]*)
            (gamma[1/2-x/π] gamma[1/2+x/π])/π
    },
    "Factorial"->{
        Factorial[x_]:>gamma[x+1]
    },
    "Binomial"->{
        Binomial[a_,n_]:>
            gamma[a+1]/(gamma[n+1]*gamma[a-n+1])
    },
    "Beta"->{
        Beta[a_,b_]:>
            (gamma[a]*gamma[b])/gamma[a+b]
    },
    "Pochhammer"->{
        Pochhammer[a_,n_]:>
            gamma[a+n]/gamma[a]
    },
    "FactorialPower"->{
        FactorialPower[a_,n_]:>
            gamma[a+1]/gamma[a-n+1]
    },
    "MultiGamma"->{
        HoldPattern[multiGamma[num_List,denom_List]]:>
            (Times@@Map[gamma,num])/(Times@@Map[gamma,denom])
    }
|>;


(* ::Subsubsection:: *)
(*Helper*)


gammaDataKeyList =
    gammaData//Keys;


gammaDataValueList =
    gammaData//Values//Flatten;


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
    Module[{expr1},
        expr1 =
            If[FreeQ[expr,_multiGamma],
                expr,
                (* Else *)
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
(*gammaTakeResidue*)


(* ::Subsubsection:: *)
(*Main*)


gammaTakeResidue[args__,opts:OptionsPattern[]][expr:Except[_List|_Plus]] :=
    gammaTakeResidueKernel[args,opts][expr];


(* _List is fully listable. *)
(* _Plus is listable at the top level if the option "PlusListable" is turned on. *)

gammaTakeResidue[args__,opts:OptionsPattern[]][expr_List] :=
    Map[gammaTakeResidue[args,opts],expr];

gammaTakeResidue[args__,opts:OptionsPattern[]][expr_Plus] :=
    If[OptionValue["PlusListable"]===True,
        (* Then *)
        Map[gammaTakeResidueKernel[args,opts],expr],
        (* Else *)
        Message[gammaTakeResidue::PlusListable];
        expr
    ];


gammaTakeResidue[] :=
    CellPrint@{
        ExpressionCell[
            ToExpression[
                "gammaTakeResidue[x,n,-x,Right]@Gamma[-x]",
                 StandardForm,
                 Defer
            ],
            "Code"
        ],
        ExpressionCell[
            ToExpression[
                "(-1)^n/(2 n!)",
                StandardForm,
                Defer
            ],
            "Output"
        ]
    };


(* ::Subsubsection:: *)
(*Helper*)


gammaTakeResidueKernel[var_,ind_,gm_,opts:OptionsPattern[gammaTakeResidue]][expr_] :=
    gammaTakeResidueKernel[var,ind,gm,1,opts][expr];

gammaTakeResidueKernel[var_,ind_,gm_,sign_,OptionsPattern[gammaTakeResidue]][expr_] :=
    (
        Message[gammaTakeResidue::InvalidExpr,Short[expr]];
        expr
    );

gammaTakeResidueKernel[var_,ind_,gm_,sign_,OptionsPattern[gammaTakeResidue]][expr:_Gamma|_multiGamma|_Times|_Power] :=
    Module[{expr1,isSpecificPole,ind1,pos,solution,residue},
        expr1 =
            expr//gammaTakeResidueHandleMultiGamma;
        {isSpecificPole,ind1,pos} =
            ind//gammaTakeResidueGetIndex;
        gammaTakeResidueCheck[var,ind1,gm,sign][expr1];
        solution =
            Part[Solve[gm==-ind1,{var}],1,1];
        residue =
            If[OptionValue["SimplePole"]===True,
                Residue[Gamma[gm],{var,solution[[2]]},Assumptions->ind1>=0&&Element[ind1,Integers]]*
                    ReplaceAll[expr1/Gamma[gm],solution],
                (* Else *)
                Residue[expr1,{var,solution[[2]]},Assumptions->ind1>=0&&Element[ind1,Integers]]
            ];
        gammaTakeResidueShowPoleData[OptionValue["ShowPole"]][solution,var,ind1,expr1];
        residueSign[sign]*residue//
            takeSpecificPole[isSpecificPole][ind1,pos]//
            ReplaceAll[gm1_Gamma:>Simplify[gm1]]//
            handleResidueWithINT[expr1,var]
    ]//Catch;


gammaTakeResidueHandleMultiGamma[expr_] :=
    If[FreeQ[expr,_multiGamma],
        expr,
        (* Else *)
        gammaFrom[expr,"Transformation"->{"MultiGamma"}]
    ];


gammaTakeResidueGetIndex[(Rule|List)[ind_,pos_]] :=
    {True,ind,pos};

gammaTakeResidueGetIndex[ind_] :=
    {False,ind,Null};


gammaTakeResidueCheck[var_,ind_,gm_,sign_][expr_] :=
    Which[
        !FreeQ[expr,ind],
            Message[gammaTakeResidue::IndexConflict,ind,Short[expr]];
            expr//Throw,
        !linearQ[gm,{var}],
            Message[gammaTakeResidue::NonlinearInVar,gm,var];
            expr//Throw,
        FreeQ[expr,Gamma[gm]],
            Message[gammaTakeResidue::GammaNotInExpr,HoldForm[Gamma][gm],Short[expr]];
            expr//Throw,
        !MatchQ[sign,1|-1|Left|Right],
            Message[gammaTakeResidue::InvalidSign,sign];
            expr//Throw
    ];


gammaTakeResidueShowPoleData[True][solution_,___] :=
    Echo[solution];

gammaTakeResidueShowPoleData[Full][solution_,var_,ind_,expr1_] :=
    Module[{sign,gammaList},

        sign =
            Simplify[Sign@Coefficient[solution[[2]],ind]];
        gammaList =
            Cases[expr1,Gamma[arg_]/;!FreeQ[arg,var]:>arg,All]//Map[{#,Simplify@ReplaceAll[#,solution],Exponent[expr1,Gamma[#]]}&];

        gammaList =
            Switch[sign,
                1,
                    separate[Simplify[Coefficient[#[[1]],var]>0]&][gammaList],
                -1,
                    separate[Simplify[Coefficient[#[[1]],var]<0]&][gammaList],
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


takeSpecificPole[True][ind_,pos_] :=
    ReplaceAll[ind->pos];

takeSpecificPole[False][ind_,pos_] :=
    Identity;


handleResidueWithINT[expr_,var_][residue_] :=
    If[FreeQ[expr,_INT],
        residue,
        (* Else *)
        residue/INT[var]
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

multiGamma/:Power[HoldPattern[multiGamma[num_List,denom_List]],-1] :=
    multiGamma[denom,num];

multiGamma/:Power[HoldPattern[multiGamma[num_List,denom_List]],n_Integer]/;n>=2 :=
    multiGamma[Catenate@ConstantArray[num,n],Catenate@ConstantArray[denom,n]];

multiGamma/:Power[HoldPattern[multiGamma[num_List,denom_List]],n_Integer]/;n<=-2 :=
    multiGamma[Catenate@ConstantArray[denom,-n],Catenate@ConstantArray[num,-n]];

(* Verbatim is necessary here to prevent the attributes of Times. *)
multiGamma/:prod:HoldPattern[Verbatim[Times][___,_multiGamma,___,_multiGamma,___]] :=
    With[{
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
    With[{
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
    With[{t1 = 2Tally[list1],t2 = Tally@Join[list1,list2]},
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
    k*multiGammaReduceByBarnesLemma[s][mg]//INTCancel[s];

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
    Module[{num,denom,numPlus,numMinus,denomPlus,numPlusMinusSum,numRest,denomRest},
        {num,denom} =
            Collect[{mg[[1]],mg[[2]]},s];
        numPlus =
            Cases[num,s+a_.:>a,{1}];
        numMinus =
            Cases[num,-s+b_.:>b,{1}];
        Which[
            (* First Barnes lemma. *)
            Length[numPlus]===Length[numMinus]===2,
                numRest =
                    num//DeleteCases[s+_.|-s+_.];
                denomRest =
                    denom;
                multiGammaReduceByFirstBarnesLemma[numPlus,numMinus,numRest,denomRest,mg,s],
            (* Second Barnes lemma. *)
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
    If[FreeQ[numRest,s]&&FreeQ[denomRest,s],
        multiGamma[
            Join[numRest,Flatten@Outer[Plus,numPlus,numMinus]],
            Join[denomRest,{Total[{numPlus,numMinus},2]}]
        ],
        (* Else *)
        Message[multiGammaReduceByBarnesLemma::NotMatch];
        mg//Throw
    ];


multiGammaReduceBySecondBarnesLemma[numPlus_,numMinus_,denomPlus_,numPlusMinusSum_,numRest_,denomRest_,mg_,s_] :=
    If[FreeQ[numRest,s]&&FreeQ[denomRest,s]&&Length[denomPlus]===1&&Simplify[denomPlus[[1]]-numPlusMinusSum]===0,
        multiGamma[
            Join[numRest,Flatten@Outer[Plus,numPlus,numMinus]],
            Join[denomRest,numPlusMinusSum-numPlus]
        ],
        (* Else *)
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
