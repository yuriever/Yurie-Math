(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Dye`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


dye::usage =
    "dye[expr_]: color the elements at the first level of expression.";

dyeIn::usage =
    "dyeIn[levelspec_:1][expr_]: color the elements at the specific levels of expression.";

dyeBy::usage =
    "dyeBy[pattern_,levelspec_,n_][expr_]: color the occurrences of pattern in expression.";

dyeAt::usage =
    "dyeAt[positions_][expr_]: color the expression at the specified positions in expression.";

dyeOff::usage =
    "dyeOff[expr_]: eliminate the colors from dye.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


dyeBy//Options =
    Options@Position;


(* ::Subsection:: *)
(*Main*)


dye[expr_] :=
    dyeAtPosition[expr,Position[expr,_,{1},Heads->False]];


dyeIn[levelspec:_:1][expr_]/;!ColorQ[levelspec] :=
    dyeAtPosition[expr,Position[expr,_,levelspec,Heads->False]];

dyeIn[color_,levelspec_:1][expr_]/;ColorQ[color]&&!ColorQ[levelspec] :=
    dyeAtPosition[expr,Position[expr,_,levelspec,Heads->False],color];


dyeBy[pattern_,args___,opts:OptionsPattern[]][expr_]/;!ColorQ[pattern] :=
    dyeAtPosition[expr,Position[expr,pattern,args,FilterRules[{opts,Options@dyeBy},Options@Position]]];

dyeBy[color_,pattern_,args___,opts:OptionsPattern[]][expr_]/;ColorQ[color]&&!ColorQ[pattern] :=
    dyeAtPosition[expr,Position[expr,pattern,args,FilterRules[{opts,Options@dyeBy},Options@Position]],color];


dyeAt[positionList_][expr_]/;!ColorQ[positionList] :=
    dyeAtPosition[expr,positionList];

dyeAt[color_,positionList_][expr_]/;ColorQ[color]&&!ColorQ[positionList] :=
    dyeAtPosition[expr,positionList,color];


dyeOff[expr_] :=
    expr//ReplaceRepeated[dyed[subexpr_,___]:>subexpr];


(* ::Subsection:: *)
(*Helper*)


dyeAtPosition[expr_,positionList_] :=
    dyeAtPositionByColorFunction[expr,positionList,rainbow];

dyeAtPosition[expr_,positionList_,color_] :=
    dyeAtPositionByColorFunction[expr,positionList,singleColor[color]];


dyeAtPositionByColorFunction[expr_,positionList_,colorFunction_] :=
    With[ {subexprList = Extract[expr,positionList],colorList = colorFunction[positionList]},
        ReplacePart[
            expr,
            MapThread[#1->dyed[#2,#3]&,{positionList,subexprList,colorList}]
        ]
    ];


singleColor[color_][positionList_] :=
    Module[ {hue,saturationList,brightness},
        {hue,brightness} =
            Extract[ColorConvert[color,"HSB"],{{1},{3}}];
        saturationList =
            Map[Length,positionList]//(#-Min[#])&//(#*1/20+1/5)&;
        Map[Hue[hue,#,brightness]&,saturationList]
    ];


rainbow[positionList_] :=
    Module[ {HSBList,opacityList},
        HSBList =
            Range[0,Length[positionList]-1]/(Length[positionList]+1/10);
        opacityList =
            Map[Length,positionList]/5+1/10;
        MapThread[Hue[#1,0.5,1,#2]&,{HSBList,opacityList}]
    ];


dyed/:Format[dyed[expr_]] :=
    expr;

dyed/:Format[dyed[expr_,color_]] :=
    Highlighted[
        expr,
        Background->color,
        StripOnInput->True
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
