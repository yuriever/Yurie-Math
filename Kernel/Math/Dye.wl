(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Dye`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


dye::usage =
    "dye[expr]: color the subexpressions at the first level.";

dyeIn::usage =
    "dyeIn[level][expr]: color the subexpressions at the specified level."<>
    "\n"<>
    "dyeIn[color, level][expr]: specify the color.";

dyeBy::usage =
    "dyeBy[pattern, level, opts][expr]: color the subexpressions matching the pattern."<>
    "\n"<>
    "dyeBy[color, pattern, level, opts][expr]: specify the color."<>
    "\n"<>
    "Default[level]: All.";

dyeAt::usage =
    "dyeAt[positions][expr]: color the subexpressions at the specified positions."<>
    "\n"<>
    "dyeAt[color, positions][expr]: specify the color.";

dyeOff::usage =
    "dyeOff[expr]: remove the coloring applied by dye functions.";


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


dyeIn[level_?levelQ][expr_] :=
    dyeAtPosition[expr,Position[expr,_,level,Heads->False]];

dyeIn[color_?ColorQ,level_?levelQ][expr_] :=
    dyeAtPosition[expr,Position[expr,_,level,Heads->False],color];


dyeBy[pattern_,args___,opts:OptionsPattern[]][expr_]/;!ColorQ[pattern] :=
    dyeAtPosition[expr,Position[expr,pattern,args,FilterRules[{opts,Options@dyeBy},Options@Position]]];

dyeBy[color_?ColorQ,pattern_,args___,opts:OptionsPattern[]][expr_]/;!ColorQ[pattern] :=
    dyeAtPosition[expr,Position[expr,pattern,args,FilterRules[{opts,Options@dyeBy},Options@Position]],color];


dyeAt[posList_List][expr_] :=
    dyeAtPosition[expr,posList];

dyeAt[color_?ColorQ,posList_List][expr_] :=
    dyeAtPosition[expr,posList,color];


dyeOff[expr_] :=
    expr//ReplaceRepeated[dyed[subexpr_,___]:>subexpr];


(* ::Subsection:: *)
(*Helper*)


dyeAtPosition[expr_,posList_] :=
    dyeAtPositionByColorFunction[expr,posList,rainbow];

dyeAtPosition[expr_,posList_,color_] :=
    dyeAtPositionByColorFunction[expr,posList,singleColor[color]];


dyeAtPositionByColorFunction[expr_,posList_,colorFunction_] :=
    With[{subexprList = Extract[expr,posList],colorList = colorFunction[posList]},
        ReplacePart[
            expr,
            MapThread[#1->dyed[#2,#3]&,{posList,subexprList,colorList}]
        ]
    ];


singleColor[color_][posList_] :=
    Module[{hue,saturationList,brightness},
        {hue,brightness} =
            Extract[ColorConvert[color,"HSB"],{{1},{3}}];
        saturationList =
            Map[Length,posList]//(#-Min[#])&//(#*1/20+1/5)&;
        Map[Hue[hue,#,brightness]&,saturationList]
    ];


rainbow[posList_] :=
    Module[{HSBList,opacityList},
        HSBList =
            Range[0,Length[posList]-1]/(Length[posList]+1/10);
        opacityList =
            Map[Length,posList]/5+1/10;
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
