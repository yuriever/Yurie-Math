(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Matrix`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


matSquareQ::usage =
    "testing if is a square matrix.";

matComm::usage =
    "matComm[a,b]=a.b-b.a.";


matJordan::usage =
    "Jordan matrix."<>
    "\n"<>
    "matJordan[dim_Integer,a_Diagonal,b_OffDiagonal:1].";

matAngularMomentum::usage =
    "spin-j representation of angular momentum in the unit of hbar."<>
    "\n"<>
    "The column/row indices run from j to -j.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


matSquareQ[x_?MatrixQ] :=
    SameQ@@Dimensions[x];

matSquareQ[_] :=
    False;


matComm[x_,y_] :=
    x . y-y . x;


matJordan[dim_Integer,a_,b_:1] :=
    Normal@SparseArray[
        {
            {i_,i_}:>a,
            {i_,j_}/;j==i+1:>b
        },
        {dim,dim}
    ];

matJordan[dim_Integer,a_] :=
    matJordan[dim,a,1];


matAngularMomentum[j_]["z"] :=
    matAngularMomentum[j]["z"] =
        Normal@SparseArray[{{i_,i_}:>id[j][i]},{dim[j],dim[j]}];

matAngularMomentum[j_][-1] :=
    matAngularMomentum[j][-1] =
        Normal@SparseArray[{{i1_,i_}/;i-i1==-1:>Sqrt[(j-id[j][i]+1)(j+id[j][i])]},{dim[j],dim[j]}];

matAngularMomentum[j_][1] :=
    matAngularMomentum[j][1] =
        Normal@SparseArray[{{i1_,i_}/;i-i1==1:>Sqrt[(j+id[j][i]+1)(j-id[j][i])]},{dim[j],dim[j]}];

matAngularMomentum[j_]["x"] :=
    matAngularMomentum[j]["x"] =
        (matAngularMomentum[j][1]+matAngularMomentum[j][-1])/2;

matAngularMomentum[j_]["y"] :=
    matAngularMomentum[j]["y"] =
        (matAngularMomentum[j][1]-matAngularMomentum[j][-1])/(2 I);


dim[j_] :=
    2j+1;

id[j_][i_] :=
    1-i+j;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
