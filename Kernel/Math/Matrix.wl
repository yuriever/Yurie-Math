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

jordanBlock::usage =
    "jordanBlock[dim_Integer,a_OffDiagonal,b_Diagonal:1]\n"<>
    "jordanBlock[dim_Integer,a_].";

sparseBlockMatrix::usage =
    "SparseArray`SparseBlockMatrix.";


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


jordanBlock[dim_Integer,a_,b_:1] :=
    SparseArray[
        {
            {i_,i_}:>a,
            {i_,j_}/;j==i+1:>b
        },
        {dim,dim}
    ];

jordanBlock[dim_Integer,a_] :=
    jordanBlock[dim,a,1];


sparseBlockMatrix :=
    SparseArray`SparseBlockMatrix;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
