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
    "\nmatJordan[dim_Integer,a_Diagonal,b_OffDiagonal:1].";

matAngularMomentum::usage =
    "spin-j representation of angular momentum in the unit of hbar."<>
    "\nThe column/row indices run from j to -j.";

matPauli::usage =
    "Pauli matrix.";

matDirac::usage =
    "Dirac matrix."<>
    "\nThe default metric signature is (-,+,+,+).";


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


matPauli[1] =
    matPauli["x"] =
        {{0,1},{1,0}};

matPauli[2] =
    matPauli["y"] =
        {{0,-I},{I,0}};

matPauli[3] =
    matPauli["z"] =
        {{1,0},{0,-1}};

matPauli[0] =
    matPauli["t"] =
        {{1,0},{0,1}};


matDirac[i:1|2|3] :=
    matDirac[i] =
        {{0,matPauli[i]},{-matPauli[i],0}}//ArrayFlatten;

matDirac[0] =
    {{0,0,1,0},{0,0,0,1},{1,0,0,0},{0,1,0,0}};


matDirac[5] =
    {{-1,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,1}};


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
