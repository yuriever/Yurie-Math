(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Matrix`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


matSquareQ::usage =
    "matSquareQ[matrix]: test if the matrix is square.";

matComm::usage =
    "matComm[a, b]: compute the commutator of the two matrices."<>
    "\n"<>
    "Sketch: a.b - b.a.";


matJordan::usage =
    "matJordan[dim, a, b]: construct a Jordan matrix of specified dimension."<>
    "\n"<>
    "Def[a]: the common diagonal element."<>
    "\n"<>
    "Def[b]: the common super-diagonal element."<>
    "\n"<>
    "Default[b]: 1.";

matAngularMomentum::usage =
    "matAngularMomentum[j][direction]: generate angular momentum matrices for the spin-j representation."<>
    "\n"<>
    "Value[direction]: {\"x\", \"y\", \"z\"|0, 1, -1}."<>
    "\n"<>
    "Hint: the column/row indices run from j to -j.";


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


matJordan[dim_Integer,a_,b_,Up] :=
    Normal@SparseArray[
        {
            {i_,i_}:>a,
            {i_,j_}/;j==i+1:>b
        },
        {dim,dim}
    ];

matJordan[dim_Integer,a_,b_,Down] :=
    Normal@SparseArray[
        {
            {i_,i_}:>a,
            {i_,j_}/;j==i-1:>b
        },
        {dim,dim}
    ];

matJordan[dim_Integer,a_] :=
    matJordan[dim,a,1,Up];

matJordan[dim_Integer,a_,b_] :=
    matJordan[dim,a,b,Up];


matAngularMomentum[j_]["z"|0] :=
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
