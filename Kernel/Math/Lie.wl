(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`Math`Lie`"];


Needs["Yurie`Math`"];


(* ::Section:: *)
(*Public*)


lie::usage =
    "simple Lie algebras.";

lieSimpleRoot::usage =
    "orthogonal simple roots of simple Lie algebras.";

lieCartan::usage =
    "Cartan matrix of simple Lie algebras.";

lieCartanInverse::usage =
    "inverse Cartan matrix of simple Lie algebras.";

lieDynkinDiagram::usage =
    "Dynkin diagram of simple Lie algebras.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Helper*)


lieRank[lie[_,n_Integer]] :=
    n;


lieType[lie[type_String,_]] :=
    type;


lieSimpleQ[lie["A",n_Integer]]/;n>=1 :=
    True;

lieSimpleQ[lie["B",n_Integer]]/;n>=1 :=
    True;

lieSimpleQ[lie["C",n_Integer]]/;n>=1 :=
    True;

lieSimpleQ[lie["D",n_Integer]]/;n>=3 :=
    True;

lieSimpleQ[lie["E",n_Integer]]/;4<=n<=8 :=
    True;

lieSimpleQ[lie["F",4]] :=
    True;

lieSimpleQ[lie["G",2]] :=
    True;

lieSimpleQ[_] :=
    False;


(* ::Subsection:: *)
(*lie*)


(*exceptional isomorphisms*)

lie["B",1] = lie["A",1];
lie["C",1] = lie["A",1];
lie["C",2] = lie["B",2];
(*lie["D",2] = lie[{"A",1},{"A",1}];*)
lie["D",3] = lie["A",3];
(*lie["E",3] = lie[{"A",1},{"A",2}];*)
lie["E",4] = lie["A",4];
lie["E",5] = lie["D",5];


lie["SL",n_Integer]/;n>=2 :=
    lie["A",n-1];

lie["SP",n_Integer]/;n>=2&&EvenQ@n :=
    lie["C",n/2];

lie["SO",n_Integer]/;n>=3&&OddQ@n :=
    lie["B",(n-1)/2];

lie["SO",n_Integer]/;n>=4&&EvenQ@n :=
    lie["D",n/2];


(* ::Subsection:: *)
(*lieSimpleRoot*)


(* ::Subsubsection:: *)
(*Main*)


lieSimpleRoot[lie["A",n_Integer]?lieSimpleQ] :=
    lieSimpleRoot[lie["A",n]] =
        SparseArray[
            {
                Band[{1,1}]->1,
                Band[{1,2}]->-1
            },
            {n,n+1}
        ];

lieSimpleRoot[lie["B",n_Integer]?lieSimpleQ] :=
    lieSimpleRoot[lie["B",n]] =
        SparseArray[
            {
                Band[{1,1}]->1,
                Band[{1,2}]->-1
            },
            {n,n}
        ];

lieSimpleRoot[lie["C",n_Integer]?lieSimpleQ] :=
    lieSimpleRoot[lie["C",n]] =
        SparseArray[
            {
                Band[{n,n}]->2,
                Band[{1,1}]->1,
                Band[{1,2}]->-1
            },
            {n,n}
        ];

lieSimpleRoot[lie["D",n_Integer]?lieSimpleQ] :=
    lieSimpleRoot[lie["D",n]] =
        SparseArray[
            {
                Band[{n,n-1}]->1,
                Band[{1,1}]->1,
                Band[{1,2}]->-1
            },
            {n,n}
        ];

lieSimpleRoot[lie["E",n_Integer]?lieSimpleQ] :=
    lieSimpleRoot[lie["E",n]] =
        Join[
            {{1/2,Sequence@@ConstantArray[-1/2,6],1/2}},
            SparseArray[
                {
                    {n-1,1}->1,
                    {n-1,2}->1,
                    Band[{1,1},{n-2,n-2}]->-1,
                    Band[{1,2},{n-1,n-1}]->1
                },
                {n-1,8}
            ]
        ];

lieSimpleRoot[lie["F",4]] =
    {
        {1,-1,0,0},
        {0,1,-1,0},
        {0,0,1,0},
        {-1/2,-1/2,-1/2,-1/2}
    };

lieSimpleRoot[lie["G",2]] =
    {
        {1,-1,0},
        {-2,1,1}
    };


(* ::Subsection:: *)
(*lieCartan*)


lieCartan[alg_lie?lieSimpleQ] :=
    lieCartan[alg] =
        2 Outer[Dot[#1,#2]/Dot[#2,#2]&,#,#,1]&@lieSimpleRoot[alg];


lieCartanInverse[alg_lie?lieSimpleQ] :=
    lieCartanInverse[alg] =
        Inverse@lieCartan[alg];


(* ::Subsection:: *)
(*lieDynkinDiagram*)


lieDynkinDiagram[alg_lie?lieSimpleQ] :=
    AdjacencyGraph[
        2 IdentityMatrix@lieRank@alg-lieCartan@alg,
        DirectedEdges->Automatic,
        GraphLayout->"SpringElectricalEmbedding"
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
