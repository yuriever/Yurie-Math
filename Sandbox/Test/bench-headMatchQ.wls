ClearAll[
    headMatchQShort, headMatchQSplit, headMatchQFast, headMatchQ0,
    headSameQShort, headSameQSplit, headSameQFast, headSameQ0,
    deepApply, bench
];

SetAttributes[headMatchQShort, HoldAllComplete];
headMatchQShort[target_, expr_] /;
    MatchQ[HoldComplete[expr], HoldPattern[HoldComplete[target]]] :=
    True;
headMatchQShort[target_, head_[___]] :=
    headMatchQShort[target, head];
headMatchQShort[_, _] :=
    False;

SetAttributes[headMatchQSplit, HoldAllComplete];
headMatchQSplit[target_, expr_] :=
    headMatchQ0[HoldComplete[target], HoldComplete[expr]];

headMatchQ0[HoldComplete[target_], HoldComplete[expr_]] /;
    MatchQ[HoldComplete[expr], HoldPattern[HoldComplete[target]]] :=
    True;
headMatchQ0[target_, HoldComplete[head_[___]]] :=
    headMatchQ0[target, HoldComplete[head]];
headMatchQ0[_, _] :=
    False;

SetAttributes[headMatchQFast, HoldAllComplete];
headMatchQFast[target_, expr_] :=
    headMatchQFast0[HoldComplete[target], HoldComplete[expr]];

headMatchQFast0[target_, expr_] /;
    MatchQ[expr, HoldPattern[target]] :=
    True;
headMatchQFast0[target_, HoldComplete[head_[___]]] :=
    headMatchQFast0[target, HoldComplete[head]];
headMatchQFast0[_, _] :=
    False;

SetAttributes[headSameQShort, HoldAllComplete];
headSameQShort[target_, expr_] /;
    HoldComplete[expr] === HoldComplete[target] :=
    True;
headSameQShort[target_, head_[___]] :=
    headSameQShort[target, head];
headSameQShort[_, _] :=
    False;

SetAttributes[headSameQSplit, HoldAllComplete];
headSameQSplit[target_, expr_] :=
    headSameQ0[HoldComplete[target], HoldComplete[expr]];

headSameQ0[target_, expr_] /;
    expr === target :=
    True;
headSameQ0[target_, HoldComplete[head_[___]]] :=
    headSameQ0[target, HoldComplete[head]];
headSameQ0[_, _] :=
    False;

SetAttributes[headSameQFast, HoldAllComplete];
headSameQFast[target_, expr_] :=
    headSameQFast0[HoldComplete[target], HoldComplete[expr]];

headSameQFast0[target_, expr_] /;
    expr === target :=
    True;
headSameQFast0[target_, HoldComplete[head_[___]]] :=
    headSameQFast0[target, HoldComplete[head]];
headSameQFast0[_, _] :=
    False;

deepApply[base_, 0] := base;
deepApply[base_, n_Integer?Positive] := Nest[#["arg"] &, base, n];

cases = {
    <|"name" -> "exact-hit", "target" -> HoldComplete[y[1]], "expr" -> HoldComplete[y[1]]|>,
    <|"name" -> "pattern-hit", "target" -> HoldComplete[y[_]], "expr" -> HoldComplete[deepApply[y[1], 12]]|>,
    <|"name" -> "pattern-miss-head", "target" -> HoldComplete[z[_]], "expr" -> HoldComplete[deepApply[y[1], 12]]|>,
    <|"name" -> "exact-miss-arg", "target" -> HoldComplete[y[2]], "expr" -> HoldComplete[deepApply[y[1], 12]]|>
};

sameCases = {
    <|"name" -> "exact-hit", "target" -> HoldComplete[y[1]], "expr" -> HoldComplete[y[1]]|>,
    <|"name" -> "deep-hit", "target" -> HoldComplete[y[1]], "expr" -> HoldComplete[deepApply[y[1], 12]]|>,
    <|"name" -> "miss-head", "target" -> HoldComplete[z[1]], "expr" -> HoldComplete[deepApply[y[1], 12]]|>,
    <|"name" -> "miss-arg", "target" -> HoldComplete[y[2]], "expr" -> HoldComplete[deepApply[y[1], 12]]|>
};

iterations = 20000;

bench[fun_, HoldComplete[target_], HoldComplete[expr_]] :=
    First @ RepeatedTiming[Do[fun[target, expr], {iterations}], 1];

results =
    Table[
        Module[{short, split},
            short = bench[headMatchQShort, case["target"], case["expr"]];
            split = bench[headMatchQSplit, case["target"], case["expr"]];
            <|
                "case" -> case["name"],
                "short" -> short,
                "split" -> split,
                "ratio_short_over_split" -> short/split
            |>
        ],
        {case, cases}
    ];

matchResultsExtended =
    Table[
        Module[{short, split, fast},
            short = bench[headMatchQShort, case["target"], case["expr"]];
            split = bench[headMatchQSplit, case["target"], case["expr"]];
            fast = bench[headMatchQFast, case["target"], case["expr"]];
            <|
                "case" -> case["name"],
                "short" -> short,
                "split" -> split,
                "fast" -> fast,
                "ratio_short_over_fast" -> short/fast
            |>
        ],
        {case, cases}
    ];

sameResults =
    Table[
        Module[{short, split, fast},
            short = bench[headSameQShort, case["target"], case["expr"]];
            split = bench[headSameQSplit, case["target"], case["expr"]];
            fast = bench[headSameQFast, case["target"], case["expr"]];
            <|
                "case" -> case["name"],
                "short" -> short,
                "split" -> split,
                "fast" -> fast,
                "ratio_short_over_fast" -> short/fast
            |>
        ],
        {case, sameCases}
    ];

Print["Iterations per timing: ", iterations];
Print["MatchQ results (seconds):"];
Scan[Print, results];
Print["MatchQ extended results (seconds):"];
Scan[Print, matchResultsExtended];
Print["SameQ results (seconds):"];
Scan[Print, sameResults];
