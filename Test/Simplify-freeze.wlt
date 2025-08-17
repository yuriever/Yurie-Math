

(* Simplify-freeze.nb *)

VerificationTest[
    Begin["Global`"];
    ClearAll["`*"]
    ,
    Null
    ,
    TestID->"[0] Simplify-freeze.nb"
]

VerificationTest[
    Get["Yurie`Base`"]; 
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"[1] Simplify-freeze.nb"
]

VerificationTest[
    freeze[a -> Negative, PowerExpand][((-a)*b)^(Δ - 1)]
    ,
    (-a)^(-1 + Δ)*b^(-1 + Δ)
    ,
    TestID->"[2] Simplify-freeze.nb"
]

VerificationTest[
    freeze[b -> Negative, PowerExpand][((-a)*b)^(Δ - 1)]
    ,
    a^(-1 + Δ)*(-b)^(-1 + Δ)
    ,
    TestID->"[3] Simplify-freeze.nb"
]

VerificationTest[
    freeze[a | b -> Negative, PowerExpand][((-a)*b)^(Δ - 1)]
    ,
    (-1)^(-1 + Δ)*(-a)^(-1 + Δ)*(-b)^(-1 + Δ)
    ,
    TestID->"[4] Simplify-freeze.nb"
]

VerificationTest[
    freeze[{a -> {f, g}, b -> Negative}, PowerExpand][((-a)*a[]*b)^(Δ - 1)]
    ,
    (-b)^(-1 + Δ)*a[]^(-1 + Δ)*f[g[a]]^(-1 + Δ)
    ,
    TestID->"[5] Simplify-freeze.nb"
]

VerificationTest[
    freeze[{a -> {f, g}, b -> {h, k}}, PowerExpand][((-a)*a[]*b)^(Δ - 1)]
    ,
    (-1)^(-1 + Δ)*a[]^(-1 + Δ)*f[g[a]]^(-1 + Δ)*h[k[b]]^(-1 + Δ)
    ,
    TestID->"[6] Simplify-freeze.nb"
]

VerificationTest[
    freeze[_Rule -> {f, g}, Identity][{a -> b, c -> d, {a, b}}]
    ,
    {f[g[a -> b]], f[g[c -> d]], {a, b}}
    ,
    TestID->"[7] Simplify-freeze.nb"
]

VerificationTest[
    fixTemporarySymbol["sub$"][Catch[freeze[_Rule, Throw][{a -> b, c -> d, {a, b}}]]]
    ,
    {C[1], C[2], {a, b}}
    ,
    TestID->"[8] Simplify-freeze.nb"
]

VerificationTest[
    fixTemporarySymbol["sub$"][Catch[freeze[Verbatim[Rule][a, _], Throw][{a -> b, c -> d, {a, b}}]]]
    ,
    {C[1], c -> d, {a, b}}
    ,
    TestID->"[9] Simplify-freeze.nb"
]

VerificationTest[
    freeze[a -> _, Throw][{a -> b, c -> d, {a, b}}]
    ,
    Quiet[HoldComplete[{a -> b, c -> d, {a, b}}]]
    ,
    {Yurie`Math`freeze::BadInput}
    ,
    TestID->"[10] Simplify-freeze.nb"
]

VerificationTest[
    fixTemporarySymbol["sub$"][Catch[freeze[_List, Throw][{a -> b, c -> d, {a, b}}]]]
    ,
    {a -> b, c -> d, C[1]}
    ,
    TestID->"[11] Simplify-freeze.nb"
]

VerificationTest[
    fixTemporarySymbol["sub$"][Catch[freeze[_List, Throw, All][{a -> b, c -> d, {a, b}, {c, d}}]]]
    ,
    {a -> b, c -> d, C[1], C[2]}
    ,
    TestID->"[12] Simplify-freeze.nb"
]

VerificationTest[
    fixTemporarySymbol["sub$"][Catch[freeze[_List, Throw, All][{a -> b, c -> d}]]]
    ,
    C[1]
    ,
    TestID->"[13] Simplify-freeze.nb"
]

VerificationTest[
    fixTemporarySymbol["sub$"][Catch[freeze[{__}, Throw][{a -> b, c -> d, {a, b}, {c, d}}]]]
    ,
    {C[1] -> C[2], C[3] -> C[4], {C[1], C[2]}, {C[3], C[4]}}
    ,
    TestID->"[14] Simplify-freeze.nb"
]

VerificationTest[
    fixTemporarySymbol["sub$"][Catch[freeze[{a}, Throw][{a -> b, c -> d, {a, b}, {c, d}}]]]
    ,
    {C[1] -> b, c -> d, {C[1], b}, {c, d}}
    ,
    TestID->"[15] Simplify-freeze.nb"
]

VerificationTest[
    fixTemporarySymbol["sub$"][Catch[freeze[Verbatim[List][__], Throw][{a -> b, c -> d, {a, b}, {c, d}}]]]
    ,
    {a -> b, c -> d, C[1], C[2]}
    ,
    TestID->"[16] Simplify-freeze.nb"
]

VerificationTest[
    fixTemporarySymbol["sub$"][Catch[freeze[Verbatim[List][a, ___], Throw][{a -> b, c -> d, {a, b}, {c, d}}]]]
    ,
    {a -> b, c -> d, C[1], {c, d}}
    ,
    TestID->"[17] Simplify-freeze.nb"
]

VerificationTest[
    expr = dot[p[1], p[2]] + 2*dot[p[1], p[3] + p[4]] + f[x]*dot[p[1], p[5]] + h[dot[p[1], p[2]], dot[p[1], p[2]]]
    ,
    dot[p[1], p[2]] + 2*dot[p[1], p[3] + p[4]] + dot[p[1], p[5]]*f[x] + h[dot[p[1], p[2]], dot[p[1], p[2]]]
    ,
    TestID->"[18] Simplify-freeze.nb"
]

VerificationTest[
    fixTemporarySymbol["sub$"][Catch[freeze[dot[_, _], Throw][expr]]]
    ,
    C[1] + 2*C[2] + C[3]*f[x] + h[C[1], C[1]]
    ,
    TestID->"[19] Simplify-freeze.nb"
]

VerificationTest[
    freeze[x -> {Identity, s}][NestList[x, x, 2]]
    ,
    {s[x], x[s[x]], x[x[s[x]]]}
    ,
    TestID->"[20] Simplify-freeze.nb"
]

VerificationTest[
    freeze[x -> {Identity, s}, Identity, 2][NestList[x, x, 2]]
    ,
    {s[x], x[s[x]], x[x[x]]}
    ,
    TestID->"[21] Simplify-freeze.nb"
]

VerificationTest[
    freeze[x -> {s, t}][NestList[x, x, 2]]
    ,
    {s[t[x]], x[s[t[x]]], x[x[s[t[x]]]]}
    ,
    TestID->"[22] Simplify-freeze.nb"
]

VerificationTest[
    freezeNegative[a, PowerExpand][((-a)*b)^(Δ - 1)]
    ,
    (-a)^(-1 + Δ)*b^(-1 + Δ)
    ,
    TestID->"[23] Simplify-freeze.nb"
]

VerificationTest[
    freezeNegative[b, PowerExpand][((-a)*b)^(Δ - 1)]
    ,
    a^(-1 + Δ)*(-b)^(-1 + Δ)
    ,
    TestID->"[24] Simplify-freeze.nb"
]

VerificationTest[
    freezeNegative[a | b, PowerExpand][((-a)*b)^(Δ - 1)]
    ,
    (-1)^(-1 + Δ)*(-a)^(-1 + Δ)*(-b)^(-1 + Δ)
    ,
    TestID->"[25] Simplify-freeze.nb"
]

VerificationTest[
    freezeNegative[{a -> {f, g}, b}, PowerExpand][((-a)*a[]*b)^(Δ - 1)]
    ,
    (-b)^(-1 + Δ)*a[]^(-1 + Δ)*f[g[a]]^(-1 + Δ)
    ,
    TestID->"[26] Simplify-freeze.nb"
]

VerificationTest[
    freezeNegative[{a -> {f, g}, b -> {h, k}}, PowerExpand][((-a)*a[]*b)^(Δ - 1)]
    ,
    (-1)^(-1 + Δ)*a[]^(-1 + Δ)*f[g[a]]^(-1 + Δ)*h[k[b]]^(-1 + Δ)
    ,
    TestID->"[27] Simplify-freeze.nb"
]

VerificationTest[
    ClearAll["`*"];
    End[]
    ,
    "Global`"
    ,
    TestID->"[∞] Simplify-freeze.nb"
]