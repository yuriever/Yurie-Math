

(*Simplify-unsafe.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Simplify-unsafe.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Simplify-unsafe.nb"
]

VerificationTest[
    expr = ((w^a)^b*((w*(x - y))/z)^(a + b)*z^(2*a))/((-x + y)/z)^b; 
    ,
    Null
    ,
    TestID->"2-Simplify-unsafe.nb"
]

VerificationTest[
    powerCollect[a][powerCollect[b][expr]]
    ,
    (-w^(1 + a))^b*(w*(x - y)*z)^a
    ,
    TestID->"3-Simplify-unsafe.nb"
]

VerificationTest[
    powerCollect[a][powerCollect[b][expr]] == powerCollect[b, a][expr]
    ,
    True
    ,
    TestID->"4-Simplify-unsafe.nb"
]

VerificationTest[
    powerCollect[a, b][expr]
    ,
    (-w)^b*(w^(1 + b)*(x - y)*z)^a
    ,
    TestID->"5-Simplify-unsafe.nb"
]

VerificationTest[
    powerApart[expr]
    ,
    (w^(a + b + a*b)*(x - y)^(a + b)*z^a)/(-x + y)^b
    ,
    TestID->"6-Simplify-unsafe.nb"
]

VerificationTest[
    powerTogether[expr]
    ,
    (-1)^b*w^(a + b + a*b)*((x - y)*z)^a
    ,
    TestID->"7-Simplify-unsafe.nb"
]

VerificationTest[
    powerSim[expr]
    ,
    (-1)^b*w^(a + b + a*b)*((x - y)*z)^a
    ,
    TestID->"8-Simplify-unsafe.nb"
]

VerificationTest[
    expr = {(-1)^a*x^a, (-x)^a, 1/x^2, I^a*x^a, (I*x)^(4*a), 1/x^2}; 
    powerApart[expr]
    ,
    {(-1)^a*x^a, (-1)^a*x^a, 1/x^2, E^((I*a*Pi)/2)*x^a, E^(2*I*a*Pi)*x^(4*a), 1/x^2}
    ,
    TestID->"9-Simplify-unsafe.nb"
]

VerificationTest[
    powerTogether[expr]
    ,
    {(-1)^a*x^a, (-1)^a*x^a, 1/x^2, E^((I*a*Pi)/2)*x^a, E^(2*I*a*Pi)*x^(4*a), 1/x^2}
    ,
    TestID->"10-Simplify-unsafe.nb"
]

VerificationTest[
    powerSim[expr]
    ,
    {(-1)^a*x^a, (-1)^a*x^a, 1/x^2, E^((I*a*Pi)/2)*x^a, E^(2*I*a*Pi)*x^(4*a), 1/x^2}
    ,
    TestID->"11-Simplify-unsafe.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Simplify-unsafe.nb"
]