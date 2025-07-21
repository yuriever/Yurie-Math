

(*Simplify-power.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-Simplify-power.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-Simplify-power.nb"
]

VerificationTest[
    powerFocus[f][x^a]
    ,
    f[x]^f[a]
    ,
    TestID->"2-Simplify-power.nb"
]

VerificationTest[
    powerBaseFocus[f][x^a]
    ,
    f[x]^a
    ,
    TestID->"3-Simplify-power.nb"
]

VerificationTest[
    powerExponentFocus[f][x^a]
    ,
    x^f[a]
    ,
    TestID->"4-Simplify-power.nb"
]

VerificationTest[
    powerSeparate[][x^a]
    ,
    {x^a, 1}
    ,
    TestID->"5-Simplify-power.nb"
]

VerificationTest[
    powerSeparate[][1 + a]
    ,
    {1, 1 + a}
    ,
    TestID->"6-Simplify-power.nb"
]

VerificationTest[
    powerSeparate[][x^a*(x + y)^b*Gamma[x]]
    ,
    {x^a*(x + y)^b, Gamma[x]}
    ,
    TestID->"7-Simplify-power.nb"
]

VerificationTest[
    powerSeparate[All][x^a*(x + y)^b*Gamma[x]]
    ,
    {x^a*(x + y)^b, Gamma[x]}
    ,
    TestID->"8-Simplify-power.nb"
]

VerificationTest[
    powerSeparate[x | x + y][x^a]
    ,
    {x^a, 1}
    ,
    TestID->"9-Simplify-power.nb"
]

VerificationTest[
    powerSeparate[x | x + y][1 + a]
    ,
    {1, 1 + a}
    ,
    TestID->"10-Simplify-power.nb"
]

VerificationTest[
    powerSeparate[x | x + y][x^a*(x + y)^b*(x + z)^c*Gamma[x]]
    ,
    {x^a*(x + y)^b, (x + z)^c*Gamma[x]}
    ,
    TestID->"11-Simplify-power.nb"
]

VerificationTest[
    powerSeparate[{x, x + y}][x^a*(x + y)^b*(x + z)^c*Gamma[x]]
    ,
    {x^a*(x + y)^b, (x + z)^c*Gamma[x]}
    ,
    TestID->"12-Simplify-power.nb"
]

VerificationTest[
    expr = (1 + a/b)^C[1]*(1 - a/b)^C[3]*(1 + x/y)^C[2]; 
    ,
    Null
    ,
    TestID->"13-Simplify-power.nb"
]

VerificationTest[
    powerBaseTogether[][expr]
    ,
    ((-a + b)/b)^C[3]*((a + b)/b)^C[1]*((x + y)/y)^C[2]
    ,
    TestID->"14-Simplify-power.nb"
]

VerificationTest[
    expr = (1 + a/b)^C[1]*(1 - a/b)^C[3]*(1 + x/y)^C[2]; 
    ,
    Null
    ,
    TestID->"15-Simplify-power.nb"
]

VerificationTest[
    powerExpand[][expr]
    ,
    (b^(-C[1] - C[3])*(-a + b)^C[3]*(a + b)^C[1]*(x + y)^C[2])/y^C[2]
    ,
    TestID->"16-Simplify-power.nb"
]

VerificationTest[
    expr = (ε - ε*χ)^a*(εb - εb*χb)^b; 
    ,
    Null
    ,
    TestID->"17-Simplify-power.nb"
]

VerificationTest[
    powerExpand[][expr]
    ,
    (ε - ε*χ)^a*(εb - εb*χb)^b
    ,
    TestID->"18-Simplify-power.nb"
]

VerificationTest[
    powerExpand[Factor][expr]
    ,
    (-1)^(a + b)*ε^a*εb^b*(-1 + χ)^a*(-1 + χb)^b
    ,
    TestID->"19-Simplify-power.nb"
]

VerificationTest[
    powerExpand[Factor, "Assumptions" -> ε > 0 && εb > 0][expr]
    ,
    ε^a*εb^b*(1 - χ)^a*(1 - χb)^b
    ,
    TestID->"20-Simplify-power.nb"
]

VerificationTest[
    expr = (ε*(1 - χ))^a*(εb*(1 - χb))^b; 
    ,
    Null
    ,
    TestID->"21-Simplify-power.nb"
]

VerificationTest[
    powerExpand[][expr]
    ,
    (ε - ε*χ)^a*(εb - εb*χb)^b
    ,
    TestID->"22-Simplify-power.nb"
]

VerificationTest[
    powerExpand[Factor][expr]
    ,
    (-1)^(a + b)*ε^a*εb^b*(-1 + χ)^a*(-1 + χb)^b
    ,
    TestID->"23-Simplify-power.nb"
]

VerificationTest[
    powerExpand[Factor, "Assumptions" -> ε > 0 && εb > 0][expr]
    ,
    ε^a*εb^b*(1 - χ)^a*(1 - χb)^b
    ,
    TestID->"24-Simplify-power.nb"
]

VerificationTest[
    expr = (ε - ε*χ)^a*(εb - εb*χb)^b; 
    ,
    Null
    ,
    TestID->"25-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[ε - ε*χ -> {ε, 1 - χ}][expr]
    ,
    ε^a*(1 - χ)^a*(εb - εb*χb)^b
    ,
    TestID->"26-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[ε - ε*χ -> {ε, 1 - χ}, εb - εb*χb -> {εb, 1 - χb}][expr]
    ,
    ε^a*εb^b*(1 - χ)^a*(1 - χb)^b
    ,
    TestID->"27-Simplify-power.nb"
]

VerificationTest[
    expr = (ε*(1 - χ))^a*(εb*(1 - χb))^b; 
    ,
    Null
    ,
    TestID->"28-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[ε*(1 - χ) -> {ε, χ - 1, Positive}][expr]
    ,
    E^(I*a*Pi)*ε^a*(-1 + χ)^a*(εb*(1 - χb))^b
    ,
    TestID->"29-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[εb*(1 - χb) -> {εb, χb - 1, Negative}][expr]
    ,
    (εb^b*(ε*(1 - χ))^a*(-1 + χb)^b)/E^(I*b*Pi)
    ,
    TestID->"30-Simplify-power.nb"
]

VerificationTest[
    expr = (ε*(1 - χ)*(1 + χ))^a*(εb*(1 - χb)*(1 + χb))^b; 
    ,
    Null
    ,
    TestID->"31-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[ε*(rest_.) :> {ε, rest}, εb*(rest_.) :> {εb, rest}][expr]
    ,
    ε^a*εb^b*((1 - χ)*(1 + χ))^a*((1 - χb)*(1 + χb))^b
    ,
    TestID->"32-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[(1 - χ)*(rest_.) :> {χ - 1, rest, Positive}][expr]
    ,
    E^(I*a*Pi)*(-1 + χ)^a*(ε*(1 + χ))^a*(εb*(1 - χb)*(1 + χb))^b
    ,
    TestID->"33-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[(1 - χb)*(rest_.) :> {χb - 1, rest, Negative}][expr]
    ,
    ((ε*(1 - χ)*(1 + χ))^a*(-1 + χb)^b*(εb*(1 + χb))^b)/E^(I*b*Pi)
    ,
    TestID->"34-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[ε*(1 - χ) -> {εb, 1 - χ}][(ε*(1 - χ))^a]
    ,
    Quiet[εb^a*(1 - χ)^a]
    ,
    {Yurie`Math`powerExpandBy::SuspiciousRule}
    ,
    TestID->"35-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[ε*(1 - χ) -> {ε, 1 - χ, Positive, Negative}][(ε*(1 - χ))^a]
    ,
    Quiet[ε^a*(1 - χ)^a]
    ,
    {Yurie`Math`powerExpandBy::InvalidPhase}
    ,
    TestID->"36-Simplify-power.nb"
]

VerificationTest[
    expr = (ε*(1 - χ))^a*(εb*(1 - χb))^b; 
    ,
    Null
    ,
    TestID->"37-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[ε*(1 - χ) -> {ε, 1 - χ, Positive}][expr]
    ,
    Quiet[E^(I*a*Pi)*ε^a*(1 - χ)^a*(εb*(1 - χb))^b]
    ,
    {Yurie`Math`powerExpandBy::SuspiciousRule}
    ,
    TestID->"38-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[εb*(1 - χb) -> {εb, 1 - χb, Negative}][expr]
    ,
    Quiet[(εb^b*(ε*(1 - χ))^a*(1 - χb)^b)/E^(I*b*Pi)]
    ,
    {Yurie`Math`powerExpandBy::SuspiciousRule}
    ,
    TestID->"39-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[(1 - χ)*(rest_.) :> {1 - χ, rest, Positive}][expr]
    ,
    Quiet[E^(I*a*Pi)*ε^a*(1 - χ)^a*(εb*(1 - χb))^b]
    ,
    {Yurie`Math`powerExpandBy::SuspiciousRule2}
    ,
    TestID->"40-Simplify-power.nb"
]

VerificationTest[
    powerExpandBy[(1 - χb)*(rest_.) :> {1 - χb, rest, Negative}][expr]
    ,
    Quiet[(εb^b*(ε*(1 - χ))^a*(1 - χb)^b)/E^(I*b*Pi)]
    ,
    {Yurie`Math`powerExpandBy::SuspiciousRule2}
    ,
    TestID->"41-Simplify-power.nb"
]

VerificationTest[
    expr = ((w^a)^b*((w*(x - y))/z)^(a + b)*z^(2*a))/((-x + y)/z)^b; 
    ,
    Null
    ,
    TestID->"42-Simplify-power.nb"
]

VerificationTest[
    powerExponentCollect[a][powerExponentCollect[b][expr]]
    ,
    (-w^(1 + a))^b*(w*(x - y)*z)^a
    ,
    TestID->"43-Simplify-power.nb"
]

VerificationTest[
    powerExponentCollect[a][powerExponentCollect[b][expr]] == powerExponentCollect[b, a][expr]
    ,
    True
    ,
    TestID->"44-Simplify-power.nb"
]

VerificationTest[
    powerExponentCollect[a, b][expr]
    ,
    (-w)^b*(w^(1 + b)*(x - y)*z)^a
    ,
    TestID->"45-Simplify-power.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Simplify-power.nb"
]