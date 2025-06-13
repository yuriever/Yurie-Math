

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
    powerBaseTogether[All][expr]
    ,
    ((-a + b)/b)^C[3]*((a + b)/b)^C[1]*((x + y)/y)^C[2]
    ,
    TestID->"15-Simplify-power.nb"
]

VerificationTest[
    powerBaseTogether[1 + a/b][expr]
    ,
    (1 - a/b)^C[3]*((a + b)/b)^C[1]*(1 + x/y)^C[2]
    ,
    TestID->"16-Simplify-power.nb"
]

VerificationTest[
    powerBaseTogether[1 + a/b | 1 - a/b][expr]
    ,
    ((-a + b)/b)^C[3]*((a + b)/b)^C[1]*(1 + x/y)^C[2]
    ,
    TestID->"17-Simplify-power.nb"
]

VerificationTest[
    powerBaseTogether[1 + (a*(_.))/b][expr]
    ,
    ((-a + b)/b)^C[3]*((a + b)/b)^C[1]*(1 + x/y)^C[2]
    ,
    TestID->"18-Simplify-power.nb"
]

VerificationTest[
    powerBaseTogether[][expr]
    ,
    ((-a + b)/b)^C[3]*((a + b)/b)^C[1]*((x + y)/y)^C[2]
    ,
    TestID->"19-Simplify-power.nb"
]

VerificationTest[
    powerBaseTogether[All, 1][expr + 1]
    ,
    1 + (1 - a/b)^C[3]*(1 + a/b)^C[1]*(1 + x/y)^C[2]
    ,
    TestID->"20-Simplify-power.nb"
]

VerificationTest[
    powerBaseTogether[All, 2][expr + 1]
    ,
    1 + ((-a + b)/b)^C[3]*((a + b)/b)^C[1]*((x + y)/y)^C[2]
    ,
    TestID->"21-Simplify-power.nb"
]

VerificationTest[
    expr = (1 + a/b)^C[1]*(1 - a/b)^C[3]*(1 + x/y)^C[2]; 
    ,
    Null
    ,
    TestID->"22-Simplify-power.nb"
]

VerificationTest[
    powerExpand[][expr]
    ,
    (b^(-C[1] - C[3])*(-a + b)^C[3]*(a + b)^C[1]*(x + y)^C[2])/y^C[2]
    ,
    TestID->"23-Simplify-power.nb"
]

VerificationTest[
    powerExpand[1 + x/y][expr]
    ,
    ((1 - a/b)^C[3]*(1 + a/b)^C[1]*(x + y)^C[2])/y^C[2]
    ,
    TestID->"24-Simplify-power.nb"
]

VerificationTest[
    powerExpand[1 + x/y, 1][1 + expr]
    ,
    1 + (1 - a/b)^C[3]*(1 + a/b)^C[1]*(1 + x/y)^C[2]
    ,
    TestID->"25-Simplify-power.nb"
]

VerificationTest[
    powerExpand[1 + x/y, 2][1 + expr]
    ,
    1 + ((1 - a/b)^C[3]*(1 + a/b)^C[1]*(x + y)^C[2])/y^C[2]
    ,
    TestID->"26-Simplify-power.nb"
]

VerificationTest[
    expr = ((w^a)^b*((w*(x - y))/z)^(a + b)*z^(2*a))/((-x + y)/z)^b; 
    ,
    Null
    ,
    TestID->"27-Simplify-power.nb"
]

VerificationTest[
    powerExponentCollect[a][powerExponentCollect[b][expr]]
    ,
    (-w^(1 + a))^b*(w*(x - y)*z)^a
    ,
    TestID->"28-Simplify-power.nb"
]

VerificationTest[
    powerExponentCollect[a][powerExponentCollect[b][expr]] == powerExponentCollect[b, a][expr]
    ,
    True
    ,
    TestID->"29-Simplify-power.nb"
]

VerificationTest[
    powerExponentCollect[a, b][expr]
    ,
    (-w)^b*(w^(1 + b)*(x - y)*z)^a
    ,
    TestID->"30-Simplify-power.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-Simplify-power.nb"
]