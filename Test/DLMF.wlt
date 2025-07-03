

(*DLMF.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-DLMF.nb"
]

VerificationTest[
    Get["Yurie`Math`"]
    ,
    Null
    ,
    TestID->"1-DLMF.nb"
]

VerificationTest[
    (DLMFAsTrue["BinomialSwapA"][#1]/#1 & )[Binomial[a, n]]
    ,
    ((-1)^n*Binomial[-1 - a + n, n])/Binomial[a, n]
    ,
    TestID->"2-DLMF.nb"
]

VerificationTest[
    trigPhaseReduce[n][FES[(DLMFAsTrue["BinomialSwapA"][#1]/#1 & )[Binomial[a, n]]]]
    ,
    1
    ,
    TestID->"3-DLMF.nb"
]

VerificationTest[
    (DLMFAsTrue["PochhammerSwapA"][#1]/#1 & )[Pochhammer[a, n]]
    ,
    ((-1)^n*Pochhammer[1 - a - n, n])/Pochhammer[a, n]
    ,
    TestID->"4-DLMF.nb"
]

VerificationTest[
    trigPhaseReduce[n][FES[(DLMFAsTrue["PochhammerSwapA"][#1]/#1 & )[Pochhammer[a, n]]]]
    ,
    1
    ,
    TestID->"5-DLMF.nb"
]

VerificationTest[
    (DLMFAsTrue["FactorialPowerSwapA"][#1]/#1 & )[FactorialPower[a, n]]
    ,
    ((-1)^n*FactorialPower[-1 - a + n, n])/FactorialPower[a, n]
    ,
    TestID->"6-DLMF.nb"
]

VerificationTest[
    trigPhaseReduce[n][FES[(DLMFAsTrue["FactorialPowerSwapA"][#1]/#1 & )[FactorialPower[a, n]]]]
    ,
    1
    ,
    TestID->"7-DLMF.nb"
]

VerificationTest[
    DLMFAsTrue["15.4.20"][Hypergeometric2F1[a, b, c, 1]]
    ,
    (Gamma[c]*Gamma[-a - b + c])/(Gamma[-a + c]*Gamma[-b + c])
    ,
    TestID->"8-DLMF.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-DLMF.nb"
]