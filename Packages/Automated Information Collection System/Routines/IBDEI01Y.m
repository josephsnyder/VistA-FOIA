IBDEI01Y ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,2045,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2045,1,2,0)
 ;;=2^V61.9
 ;;^UTILITY(U,$J,358.3,2045,1,5,0)
 ;;=5^RELATIONAL PROBLEM
 ;;^UTILITY(U,$J,358.3,2045,2)
 ;;=^303721
 ;;^UTILITY(U,$J,358.3,2046,0)
 ;;=V65.42^^21^154^39
 ;;^UTILITY(U,$J,358.3,2046,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2046,1,2,0)
 ;;=2^V65.42
 ;;^UTILITY(U,$J,358.3,2046,1,5,0)
 ;;=5^SUBSTANCE ABUSE COUNSELING
 ;;^UTILITY(U,$J,358.3,2046,2)
 ;;=^303467
 ;;^UTILITY(U,$J,358.3,2047,0)
 ;;=V62.0^^21^154^40
 ;;^UTILITY(U,$J,358.3,2047,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2047,1,2,0)
 ;;=2^V62.0
 ;;^UTILITY(U,$J,358.3,2047,1,5,0)
 ;;=5^UNEMPLOYMENT
 ;;^UTILITY(U,$J,358.3,2047,2)
 ;;=^123545
 ;;^UTILITY(U,$J,358.3,2048,0)
 ;;=V65.49^^21^154^30
 ;;^UTILITY(U,$J,358.3,2048,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2048,1,2,0)
 ;;=2^V65.49
 ;;^UTILITY(U,$J,358.3,2048,1,5,0)
 ;;=5^OTHER SPECIFIED COUNSELING
 ;;^UTILITY(U,$J,358.3,2048,2)
 ;;=^303471
 ;;^UTILITY(U,$J,358.3,2049,0)
 ;;=V58.61^^21^154^17
 ;;^UTILITY(U,$J,358.3,2049,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2049,1,2,0)
 ;;=2^V58.61
 ;;^UTILITY(U,$J,358.3,2049,1,5,0)
 ;;=5^L/T (CURRENT) USE - ANTICOAG
 ;;^UTILITY(U,$J,358.3,2049,2)
 ;;=^303459
 ;;^UTILITY(U,$J,358.3,2050,0)
 ;;=V58.62^^21^154^19
 ;;^UTILITY(U,$J,358.3,2050,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2050,1,2,0)
 ;;=2^V58.62
 ;;^UTILITY(U,$J,358.3,2050,1,5,0)
 ;;=5^L/T (CURRENT) USE ANTIBIOTIC
 ;;^UTILITY(U,$J,358.3,2050,2)
 ;;=^321546
 ;;^UTILITY(U,$J,358.3,2051,0)
 ;;=V58.63^^21^154^21
 ;;^UTILITY(U,$J,358.3,2051,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2051,1,2,0)
 ;;=2^V58.63
 ;;^UTILITY(U,$J,358.3,2051,1,5,0)
 ;;=5^LNG USE ANTIPLTE/THRMBTC
 ;;^UTILITY(U,$J,358.3,2051,2)
 ;;=^329978
 ;;^UTILITY(U,$J,358.3,2052,0)
 ;;=V58.64^^21^154^22
 ;;^UTILITY(U,$J,358.3,2052,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2052,1,2,0)
 ;;=2^V58.64
 ;;^UTILITY(U,$J,358.3,2052,1,5,0)
 ;;=5^LONG-TERM ANTI-INFLAMTRY
 ;;^UTILITY(U,$J,358.3,2052,2)
 ;;=^329979
 ;;^UTILITY(U,$J,358.3,2053,0)
 ;;=V58.65^^21^154^25
 ;;^UTILITY(U,$J,358.3,2053,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2053,1,2,0)
 ;;=2^V58.65
 ;;^UTILITY(U,$J,358.3,2053,1,5,0)
 ;;=5^LONG-TERM USE STEROIDS
 ;;^UTILITY(U,$J,358.3,2053,2)
 ;;=^329980
 ;;^UTILITY(U,$J,358.3,2054,0)
 ;;=V58.66^^21^154^23
 ;;^UTILITY(U,$J,358.3,2054,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2054,1,2,0)
 ;;=2^V58.66
 ;;^UTILITY(U,$J,358.3,2054,1,5,0)
 ;;=5^LONG-TERM USE OF ASPIRIN
 ;;^UTILITY(U,$J,358.3,2054,2)
 ;;=^331584
 ;;^UTILITY(U,$J,358.3,2055,0)
 ;;=V58.67^^21^154^24
 ;;^UTILITY(U,$J,358.3,2055,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2055,1,2,0)
 ;;=2^V58.67
 ;;^UTILITY(U,$J,358.3,2055,1,5,0)
 ;;=5^LONG-TERM USE OF INSULIN
 ;;^UTILITY(U,$J,358.3,2055,2)
 ;;=^331585
 ;;^UTILITY(U,$J,358.3,2056,0)
 ;;=V58.69^^21^154^18
 ;;^UTILITY(U,$J,358.3,2056,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2056,1,2,0)
 ;;=2^V58.69
 ;;^UTILITY(U,$J,358.3,2056,1,5,0)
 ;;=5^L/T (CURRENT) USE - OTH MEDS
 ;;^UTILITY(U,$J,358.3,2056,2)
 ;;=^303460
 ;;^UTILITY(U,$J,358.3,2057,0)
 ;;=V65.40^^21^154^29
 ;;^UTILITY(U,$J,358.3,2057,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2057,1,2,0)
 ;;=2^V65.40
 ;;^UTILITY(U,$J,358.3,2057,1,5,0)
 ;;=5^OTH UNSP COUNSEL NOC
 ;;^UTILITY(U,$J,358.3,2057,2)
 ;;=^87449
 ;;^UTILITY(U,$J,358.3,2058,0)
 ;;=V65.41^^21^154^9
 ;;^UTILITY(U,$J,358.3,2058,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2058,1,2,0)
 ;;=2^V65.41
 ;;^UTILITY(U,$J,358.3,2058,1,5,0)
 ;;=5^EXERCISE COUNSELING
 ;;^UTILITY(U,$J,358.3,2058,2)
 ;;=^303466
 ;;^UTILITY(U,$J,358.3,2059,0)
 ;;=V65.43^^21^154^4
 ;;^UTILITY(U,$J,358.3,2059,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2059,1,2,0)
 ;;=2^V65.43
 ;;^UTILITY(U,$J,358.3,2059,1,5,0)
 ;;=5^COUNSEL-INJURY PREVEN
 ;;^UTILITY(U,$J,358.3,2059,2)
 ;;=^303468
 ;;^UTILITY(U,$J,358.3,2060,0)
 ;;=V65.45^^21^154^5
 ;;^UTILITY(U,$J,358.3,2060,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2060,1,2,0)
 ;;=2^V65.45
 ;;^UTILITY(U,$J,358.3,2060,1,5,0)
 ;;=5^COUNSEL-OTH SEX TX DISEASES
 ;;^UTILITY(U,$J,358.3,2060,2)
 ;;=^303470
 ;;^UTILITY(U,$J,358.3,2061,0)
 ;;=V71.09^^21^155^1
 ;;^UTILITY(U,$J,358.3,2061,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2061,1,2,0)
 ;;=2^V71.09
 ;;^UTILITY(U,$J,358.3,2061,1,5,0)
 ;;=5^OBSERV-MENTAL COND NEC
 ;;^UTILITY(U,$J,358.3,2061,2)
 ;;=^295604
 ;;^UTILITY(U,$J,358.3,2062,0)
 ;;=99211^^22^156^1
 ;;^UTILITY(U,$J,358.3,2062,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2062,1,1,0)
 ;;=1^Brief Office Visit
 ;;^UTILITY(U,$J,358.3,2062,1,2,0)
 ;;=2^99211
 ;;^UTILITY(U,$J,358.3,2063,0)
 ;;=99212^^22^156^2
 ;;^UTILITY(U,$J,358.3,2063,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2063,1,1,0)
 ;;=1^Problem Focused
 ;;^UTILITY(U,$J,358.3,2063,1,2,0)
 ;;=2^99212
 ;;^UTILITY(U,$J,358.3,2064,0)
 ;;=99213^^22^156^3
 ;;^UTILITY(U,$J,358.3,2064,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2064,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,2064,1,2,0)
 ;;=2^99213
 ;;^UTILITY(U,$J,358.3,2065,0)
 ;;=99214^^22^156^4
 ;;^UTILITY(U,$J,358.3,2065,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2065,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,2065,1,2,0)
 ;;=2^99214
 ;;^UTILITY(U,$J,358.3,2066,0)
 ;;=99215^^22^156^5
 ;;^UTILITY(U,$J,358.3,2066,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2066,1,1,0)
 ;;=1^Comprehensive
 ;;^UTILITY(U,$J,358.3,2066,1,2,0)
 ;;=2^99215
 ;;^UTILITY(U,$J,358.3,2067,0)
 ;;=99201^^22^157^1
 ;;^UTILITY(U,$J,358.3,2067,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2067,1,1,0)
 ;;=1^Problem Focus
 ;;^UTILITY(U,$J,358.3,2067,1,2,0)
 ;;=2^99201
 ;;^UTILITY(U,$J,358.3,2068,0)
 ;;=99202^^22^157^2
 ;;^UTILITY(U,$J,358.3,2068,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2068,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,2068,1,2,0)
 ;;=2^99202
 ;;^UTILITY(U,$J,358.3,2069,0)
 ;;=99203^^22^157^3
 ;;^UTILITY(U,$J,358.3,2069,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2069,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,2069,1,2,0)
 ;;=2^99203
 ;;^UTILITY(U,$J,358.3,2070,0)
 ;;=99204^^22^157^4
 ;;^UTILITY(U,$J,358.3,2070,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2070,1,1,0)
 ;;=1^Comprehensive, Moderate
 ;;^UTILITY(U,$J,358.3,2070,1,2,0)
 ;;=2^99204
 ;;^UTILITY(U,$J,358.3,2071,0)
 ;;=99205^^22^157^5
 ;;^UTILITY(U,$J,358.3,2071,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2071,1,1,0)
 ;;=1^Comprehensive, High
 ;;^UTILITY(U,$J,358.3,2071,1,2,0)
 ;;=2^99205
 ;;^UTILITY(U,$J,358.3,2072,0)
 ;;=99211^^22^158^1
 ;;^UTILITY(U,$J,358.3,2072,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2072,1,1,0)
 ;;=1^Brief Visit w/ mod
 ;;^UTILITY(U,$J,358.3,2072,1,2,0)
 ;;=2^99211
 ;;^UTILITY(U,$J,358.3,2072,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,2072,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,2073,0)
 ;;=99212^^22^158^2
 ;;^UTILITY(U,$J,358.3,2073,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2073,1,1,0)
 ;;=1^Problem Focused w/ mod
 ;;^UTILITY(U,$J,358.3,2073,1,2,0)
 ;;=2^99212
 ;;^UTILITY(U,$J,358.3,2073,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,2073,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,2074,0)
 ;;=99213^^22^158^3
 ;;^UTILITY(U,$J,358.3,2074,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2074,1,1,0)
 ;;=1^Expanded Problem Focus w/ mod
 ;;^UTILITY(U,$J,358.3,2074,1,2,0)
 ;;=2^99213
 ;;^UTILITY(U,$J,358.3,2074,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,2074,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,2075,0)
 ;;=99214^^22^158^4
 ;;^UTILITY(U,$J,358.3,2075,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2075,1,1,0)
 ;;=1^Detailed w/ mod
 ;;^UTILITY(U,$J,358.3,2075,1,2,0)
 ;;=2^99214
 ;;^UTILITY(U,$J,358.3,2075,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,2075,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,2076,0)
 ;;=99215^^22^158^5
 ;;^UTILITY(U,$J,358.3,2076,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2076,1,1,0)
 ;;=1^Comprehensive w/ mod
 ;;^UTILITY(U,$J,358.3,2076,1,2,0)
 ;;=2^99215
 ;;^UTILITY(U,$J,358.3,2076,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,2076,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,2077,0)
 ;;=99201^^22^159^1
 ;;^UTILITY(U,$J,358.3,2077,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2077,1,1,0)
 ;;=1^Problem Focus w/ mod
 ;;^UTILITY(U,$J,358.3,2077,1,2,0)
 ;;=2^99201
 ;;^UTILITY(U,$J,358.3,2077,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,2077,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,2078,0)
 ;;=99202^^22^159^2
 ;;^UTILITY(U,$J,358.3,2078,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2078,1,1,0)
 ;;=1^Expanded Problem Focus w/ mod
 ;;^UTILITY(U,$J,358.3,2078,1,2,0)
 ;;=2^99202
 ;;^UTILITY(U,$J,358.3,2078,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,2078,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,2079,0)
 ;;=99203^^22^159^3
 ;;^UTILITY(U,$J,358.3,2079,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2079,1,1,0)
 ;;=1^Detailed w/ mod
 ;;^UTILITY(U,$J,358.3,2079,1,2,0)
 ;;=2^99203
 ;;^UTILITY(U,$J,358.3,2079,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,2079,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,2080,0)
 ;;=99204^^22^159^4
 ;;^UTILITY(U,$J,358.3,2080,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2080,1,1,0)
 ;;=1^Comprehensive, Moderate w/ mod
 ;;^UTILITY(U,$J,358.3,2080,1,2,0)
 ;;=2^99204
 ;;^UTILITY(U,$J,358.3,2080,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,2080,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,2081,0)
 ;;=99205^^22^159^5
 ;;^UTILITY(U,$J,358.3,2081,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2081,1,1,0)
 ;;=1^Comprehensive, High w/ mod
 ;;^UTILITY(U,$J,358.3,2081,1,2,0)
 ;;=2^99205
 ;;^UTILITY(U,$J,358.3,2081,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,2081,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,2082,0)
 ;;=V71.89^^23^160^1
 ;;^UTILITY(U,$J,358.3,2082,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,2082,1,3,0)
 ;;=3^ENROLLMENT IN CCHT PROGRAM
 ;;^UTILITY(U,$J,358.3,2082,1,4,0)
 ;;=4^V71.89
 ;;^UTILITY(U,$J,358.3,2082,2)
 ;;=^322082
 ;;^UTILITY(U,$J,358.3,2083,0)
 ;;=99078^^24^161^3^^^^1
 ;;^UTILITY(U,$J,358.3,2083,1,0)
 ;;=^358.31IA^3^2
