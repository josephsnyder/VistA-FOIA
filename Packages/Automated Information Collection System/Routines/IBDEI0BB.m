IBDEI0BB ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,15035,1,2,0)
 ;;=2^V71.09
 ;;^UTILITY(U,$J,358.3,15035,1,5,0)
 ;;=5^Observ-Mental Condition
 ;;^UTILITY(U,$J,358.3,15035,2)
 ;;=^295604
 ;;^UTILITY(U,$J,358.3,15036,0)
 ;;=V15.41^^109^904^17
 ;;^UTILITY(U,$J,358.3,15036,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15036,1,2,0)
 ;;=2^V15.41
 ;;^UTILITY(U,$J,358.3,15036,1,5,0)
 ;;=5^Hx Of Sexual Abuse
 ;;^UTILITY(U,$J,358.3,15036,2)
 ;;=^304352
 ;;^UTILITY(U,$J,358.3,15037,0)
 ;;=V61.01^^109^904^10
 ;;^UTILITY(U,$J,358.3,15037,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15037,1,2,0)
 ;;=2^V61.01
 ;;^UTILITY(U,$J,358.3,15037,1,5,0)
 ;;=5^Fmily Dsrpt-Fam Military
 ;;^UTILITY(U,$J,358.3,15037,2)
 ;;=^336799
 ;;^UTILITY(U,$J,358.3,15038,0)
 ;;=V61.02^^109^904^11
 ;;^UTILITY(U,$J,358.3,15038,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15038,1,2,0)
 ;;=2^V61.02
 ;;^UTILITY(U,$J,358.3,15038,1,5,0)
 ;;=5^Fmily Dsrpt-Ret Military
 ;;^UTILITY(U,$J,358.3,15038,2)
 ;;=^336800
 ;;^UTILITY(U,$J,358.3,15039,0)
 ;;=V61.03^^109^904^9
 ;;^UTILITY(U,$J,358.3,15039,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15039,1,2,0)
 ;;=2^V61.03
 ;;^UTILITY(U,$J,358.3,15039,1,5,0)
 ;;=5^Fmily Dsrpt-Divorce/Sep
 ;;^UTILITY(U,$J,358.3,15039,2)
 ;;=^336801
 ;;^UTILITY(U,$J,358.3,15040,0)
 ;;=V61.04^^109^904^7
 ;;^UTILITY(U,$J,358.3,15040,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15040,1,2,0)
 ;;=2^V61.04
 ;;^UTILITY(U,$J,358.3,15040,1,5,0)
 ;;=5^Family Dsrpt-Estrangment
 ;;^UTILITY(U,$J,358.3,15040,2)
 ;;=^336802
 ;;^UTILITY(U,$J,358.3,15041,0)
 ;;=V61.05^^109^904^8
 ;;^UTILITY(U,$J,358.3,15041,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15041,1,2,0)
 ;;=2^V61.05
 ;;^UTILITY(U,$J,358.3,15041,1,5,0)
 ;;=5^Fmily Dsrpt-Chld Custody
 ;;^UTILITY(U,$J,358.3,15041,2)
 ;;=^336803
 ;;^UTILITY(U,$J,358.3,15042,0)
 ;;=V61.09^^109^904^6
 ;;^UTILITY(U,$J,358.3,15042,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15042,1,2,0)
 ;;=2^V61.09
 ;;^UTILITY(U,$J,358.3,15042,1,5,0)
 ;;=5^Family Disruption NEC
 ;;^UTILITY(U,$J,358.3,15042,2)
 ;;=^336805
 ;;^UTILITY(U,$J,358.3,15043,0)
 ;;=V62.21^^109^904^13
 ;;^UTILITY(U,$J,358.3,15043,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15043,1,2,0)
 ;;=2^V62.21
 ;;^UTILITY(U,$J,358.3,15043,1,5,0)
 ;;=5^HX Military Deployment
 ;;^UTILITY(U,$J,358.3,15043,2)
 ;;=^336806
 ;;^UTILITY(U,$J,358.3,15044,0)
 ;;=V62.22^^109^904^14
 ;;^UTILITY(U,$J,358.3,15044,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15044,1,2,0)
 ;;=2^V62.22
 ;;^UTILITY(U,$J,358.3,15044,1,5,0)
 ;;=5^HX Retrn Military Deploy
 ;;^UTILITY(U,$J,358.3,15044,2)
 ;;=^336807
 ;;^UTILITY(U,$J,358.3,15045,0)
 ;;=V62.29^^109^904^23
 ;;^UTILITY(U,$J,358.3,15045,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15045,1,2,0)
 ;;=2^V62.29
 ;;^UTILITY(U,$J,358.3,15045,1,5,0)
 ;;=5^Occupationl Circumst NEC
 ;;^UTILITY(U,$J,358.3,15045,2)
 ;;=^87746
 ;;^UTILITY(U,$J,358.3,15046,0)
 ;;=V60.81^^109^904^12
 ;;^UTILITY(U,$J,358.3,15046,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15046,1,2,0)
 ;;=2^V60.81
 ;;^UTILITY(U,$J,358.3,15046,1,5,0)
 ;;=5^Foster Care (Status)
 ;;^UTILITY(U,$J,358.3,15046,2)
 ;;=^338505
 ;;^UTILITY(U,$J,358.3,15047,0)
 ;;=V60.89^^109^904^16
 ;;^UTILITY(U,$J,358.3,15047,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15047,1,2,0)
 ;;=2^V60.89
 ;;^UTILITY(U,$J,358.3,15047,1,5,0)
 ;;=5^Housing/Econom Circum NEC
 ;;^UTILITY(U,$J,358.3,15047,2)
 ;;=^295545
 ;;^UTILITY(U,$J,358.3,15048,0)
 ;;=V61.22^^109^904^31
 ;;^UTILITY(U,$J,358.3,15048,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15048,1,2,0)
 ;;=2^V61.22
 ;;^UTILITY(U,$J,358.3,15048,1,5,0)
 ;;=5^Perpetrator-Parental Child
 ;;^UTILITY(U,$J,358.3,15048,2)
 ;;=^304358
 ;;^UTILITY(U,$J,358.3,15049,0)
 ;;=V61.23^^109^904^27
 ;;^UTILITY(U,$J,358.3,15049,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15049,1,2,0)
 ;;=2^V61.23
 ;;^UTILITY(U,$J,358.3,15049,1,5,0)
 ;;=5^Parent-Biological Child Prob
 ;;^UTILITY(U,$J,358.3,15049,2)
 ;;=^338508
 ;;^UTILITY(U,$J,358.3,15050,0)
 ;;=V61.24^^109^904^26
 ;;^UTILITY(U,$J,358.3,15050,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15050,1,2,0)
 ;;=2^V61.24
 ;;^UTILITY(U,$J,358.3,15050,1,5,0)
 ;;=5^Parent-Adopted Child Prob
 ;;^UTILITY(U,$J,358.3,15050,2)
 ;;=^338509
 ;;^UTILITY(U,$J,358.3,15051,0)
 ;;=V61.25^^109^904^29
 ;;^UTILITY(U,$J,358.3,15051,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15051,1,2,0)
 ;;=2^V61.25
 ;;^UTILITY(U,$J,358.3,15051,1,5,0)
 ;;=5^Parent-Foster Child Prob
 ;;^UTILITY(U,$J,358.3,15051,2)
 ;;=^338510
 ;;^UTILITY(U,$J,358.3,15052,0)
 ;;=V40.31^^109^904^37
 ;;^UTILITY(U,$J,358.3,15052,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15052,1,2,0)
 ;;=2^V40.31
 ;;^UTILITY(U,$J,358.3,15052,1,5,0)
 ;;=5^Wandering-Dis Classified Elsewhere
 ;;^UTILITY(U,$J,358.3,15052,2)
 ;;=^340621
 ;;^UTILITY(U,$J,358.3,15053,0)
 ;;=V40.39^^109^904^24
 ;;^UTILITY(U,$J,358.3,15053,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15053,1,2,0)
 ;;=2^V40.39
 ;;^UTILITY(U,$J,358.3,15053,1,5,0)
 ;;=5^Oth Specified Behavioral Problem
 ;;^UTILITY(U,$J,358.3,15053,2)
 ;;=^340622
 ;;^UTILITY(U,$J,358.3,15054,0)
 ;;=V65.19^^109^904^32
 ;;^UTILITY(U,$J,358.3,15054,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15054,1,2,0)
 ;;=2^V65.19
 ;;^UTILITY(U,$J,358.3,15054,1,5,0)
 ;;=5^Person Consulting on Behalf of Pt
 ;;^UTILITY(U,$J,358.3,15054,2)
 ;;=^329985
 ;;^UTILITY(U,$J,358.3,15055,0)
 ;;=V66.7^^109^904^5
 ;;^UTILITY(U,$J,358.3,15055,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15055,1,2,0)
 ;;=2^V66.7
 ;;^UTILITY(U,$J,358.3,15055,1,5,0)
 ;;=5^Encounter for Palliative Care
 ;;^UTILITY(U,$J,358.3,15055,2)
 ;;=^89209
 ;;^UTILITY(U,$J,358.3,15056,0)
 ;;=V65.40^^109^905^3
 ;;^UTILITY(U,$J,358.3,15056,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15056,1,2,0)
 ;;=2^V65.40
 ;;^UTILITY(U,$J,358.3,15056,1,5,0)
 ;;=5^Counseling, NOS
 ;;^UTILITY(U,$J,358.3,15056,2)
 ;;=^87449
 ;;^UTILITY(U,$J,358.3,15057,0)
 ;;=V65.42^^109^905^7
 ;;^UTILITY(U,$J,358.3,15057,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15057,1,2,0)
 ;;=2^V65.42
 ;;^UTILITY(U,$J,358.3,15057,1,5,0)
 ;;=5^Substance Use/Abuse Couns
 ;;^UTILITY(U,$J,358.3,15057,2)
 ;;=^303467
 ;;^UTILITY(U,$J,358.3,15058,0)
 ;;=V79.1^^109^905^1
 ;;^UTILITY(U,$J,358.3,15058,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15058,1,2,0)
 ;;=2^V79.1
 ;;^UTILITY(U,$J,358.3,15058,1,5,0)
 ;;=5^Alcohol Screen
 ;;^UTILITY(U,$J,358.3,15058,2)
 ;;=^295678
 ;;^UTILITY(U,$J,358.3,15059,0)
 ;;=V65.44^^109^905^4
 ;;^UTILITY(U,$J,358.3,15059,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15059,1,2,0)
 ;;=2^V65.44
 ;;^UTILITY(U,$J,358.3,15059,1,5,0)
 ;;=5^HIV Counseling
 ;;^UTILITY(U,$J,358.3,15059,2)
 ;;=^303469
 ;;^UTILITY(U,$J,358.3,15060,0)
 ;;=V65.49^^109^905^5
 ;;^UTILITY(U,$J,358.3,15060,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15060,1,2,0)
 ;;=2^V65.49
 ;;^UTILITY(U,$J,358.3,15060,1,5,0)
 ;;=5^Other Specified Counseling
 ;;^UTILITY(U,$J,358.3,15060,2)
 ;;=^303471
 ;;^UTILITY(U,$J,358.3,15061,0)
 ;;=V70.2^^109^905^6
 ;;^UTILITY(U,$J,358.3,15061,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15061,1,2,0)
 ;;=2^V70.2
 ;;^UTILITY(U,$J,358.3,15061,1,5,0)
 ;;=5^Psychiatric Examination
 ;;^UTILITY(U,$J,358.3,15061,2)
 ;;=^295592
 ;;^UTILITY(U,$J,358.3,15062,0)
 ;;=V61.21^^109^905^2
 ;;^UTILITY(U,$J,358.3,15062,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15062,1,2,0)
 ;;=2^V61.21
 ;;^UTILITY(U,$J,358.3,15062,1,5,0)
 ;;=5^Child Abuse Victim Couns
 ;;^UTILITY(U,$J,358.3,15062,2)
 ;;=^304301
 ;;^UTILITY(U,$J,358.3,15063,0)
 ;;=296.20^^109^906^14
 ;;^UTILITY(U,$J,358.3,15063,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15063,1,2,0)
 ;;=2^296.20
 ;;^UTILITY(U,$J,358.3,15063,1,5,0)
 ;;=5^MDD, Single, NOS
 ;;^UTILITY(U,$J,358.3,15063,2)
 ;;=^73311
 ;;^UTILITY(U,$J,358.3,15064,0)
 ;;=296.21^^109^906^12
 ;;^UTILITY(U,$J,358.3,15064,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15064,1,2,0)
 ;;=2^296.21
 ;;^UTILITY(U,$J,358.3,15064,1,5,0)
 ;;=5^MDD, Single, Mild
 ;;^UTILITY(U,$J,358.3,15064,2)
 ;;=^268110
 ;;^UTILITY(U,$J,358.3,15065,0)
 ;;=296.22^^109^906^13
 ;;^UTILITY(U,$J,358.3,15065,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15065,1,2,0)
 ;;=2^296.22
 ;;^UTILITY(U,$J,358.3,15065,1,5,0)
 ;;=5^MDD, Single, Moderate
 ;;^UTILITY(U,$J,358.3,15065,2)
 ;;=^268111
 ;;^UTILITY(U,$J,358.3,15066,0)
 ;;=296.23^^109^906^5
 ;;^UTILITY(U,$J,358.3,15066,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15066,1,2,0)
 ;;=2^296.23
 ;;^UTILITY(U,$J,358.3,15066,1,5,0)
 ;;=5^MDD Sing, Sev w/o Psychosis
 ;;^UTILITY(U,$J,358.3,15066,2)
 ;;=^268112
 ;;^UTILITY(U,$J,358.3,15067,0)
 ;;=296.24^^109^906^4
 ;;^UTILITY(U,$J,358.3,15067,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15067,1,2,0)
 ;;=2^296.24
 ;;^UTILITY(U,$J,358.3,15067,1,5,0)
 ;;=5^MDD Sing, Sev w/Psychosis
 ;;^UTILITY(U,$J,358.3,15067,2)
 ;;=^268113
 ;;^UTILITY(U,$J,358.3,15068,0)
 ;;=296.25^^109^906^15
 ;;^UTILITY(U,$J,358.3,15068,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15068,1,2,0)
 ;;=2^296.25
 ;;^UTILITY(U,$J,358.3,15068,1,5,0)
 ;;=5^MDD, Single, Part Remiss
 ;;^UTILITY(U,$J,358.3,15068,2)
 ;;=^268114
 ;;^UTILITY(U,$J,358.3,15069,0)
 ;;=296.30^^109^906^9
 ;;^UTILITY(U,$J,358.3,15069,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15069,1,2,0)
 ;;=2^296.30
 ;;^UTILITY(U,$J,358.3,15069,1,5,0)
 ;;=5^MDD, Recur, NOS
 ;;^UTILITY(U,$J,358.3,15069,2)
 ;;=^268116
 ;;^UTILITY(U,$J,358.3,15070,0)
 ;;=296.31^^109^906^7
 ;;^UTILITY(U,$J,358.3,15070,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15070,1,2,0)
 ;;=2^296.31
 ;;^UTILITY(U,$J,358.3,15070,1,5,0)
 ;;=5^MDD, Recur, Mild
 ;;^UTILITY(U,$J,358.3,15070,2)
 ;;=^268117
 ;;^UTILITY(U,$J,358.3,15071,0)
 ;;=296.32^^109^906^8
 ;;^UTILITY(U,$J,358.3,15071,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15071,1,2,0)
 ;;=2^296.32
 ;;^UTILITY(U,$J,358.3,15071,1,5,0)
 ;;=5^MDD, Recur, Moderate
 ;;^UTILITY(U,$J,358.3,15071,2)
 ;;=^268118
 ;;^UTILITY(U,$J,358.3,15072,0)
 ;;=296.33^^109^906^3
 ;;^UTILITY(U,$J,358.3,15072,1,0)
 ;;=^358.31IA^5^2