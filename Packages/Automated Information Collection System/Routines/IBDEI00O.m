IBDEI00O ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,195,1,5,0)
 ;;=5^Parent-Child Problem
 ;;^UTILITY(U,$J,358.3,195,2)
 ;;=^304300
 ;;^UTILITY(U,$J,358.3,196,0)
 ;;=V61.12^^1^14^3
 ;;^UTILITY(U,$J,358.3,196,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,196,1,2,0)
 ;;=2^V61.12
 ;;^UTILITY(U,$J,358.3,196,1,5,0)
 ;;=5^Domestic Violence/Perpet
 ;;^UTILITY(U,$J,358.3,196,2)
 ;;=^304356
 ;;^UTILITY(U,$J,358.3,197,0)
 ;;=V61.11^^1^14^4
 ;;^UTILITY(U,$J,358.3,197,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,197,1,2,0)
 ;;=2^V61.11
 ;;^UTILITY(U,$J,358.3,197,1,5,0)
 ;;=5^Domestic Violence/Victim
 ;;^UTILITY(U,$J,358.3,197,2)
 ;;=^304357
 ;;^UTILITY(U,$J,358.3,198,0)
 ;;=V62.0^^1^14^25
 ;;^UTILITY(U,$J,358.3,198,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,198,1,2,0)
 ;;=2^V62.0
 ;;^UTILITY(U,$J,358.3,198,1,5,0)
 ;;=5^Unemployment
 ;;^UTILITY(U,$J,358.3,198,2)
 ;;=^123545
 ;;^UTILITY(U,$J,358.3,199,0)
 ;;=V69.2^^1^14^11
 ;;^UTILITY(U,$J,358.3,199,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,199,1,2,0)
 ;;=2^V69.2
 ;;^UTILITY(U,$J,358.3,199,1,5,0)
 ;;=5^Hi-Risk Sexual Behavior
 ;;^UTILITY(U,$J,358.3,199,2)
 ;;=^303474
 ;;^UTILITY(U,$J,358.3,200,0)
 ;;=V62.82^^1^14^1
 ;;^UTILITY(U,$J,358.3,200,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,200,1,2,0)
 ;;=2^V62.82
 ;;^UTILITY(U,$J,358.3,200,1,5,0)
 ;;=5^Bereavement/Uncomplicat
 ;;^UTILITY(U,$J,358.3,200,2)
 ;;=^123500
 ;;^UTILITY(U,$J,358.3,201,0)
 ;;=V70.1^^1^14^8
 ;;^UTILITY(U,$J,358.3,201,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,201,1,2,0)
 ;;=2^V70.1
 ;;^UTILITY(U,$J,358.3,201,1,5,0)
 ;;=5^Gen Psy Exam Requested by Authority
 ;;^UTILITY(U,$J,358.3,201,2)
 ;;=^295591
 ;;^UTILITY(U,$J,358.3,202,0)
 ;;=V60.2^^1^14^5
 ;;^UTILITY(U,$J,358.3,202,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,202,1,2,0)
 ;;=2^V60.2
 ;;^UTILITY(U,$J,358.3,202,1,5,0)
 ;;=5^Economic Problem
 ;;^UTILITY(U,$J,358.3,202,2)
 ;;=^62174
 ;;^UTILITY(U,$J,358.3,203,0)
 ;;=V62.89^^1^14^23
 ;;^UTILITY(U,$J,358.3,203,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,203,1,2,0)
 ;;=2^V62.89
 ;;^UTILITY(U,$J,358.3,203,1,5,0)
 ;;=5^Psychological Stress
 ;;^UTILITY(U,$J,358.3,203,2)
 ;;=^87822
 ;;^UTILITY(U,$J,358.3,204,0)
 ;;=V62.9^^1^14^24
 ;;^UTILITY(U,$J,358.3,204,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,204,1,2,0)
 ;;=2^V62.9
 ;;^UTILITY(U,$J,358.3,204,1,5,0)
 ;;=5^Psychosocial Circum
 ;;^UTILITY(U,$J,358.3,204,2)
 ;;=^295551
 ;;^UTILITY(U,$J,358.3,205,0)
 ;;=V60.0^^1^14^15
 ;;^UTILITY(U,$J,358.3,205,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,205,1,2,0)
 ;;=2^V60.0
 ;;^UTILITY(U,$J,358.3,205,1,5,0)
 ;;=5^Lack Of Housing
 ;;^UTILITY(U,$J,358.3,205,2)
 ;;=^295539
 ;;^UTILITY(U,$J,358.3,206,0)
 ;;=V62.81^^1^14^14
 ;;^UTILITY(U,$J,358.3,206,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,206,1,2,0)
 ;;=2^V62.81
 ;;^UTILITY(U,$J,358.3,206,1,5,0)
 ;;=5^Interpersonal Problem
 ;;^UTILITY(U,$J,358.3,206,2)
 ;;=^276358
 ;;^UTILITY(U,$J,358.3,207,0)
 ;;=V71.01^^1^14^17
 ;;^UTILITY(U,$J,358.3,207,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,207,1,2,0)
 ;;=2^V71.01
 ;;^UTILITY(U,$J,358.3,207,1,5,0)
 ;;=5^Observ-Antisocial Behav
 ;;^UTILITY(U,$J,358.3,207,2)
 ;;=^295603
 ;;^UTILITY(U,$J,358.3,208,0)
 ;;=V71.09^^1^14^18
 ;;^UTILITY(U,$J,358.3,208,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,208,1,2,0)
 ;;=2^V71.09
 ;;^UTILITY(U,$J,358.3,208,1,5,0)
 ;;=5^Observ-Mental Condition
 ;;^UTILITY(U,$J,358.3,208,2)
 ;;=^295604
 ;;^UTILITY(U,$J,358.3,209,0)
 ;;=V15.41^^1^14^13
 ;;^UTILITY(U,$J,358.3,209,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,209,1,2,0)
 ;;=2^V15.41
 ;;^UTILITY(U,$J,358.3,209,1,5,0)
 ;;=5^Hx Of Sexual Abuse
 ;;^UTILITY(U,$J,358.3,209,2)
 ;;=^304352
 ;;^UTILITY(U,$J,358.3,210,0)
 ;;=V61.09^^1^14^6
 ;;^UTILITY(U,$J,358.3,210,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,210,1,2,0)
 ;;=2^V61.09
 ;;^UTILITY(U,$J,358.3,210,1,5,0)
 ;;=5^Family Disruption NEC
 ;;^UTILITY(U,$J,358.3,210,2)
 ;;=^336805
 ;;^UTILITY(U,$J,358.3,211,0)
 ;;=V62.21^^1^14^9
 ;;^UTILITY(U,$J,358.3,211,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,211,1,2,0)
 ;;=2^V62.21
 ;;^UTILITY(U,$J,358.3,211,1,5,0)
 ;;=5^HX Military Deployment
 ;;^UTILITY(U,$J,358.3,211,2)
 ;;=^336806
 ;;^UTILITY(U,$J,358.3,212,0)
 ;;=V62.22^^1^14^10
 ;;^UTILITY(U,$J,358.3,212,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,212,1,2,0)
 ;;=2^V62.22
 ;;^UTILITY(U,$J,358.3,212,1,5,0)
 ;;=5^HX Retrn Military Deploy
 ;;^UTILITY(U,$J,358.3,212,2)
 ;;=^336807
 ;;^UTILITY(U,$J,358.3,213,0)
 ;;=V62.29^^1^14^19
 ;;^UTILITY(U,$J,358.3,213,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,213,1,2,0)
 ;;=2^V62.29
 ;;^UTILITY(U,$J,358.3,213,1,5,0)
 ;;=5^Occupationl Circumst NEC
 ;;^UTILITY(U,$J,358.3,213,2)
 ;;=^87746
 ;;^UTILITY(U,$J,358.3,214,0)
 ;;=V60.81^^1^14^7
 ;;^UTILITY(U,$J,358.3,214,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,214,1,2,0)
 ;;=2^V60.81
 ;;^UTILITY(U,$J,358.3,214,1,5,0)
 ;;=5^Foster Care (status)
 ;;^UTILITY(U,$J,358.3,214,2)
 ;;=^338505
 ;;^UTILITY(U,$J,358.3,215,0)
 ;;=V60.89^^1^14^12
 ;;^UTILITY(U,$J,358.3,215,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,215,1,2,0)
 ;;=2^V60.89
 ;;^UTILITY(U,$J,358.3,215,1,5,0)
 ;;=5^Housing/Econom Circum NEC
 ;;^UTILITY(U,$J,358.3,215,2)
 ;;=^295545
 ;;^UTILITY(U,$J,358.3,216,0)
 ;;=V87.39^^1^14^2
 ;;^UTILITY(U,$J,358.3,216,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,216,1,2,0)
 ;;=2^V87.39
 ;;^UTILITY(U,$J,358.3,216,1,5,0)
 ;;=5^Cont/Exp Hazard Sub NEC
 ;;^UTILITY(U,$J,358.3,216,2)
 ;;=^336815
 ;;^UTILITY(U,$J,358.3,217,0)
 ;;=V65.40^^1^15^2
 ;;^UTILITY(U,$J,358.3,217,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,217,1,2,0)
 ;;=2^V65.40
 ;;^UTILITY(U,$J,358.3,217,1,5,0)
 ;;=5^Counseling
 ;;^UTILITY(U,$J,358.3,217,2)
 ;;=^87449
 ;;^UTILITY(U,$J,358.3,218,0)
 ;;=V65.42^^1^15^7
 ;;^UTILITY(U,$J,358.3,218,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,218,1,2,0)
 ;;=2^V65.42
 ;;^UTILITY(U,$J,358.3,218,1,5,0)
 ;;=5^Substance Use/Abuse Counseling
 ;;^UTILITY(U,$J,358.3,218,2)
 ;;=^303467
 ;;^UTILITY(U,$J,358.3,219,0)
 ;;=V79.1^^1^15^1
 ;;^UTILITY(U,$J,358.3,219,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,219,1,2,0)
 ;;=2^V79.1
 ;;^UTILITY(U,$J,358.3,219,1,5,0)
 ;;=5^Alcohol Screen
 ;;^UTILITY(U,$J,358.3,219,2)
 ;;=^295678
 ;;^UTILITY(U,$J,358.3,220,0)
 ;;=V68.1^^1^15^4
 ;;^UTILITY(U,$J,358.3,220,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,220,1,2,0)
 ;;=2^V68.1
 ;;^UTILITY(U,$J,358.3,220,1,5,0)
 ;;=5^Issue Repeat Prescription
 ;;^UTILITY(U,$J,358.3,220,2)
 ;;=^295585
 ;;^UTILITY(U,$J,358.3,221,0)
 ;;=V65.44^^1^15^3
 ;;^UTILITY(U,$J,358.3,221,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,221,1,2,0)
 ;;=2^V65.44
 ;;^UTILITY(U,$J,358.3,221,1,5,0)
 ;;=5^HIV Counseling
 ;;^UTILITY(U,$J,358.3,221,2)
 ;;=^303469
 ;;^UTILITY(U,$J,358.3,222,0)
 ;;=V65.49^^1^15^5
 ;;^UTILITY(U,$J,358.3,222,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,222,1,2,0)
 ;;=2^V65.49
 ;;^UTILITY(U,$J,358.3,222,1,5,0)
 ;;=5^Other Specified Counseling
 ;;^UTILITY(U,$J,358.3,222,2)
 ;;=^303471
 ;;^UTILITY(U,$J,358.3,223,0)
 ;;=V70.2^^1^15^6
 ;;^UTILITY(U,$J,358.3,223,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,223,1,2,0)
 ;;=2^V70.2
 ;;^UTILITY(U,$J,358.3,223,1,5,0)
 ;;=5^Psychiatric Examination
 ;;^UTILITY(U,$J,358.3,223,2)
 ;;=^295592
 ;;^UTILITY(U,$J,358.3,224,0)
 ;;=V61.21^^1^15^8
 ;;^UTILITY(U,$J,358.3,224,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,224,1,2,0)
 ;;=2^V61.21
 ;;^UTILITY(U,$J,358.3,224,1,5,0)
 ;;=5^Victim Child Abuse Counseling
 ;;^UTILITY(U,$J,358.3,224,2)
 ;;=^304301
 ;;^UTILITY(U,$J,358.3,225,0)
 ;;=296.20^^1^16^1
 ;;^UTILITY(U,$J,358.3,225,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,225,1,2,0)
 ;;=2^296.20
 ;;^UTILITY(U,$J,358.3,225,1,5,0)
 ;;=5^MDD, Sing, Unspec
 ;;^UTILITY(U,$J,358.3,225,2)
 ;;=^73311
 ;;^UTILITY(U,$J,358.3,226,0)
 ;;=296.21^^1^16^2
 ;;^UTILITY(U,$J,358.3,226,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,226,1,2,0)
 ;;=2^296.21
 ;;^UTILITY(U,$J,358.3,226,1,5,0)
 ;;=5^MDD, Single, Mild
 ;;^UTILITY(U,$J,358.3,226,2)
 ;;=MDD, Single, Mild^268110
 ;;^UTILITY(U,$J,358.3,227,0)
 ;;=296.22^^1^16^3
 ;;^UTILITY(U,$J,358.3,227,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,227,1,2,0)
 ;;=2^296.22
 ;;^UTILITY(U,$J,358.3,227,1,5,0)
 ;;=5^MDD, Single, Moderate
 ;;^UTILITY(U,$J,358.3,227,2)
 ;;=MDD, Single, Moderate^268111
 ;;^UTILITY(U,$J,358.3,228,0)
 ;;=296.23^^1^16^4
 ;;^UTILITY(U,$J,358.3,228,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,228,1,2,0)
 ;;=2^296.23
 ;;^UTILITY(U,$J,358.3,228,1,5,0)
 ;;=5^MDD Sing, Sev w/o Psychosis
 ;;^UTILITY(U,$J,358.3,228,2)
 ;;=MDD S^268112
 ;;^UTILITY(U,$J,358.3,229,0)
 ;;=296.24^^1^16^5
 ;;^UTILITY(U,$J,358.3,229,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,229,1,2,0)
 ;;=2^296.24
 ;;^UTILITY(U,$J,358.3,229,1,5,0)
 ;;=5^MDD Sing, Sev w/Psychosis
 ;;^UTILITY(U,$J,358.3,229,2)
 ;;=^268113
 ;;^UTILITY(U,$J,358.3,230,0)
 ;;=296.25^^1^16^6
 ;;^UTILITY(U,$J,358.3,230,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,230,1,2,0)
 ;;=2^296.25
 ;;^UTILITY(U,$J,358.3,230,1,5,0)
 ;;=5^MDD, Single, Part Remission
 ;;^UTILITY(U,$J,358.3,230,2)
 ;;=^268114
 ;;^UTILITY(U,$J,358.3,231,0)
 ;;=296.30^^1^16^7
 ;;^UTILITY(U,$J,358.3,231,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,231,1,2,0)
 ;;=2^296.30
 ;;^UTILITY(U,$J,358.3,231,1,5,0)
 ;;=5^MDD, Recurrent, Unspe
 ;;^UTILITY(U,$J,358.3,231,2)
 ;;=MDD, Recurrent, Unspe^268116
 ;;^UTILITY(U,$J,358.3,232,0)
 ;;=296.31^^1^16^8
 ;;^UTILITY(U,$J,358.3,232,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,232,1,2,0)
 ;;=2^296.31
 ;;^UTILITY(U,$J,358.3,232,1,5,0)
 ;;=5^MDD, Recurrent, Mild
 ;;^UTILITY(U,$J,358.3,232,2)
 ;;=^268117
 ;;^UTILITY(U,$J,358.3,233,0)
 ;;=296.32^^1^16^9
 ;;^UTILITY(U,$J,358.3,233,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,233,1,2,0)
 ;;=2^296.32
 ;;^UTILITY(U,$J,358.3,233,1,5,0)
 ;;=5^MDD, Recur, Moderate
 ;;^UTILITY(U,$J,358.3,233,2)
 ;;=^268118
 ;;^UTILITY(U,$J,358.3,234,0)
 ;;=296.33^^1^16^10
 ;;^UTILITY(U,$J,358.3,234,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,234,1,2,0)
 ;;=2^296.33
