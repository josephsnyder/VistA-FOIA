IBDEI0BF ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,15196,1,2,0)
 ;;=2^99221
 ;;^UTILITY(U,$J,358.3,15197,0)
 ;;=99222^^111^928^2
 ;;^UTILITY(U,$J,358.3,15197,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15197,1,1,0)
 ;;=1^DETAIL/COMP HX/EXAM;MOD MDM
 ;;^UTILITY(U,$J,358.3,15197,1,2,0)
 ;;=2^99222
 ;;^UTILITY(U,$J,358.3,15198,0)
 ;;=99223^^111^928^3
 ;;^UTILITY(U,$J,358.3,15198,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15198,1,1,0)
 ;;=1^COMP HX/EXAM;HIGH COMP MDM
 ;;^UTILITY(U,$J,358.3,15198,1,2,0)
 ;;=2^99223
 ;;^UTILITY(U,$J,358.3,15199,0)
 ;;=99238^^111^929^1
 ;;^UTILITY(U,$J,358.3,15199,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15199,1,1,0)
 ;;=1^DISCHARGE DAY MGMT < 30MIN
 ;;^UTILITY(U,$J,358.3,15199,1,2,0)
 ;;=2^99238
 ;;^UTILITY(U,$J,358.3,15200,0)
 ;;=99218^^111^930^1
 ;;^UTILITY(U,$J,358.3,15200,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15200,1,1,0)
 ;;=1^DETAIL/COMP HX/EXAM;STRGHT MDM
 ;;^UTILITY(U,$J,358.3,15200,1,2,0)
 ;;=2^99218
 ;;^UTILITY(U,$J,358.3,15201,0)
 ;;=99219^^111^930^2
 ;;^UTILITY(U,$J,358.3,15201,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15201,1,1,0)
 ;;=1^DETAIL/COMP HX/EXAM;MOD MDM
 ;;^UTILITY(U,$J,358.3,15201,1,2,0)
 ;;=2^99219
 ;;^UTILITY(U,$J,358.3,15202,0)
 ;;=99220^^111^930^3
 ;;^UTILITY(U,$J,358.3,15202,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15202,1,1,0)
 ;;=1^COMP HX/EXAM;HIGH COMP MDM
 ;;^UTILITY(U,$J,358.3,15202,1,2,0)
 ;;=2^99220
 ;;^UTILITY(U,$J,358.3,15203,0)
 ;;=99217^^111^931^1
 ;;^UTILITY(U,$J,358.3,15203,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15203,1,1,0)
 ;;=1^Discharge Day Mgmt.
 ;;^UTILITY(U,$J,358.3,15203,1,2,0)
 ;;=2^99217
 ;;^UTILITY(U,$J,358.3,15204,0)
 ;;=99234^^111^932^1
 ;;^UTILITY(U,$J,358.3,15204,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15204,1,1,0)
 ;;=1^DETAIL/COMP HX/EXAM;STRGHT MDM
 ;;^UTILITY(U,$J,358.3,15204,1,2,0)
 ;;=2^99234
 ;;^UTILITY(U,$J,358.3,15205,0)
 ;;=99235^^111^932^2
 ;;^UTILITY(U,$J,358.3,15205,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15205,1,1,0)
 ;;=1^DETAIL/COMP HX/EXAM;MOD MDM
 ;;^UTILITY(U,$J,358.3,15205,1,2,0)
 ;;=2^99235
 ;;^UTILITY(U,$J,358.3,15206,0)
 ;;=99236^^111^932^3
 ;;^UTILITY(U,$J,358.3,15206,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15206,1,1,0)
 ;;=1^COMP HX/EXAM;HIGH COMP MDM
 ;;^UTILITY(U,$J,358.3,15206,1,2,0)
 ;;=2^99236
 ;;^UTILITY(U,$J,358.3,15207,0)
 ;;=99231^^111^933^1
 ;;^UTILITY(U,$J,358.3,15207,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15207,1,1,0)
 ;;=1^PROB FOC HS/EXAM;STRGHT MDM
 ;;^UTILITY(U,$J,358.3,15207,1,2,0)
 ;;=2^99231
 ;;^UTILITY(U,$J,358.3,15208,0)
 ;;=99232^^111^933^2
 ;;^UTILITY(U,$J,358.3,15208,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15208,1,1,0)
 ;;=1^EXP PROB FOC HX/EXAM;MOD MDM
 ;;^UTILITY(U,$J,358.3,15208,1,2,0)
 ;;=2^99232
 ;;^UTILITY(U,$J,358.3,15209,0)
 ;;=99233^^111^933^3
 ;;^UTILITY(U,$J,358.3,15209,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,15209,1,1,0)
 ;;=1^COMP HX/EXAM;HIGH COMP MDM
 ;;^UTILITY(U,$J,358.3,15209,1,2,0)
 ;;=2^99233
 ;;^UTILITY(U,$J,358.3,15210,0)
 ;;=309.24^^112^934^3
 ;;^UTILITY(U,$J,358.3,15210,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15210,1,2,0)
 ;;=2^309.24
 ;;^UTILITY(U,$J,358.3,15210,1,5,0)
 ;;=5^Adj Reac w/Anx Mood
 ;;^UTILITY(U,$J,358.3,15210,2)
 ;;=^268308
 ;;^UTILITY(U,$J,358.3,15211,0)
 ;;=309.4^^112^934^5
 ;;^UTILITY(U,$J,358.3,15211,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15211,1,2,0)
 ;;=2^309.4
 ;;^UTILITY(U,$J,358.3,15211,1,5,0)
 ;;=5^Adj Reac w/Emotion & Conduct
 ;;^UTILITY(U,$J,358.3,15211,2)
 ;;=^268312
 ;;^UTILITY(U,$J,358.3,15212,0)
 ;;=309.28^^112^934^1
 ;;^UTILITY(U,$J,358.3,15212,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15212,1,2,0)
 ;;=2^309.28
 ;;^UTILITY(U,$J,358.3,15212,1,5,0)
 ;;=5^Adj Reac W/Mixed Emotion
 ;;^UTILITY(U,$J,358.3,15212,2)
 ;;=^268309
 ;;^UTILITY(U,$J,358.3,15213,0)
 ;;=309.9^^112^934^8
 ;;^UTILITY(U,$J,358.3,15213,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15213,1,2,0)
 ;;=2^309.9
 ;;^UTILITY(U,$J,358.3,15213,1,5,0)
 ;;=5^Adjustment Reaction NOS
 ;;^UTILITY(U,$J,358.3,15213,2)
 ;;=^123757
 ;;^UTILITY(U,$J,358.3,15214,0)
 ;;=309.0^^112^934^10
 ;;^UTILITY(U,$J,358.3,15214,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15214,1,2,0)
 ;;=2^309.0
 ;;^UTILITY(U,$J,358.3,15214,1,5,0)
 ;;=5^Depressive Reac-Brief
 ;;^UTILITY(U,$J,358.3,15214,2)
 ;;=^3308
 ;;^UTILITY(U,$J,358.3,15215,0)
 ;;=309.1^^112^934^11
 ;;^UTILITY(U,$J,358.3,15215,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15215,1,2,0)
 ;;=2^309.1
 ;;^UTILITY(U,$J,358.3,15215,1,5,0)
 ;;=5^Depressive Reac-Prolong
 ;;^UTILITY(U,$J,358.3,15215,2)
 ;;=^268304
 ;;^UTILITY(U,$J,358.3,15216,0)
 ;;=309.3^^112^934^4
 ;;^UTILITY(U,$J,358.3,15216,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15216,1,2,0)
 ;;=2^309.3
 ;;^UTILITY(U,$J,358.3,15216,1,5,0)
 ;;=5^Adj Reac w/Conduct Disord
 ;;^UTILITY(U,$J,358.3,15216,2)
 ;;=^268311
 ;;^UTILITY(U,$J,358.3,15217,0)
 ;;=V62.82^^112^934^9
 ;;^UTILITY(U,$J,358.3,15217,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15217,1,2,0)
 ;;=2^V62.82
 ;;^UTILITY(U,$J,358.3,15217,1,5,0)
 ;;=5^Bereavement, Uncomplcated
 ;;^UTILITY(U,$J,358.3,15217,2)
 ;;=^13552
 ;;^UTILITY(U,$J,358.3,15218,0)
 ;;=309.81^^112^934^12
 ;;^UTILITY(U,$J,358.3,15218,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15218,1,2,0)
 ;;=2^309.81
 ;;^UTILITY(U,$J,358.3,15218,1,5,0)
 ;;=5^PTSD, Chronic
 ;;^UTILITY(U,$J,358.3,15218,2)
 ;;=^114692
 ;;^UTILITY(U,$J,358.3,15219,0)
 ;;=309.82^^112^934^6
 ;;^UTILITY(U,$J,358.3,15219,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15219,1,2,0)
 ;;=2^309.82
 ;;^UTILITY(U,$J,358.3,15219,1,5,0)
 ;;=5^Adj React w/ Phys Symptom
 ;;^UTILITY(U,$J,358.3,15219,2)
 ;;=^268315
 ;;^UTILITY(U,$J,358.3,15220,0)
 ;;=309.83^^112^934^2
 ;;^UTILITY(U,$J,358.3,15220,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15220,1,2,0)
 ;;=2^309.83
 ;;^UTILITY(U,$J,358.3,15220,1,5,0)
 ;;=5^Adj Reac w/ Withdrawal
 ;;^UTILITY(U,$J,358.3,15220,2)
 ;;=^268316
 ;;^UTILITY(U,$J,358.3,15221,0)
 ;;=309.89^^112^934^7
 ;;^UTILITY(U,$J,358.3,15221,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15221,1,2,0)
 ;;=2^309.89
 ;;^UTILITY(U,$J,358.3,15221,1,5,0)
 ;;=5^Adj Reaction NEC
 ;;^UTILITY(U,$J,358.3,15221,2)
 ;;=^268313
 ;;^UTILITY(U,$J,358.3,15222,0)
 ;;=300.00^^112^935^4
 ;;^UTILITY(U,$J,358.3,15222,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15222,1,2,0)
 ;;=2^300.00
 ;;^UTILITY(U,$J,358.3,15222,1,5,0)
 ;;=5^Anxiety State
 ;;^UTILITY(U,$J,358.3,15222,2)
 ;;=^9200
 ;;^UTILITY(U,$J,358.3,15223,0)
 ;;=300.01^^112^935^9
 ;;^UTILITY(U,$J,358.3,15223,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15223,1,2,0)
 ;;=2^300.01
 ;;^UTILITY(U,$J,358.3,15223,1,5,0)
 ;;=5^Panic Disord w/o Agoraphobia
 ;;^UTILITY(U,$J,358.3,15223,2)
 ;;=^89489
 ;;^UTILITY(U,$J,358.3,15224,0)
 ;;=300.02^^112^935^7
 ;;^UTILITY(U,$J,358.3,15224,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15224,1,2,0)
 ;;=2^300.02
 ;;^UTILITY(U,$J,358.3,15224,1,5,0)
 ;;=5^Generalized Anxiety Dis
 ;;^UTILITY(U,$J,358.3,15224,2)
 ;;=^50059
 ;;^UTILITY(U,$J,358.3,15225,0)
 ;;=300.20^^112^935^14
 ;;^UTILITY(U,$J,358.3,15225,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15225,1,2,0)
 ;;=2^300.20
 ;;^UTILITY(U,$J,358.3,15225,1,5,0)
 ;;=5^Phobia, Unspecified
 ;;^UTILITY(U,$J,358.3,15225,2)
 ;;=^93428
 ;;^UTILITY(U,$J,358.3,15226,0)
 ;;=300.21^^112^935^10
 ;;^UTILITY(U,$J,358.3,15226,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15226,1,2,0)
 ;;=2^300.21
 ;;^UTILITY(U,$J,358.3,15226,1,5,0)
 ;;=5^Panic W/Agoraphobia
 ;;^UTILITY(U,$J,358.3,15226,2)
 ;;=^268168
 ;;^UTILITY(U,$J,358.3,15227,0)
 ;;=300.22^^112^935^3
 ;;^UTILITY(U,$J,358.3,15227,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15227,1,2,0)
 ;;=2^300.22
 ;;^UTILITY(U,$J,358.3,15227,1,5,0)
 ;;=5^Agoraphobia w/o Panic
 ;;^UTILITY(U,$J,358.3,15227,2)
 ;;=^4218
 ;;^UTILITY(U,$J,358.3,15228,0)
 ;;=300.23^^112^935^13
 ;;^UTILITY(U,$J,358.3,15228,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15228,1,2,0)
 ;;=2^300.23
 ;;^UTILITY(U,$J,358.3,15228,1,5,0)
 ;;=5^Phobia, Social
 ;;^UTILITY(U,$J,358.3,15228,2)
 ;;=^93420
 ;;^UTILITY(U,$J,358.3,15229,0)
 ;;=300.29^^112^935^12
 ;;^UTILITY(U,$J,358.3,15229,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15229,1,2,0)
 ;;=2^300.29
 ;;^UTILITY(U,$J,358.3,15229,1,5,0)
 ;;=5^Phobia, Simple
 ;;^UTILITY(U,$J,358.3,15229,2)
 ;;=^87670
 ;;^UTILITY(U,$J,358.3,15230,0)
 ;;=300.3^^112^935^8
 ;;^UTILITY(U,$J,358.3,15230,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15230,1,2,0)
 ;;=2^300.3
 ;;^UTILITY(U,$J,358.3,15230,1,5,0)
 ;;=5^Obsessive/Compulsive
 ;;^UTILITY(U,$J,358.3,15230,2)
 ;;=^84904
 ;;^UTILITY(U,$J,358.3,15231,0)
 ;;=308.9^^112^935^1
 ;;^UTILITY(U,$J,358.3,15231,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15231,1,2,0)
 ;;=2^308.9
 ;;^UTILITY(U,$J,358.3,15231,1,5,0)
 ;;=5^Acute Stress Reaction
 ;;^UTILITY(U,$J,358.3,15231,2)
 ;;=^268303
 ;;^UTILITY(U,$J,358.3,15232,0)
 ;;=300.15^^112^935^6
 ;;^UTILITY(U,$J,358.3,15232,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15232,1,2,0)
 ;;=2^300.15
 ;;^UTILITY(U,$J,358.3,15232,1,5,0)
 ;;=5^Dissociative Reaction
 ;;^UTILITY(U,$J,358.3,15232,2)
 ;;=^35700
 ;;^UTILITY(U,$J,358.3,15233,0)
 ;;=291.1^^112^936^1
 ;;^UTILITY(U,$J,358.3,15233,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15233,1,2,0)
 ;;=2^291.1
 ;;^UTILITY(U,$J,358.3,15233,1,5,0)
 ;;=5^Amnestic Syndrome Due to Alcohol
 ;;^UTILITY(U,$J,358.3,15233,2)
 ;;=^303492
 ;;^UTILITY(U,$J,358.3,15234,0)
 ;;=294.0^^112^936^3
 ;;^UTILITY(U,$J,358.3,15234,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15234,1,2,0)
 ;;=2^294.0
 ;;^UTILITY(U,$J,358.3,15234,1,5,0)
 ;;=5^Amnestic Syndrome, NOS
 ;;^UTILITY(U,$J,358.3,15234,2)
 ;;=^6319
 ;;^UTILITY(U,$J,358.3,15235,0)
 ;;=292.83^^112^936^2
 ;;^UTILITY(U,$J,358.3,15235,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15235,1,2,0)
 ;;=2^292.83
 ;;^UTILITY(U,$J,358.3,15235,1,5,0)
 ;;=5^Amnestic Syndrome Due to Drugs
 ;;^UTILITY(U,$J,358.3,15235,2)
 ;;=^268027
 ;;^UTILITY(U,$J,358.3,15236,0)
 ;;=310.1^^112^937^7
