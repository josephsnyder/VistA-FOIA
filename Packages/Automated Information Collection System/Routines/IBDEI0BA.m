IBDEI0BA ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,14998,1,2,0)
 ;;=2^304.62
 ;;^UTILITY(U,$J,358.3,14998,1,5,0)
 ;;=5^Drug Depend-Other, Episodic
 ;;^UTILITY(U,$J,358.3,14998,2)
 ;;=^268212
 ;;^UTILITY(U,$J,358.3,14999,0)
 ;;=304.71^^109^903^66
 ;;^UTILITY(U,$J,358.3,14999,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14999,1,2,0)
 ;;=2^304.71
 ;;^UTILITY(U,$J,358.3,14999,1,5,0)
 ;;=5^Opioid + Other Depend, Continuous
 ;;^UTILITY(U,$J,358.3,14999,2)
 ;;=^268215
 ;;^UTILITY(U,$J,358.3,15000,0)
 ;;=304.72^^109^903^67
 ;;^UTILITY(U,$J,358.3,15000,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15000,1,2,0)
 ;;=2^304.72
 ;;^UTILITY(U,$J,358.3,15000,1,5,0)
 ;;=5^Opioid + Other Depend, Episodic
 ;;^UTILITY(U,$J,358.3,15000,2)
 ;;=^268216
 ;;^UTILITY(U,$J,358.3,15001,0)
 ;;=304.81^^109^903^48
 ;;^UTILITY(U,$J,358.3,15001,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15001,1,2,0)
 ;;=2^304.81
 ;;^UTILITY(U,$J,358.3,15001,1,5,0)
 ;;=5^Comb Drug Depend, Continuous
 ;;^UTILITY(U,$J,358.3,15001,2)
 ;;=^268219
 ;;^UTILITY(U,$J,358.3,15002,0)
 ;;=304.82^^109^903^49
 ;;^UTILITY(U,$J,358.3,15002,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15002,1,2,0)
 ;;=2^304.82
 ;;^UTILITY(U,$J,358.3,15002,1,5,0)
 ;;=5^Comb Drug Depend, Episodic
 ;;^UTILITY(U,$J,358.3,15002,2)
 ;;=^268220
 ;;^UTILITY(U,$J,358.3,15003,0)
 ;;=305.01^^109^903^1
 ;;^UTILITY(U,$J,358.3,15003,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15003,1,2,0)
 ;;=2^305.01
 ;;^UTILITY(U,$J,358.3,15003,1,5,0)
 ;;=5^Alc Abuse, Continuous
 ;;^UTILITY(U,$J,358.3,15003,2)
 ;;=^268228
 ;;^UTILITY(U,$J,358.3,15004,0)
 ;;=305.02^^109^903^2
 ;;^UTILITY(U,$J,358.3,15004,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15004,1,2,0)
 ;;=2^305.02
 ;;^UTILITY(U,$J,358.3,15004,1,5,0)
 ;;=5^Alc Abuse, Episodic
 ;;^UTILITY(U,$J,358.3,15004,2)
 ;;=^268229
 ;;^UTILITY(U,$J,358.3,15005,0)
 ;;=305.21^^109^903^33
 ;;^UTILITY(U,$J,358.3,15005,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15005,1,2,0)
 ;;=2^305.21
 ;;^UTILITY(U,$J,358.3,15005,1,5,0)
 ;;=5^Cannabis Abuse, Continued
 ;;^UTILITY(U,$J,358.3,15005,2)
 ;;=^268234
 ;;^UTILITY(U,$J,358.3,15006,0)
 ;;=305.22^^109^903^34
 ;;^UTILITY(U,$J,358.3,15006,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15006,1,2,0)
 ;;=2^305.22
 ;;^UTILITY(U,$J,358.3,15006,1,5,0)
 ;;=5^Cannabis Abuse, Episodic
 ;;^UTILITY(U,$J,358.3,15006,2)
 ;;=^268235
 ;;^UTILITY(U,$J,358.3,15007,0)
 ;;=305.31^^109^903^57
 ;;^UTILITY(U,$J,358.3,15007,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15007,1,2,0)
 ;;=2^305.31
 ;;^UTILITY(U,$J,358.3,15007,1,5,0)
 ;;=5^Hallucinogen Abuse, Continuous
 ;;^UTILITY(U,$J,358.3,15007,2)
 ;;=^268237
 ;;^UTILITY(U,$J,358.3,15008,0)
 ;;=305.32^^109^903^58
 ;;^UTILITY(U,$J,358.3,15008,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15008,1,2,0)
 ;;=2^305.32
 ;;^UTILITY(U,$J,358.3,15008,1,5,0)
 ;;=5^Hallucinogen Abuse, Episodic
 ;;^UTILITY(U,$J,358.3,15008,2)
 ;;=^268238
 ;;^UTILITY(U,$J,358.3,15009,0)
 ;;=305.41^^109^903^25
 ;;^UTILITY(U,$J,358.3,15009,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15009,1,2,0)
 ;;=2^305.41
 ;;^UTILITY(U,$J,358.3,15009,1,5,0)
 ;;=5^Anxiolytic Abuse, Continuous
 ;;^UTILITY(U,$J,358.3,15009,2)
 ;;=^331936
 ;;^UTILITY(U,$J,358.3,15010,0)
 ;;=305.42^^109^903^26
 ;;^UTILITY(U,$J,358.3,15010,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15010,1,2,0)
 ;;=2^305.42
 ;;^UTILITY(U,$J,358.3,15010,1,5,0)
 ;;=5^Anxiolytic Abuse, Episodic
 ;;^UTILITY(U,$J,358.3,15010,2)
 ;;=^331937
 ;;^UTILITY(U,$J,358.3,15011,0)
 ;;=305.51^^109^903^69
 ;;^UTILITY(U,$J,358.3,15011,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15011,1,2,0)
 ;;=2^305.51
 ;;^UTILITY(U,$J,358.3,15011,1,5,0)
 ;;=5^Opioid Abuse, Continuous
 ;;^UTILITY(U,$J,358.3,15011,2)
 ;;=^268244
 ;;^UTILITY(U,$J,358.3,15012,0)
 ;;=305.52^^109^903^70
 ;;^UTILITY(U,$J,358.3,15012,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15012,1,2,0)
 ;;=2^305.52
 ;;^UTILITY(U,$J,358.3,15012,1,5,0)
 ;;=5^Opioid Abuse, Episodic
 ;;^UTILITY(U,$J,358.3,15012,2)
 ;;=^268245
 ;;^UTILITY(U,$J,358.3,15013,0)
 ;;=305.61^^109^903^41
 ;;^UTILITY(U,$J,358.3,15013,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15013,1,2,0)
 ;;=2^305.61
 ;;^UTILITY(U,$J,358.3,15013,1,5,0)
 ;;=5^Cocaine Abuse, Continuous
 ;;^UTILITY(U,$J,358.3,15013,2)
 ;;=^268247
 ;;^UTILITY(U,$J,358.3,15014,0)
 ;;=305.62^^109^903^42
 ;;^UTILITY(U,$J,358.3,15014,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15014,1,2,0)
 ;;=2^305.62
 ;;^UTILITY(U,$J,358.3,15014,1,5,0)
 ;;=5^Cocaine Abuse, Episodic
 ;;^UTILITY(U,$J,358.3,15014,2)
 ;;=^268248
 ;;^UTILITY(U,$J,358.3,15015,0)
 ;;=305.71^^109^903^17
 ;;^UTILITY(U,$J,358.3,15015,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15015,1,2,0)
 ;;=2^305.71
 ;;^UTILITY(U,$J,358.3,15015,1,5,0)
 ;;=5^Amphetamine Abuse, Continuous
 ;;^UTILITY(U,$J,358.3,15015,2)
 ;;=^268251
 ;;^UTILITY(U,$J,358.3,15016,0)
 ;;=305.72^^109^903^18
 ;;^UTILITY(U,$J,358.3,15016,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15016,1,2,0)
 ;;=2^305.72
 ;;^UTILITY(U,$J,358.3,15016,1,5,0)
 ;;=5^Amphetamine Abuse, Episodic
 ;;^UTILITY(U,$J,358.3,15016,2)
 ;;=^268252
 ;;^UTILITY(U,$J,358.3,15017,0)
 ;;=305.91^^109^903^77
 ;;^UTILITY(U,$J,358.3,15017,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15017,1,2,0)
 ;;=2^305.91
 ;;^UTILITY(U,$J,358.3,15017,1,5,0)
 ;;=5^Other Drug Abuse, Continuous
 ;;^UTILITY(U,$J,358.3,15017,2)
 ;;=^268259
 ;;^UTILITY(U,$J,358.3,15018,0)
 ;;=305.92^^109^903^78
 ;;^UTILITY(U,$J,358.3,15018,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15018,1,2,0)
 ;;=2^305.92
 ;;^UTILITY(U,$J,358.3,15018,1,5,0)
 ;;=5^Other Drug Abuse, Episodic
 ;;^UTILITY(U,$J,358.3,15018,2)
 ;;=^268260
 ;;^UTILITY(U,$J,358.3,15019,0)
 ;;=V65.2^^109^904^20
 ;;^UTILITY(U,$J,358.3,15019,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15019,1,2,0)
 ;;=2^V65.2
 ;;^UTILITY(U,$J,358.3,15019,1,5,0)
 ;;=5^Malingering
 ;;^UTILITY(U,$J,358.3,15019,2)
 ;;=^92393
 ;;^UTILITY(U,$J,358.3,15020,0)
 ;;=V65.49^^109^904^25
 ;;^UTILITY(U,$J,358.3,15020,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15020,1,2,0)
 ;;=2^V65.49
 ;;^UTILITY(U,$J,358.3,15020,1,5,0)
 ;;=5^Other Specified Counseling
 ;;^UTILITY(U,$J,358.3,15020,2)
 ;;=^303471
 ;;^UTILITY(U,$J,358.3,15021,0)
 ;;=V61.10^^109^904^30
 ;;^UTILITY(U,$J,358.3,15021,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15021,1,2,0)
 ;;=2^V61.10
 ;;^UTILITY(U,$J,358.3,15021,1,5,0)
 ;;=5^Partner Relational Problem
 ;;^UTILITY(U,$J,358.3,15021,2)
 ;;=^74110
 ;;^UTILITY(U,$J,358.3,15022,0)
 ;;=V61.20^^109^904^28
 ;;^UTILITY(U,$J,358.3,15022,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15022,1,2,0)
 ;;=2^V61.20
 ;;^UTILITY(U,$J,358.3,15022,1,5,0)
 ;;=5^Parent-Child Problem NOS
 ;;^UTILITY(U,$J,358.3,15022,2)
 ;;=^304300
 ;;^UTILITY(U,$J,358.3,15023,0)
 ;;=V61.12^^109^904^2
 ;;^UTILITY(U,$J,358.3,15023,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15023,1,2,0)
 ;;=2^V61.12
 ;;^UTILITY(U,$J,358.3,15023,1,5,0)
 ;;=5^Domestic Violence/Perpet
 ;;^UTILITY(U,$J,358.3,15023,2)
 ;;=^304356
 ;;^UTILITY(U,$J,358.3,15024,0)
 ;;=V61.11^^109^904^3
 ;;^UTILITY(U,$J,358.3,15024,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15024,1,2,0)
 ;;=2^V61.11
 ;;^UTILITY(U,$J,358.3,15024,1,5,0)
 ;;=5^Domestic Violence/Victim
 ;;^UTILITY(U,$J,358.3,15024,2)
 ;;=^304357
 ;;^UTILITY(U,$J,358.3,15025,0)
 ;;=V62.0^^109^904^36
 ;;^UTILITY(U,$J,358.3,15025,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15025,1,2,0)
 ;;=2^V62.0
 ;;^UTILITY(U,$J,358.3,15025,1,5,0)
 ;;=5^Unemployment
 ;;^UTILITY(U,$J,358.3,15025,2)
 ;;=^123545
 ;;^UTILITY(U,$J,358.3,15026,0)
 ;;=V69.2^^109^904^15
 ;;^UTILITY(U,$J,358.3,15026,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15026,1,2,0)
 ;;=2^V69.2
 ;;^UTILITY(U,$J,358.3,15026,1,5,0)
 ;;=5^Hi-Risk Sexual Behavior
 ;;^UTILITY(U,$J,358.3,15026,2)
 ;;=^303474
 ;;^UTILITY(U,$J,358.3,15027,0)
 ;;=V62.82^^109^904^1
 ;;^UTILITY(U,$J,358.3,15027,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15027,1,2,0)
 ;;=2^V62.82
 ;;^UTILITY(U,$J,358.3,15027,1,5,0)
 ;;=5^Bereavement/Uncomplicat
 ;;^UTILITY(U,$J,358.3,15027,2)
 ;;=^123500
 ;;^UTILITY(U,$J,358.3,15028,0)
 ;;=V70.1^^109^904^33
 ;;^UTILITY(U,$J,358.3,15028,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15028,1,2,0)
 ;;=2^V70.1
 ;;^UTILITY(U,$J,358.3,15028,1,5,0)
 ;;=5^Psych Exam, Mandated
 ;;^UTILITY(U,$J,358.3,15028,2)
 ;;=^295591
 ;;^UTILITY(U,$J,358.3,15029,0)
 ;;=V60.2^^109^904^4
 ;;^UTILITY(U,$J,358.3,15029,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15029,1,2,0)
 ;;=2^V60.2
 ;;^UTILITY(U,$J,358.3,15029,1,5,0)
 ;;=5^Economic Problem
 ;;^UTILITY(U,$J,358.3,15029,2)
 ;;=^62174
 ;;^UTILITY(U,$J,358.3,15030,0)
 ;;=V62.89^^109^904^34
 ;;^UTILITY(U,$J,358.3,15030,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15030,1,2,0)
 ;;=2^V62.89
 ;;^UTILITY(U,$J,358.3,15030,1,5,0)
 ;;=5^Psychological Stress
 ;;^UTILITY(U,$J,358.3,15030,2)
 ;;=^87822
 ;;^UTILITY(U,$J,358.3,15031,0)
 ;;=V62.9^^109^904^35
 ;;^UTILITY(U,$J,358.3,15031,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15031,1,2,0)
 ;;=2^V62.9
 ;;^UTILITY(U,$J,358.3,15031,1,5,0)
 ;;=5^Psychosocial Circum
 ;;^UTILITY(U,$J,358.3,15031,2)
 ;;=^295551
 ;;^UTILITY(U,$J,358.3,15032,0)
 ;;=V60.0^^109^904^19
 ;;^UTILITY(U,$J,358.3,15032,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15032,1,2,0)
 ;;=2^V60.0
 ;;^UTILITY(U,$J,358.3,15032,1,5,0)
 ;;=5^Lack Of Housing
 ;;^UTILITY(U,$J,358.3,15032,2)
 ;;=^295539
 ;;^UTILITY(U,$J,358.3,15033,0)
 ;;=V62.81^^109^904^18
 ;;^UTILITY(U,$J,358.3,15033,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15033,1,2,0)
 ;;=2^V62.81
 ;;^UTILITY(U,$J,358.3,15033,1,5,0)
 ;;=5^Interpersonal Problem
 ;;^UTILITY(U,$J,358.3,15033,2)
 ;;=^276358
 ;;^UTILITY(U,$J,358.3,15034,0)
 ;;=V71.01^^109^904^21
 ;;^UTILITY(U,$J,358.3,15034,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,15034,1,2,0)
 ;;=2^V71.01
 ;;^UTILITY(U,$J,358.3,15034,1,5,0)
 ;;=5^Observ-Antisocial Behav
 ;;^UTILITY(U,$J,358.3,15034,2)
 ;;=^295603
 ;;^UTILITY(U,$J,358.3,15035,0)
 ;;=V71.09^^109^904^22
 ;;^UTILITY(U,$J,358.3,15035,1,0)
 ;;=^358.31IA^5^2
