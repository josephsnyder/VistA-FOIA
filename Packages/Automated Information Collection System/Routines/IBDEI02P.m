IBDEI02P ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,3125,0)
 ;;=99217^^39^246^1
 ;;^UTILITY(U,$J,358.3,3125,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,3125,1,1,0)
 ;;=1^OBSERVATION D/C DAY
 ;;^UTILITY(U,$J,358.3,3125,1,2,0)
 ;;=2^99217
 ;;^UTILITY(U,$J,358.3,3126,0)
 ;;=99291^^39^247^1
 ;;^UTILITY(U,$J,358.3,3126,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,3126,1,1,0)
 ;;=1^CRITICAL CARE E&M 30-74MIN
 ;;^UTILITY(U,$J,358.3,3126,1,2,0)
 ;;=2^99291
 ;;^UTILITY(U,$J,358.3,3127,0)
 ;;=99292^^39^247^2
 ;;^UTILITY(U,$J,358.3,3127,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,3127,1,1,0)
 ;;=1^CRITICAL CARE,EA ADDL 30 MIN
 ;;^UTILITY(U,$J,358.3,3127,1,2,0)
 ;;=2^99292
 ;;^UTILITY(U,$J,358.3,3128,0)
 ;;=414.01^^40^248^14
 ;;^UTILITY(U,$J,358.3,3128,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3128,1,4,0)
 ;;=4^414.01
 ;;^UTILITY(U,$J,358.3,3128,1,5,0)
 ;;=5^Atherosclerosis, native coronary
 ;;^UTILITY(U,$J,358.3,3128,2)
 ;;=CAD, Native Vessel^303281
 ;;^UTILITY(U,$J,358.3,3129,0)
 ;;=413.9^^40^248^4
 ;;^UTILITY(U,$J,358.3,3129,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3129,1,4,0)
 ;;=4^413.9
 ;;^UTILITY(U,$J,358.3,3129,1,5,0)
 ;;=5^Angina Pectoris
 ;;^UTILITY(U,$J,358.3,3129,2)
 ;;=Angina Pectoris^87258
 ;;^UTILITY(U,$J,358.3,3130,0)
 ;;=413.0^^40^248^5
 ;;^UTILITY(U,$J,358.3,3130,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3130,1,4,0)
 ;;=4^413.0
 ;;^UTILITY(U,$J,358.3,3130,1,5,0)
 ;;=5^Angina at Rest
 ;;^UTILITY(U,$J,358.3,3130,2)
 ;;=Angina at Rest^265313
 ;;^UTILITY(U,$J,358.3,3131,0)
 ;;=411.1^^40^248^7
 ;;^UTILITY(U,$J,358.3,3131,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3131,1,4,0)
 ;;=4^411.1
 ;;^UTILITY(U,$J,358.3,3131,1,5,0)
 ;;=5^Angina, Unstable
 ;;^UTILITY(U,$J,358.3,3131,2)
 ;;=Angina, Unstable^7455
 ;;^UTILITY(U,$J,358.3,3132,0)
 ;;=413.1^^40^248^6
 ;;^UTILITY(U,$J,358.3,3132,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3132,1,4,0)
 ;;=4^413.1
 ;;^UTILITY(U,$J,358.3,3132,1,5,0)
 ;;=5^Angina, Prinzmetal
 ;;^UTILITY(U,$J,358.3,3132,2)
 ;;=^7448
 ;;^UTILITY(U,$J,358.3,3133,0)
 ;;=V58.61^^40^248^8
 ;;^UTILITY(U,$J,358.3,3133,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3133,1,4,0)
 ;;=4^V58.61
 ;;^UTILITY(U,$J,358.3,3133,1,5,0)
 ;;=5^Anticoag Rx, chronic
 ;;^UTILITY(U,$J,358.3,3133,2)
 ;;=^303459
 ;;^UTILITY(U,$J,358.3,3134,0)
 ;;=441.4^^40^248^9
 ;;^UTILITY(U,$J,358.3,3134,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3134,1,4,0)
 ;;=4^441.4
 ;;^UTILITY(U,$J,358.3,3134,1,5,0)
 ;;=5^Aortic Aneursym, abdominal
 ;;^UTILITY(U,$J,358.3,3134,2)
 ;;=^269769
 ;;^UTILITY(U,$J,358.3,3135,0)
 ;;=441.2^^40^248^10
 ;;^UTILITY(U,$J,358.3,3135,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3135,1,4,0)
 ;;=4^441.2
 ;;^UTILITY(U,$J,358.3,3135,1,5,0)
 ;;=5^Aortic Aneursym, thoracic
 ;;^UTILITY(U,$J,358.3,3135,2)
 ;;=^269765
 ;;^UTILITY(U,$J,358.3,3136,0)
 ;;=786.59^^40^248^16
 ;;^UTILITY(U,$J,358.3,3136,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3136,1,4,0)
 ;;=4^786.59
 ;;^UTILITY(U,$J,358.3,3136,1,5,0)
 ;;=5^Atypical Chest Pain
 ;;^UTILITY(U,$J,358.3,3136,2)
 ;;=^87384
 ;;^UTILITY(U,$J,358.3,3137,0)
 ;;=428.0^^40^248^21
 ;;^UTILITY(U,$J,358.3,3137,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3137,1,4,0)
 ;;=4^428.0
 ;;^UTILITY(U,$J,358.3,3137,1,5,0)
 ;;=5^CHF
 ;;^UTILITY(U,$J,358.3,3137,2)
 ;;=^54758
 ;;^UTILITY(U,$J,358.3,3138,0)
 ;;=428.1^^40^248^22
 ;;^UTILITY(U,$J,358.3,3138,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3138,1,4,0)
 ;;=4^428.1
 ;;^UTILITY(U,$J,358.3,3138,1,5,0)
 ;;=5^CHF, left ventricular
 ;;^UTILITY(U,$J,358.3,3138,2)
 ;;=^68721
 ;;^UTILITY(U,$J,358.3,3139,0)
 ;;=785.2^^40^248^81
 ;;^UTILITY(U,$J,358.3,3139,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3139,1,4,0)
 ;;=4^785.2
 ;;^UTILITY(U,$J,358.3,3139,1,5,0)
 ;;=5^Undiag Cardiac murmurs
 ;;^UTILITY(U,$J,358.3,3139,2)
 ;;=^295854
 ;;^UTILITY(U,$J,358.3,3140,0)
 ;;=429.3^^40^248^24
 ;;^UTILITY(U,$J,358.3,3140,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3140,1,4,0)
 ;;=4^429.3
 ;;^UTILITY(U,$J,358.3,3140,1,5,0)
 ;;=5^Cardiomegaly
 ;;^UTILITY(U,$J,358.3,3140,2)
 ;;=^54748
 ;;^UTILITY(U,$J,358.3,3141,0)
 ;;=425.5^^40^248^25
 ;;^UTILITY(U,$J,358.3,3141,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3141,1,4,0)
 ;;=4^425.5
 ;;^UTILITY(U,$J,358.3,3141,1,5,0)
 ;;=5^Cardiomyopathy, Alcoholic
 ;;^UTILITY(U,$J,358.3,3141,2)
 ;;=^19623
 ;;^UTILITY(U,$J,358.3,3142,0)
 ;;=433.10^^40^248^26
 ;;^UTILITY(U,$J,358.3,3142,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3142,1,4,0)
 ;;=4^433.10
 ;;^UTILITY(U,$J,358.3,3142,1,5,0)
 ;;=5^Carotid Artery disease
 ;;^UTILITY(U,$J,358.3,3142,2)
 ;;=^295801
 ;;^UTILITY(U,$J,358.3,3143,0)
 ;;=786.52^^40^248^27
 ;;^UTILITY(U,$J,358.3,3143,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3143,1,4,0)
 ;;=4^786.52
 ;;^UTILITY(U,$J,358.3,3143,1,5,0)
 ;;=5^Chest Pain, pleuritic
 ;;^UTILITY(U,$J,358.3,3143,2)
 ;;=^89126
 ;;^UTILITY(U,$J,358.3,3144,0)
 ;;=786.51^^40^248^28
 ;;^UTILITY(U,$J,358.3,3144,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3144,1,4,0)
 ;;=4^786.51
 ;;^UTILITY(U,$J,358.3,3144,1,5,0)
 ;;=5^Chest Pain, precordial
 ;;^UTILITY(U,$J,358.3,3144,2)
 ;;=^276877
 ;;^UTILITY(U,$J,358.3,3145,0)
 ;;=V12.51^^40^248^55
 ;;^UTILITY(U,$J,358.3,3145,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3145,1,4,0)
 ;;=4^V12.51
 ;;^UTILITY(U,$J,358.3,3145,1,5,0)
 ;;=5^Hx of DVT
 ;;^UTILITY(U,$J,358.3,3145,2)
 ;;=Hx of DVT^303397
 ;;^UTILITY(U,$J,358.3,3146,0)
 ;;=780.4^^40^248^31
 ;;^UTILITY(U,$J,358.3,3146,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3146,1,4,0)
 ;;=4^780.4
 ;;^UTILITY(U,$J,358.3,3146,1,5,0)
 ;;=5^Dizziness
 ;;^UTILITY(U,$J,358.3,3146,2)
 ;;=Dizziness^35946
 ;;^UTILITY(U,$J,358.3,3147,0)
 ;;=412.^^40^248^72
 ;;^UTILITY(U,$J,358.3,3147,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3147,1,4,0)
 ;;=4^412.
 ;;^UTILITY(U,$J,358.3,3147,1,5,0)
 ;;=5^Past MI
 ;;^UTILITY(U,$J,358.3,3147,2)
 ;;=Past MI^259884
 ;;^UTILITY(U,$J,358.3,3148,0)
 ;;=458.0^^40^248^64
 ;;^UTILITY(U,$J,358.3,3148,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3148,1,4,0)
 ;;=4^458.0
 ;;^UTILITY(U,$J,358.3,3148,1,5,0)
 ;;=5^Orthostatic Hypotension
 ;;^UTILITY(U,$J,358.3,3148,2)
 ;;=^60741
 ;;^UTILITY(U,$J,358.3,3149,0)
 ;;=420.91^^40^248^73
 ;;^UTILITY(U,$J,358.3,3149,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3149,1,4,0)
 ;;=4^420.91
 ;;^UTILITY(U,$J,358.3,3149,1,5,0)
 ;;=5^Pericarditis, Acute idiopathic
 ;;^UTILITY(U,$J,358.3,3149,2)
 ;;=   ^269695
 ;;^UTILITY(U,$J,358.3,3150,0)
 ;;=780.2^^40^248^79
 ;;^UTILITY(U,$J,358.3,3150,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3150,1,4,0)
 ;;=4^780.2
 ;;^UTILITY(U,$J,358.3,3150,1,5,0)
 ;;=5^Syncope
 ;;^UTILITY(U,$J,358.3,3150,2)
 ;;=Syncope^116707
 ;;^UTILITY(U,$J,358.3,3151,0)
 ;;=443.9^^40^248^66
 ;;^UTILITY(U,$J,358.3,3151,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3151,1,4,0)
 ;;=4^443.9
 ;;^UTILITY(U,$J,358.3,3151,1,5,0)
 ;;=5^PVD
 ;;^UTILITY(U,$J,358.3,3151,2)
 ;;=^184182
 ;;^UTILITY(U,$J,358.3,3152,0)
 ;;=440.21^^40^248^68
 ;;^UTILITY(U,$J,358.3,3152,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3152,1,4,0)
 ;;=4^440.21
 ;;^UTILITY(U,$J,358.3,3152,1,5,0)
 ;;=5^PVD w/ intermittent claudication
 ;;^UTILITY(U,$J,358.3,3152,2)
 ;;=^293885
 ;;^UTILITY(U,$J,358.3,3153,0)
 ;;=440.23^^40^248^69
 ;;^UTILITY(U,$J,358.3,3153,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3153,1,4,0)
 ;;=4^440.23
 ;;^UTILITY(U,$J,358.3,3153,1,5,0)
 ;;=5^PVD w/ ulceration
 ;;^UTILITY(U,$J,358.3,3153,2)
 ;;=^295739
 ;;^UTILITY(U,$J,358.3,3154,0)
 ;;=440.24^^40^248^67
 ;;^UTILITY(U,$J,358.3,3154,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3154,1,4,0)
 ;;=4^440.24
 ;;^UTILITY(U,$J,358.3,3154,1,5,0)
 ;;=5^PVD w/ Gangrene
 ;;^UTILITY(U,$J,358.3,3154,2)
 ;;=PVD w/ Gangrene^295740
 ;;^UTILITY(U,$J,358.3,3155,0)
 ;;=V45.81^^40^248^75
 ;;^UTILITY(U,$J,358.3,3155,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3155,1,4,0)
 ;;=4^V45.81
 ;;^UTILITY(U,$J,358.3,3155,1,5,0)
 ;;=5^S/P CABG
 ;;^UTILITY(U,$J,358.3,3155,2)
 ;;=^97129
 ;;^UTILITY(U,$J,358.3,3156,0)
 ;;=459.81^^40^248^85
 ;;^UTILITY(U,$J,358.3,3156,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3156,1,4,0)
 ;;=4^459.81
 ;;^UTILITY(U,$J,358.3,3156,1,5,0)
 ;;=5^Venous Insufficiency
 ;;^UTILITY(U,$J,358.3,3156,2)
 ;;=^125826
 ;;^UTILITY(U,$J,358.3,3157,0)
 ;;=V45.01^^40^248^77
 ;;^UTILITY(U,$J,358.3,3157,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3157,1,4,0)
 ;;=4^V45.01
 ;;^UTILITY(U,$J,358.3,3157,1,5,0)
 ;;=5^S/P Pacer Placement
 ;;^UTILITY(U,$J,358.3,3157,2)
 ;;=^303419
 ;;^UTILITY(U,$J,358.3,3158,0)
 ;;=427.31^^40^248^15
 ;;^UTILITY(U,$J,358.3,3158,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3158,1,4,0)
 ;;=4^427.31
 ;;^UTILITY(U,$J,358.3,3158,1,5,0)
 ;;=5^Atrial Fibrillation
 ;;^UTILITY(U,$J,358.3,3158,2)
 ;;=^11378
 ;;^UTILITY(U,$J,358.3,3159,0)
 ;;=427.89^^40^248^17
 ;;^UTILITY(U,$J,358.3,3159,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3159,1,4,0)
 ;;=4^427.89
 ;;^UTILITY(U,$J,358.3,3159,1,5,0)
 ;;=5^Bradycardia
 ;;^UTILITY(U,$J,358.3,3159,2)
 ;;=Bradycardia^87896
 ;;^UTILITY(U,$J,358.3,3160,0)
 ;;=427.9^^40^248^23
 ;;^UTILITY(U,$J,358.3,3160,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3160,1,4,0)
 ;;=4^427.9
 ;;^UTILITY(U,$J,358.3,3160,1,5,0)
 ;;=5^Cardiac Dysrythmia
 ;;^UTILITY(U,$J,358.3,3160,2)
 ;;=^10166
 ;;^UTILITY(U,$J,358.3,3161,0)
 ;;=427.0^^40^248^71
 ;;^UTILITY(U,$J,358.3,3161,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3161,1,4,0)
 ;;=4^427.0
 ;;^UTILITY(U,$J,358.3,3161,1,5,0)
 ;;=5^Paroxysmal Supravent Tachycardia
 ;;^UTILITY(U,$J,358.3,3161,2)
 ;;=^90479
 ;;^UTILITY(U,$J,358.3,3162,0)
 ;;=427.81^^40^248^78
 ;;^UTILITY(U,$J,358.3,3162,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3162,1,4,0)
 ;;=4^427.81
 ;;^UTILITY(U,$J,358.3,3162,1,5,0)
 ;;=5^Sick Sinus Syndrome
 ;;^UTILITY(U,$J,358.3,3162,2)
 ;;=^110852
 ;;^UTILITY(U,$J,358.3,3163,0)
 ;;=785.0^^40^248^80
 ;;^UTILITY(U,$J,358.3,3163,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3163,1,4,0)
 ;;=4^785.0
 ;;^UTILITY(U,$J,358.3,3163,1,5,0)
 ;;=5^Tachycardia
