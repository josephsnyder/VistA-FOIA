IBDEI09B ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,12352,0)
 ;;=96360^^104^832^2^^^^1
 ;;^UTILITY(U,$J,358.3,12352,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12352,1,2,0)
 ;;=2^96360
 ;;^UTILITY(U,$J,358.3,12352,1,3,0)
 ;;=3^Hydration,IV,first hour
 ;;^UTILITY(U,$J,358.3,12353,0)
 ;;=96361^^104^832^3^^^^1
 ;;^UTILITY(U,$J,358.3,12353,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12353,1,2,0)
 ;;=2^96361
 ;;^UTILITY(U,$J,358.3,12353,1,3,0)
 ;;=3^Hydration,IV,EA addl hour (+96360)
 ;;^UTILITY(U,$J,358.3,12354,0)
 ;;=96365^^104^832^4^^^^1
 ;;^UTILITY(U,$J,358.3,12354,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12354,1,2,0)
 ;;=2^96365
 ;;^UTILITY(U,$J,358.3,12354,1,3,0)
 ;;=3^Infusion,IV up to 1 hour
 ;;^UTILITY(U,$J,358.3,12355,0)
 ;;=96366^^104^832^5^^^^1
 ;;^UTILITY(U,$J,358.3,12355,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12355,1,2,0)
 ;;=2^96366
 ;;^UTILITY(U,$J,358.3,12355,1,3,0)
 ;;=3^Infusion,IV ea add hr (+96365)
 ;;^UTILITY(U,$J,358.3,12356,0)
 ;;=29105^^104^833^1^^^^1
 ;;^UTILITY(U,$J,358.3,12356,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12356,1,2,0)
 ;;=2^29105
 ;;^UTILITY(U,$J,358.3,12356,1,3,0)
 ;;=3^Long Arm Splint
 ;;^UTILITY(U,$J,358.3,12357,0)
 ;;=29125^^104^833^2^^^^1
 ;;^UTILITY(U,$J,358.3,12357,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12357,1,2,0)
 ;;=2^29125
 ;;^UTILITY(U,$J,358.3,12357,1,3,0)
 ;;=3^Short Arm Splint; Static
 ;;^UTILITY(U,$J,358.3,12358,0)
 ;;=29126^^104^833^3^^^^1
 ;;^UTILITY(U,$J,358.3,12358,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12358,1,2,0)
 ;;=2^29126
 ;;^UTILITY(U,$J,358.3,12358,1,3,0)
 ;;=3^Short Arm Splint; Dynamic
 ;;^UTILITY(U,$J,358.3,12359,0)
 ;;=29130^^104^833^4^^^^1
 ;;^UTILITY(U,$J,358.3,12359,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12359,1,2,0)
 ;;=2^29130
 ;;^UTILITY(U,$J,358.3,12359,1,3,0)
 ;;=3^Finger Splint
 ;;^UTILITY(U,$J,358.3,12360,0)
 ;;=99347^^104^834^1^^^^1
 ;;^UTILITY(U,$J,358.3,12360,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12360,1,2,0)
 ;;=2^99347
 ;;^UTILITY(U,$J,358.3,12360,1,3,0)
 ;;=3^Est Pt-Prob Focus Home Visit
 ;;^UTILITY(U,$J,358.3,12361,0)
 ;;=99348^^104^834^2^^^^1
 ;;^UTILITY(U,$J,358.3,12361,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12361,1,2,0)
 ;;=2^99348
 ;;^UTILITY(U,$J,358.3,12361,1,3,0)
 ;;=3^Est Pt-Exp Prob Focus Home Visit
 ;;^UTILITY(U,$J,358.3,12362,0)
 ;;=99349^^104^834^3^^^^1
 ;;^UTILITY(U,$J,358.3,12362,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12362,1,2,0)
 ;;=2^99349
 ;;^UTILITY(U,$J,358.3,12362,1,3,0)
 ;;=3^Est Pt-Detailed Home Visit
 ;;^UTILITY(U,$J,358.3,12363,0)
 ;;=99350^^104^834^4^^^^1
 ;;^UTILITY(U,$J,358.3,12363,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12363,1,2,0)
 ;;=2^99350
 ;;^UTILITY(U,$J,358.3,12363,1,3,0)
 ;;=3^Est Pt-Comp Mod/High Home Visit
 ;;^UTILITY(U,$J,358.3,12364,0)
 ;;=99341^^104^834^5^^^^1
 ;;^UTILITY(U,$J,358.3,12364,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12364,1,2,0)
 ;;=2^99341
 ;;^UTILITY(U,$J,358.3,12364,1,3,0)
 ;;=3^New Pt-Prob Focus Home Visit
 ;;^UTILITY(U,$J,358.3,12365,0)
 ;;=99342^^104^834^6^^^^1
 ;;^UTILITY(U,$J,358.3,12365,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12365,1,2,0)
 ;;=2^99342
 ;;^UTILITY(U,$J,358.3,12365,1,3,0)
 ;;=3^New Pt-Exp Prob Focus Home Visit
 ;;^UTILITY(U,$J,358.3,12366,0)
 ;;=99343^^104^834^7^^^^1
 ;;^UTILITY(U,$J,358.3,12366,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12366,1,2,0)
 ;;=2^99343
 ;;^UTILITY(U,$J,358.3,12366,1,3,0)
 ;;=3^New Pt-Detailed Home Visit
 ;;^UTILITY(U,$J,358.3,12367,0)
 ;;=99344^^104^834^8^^^^1
 ;;^UTILITY(U,$J,358.3,12367,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12367,1,2,0)
 ;;=2^99344
 ;;^UTILITY(U,$J,358.3,12367,1,3,0)
 ;;=3^New Pt-Comp Moderate Home Visit
 ;;^UTILITY(U,$J,358.3,12368,0)
 ;;=99345^^104^834^9^^^^1
 ;;^UTILITY(U,$J,358.3,12368,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,12368,1,2,0)
 ;;=2^99345
 ;;^UTILITY(U,$J,358.3,12368,1,3,0)
 ;;=3^New Pt-Comp High Home Visit
 ;;^UTILITY(U,$J,358.3,12369,0)
 ;;=414.01^^105^835^12
 ;;^UTILITY(U,$J,358.3,12369,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12369,1,4,0)
 ;;=4^414.01
 ;;^UTILITY(U,$J,358.3,12369,1,5,0)
 ;;=5^Atherosclerosis, native coronary
 ;;^UTILITY(U,$J,358.3,12369,2)
 ;;=CAD, Native Vessel^303281
 ;;^UTILITY(U,$J,358.3,12370,0)
 ;;=413.9^^105^835^2
 ;;^UTILITY(U,$J,358.3,12370,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12370,1,4,0)
 ;;=4^413.9
 ;;^UTILITY(U,$J,358.3,12370,1,5,0)
 ;;=5^Angina Pectoris
 ;;^UTILITY(U,$J,358.3,12370,2)
 ;;=Angina Pectoris^87258
 ;;^UTILITY(U,$J,358.3,12371,0)
 ;;=413.0^^105^835^3
 ;;^UTILITY(U,$J,358.3,12371,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12371,1,4,0)
 ;;=4^413.0
 ;;^UTILITY(U,$J,358.3,12371,1,5,0)
 ;;=5^Angina at Rest
 ;;^UTILITY(U,$J,358.3,12371,2)
 ;;=Angina at Rest^265313
 ;;^UTILITY(U,$J,358.3,12372,0)
 ;;=411.1^^105^835^5
 ;;^UTILITY(U,$J,358.3,12372,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12372,1,4,0)
 ;;=4^411.1
 ;;^UTILITY(U,$J,358.3,12372,1,5,0)
 ;;=5^Angina, Unstable
 ;;^UTILITY(U,$J,358.3,12372,2)
 ;;=Angina, Unstable^7455
 ;;^UTILITY(U,$J,358.3,12373,0)
 ;;=413.1^^105^835^4
 ;;^UTILITY(U,$J,358.3,12373,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12373,1,4,0)
 ;;=4^413.1
 ;;^UTILITY(U,$J,358.3,12373,1,5,0)
 ;;=5^Angina, Prinzmetal
 ;;^UTILITY(U,$J,358.3,12373,2)
 ;;=^7448
 ;;^UTILITY(U,$J,358.3,12374,0)
 ;;=V58.61^^105^835^6
 ;;^UTILITY(U,$J,358.3,12374,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12374,1,4,0)
 ;;=4^V58.61
 ;;^UTILITY(U,$J,358.3,12374,1,5,0)
 ;;=5^Anticoag Rx, chronic
 ;;^UTILITY(U,$J,358.3,12374,2)
 ;;=^303459
 ;;^UTILITY(U,$J,358.3,12375,0)
 ;;=441.4^^105^835^7
 ;;^UTILITY(U,$J,358.3,12375,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12375,1,4,0)
 ;;=4^441.4
 ;;^UTILITY(U,$J,358.3,12375,1,5,0)
 ;;=5^Aortic Aneursym, abdominal
 ;;^UTILITY(U,$J,358.3,12375,2)
 ;;=^269769
 ;;^UTILITY(U,$J,358.3,12376,0)
 ;;=441.2^^105^835^8
 ;;^UTILITY(U,$J,358.3,12376,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12376,1,4,0)
 ;;=4^441.2
 ;;^UTILITY(U,$J,358.3,12376,1,5,0)
 ;;=5^Aortic Aneursym, thoracic
 ;;^UTILITY(U,$J,358.3,12376,2)
 ;;=^269765
 ;;^UTILITY(U,$J,358.3,12377,0)
 ;;=786.59^^105^835^14
 ;;^UTILITY(U,$J,358.3,12377,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12377,1,4,0)
 ;;=4^786.59
 ;;^UTILITY(U,$J,358.3,12377,1,5,0)
 ;;=5^Atypical Chest Pain
 ;;^UTILITY(U,$J,358.3,12377,2)
 ;;=^87384
 ;;^UTILITY(U,$J,358.3,12378,0)
 ;;=428.0^^105^835^17
 ;;^UTILITY(U,$J,358.3,12378,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12378,1,4,0)
 ;;=4^428.0
 ;;^UTILITY(U,$J,358.3,12378,1,5,0)
 ;;=5^CHF
 ;;^UTILITY(U,$J,358.3,12378,2)
 ;;=^54758
 ;;^UTILITY(U,$J,358.3,12379,0)
 ;;=428.1^^105^835^18
 ;;^UTILITY(U,$J,358.3,12379,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12379,1,4,0)
 ;;=4^428.1
 ;;^UTILITY(U,$J,358.3,12379,1,5,0)
 ;;=5^CHF, left ventricular
 ;;^UTILITY(U,$J,358.3,12379,2)
 ;;=^68721
 ;;^UTILITY(U,$J,358.3,12380,0)
 ;;=785.2^^105^835^76
 ;;^UTILITY(U,$J,358.3,12380,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12380,1,4,0)
 ;;=4^785.2
 ;;^UTILITY(U,$J,358.3,12380,1,5,0)
 ;;=5^Undiag Cardiac murmurs
 ;;^UTILITY(U,$J,358.3,12380,2)
 ;;=^295854
 ;;^UTILITY(U,$J,358.3,12381,0)
 ;;=429.3^^105^835^20
 ;;^UTILITY(U,$J,358.3,12381,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12381,1,4,0)
 ;;=4^429.3
 ;;^UTILITY(U,$J,358.3,12381,1,5,0)
 ;;=5^Cardiomegaly
 ;;^UTILITY(U,$J,358.3,12381,2)
 ;;=^54748
 ;;^UTILITY(U,$J,358.3,12382,0)
 ;;=425.5^^105^835^21
 ;;^UTILITY(U,$J,358.3,12382,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12382,1,4,0)
 ;;=4^425.5
 ;;^UTILITY(U,$J,358.3,12382,1,5,0)
 ;;=5^Cardiomyopathy, Alcoholic
 ;;^UTILITY(U,$J,358.3,12382,2)
 ;;=^19623
 ;;^UTILITY(U,$J,358.3,12383,0)
 ;;=433.10^^105^835^22
 ;;^UTILITY(U,$J,358.3,12383,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12383,1,4,0)
 ;;=4^433.10
 ;;^UTILITY(U,$J,358.3,12383,1,5,0)
 ;;=5^Carotid Artery disease
 ;;^UTILITY(U,$J,358.3,12383,2)
 ;;=^295801
 ;;^UTILITY(U,$J,358.3,12384,0)
 ;;=786.52^^105^835^23
 ;;^UTILITY(U,$J,358.3,12384,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12384,1,4,0)
 ;;=4^786.52
 ;;^UTILITY(U,$J,358.3,12384,1,5,0)
 ;;=5^Chest Pain, pleuritic
 ;;^UTILITY(U,$J,358.3,12384,2)
 ;;=^89126
 ;;^UTILITY(U,$J,358.3,12385,0)
 ;;=786.51^^105^835^24
 ;;^UTILITY(U,$J,358.3,12385,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12385,1,4,0)
 ;;=4^786.51
 ;;^UTILITY(U,$J,358.3,12385,1,5,0)
 ;;=5^Chest Pain, precordial
 ;;^UTILITY(U,$J,358.3,12385,2)
 ;;=^276877
 ;;^UTILITY(U,$J,358.3,12386,0)
 ;;=V12.51^^105^835^52
 ;;^UTILITY(U,$J,358.3,12386,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12386,1,4,0)
 ;;=4^V12.51
 ;;^UTILITY(U,$J,358.3,12386,1,5,0)
 ;;=5^Hx of DVT
 ;;^UTILITY(U,$J,358.3,12386,2)
 ;;=Hx of DVT^303397
 ;;^UTILITY(U,$J,358.3,12387,0)
 ;;=780.4^^105^835^27
 ;;^UTILITY(U,$J,358.3,12387,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12387,1,4,0)
 ;;=4^780.4
 ;;^UTILITY(U,$J,358.3,12387,1,5,0)
 ;;=5^Dizziness
 ;;^UTILITY(U,$J,358.3,12387,2)
 ;;=Dizziness^35946
 ;;^UTILITY(U,$J,358.3,12388,0)
 ;;=412.^^105^835^60
 ;;^UTILITY(U,$J,358.3,12388,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12388,1,4,0)
 ;;=4^412.
 ;;^UTILITY(U,$J,358.3,12388,1,5,0)
 ;;=5^Old/Past MI
 ;;^UTILITY(U,$J,358.3,12388,2)
 ;;=Past MI^259884
 ;;^UTILITY(U,$J,358.3,12389,0)
 ;;=458.0^^105^835^61
 ;;^UTILITY(U,$J,358.3,12389,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12389,1,4,0)
 ;;=4^458.0
 ;;^UTILITY(U,$J,358.3,12389,1,5,0)
 ;;=5^Orthostatic Hypotension
 ;;^UTILITY(U,$J,358.3,12389,2)
 ;;=^60741
 ;;^UTILITY(U,$J,358.3,12390,0)
 ;;=420.91^^105^835^68
 ;;^UTILITY(U,$J,358.3,12390,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12390,1,4,0)
 ;;=4^420.91
 ;;^UTILITY(U,$J,358.3,12390,1,5,0)
 ;;=5^Pericarditis, Acute idiopathic
 ;;^UTILITY(U,$J,358.3,12390,2)
 ;;=   ^269695
 ;;^UTILITY(U,$J,358.3,12391,0)
 ;;=780.2^^105^835^74
 ;;^UTILITY(U,$J,358.3,12391,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12391,1,4,0)
 ;;=4^780.2
