IBDEI03T ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,4679,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4679,1,4,0)
 ;;=4^Oth Compl Colost/Enterost
 ;;^UTILITY(U,$J,358.3,4679,1,5,0)
 ;;=5^569.69
 ;;^UTILITY(U,$J,358.3,4679,2)
 ;;=^303301
 ;;^UTILITY(U,$J,358.3,4680,0)
 ;;=278.00^^44^321^73
 ;;^UTILITY(U,$J,358.3,4680,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4680,1,4,0)
 ;;=4^Obesity, Unsp
 ;;^UTILITY(U,$J,358.3,4680,1,5,0)
 ;;=5^278.00
 ;;^UTILITY(U,$J,358.3,4680,2)
 ;;=^84823
 ;;^UTILITY(U,$J,358.3,4681,0)
 ;;=278.01^^44^321^72
 ;;^UTILITY(U,$J,358.3,4681,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4681,1,4,0)
 ;;=4^Morbid Obesity
 ;;^UTILITY(U,$J,358.3,4681,1,5,0)
 ;;=5^278.01
 ;;^UTILITY(U,$J,358.3,4681,2)
 ;;=^84844
 ;;^UTILITY(U,$J,358.3,4682,0)
 ;;=560.9^^44^321^67
 ;;^UTILITY(U,$J,358.3,4682,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4682,1,4,0)
 ;;=4^Intestinal Obstruct NOS
 ;;^UTILITY(U,$J,358.3,4682,1,5,0)
 ;;=5^560.9
 ;;^UTILITY(U,$J,358.3,4682,2)
 ;;=^64849
 ;;^UTILITY(U,$J,358.3,4683,0)
 ;;=V45.86^^44^321^2
 ;;^UTILITY(U,$J,358.3,4683,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4683,1,4,0)
 ;;=4^Bariatric Surgery Status
 ;;^UTILITY(U,$J,358.3,4683,1,5,0)
 ;;=5^V45.86
 ;;^UTILITY(U,$J,358.3,4683,2)
 ;;=^334214
 ;;^UTILITY(U,$J,358.3,4684,0)
 ;;=496.^^44^322^6
 ;;^UTILITY(U,$J,358.3,4684,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4684,1,4,0)
 ;;=4^COPD
 ;;^UTILITY(U,$J,358.3,4684,1,5,0)
 ;;=5^496.
 ;;^UTILITY(U,$J,358.3,4684,2)
 ;;=COPD^24355
 ;;^UTILITY(U,$J,358.3,4685,0)
 ;;=780.57^^44^322^15
 ;;^UTILITY(U,$J,358.3,4685,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4685,1,4,0)
 ;;=4^Sleep Apnea
 ;;^UTILITY(U,$J,358.3,4685,1,5,0)
 ;;=5^780.57
 ;;^UTILITY(U,$J,358.3,4685,2)
 ;;=^293933
 ;;^UTILITY(U,$J,358.3,4686,0)
 ;;=518.89^^44^322^8
 ;;^UTILITY(U,$J,358.3,4686,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4686,1,4,0)
 ;;=4^Lung Lesion
 ;;^UTILITY(U,$J,358.3,4686,1,5,0)
 ;;=5^518.89
 ;;^UTILITY(U,$J,358.3,4686,2)
 ;;=^87486
 ;;^UTILITY(U,$J,358.3,4687,0)
 ;;=786.6^^44^322^10
 ;;^UTILITY(U,$J,358.3,4687,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4687,1,4,0)
 ;;=4^Lung Mass
 ;;^UTILITY(U,$J,358.3,4687,1,5,0)
 ;;=5^786.6
 ;;^UTILITY(U,$J,358.3,4687,2)
 ;;=^273380
 ;;^UTILITY(U,$J,358.3,4688,0)
 ;;=493.90^^44^322^3
 ;;^UTILITY(U,$J,358.3,4688,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4688,1,4,0)
 ;;=4^Asthma W/O Status Asthm
 ;;^UTILITY(U,$J,358.3,4688,1,5,0)
 ;;=5^493.90
 ;;^UTILITY(U,$J,358.3,4688,2)
 ;;=^269966
 ;;^UTILITY(U,$J,358.3,4689,0)
 ;;=162.9^^44^322^7
 ;;^UTILITY(U,$J,358.3,4689,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4689,1,4,0)
 ;;=4^Lung Cancer
 ;;^UTILITY(U,$J,358.3,4689,1,5,0)
 ;;=5^162.9
 ;;^UTILITY(U,$J,358.3,4689,2)
 ;;=Lung Cancer^73521
 ;;^UTILITY(U,$J,358.3,4690,0)
 ;;=786.09^^44^322^13
 ;;^UTILITY(U,$J,358.3,4690,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4690,1,4,0)
 ;;=4^Respiratory Distress
 ;;^UTILITY(U,$J,358.3,4690,1,5,0)
 ;;=5^786.09
 ;;^UTILITY(U,$J,358.3,4690,2)
 ;;=^87547
 ;;^UTILITY(U,$J,358.3,4691,0)
 ;;=515.^^44^322^12
 ;;^UTILITY(U,$J,358.3,4691,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4691,1,4,0)
 ;;=4^Pulmonary Fibrosis
 ;;^UTILITY(U,$J,358.3,4691,1,5,0)
 ;;=5^515.
 ;;^UTILITY(U,$J,358.3,4691,2)
 ;;=^101072
 ;;^UTILITY(U,$J,358.3,4692,0)
 ;;=493.91^^44^322^2
 ;;^UTILITY(U,$J,358.3,4692,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4692,1,4,0)
 ;;=4^Asthma W Status Asthmat
 ;;^UTILITY(U,$J,358.3,4692,1,5,0)
 ;;=5^493.91
 ;;^UTILITY(U,$J,358.3,4692,2)
 ;;=^269967
 ;;^UTILITY(U,$J,358.3,4693,0)
 ;;=466.0^^44^322^4
 ;;^UTILITY(U,$J,358.3,4693,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4693,1,4,0)
 ;;=4^Bronchitis,Acute
 ;;^UTILITY(U,$J,358.3,4693,1,5,0)
 ;;=5^466.0
 ;;^UTILITY(U,$J,358.3,4693,2)
 ;;=^259084
 ;;^UTILITY(U,$J,358.3,4694,0)
 ;;=491.9^^44^322^5
 ;;^UTILITY(U,$J,358.3,4694,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4694,1,4,0)
 ;;=4^Bronchitis,Chronic
 ;;^UTILITY(U,$J,358.3,4694,1,5,0)
 ;;=5^491.9
 ;;^UTILITY(U,$J,358.3,4694,2)
 ;;=^24359
 ;;^UTILITY(U,$J,358.3,4695,0)
 ;;=501.^^44^322^1
 ;;^UTILITY(U,$J,358.3,4695,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4695,1,4,0)
 ;;=4^Asbestosis
 ;;^UTILITY(U,$J,358.3,4695,1,5,0)
 ;;=5^501.
 ;;^UTILITY(U,$J,358.3,4695,2)
 ;;=^10704
 ;;^UTILITY(U,$J,358.3,4696,0)
 ;;=786.05^^44^322^14
 ;;^UTILITY(U,$J,358.3,4696,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4696,1,4,0)
 ;;=4^Shortness Of Breath
 ;;^UTILITY(U,$J,358.3,4696,1,5,0)
 ;;=5^786.05
 ;;^UTILITY(U,$J,358.3,4696,2)
 ;;=^37632
 ;;^UTILITY(U,$J,358.3,4697,0)
 ;;=511.0^^44^322^11
 ;;^UTILITY(U,$J,358.3,4697,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4697,1,4,0)
 ;;=4^Pleurisy
 ;;^UTILITY(U,$J,358.3,4697,1,5,0)
 ;;=5^511.0
 ;;^UTILITY(U,$J,358.3,4697,2)
 ;;=^95432
 ;;^UTILITY(U,$J,358.3,4698,0)
 ;;=793.11^^44^322^9
 ;;^UTILITY(U,$J,358.3,4698,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4698,1,4,0)
 ;;=4^Lung Lesion, Coin
 ;;^UTILITY(U,$J,358.3,4698,1,5,0)
 ;;=5^793.11
 ;;^UTILITY(U,$J,358.3,4698,2)
 ;;=^340570
 ;;^UTILITY(U,$J,358.3,4699,0)
 ;;=716.91^^44^323^5
 ;;^UTILITY(U,$J,358.3,4699,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4699,1,4,0)
 ;;=4^Arthropathy Shoulder
 ;;^UTILITY(U,$J,358.3,4699,1,5,0)
 ;;=5^716.91
 ;;^UTILITY(U,$J,358.3,4699,2)
 ;;=^272248
 ;;^UTILITY(U,$J,358.3,4700,0)
 ;;=716.95^^44^323^4
 ;;^UTILITY(U,$J,358.3,4700,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4700,1,4,0)
 ;;=4^Arthropathy Pelvis/Hip
 ;;^UTILITY(U,$J,358.3,4700,1,5,0)
 ;;=5^716.95
 ;;^UTILITY(U,$J,358.3,4700,2)
 ;;=^272252
 ;;^UTILITY(U,$J,358.3,4701,0)
 ;;=716.96^^44^323^3
 ;;^UTILITY(U,$J,358.3,4701,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4701,1,4,0)
 ;;=4^Arthropathy L/Leg
 ;;^UTILITY(U,$J,358.3,4701,1,5,0)
 ;;=5^716.96
 ;;^UTILITY(U,$J,358.3,4701,2)
 ;;=^272253
 ;;^UTILITY(U,$J,358.3,4702,0)
 ;;=716.97^^44^323^1
 ;;^UTILITY(U,$J,358.3,4702,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4702,1,4,0)
 ;;=4^Arthropathy Ankle/Foot
 ;;^UTILITY(U,$J,358.3,4702,1,5,0)
 ;;=5^716.97
 ;;^UTILITY(U,$J,358.3,4702,2)
 ;;=^272254
 ;;^UTILITY(U,$J,358.3,4703,0)
 ;;=721.0^^44^323^20
 ;;^UTILITY(U,$J,358.3,4703,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4703,1,4,0)
 ;;=4^Spondylosis,Cervical
 ;;^UTILITY(U,$J,358.3,4703,1,5,0)
 ;;=5^721.0
 ;;^UTILITY(U,$J,358.3,4703,2)
 ;;=^272452
 ;;^UTILITY(U,$J,358.3,4704,0)
 ;;=721.3^^44^323^21
 ;;^UTILITY(U,$J,358.3,4704,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4704,1,4,0)
 ;;=4^Spondylosis,L-S
 ;;^UTILITY(U,$J,358.3,4704,1,5,0)
 ;;=5^721.3
 ;;^UTILITY(U,$J,358.3,4704,2)
 ;;=^272456
 ;;^UTILITY(U,$J,358.3,4705,0)
 ;;=716.94^^44^323^2
 ;;^UTILITY(U,$J,358.3,4705,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4705,1,4,0)
 ;;=4^Arthropathy Hand
 ;;^UTILITY(U,$J,358.3,4705,1,5,0)
 ;;=5^716.94
 ;;^UTILITY(U,$J,358.3,4705,2)
 ;;=^272251
 ;;^UTILITY(U,$J,358.3,4706,0)
 ;;=726.10^^44^323^18
 ;;^UTILITY(U,$J,358.3,4706,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4706,1,4,0)
 ;;=4^Rotator Cuff Syndrome
 ;;^UTILITY(U,$J,358.3,4706,1,5,0)
 ;;=5^726.10
 ;;^UTILITY(U,$J,358.3,4706,2)
 ;;=^272523
 ;;^UTILITY(U,$J,358.3,4707,0)
 ;;=717.2^^44^323^17
 ;;^UTILITY(U,$J,358.3,4707,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4707,1,4,0)
 ;;=4^Derangement, Old, Medial Meniscus
 ;;^UTILITY(U,$J,358.3,4707,1,5,0)
 ;;=5^717.2
 ;;^UTILITY(U,$J,358.3,4707,2)
 ;;=Derangement, Old, Medial Meniscus^272260
 ;;^UTILITY(U,$J,358.3,4708,0)
 ;;=715.95^^44^323^9
 ;;^UTILITY(U,$J,358.3,4708,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4708,1,4,0)
 ;;=4^DJD of Hip
 ;;^UTILITY(U,$J,358.3,4708,1,5,0)
 ;;=5^715.95
 ;;^UTILITY(U,$J,358.3,4708,2)
 ;;=DJD of Hip^272166
 ;;^UTILITY(U,$J,358.3,4709,0)
 ;;=715.96^^44^323^10
 ;;^UTILITY(U,$J,358.3,4709,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4709,1,4,0)
 ;;=4^DJD of Knee
 ;;^UTILITY(U,$J,358.3,4709,1,5,0)
 ;;=5^715.96
 ;;^UTILITY(U,$J,358.3,4709,2)
 ;;=DJD of Knee^272167
 ;;^UTILITY(U,$J,358.3,4710,0)
 ;;=716.92^^44^323^8
 ;;^UTILITY(U,$J,358.3,4710,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4710,1,4,0)
 ;;=4^Arthropathy-Upper Arm
 ;;^UTILITY(U,$J,358.3,4710,1,5,0)
 ;;=5^716.92
 ;;^UTILITY(U,$J,358.3,4710,2)
 ;;=^272249
 ;;^UTILITY(U,$J,358.3,4711,0)
 ;;=716.93^^44^323^6
 ;;^UTILITY(U,$J,358.3,4711,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4711,1,4,0)
 ;;=4^Arthropathy-Forearm
 ;;^UTILITY(U,$J,358.3,4711,1,5,0)
 ;;=5^716.93
 ;;^UTILITY(U,$J,358.3,4711,2)
 ;;=^272250
 ;;^UTILITY(U,$J,358.3,4712,0)
 ;;=716.99^^44^323^7
 ;;^UTILITY(U,$J,358.3,4712,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4712,1,4,0)
 ;;=4^Arthropathy-Mult Sites
 ;;^UTILITY(U,$J,358.3,4712,1,5,0)
 ;;=5^716.99
 ;;^UTILITY(U,$J,358.3,4712,2)
 ;;=^272256
 ;;^UTILITY(U,$J,358.3,4713,0)
 ;;=715.91^^44^323^15
 ;;^UTILITY(U,$J,358.3,4713,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4713,1,4,0)
 ;;=4^DJD-Shoulder
 ;;^UTILITY(U,$J,358.3,4713,1,5,0)
 ;;=5^715.91
 ;;^UTILITY(U,$J,358.3,4713,2)
 ;;=^272162
 ;;^UTILITY(U,$J,358.3,4714,0)
 ;;=715.92^^44^323^16
 ;;^UTILITY(U,$J,358.3,4714,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4714,1,4,0)
 ;;=4^DJD-Upper Arm
 ;;^UTILITY(U,$J,358.3,4714,1,5,0)
 ;;=5^715.92
 ;;^UTILITY(U,$J,358.3,4714,2)
 ;;=^272163
 ;;^UTILITY(U,$J,358.3,4715,0)
 ;;=715.93^^44^323^12
 ;;^UTILITY(U,$J,358.3,4715,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4715,1,4,0)
 ;;=4^DJD-Forearm
 ;;^UTILITY(U,$J,358.3,4715,1,5,0)
 ;;=5^715.93
 ;;^UTILITY(U,$J,358.3,4715,2)
 ;;=^272164
 ;;^UTILITY(U,$J,358.3,4716,0)
 ;;=715.94^^44^323^13
 ;;^UTILITY(U,$J,358.3,4716,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4716,1,4,0)
 ;;=4^DJD-Hand
 ;;^UTILITY(U,$J,358.3,4716,1,5,0)
 ;;=5^715.94
 ;;^UTILITY(U,$J,358.3,4716,2)
 ;;=^272165
 ;;^UTILITY(U,$J,358.3,4717,0)
 ;;=715.97^^44^323^11
 ;;^UTILITY(U,$J,358.3,4717,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4717,1,4,0)
 ;;=4^DJD-Ankle
 ;;^UTILITY(U,$J,358.3,4717,1,5,0)
 ;;=5^715.97
 ;;^UTILITY(U,$J,358.3,4717,2)
 ;;=^272168
