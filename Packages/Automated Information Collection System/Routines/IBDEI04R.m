IBDEI04R ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,6032,0)
 ;;=790.29^^58^499^1
 ;;^UTILITY(U,$J,358.3,6032,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6032,1,4,0)
 ;;=4^790.29
 ;;^UTILITY(U,$J,358.3,6032,1,5,0)
 ;;=5^Abnormal Glucose NOS
 ;;^UTILITY(U,$J,358.3,6032,2)
 ;;=^329955
 ;;^UTILITY(U,$J,358.3,6033,0)
 ;;=252.00^^58^499^9
 ;;^UTILITY(U,$J,358.3,6033,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6033,1,4,0)
 ;;=4^252.00
 ;;^UTILITY(U,$J,358.3,6033,1,5,0)
 ;;=5^Hyperparathyroid NOS
 ;;^UTILITY(U,$J,358.3,6033,2)
 ;;=^331438
 ;;^UTILITY(U,$J,358.3,6034,0)
 ;;=252.01^^58^499^18
 ;;^UTILITY(U,$J,358.3,6034,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6034,1,4,0)
 ;;=4^252.01
 ;;^UTILITY(U,$J,358.3,6034,1,5,0)
 ;;=5^Primary Hyperparathyroid
 ;;^UTILITY(U,$J,358.3,6034,2)
 ;;=^331439
 ;;^UTILITY(U,$J,358.3,6035,0)
 ;;=252.02^^58^499^19
 ;;^UTILITY(U,$J,358.3,6035,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6035,1,4,0)
 ;;=4^252.02
 ;;^UTILITY(U,$J,358.3,6035,1,5,0)
 ;;=5^Sec Hyperparathyroid NonRenal
 ;;^UTILITY(U,$J,358.3,6035,2)
 ;;=^331440
 ;;^UTILITY(U,$J,358.3,6036,0)
 ;;=252.08^^58^499^10
 ;;^UTILITY(U,$J,358.3,6036,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6036,1,4,0)
 ;;=4^252.08
 ;;^UTILITY(U,$J,358.3,6036,1,5,0)
 ;;=5^Hyperparathyroidism Oth
 ;;^UTILITY(U,$J,358.3,6036,2)
 ;;=^331441
 ;;^UTILITY(U,$J,358.3,6037,0)
 ;;=793.2^^58^500^3
 ;;^UTILITY(U,$J,358.3,6037,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6037,1,4,0)
 ;;=4^793.2
 ;;^UTILITY(U,$J,358.3,6037,1,5,0)
 ;;=5^Abnormal Chest x-ray, other
 ;;^UTILITY(U,$J,358.3,6037,2)
 ;;=^273419
 ;;^UTILITY(U,$J,358.3,6038,0)
 ;;=493.92^^58^500^10
 ;;^UTILITY(U,$J,358.3,6038,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6038,1,4,0)
 ;;=4^493.92
 ;;^UTILITY(U,$J,358.3,6038,1,5,0)
 ;;=5^Asthma, Acute Exacerbation
 ;;^UTILITY(U,$J,358.3,6038,2)
 ;;=^322001
 ;;^UTILITY(U,$J,358.3,6039,0)
 ;;=493.20^^58^500^17
 ;;^UTILITY(U,$J,358.3,6039,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6039,1,4,0)
 ;;=4^493.20
 ;;^UTILITY(U,$J,358.3,6039,1,5,0)
 ;;=5^COPD with Asthma
 ;;^UTILITY(U,$J,358.3,6039,2)
 ;;=COPD with Asthma^269964
 ;;^UTILITY(U,$J,358.3,6040,0)
 ;;=493.91^^58^500^11
 ;;^UTILITY(U,$J,358.3,6040,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6040,1,4,0)
 ;;=4^493.91
 ;;^UTILITY(U,$J,358.3,6040,1,5,0)
 ;;=5^Asthma, with Status Asthmat
 ;;^UTILITY(U,$J,358.3,6040,2)
 ;;=^269967
 ;;^UTILITY(U,$J,358.3,6041,0)
 ;;=491.21^^58^500^16
 ;;^UTILITY(U,$J,358.3,6041,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6041,1,4,0)
 ;;=4^491.21
 ;;^UTILITY(U,$J,358.3,6041,1,5,0)
 ;;=5^COPD Exacerbation
 ;;^UTILITY(U,$J,358.3,6041,2)
 ;;=COPD Exacerbation^269954
 ;;^UTILITY(U,$J,358.3,6042,0)
 ;;=494.0^^58^500^14
 ;;^UTILITY(U,$J,358.3,6042,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6042,1,4,0)
 ;;=4^494.0
 ;;^UTILITY(U,$J,358.3,6042,1,5,0)
 ;;=5^Bronchiectasis, chronic
 ;;^UTILITY(U,$J,358.3,6042,2)
 ;;=^321990
 ;;^UTILITY(U,$J,358.3,6043,0)
 ;;=494.1^^58^500^13
 ;;^UTILITY(U,$J,358.3,6043,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6043,1,4,0)
 ;;=4^494.1
 ;;^UTILITY(U,$J,358.3,6043,1,5,0)
 ;;=5^Bronchiectasis with exacerb
 ;;^UTILITY(U,$J,358.3,6043,2)
 ;;=^321991
 ;;^UTILITY(U,$J,358.3,6044,0)
 ;;=491.20^^58^500^19
 ;;^UTILITY(U,$J,358.3,6044,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6044,1,4,0)
 ;;=4^491.20
 ;;^UTILITY(U,$J,358.3,6044,1,5,0)
 ;;=5^Chronic Asthmatic Bronchitis
 ;;^UTILITY(U,$J,358.3,6044,2)
 ;;=Chronic Asthmatic Bronchitis^269953
 ;;^UTILITY(U,$J,358.3,6045,0)
 ;;=786.2^^58^500^22
 ;;^UTILITY(U,$J,358.3,6045,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6045,1,4,0)
 ;;=4^786.2
 ;;^UTILITY(U,$J,358.3,6045,1,5,0)
 ;;=5^Cough
 ;;^UTILITY(U,$J,358.3,6045,2)
 ;;=Cough^28905
 ;;^UTILITY(U,$J,358.3,6046,0)
 ;;=786.09^^58^500^23
 ;;^UTILITY(U,$J,358.3,6046,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6046,1,4,0)
 ;;=4^786.09
 ;;^UTILITY(U,$J,358.3,6046,1,5,0)
 ;;=5^Dyspnea
 ;;^UTILITY(U,$J,358.3,6046,2)
 ;;=Dyspnea^87547
 ;;^UTILITY(U,$J,358.3,6047,0)
 ;;=492.8^^58^500^24
 ;;^UTILITY(U,$J,358.3,6047,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6047,1,4,0)
 ;;=4^492.8
 ;;^UTILITY(U,$J,358.3,6047,1,5,0)
 ;;=5^Emphysema
 ;;^UTILITY(U,$J,358.3,6047,2)
 ;;=Emphysema^87569
 ;;^UTILITY(U,$J,358.3,6048,0)
 ;;=487.1^^58^500^32
 ;;^UTILITY(U,$J,358.3,6048,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6048,1,4,0)
 ;;=4^487.1
 ;;^UTILITY(U,$J,358.3,6048,1,5,0)
 ;;=5^Influenza with other Resp Manifest
 ;;^UTILITY(U,$J,358.3,6048,2)
 ;;=^63125
 ;;^UTILITY(U,$J,358.3,6049,0)
 ;;=487.0^^58^500^31
 ;;^UTILITY(U,$J,358.3,6049,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6049,1,4,0)
 ;;=4^487.0
 ;;^UTILITY(U,$J,358.3,6049,1,5,0)
 ;;=5^Influenza w Pneumonia
 ;;^UTILITY(U,$J,358.3,6049,2)
 ;;=^269942
 ;;^UTILITY(U,$J,358.3,6050,0)
 ;;=515.^^58^500^33
 ;;^UTILITY(U,$J,358.3,6050,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6050,1,4,0)
 ;;=4^515.
 ;;^UTILITY(U,$J,358.3,6050,1,5,0)
 ;;=5^Interstitial Lung disease
 ;;^UTILITY(U,$J,358.3,6050,2)
 ;;=^101072
 ;;^UTILITY(U,$J,358.3,6051,0)
 ;;=786.52^^58^500^34
 ;;^UTILITY(U,$J,358.3,6051,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6051,1,4,0)
 ;;=4^786.52
 ;;^UTILITY(U,$J,358.3,6051,1,5,0)
 ;;=5^Painful resp, Pleurodynia
 ;;^UTILITY(U,$J,358.3,6051,2)
 ;;=^89126
 ;;^UTILITY(U,$J,358.3,6052,0)
 ;;=511.9^^58^500^36
 ;;^UTILITY(U,$J,358.3,6052,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6052,1,4,0)
 ;;=4^511.9
 ;;^UTILITY(U,$J,358.3,6052,1,5,0)
 ;;=5^Pleural Effusion, Unsp type
 ;;^UTILITY(U,$J,358.3,6052,2)
 ;;=^123973
 ;;^UTILITY(U,$J,358.3,6053,0)
 ;;=511.0^^58^500^37
 ;;^UTILITY(U,$J,358.3,6053,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6053,1,4,0)
 ;;=4^511.0
 ;;^UTILITY(U,$J,358.3,6053,1,5,0)
 ;;=5^Pleurisy
 ;;^UTILITY(U,$J,358.3,6053,2)
 ;;=Pleurisy^95432
 ;;^UTILITY(U,$J,358.3,6054,0)
 ;;=486.^^58^500^38
 ;;^UTILITY(U,$J,358.3,6054,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6054,1,4,0)
 ;;=4^486.
 ;;^UTILITY(U,$J,358.3,6054,1,5,0)
 ;;=5^Pneumonia, Unsp Organism
 ;;^UTILITY(U,$J,358.3,6054,2)
 ;;=^95632
 ;;^UTILITY(U,$J,358.3,6055,0)
 ;;=135.^^58^500^42
 ;;^UTILITY(U,$J,358.3,6055,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6055,1,4,0)
 ;;=4^135.
 ;;^UTILITY(U,$J,358.3,6055,1,5,0)
 ;;=5^Sarcoidosis
 ;;^UTILITY(U,$J,358.3,6055,2)
 ;;=Sarcoidosis^107916^517.8
 ;;^UTILITY(U,$J,358.3,6056,0)
 ;;=786.05^^58^500^43
 ;;^UTILITY(U,$J,358.3,6056,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6056,1,4,0)
 ;;=4^786.05
 ;;^UTILITY(U,$J,358.3,6056,1,5,0)
 ;;=5^Shortness of Breath
 ;;^UTILITY(U,$J,358.3,6056,2)
 ;;=Shortness of Breath^37632
 ;;^UTILITY(U,$J,358.3,6057,0)
 ;;=780.57^^58^500^44
 ;;^UTILITY(U,$J,358.3,6057,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6057,1,4,0)
 ;;=4^780.57
 ;;^UTILITY(U,$J,358.3,6057,1,5,0)
 ;;=5^Sleep Apnea
 ;;^UTILITY(U,$J,358.3,6057,2)
 ;;=Sleep Apnea^293933
 ;;^UTILITY(U,$J,358.3,6058,0)
 ;;=786.1^^58^500^46
 ;;^UTILITY(U,$J,358.3,6058,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6058,1,4,0)
 ;;=4^786.1
 ;;^UTILITY(U,$J,358.3,6058,1,5,0)
 ;;=5^Stridor
 ;;^UTILITY(U,$J,358.3,6058,2)
 ;;=Stridor^114767
 ;;^UTILITY(U,$J,358.3,6059,0)
 ;;=011.90^^58^500^47
 ;;^UTILITY(U,$J,358.3,6059,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6059,1,4,0)
 ;;=4^011.90
 ;;^UTILITY(U,$J,358.3,6059,1,5,0)
 ;;=5^TB, Pulmonary, NOS
 ;;^UTILITY(U,$J,358.3,6059,2)
 ;;=TB, Pulmonary^122756
 ;;^UTILITY(U,$J,358.3,6060,0)
 ;;=786.06^^58^500^48
 ;;^UTILITY(U,$J,358.3,6060,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6060,1,4,0)
 ;;=4^786.06
 ;;^UTILITY(U,$J,358.3,6060,1,5,0)
 ;;=5^Tachypnea
 ;;^UTILITY(U,$J,358.3,6060,2)
 ;;=Tachypnea^321213
 ;;^UTILITY(U,$J,358.3,6061,0)
 ;;=305.1^^58^500^49
 ;;^UTILITY(U,$J,358.3,6061,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6061,1,4,0)
 ;;=4^305.1
 ;;^UTILITY(U,$J,358.3,6061,1,5,0)
 ;;=5^Tobacco Use
 ;;^UTILITY(U,$J,358.3,6061,2)
 ;;=Tobacco Use^119899
 ;;^UTILITY(U,$J,358.3,6062,0)
 ;;=786.07^^58^500^50
 ;;^UTILITY(U,$J,358.3,6062,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6062,1,4,0)
 ;;=4^786.07
 ;;^UTILITY(U,$J,358.3,6062,1,5,0)
 ;;=5^Wheezing
 ;;^UTILITY(U,$J,358.3,6062,2)
 ;;=Wheezing^127848
 ;;^UTILITY(U,$J,358.3,6063,0)
 ;;=519.11^^58^500^5
 ;;^UTILITY(U,$J,358.3,6063,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6063,1,4,0)
 ;;=4^519.11
 ;;^UTILITY(U,$J,358.3,6063,1,5,0)
 ;;=5^Acute Bronchospasm
 ;;^UTILITY(U,$J,358.3,6063,2)
 ;;=^334092
 ;;^UTILITY(U,$J,358.3,6064,0)
 ;;=488.01^^58^500^28
 ;;^UTILITY(U,$J,358.3,6064,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6064,1,4,0)
 ;;=4^488.01
 ;;^UTILITY(U,$J,358.3,6064,1,5,0)
 ;;=5^Flu DT Iden AVIAN w Pneu
 ;;^UTILITY(U,$J,358.3,6064,2)
 ;;=^339615
 ;;^UTILITY(U,$J,358.3,6065,0)
 ;;=488.02^^58^500^27
 ;;^UTILITY(U,$J,358.3,6065,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6065,1,4,0)
 ;;=4^488.02
 ;;^UTILITY(U,$J,358.3,6065,1,5,0)
 ;;=5^Flu DT AVIAN w oth Resp
 ;;^UTILITY(U,$J,358.3,6065,2)
 ;;=^339616
 ;;^UTILITY(U,$J,358.3,6066,0)
 ;;=488.09^^58^500^26
 ;;^UTILITY(U,$J,358.3,6066,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6066,1,4,0)
 ;;=4^488.09
 ;;^UTILITY(U,$J,358.3,6066,1,5,0)
 ;;=5^Flu DT AVIAN Manfest
 ;;^UTILITY(U,$J,358.3,6066,2)
 ;;=^339617
 ;;^UTILITY(U,$J,358.3,6067,0)
 ;;=786.30^^58^500^29
 ;;^UTILITY(U,$J,358.3,6067,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6067,1,4,0)
 ;;=4^786.30
 ;;^UTILITY(U,$J,358.3,6067,1,5,0)
 ;;=5^Hemoptysis
 ;;^UTILITY(U,$J,358.3,6067,2)
 ;;=^339669
 ;;^UTILITY(U,$J,358.3,6068,0)
 ;;=793.11^^58^500^1
 ;;^UTILITY(U,$J,358.3,6068,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6068,1,4,0)
 ;;=4^793.11
 ;;^UTILITY(U,$J,358.3,6068,1,5,0)
 ;;=5^Abn Chest Xray, Lung, Solitary Nodule
 ;;^UTILITY(U,$J,358.3,6068,2)
 ;;=^340570
 ;;^UTILITY(U,$J,358.3,6069,0)
 ;;=793.19^^58^500^2
 ;;^UTILITY(U,$J,358.3,6069,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6069,1,4,0)
 ;;=4^793.19
 ;;^UTILITY(U,$J,358.3,6069,1,5,0)
 ;;=5^Abn Chest Xray,Oth Finding, Lung