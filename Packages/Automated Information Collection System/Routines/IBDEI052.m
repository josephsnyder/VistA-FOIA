IBDEI052 ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,6454,2)
 ;;=^273388
 ;;^UTILITY(U,$J,358.3,6455,0)
 ;;=789.00^^58^505^1
 ;;^UTILITY(U,$J,358.3,6455,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6455,1,4,0)
 ;;=4^789.00
 ;;^UTILITY(U,$J,358.3,6455,1,5,0)
 ;;=5^Abdom Pain,Unsp Site
 ;;^UTILITY(U,$J,358.3,6455,2)
 ;;=^303317
 ;;^UTILITY(U,$J,358.3,6456,0)
 ;;=790.5^^58^505^77
 ;;^UTILITY(U,$J,358.3,6456,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6456,1,4,0)
 ;;=4^790.5
 ;;^UTILITY(U,$J,358.3,6456,1,5,0)
 ;;=5^Liver Chem,Abnormal
 ;;^UTILITY(U,$J,358.3,6456,2)
 ;;=^273402
 ;;^UTILITY(U,$J,358.3,6457,0)
 ;;=584.9^^58^506^2
 ;;^UTILITY(U,$J,358.3,6457,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6457,1,4,0)
 ;;=4^584.9
 ;;^UTILITY(U,$J,358.3,6457,1,5,0)
 ;;=5^Acute Renal Failure
 ;;^UTILITY(U,$J,358.3,6457,2)
 ;;=^67114
 ;;^UTILITY(U,$J,358.3,6458,0)
 ;;=583.9^^58^506^16
 ;;^UTILITY(U,$J,358.3,6458,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6458,1,4,0)
 ;;=4^583.9
 ;;^UTILITY(U,$J,358.3,6458,1,5,0)
 ;;=5^Glomerulonephritis
 ;;^UTILITY(U,$J,358.3,6458,2)
 ;;=^83446
 ;;^UTILITY(U,$J,358.3,6459,0)
 ;;=403.90^^58^506^50
 ;;^UTILITY(U,$J,358.3,6459,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6459,1,4,0)
 ;;=4^403.90
 ;;^UTILITY(U,$J,358.3,6459,1,5,0)
 ;;=5^Renal Insufficiency with Hypertension (CRI and HTN)
 ;;^UTILITY(U,$J,358.3,6459,2)
 ;;=Renal Insufficiency with Hypertension (CRI and HTN)^269609
 ;;^UTILITY(U,$J,358.3,6460,0)
 ;;=593.9^^58^506^9
 ;;^UTILITY(U,$J,358.3,6460,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6460,1,4,0)
 ;;=4^593.9
 ;;^UTILITY(U,$J,358.3,6460,1,5,0)
 ;;=5^Chronic Renal Insufficiency
 ;;^UTILITY(U,$J,358.3,6460,2)
 ;;=Chronic Renal Insufficiency^123849
 ;;^UTILITY(U,$J,358.3,6461,0)
 ;;=581.9^^58^506^36
 ;;^UTILITY(U,$J,358.3,6461,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6461,1,4,0)
 ;;=4^581.9
 ;;^UTILITY(U,$J,358.3,6461,1,5,0)
 ;;=5^Nephrotic Syndrome
 ;;^UTILITY(U,$J,358.3,6461,2)
 ;;=^82357
 ;;^UTILITY(U,$J,358.3,6462,0)
 ;;=753.12^^58^506^42
 ;;^UTILITY(U,$J,358.3,6462,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6462,1,4,0)
 ;;=4^753.12
 ;;^UTILITY(U,$J,358.3,6462,1,5,0)
 ;;=5^Polycystic Kidney Disease
 ;;^UTILITY(U,$J,358.3,6462,2)
 ;;=^67295
 ;;^UTILITY(U,$J,358.3,6463,0)
 ;;=791.0^^58^506^44
 ;;^UTILITY(U,$J,358.3,6463,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6463,1,4,0)
 ;;=4^791.0
 ;;^UTILITY(U,$J,358.3,6463,1,5,0)
 ;;=5^Proteinuria
 ;;^UTILITY(U,$J,358.3,6463,2)
 ;;=Proteinuria^99873
 ;;^UTILITY(U,$J,358.3,6464,0)
 ;;=791.9^^58^506^47
 ;;^UTILITY(U,$J,358.3,6464,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6464,1,4,0)
 ;;=4^791.9
 ;;^UTILITY(U,$J,358.3,6464,1,5,0)
 ;;=5^Pyuria
 ;;^UTILITY(U,$J,358.3,6464,2)
 ;;=^273408
 ;;^UTILITY(U,$J,358.3,6465,0)
 ;;=592.0^^58^506^48
 ;;^UTILITY(U,$J,358.3,6465,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6465,1,4,0)
 ;;=4^592.0
 ;;^UTILITY(U,$J,358.3,6465,1,5,0)
 ;;=5^Renal Calculi
 ;;^UTILITY(U,$J,358.3,6465,2)
 ;;=^67056
 ;;^UTILITY(U,$J,358.3,6466,0)
 ;;=403.91^^58^506^49
 ;;^UTILITY(U,$J,358.3,6466,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6466,1,4,0)
 ;;=4^403.91
 ;;^UTILITY(U,$J,358.3,6466,1,5,0)
 ;;=5^Renal Failure, Chronic Hypertensive
 ;;^UTILITY(U,$J,358.3,6466,2)
 ;;=^269610
 ;;^UTILITY(U,$J,358.3,6467,0)
 ;;=586.^^58^506^54
 ;;^UTILITY(U,$J,358.3,6467,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6467,1,4,0)
 ;;=4^586.
 ;;^UTILITY(U,$J,358.3,6467,1,5,0)
 ;;=5^Uremia
 ;;^UTILITY(U,$J,358.3,6467,2)
 ;;=Uremia^104733
 ;;^UTILITY(U,$J,358.3,6468,0)
 ;;=599.0^^58^506^66
 ;;^UTILITY(U,$J,358.3,6468,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6468,1,4,0)
 ;;=4^599.0
 ;;^UTILITY(U,$J,358.3,6468,1,5,0)
 ;;=5^Urinary Tract Infection
 ;;^UTILITY(U,$J,358.3,6468,2)
 ;;=Urinary Tract Infection^124436
 ;;^UTILITY(U,$J,358.3,6469,0)
 ;;=275.42^^58^506^25
 ;;^UTILITY(U,$J,358.3,6469,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6469,1,4,0)
 ;;=4^275.42
 ;;^UTILITY(U,$J,358.3,6469,1,5,0)
 ;;=5^Hypercalcemia
 ;;^UTILITY(U,$J,358.3,6469,2)
 ;;=Hypercalcemia^59932
 ;;^UTILITY(U,$J,358.3,6470,0)
 ;;=275.41^^58^506^28
 ;;^UTILITY(U,$J,358.3,6470,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6470,1,4,0)
 ;;=4^275.41
 ;;^UTILITY(U,$J,358.3,6470,1,5,0)
 ;;=5^Hypocalcemia
 ;;^UTILITY(U,$J,358.3,6470,2)
 ;;=Hypocalcemia^60542
 ;;^UTILITY(U,$J,358.3,6471,0)
 ;;=276.7^^58^506^26
 ;;^UTILITY(U,$J,358.3,6471,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6471,1,4,0)
 ;;=4^276.7
 ;;^UTILITY(U,$J,358.3,6471,1,5,0)
 ;;=5^Hyperkalemia/Hyperpotassemia
 ;;^UTILITY(U,$J,358.3,6471,2)
 ;;=Hyperkalemia/Hyperpotassemia^60042
 ;;^UTILITY(U,$J,358.3,6472,0)
 ;;=276.8^^58^506^29
 ;;^UTILITY(U,$J,358.3,6472,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6472,1,4,0)
 ;;=4^276.8
 ;;^UTILITY(U,$J,358.3,6472,1,5,0)
 ;;=5^Hypokalemia/Hypopotassemia
 ;;^UTILITY(U,$J,358.3,6472,2)
 ;;=Hypokalemia/Hypopotassemia^60611
 ;;^UTILITY(U,$J,358.3,6473,0)
 ;;=275.2^^58^506^23
 ;;^UTILITY(U,$J,358.3,6473,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6473,1,4,0)
 ;;=4^275.2
 ;;^UTILITY(U,$J,358.3,6473,1,5,0)
 ;;=5^Hyper Or Hypomagnesemia
 ;;^UTILITY(U,$J,358.3,6473,2)
 ;;=^35626
 ;;^UTILITY(U,$J,358.3,6474,0)
 ;;=276.0^^58^506^27
 ;;^UTILITY(U,$J,358.3,6474,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6474,1,4,0)
 ;;=4^276.0
 ;;^UTILITY(U,$J,358.3,6474,1,5,0)
 ;;=5^Hypernatremia
 ;;^UTILITY(U,$J,358.3,6474,2)
 ;;=^60144
 ;;^UTILITY(U,$J,358.3,6475,0)
 ;;=276.1^^58^506^30
 ;;^UTILITY(U,$J,358.3,6475,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6475,1,4,0)
 ;;=4^276.1
 ;;^UTILITY(U,$J,358.3,6475,1,5,0)
 ;;=5^Hyponatremia
 ;;^UTILITY(U,$J,358.3,6475,2)
 ;;=Hyponatremia^60722
 ;;^UTILITY(U,$J,358.3,6476,0)
 ;;=275.3^^58^506^24
 ;;^UTILITY(U,$J,358.3,6476,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6476,1,4,0)
 ;;=4^275.3
 ;;^UTILITY(U,$J,358.3,6476,1,5,0)
 ;;=5^Hyper Or Hypophosphatemia
 ;;^UTILITY(U,$J,358.3,6476,2)
 ;;=^93796
 ;;^UTILITY(U,$J,358.3,6477,0)
 ;;=250.40^^58^506^13
 ;;^UTILITY(U,$J,358.3,6477,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6477,1,4,0)
 ;;=4^250.40
 ;;^UTILITY(U,$J,358.3,6477,1,5,0)
 ;;=5^DM type II with Nephropathy
 ;;^UTILITY(U,$J,358.3,6477,2)
 ;;=DM type II with Nephropathy^267837^583.81
 ;;^UTILITY(U,$J,358.3,6478,0)
 ;;=790.93^^58^506^1
 ;;^UTILITY(U,$J,358.3,6478,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6478,1,4,0)
 ;;=4^790.93
 ;;^UTILITY(U,$J,358.3,6478,1,5,0)
 ;;=5^Abnormal PSA
 ;;^UTILITY(U,$J,358.3,6478,2)
 ;;=Abnormal PSA^295772
 ;;^UTILITY(U,$J,358.3,6479,0)
 ;;=627.3^^58^506^3
 ;;^UTILITY(U,$J,358.3,6479,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6479,1,4,0)
 ;;=4^627.3
 ;;^UTILITY(U,$J,358.3,6479,1,5,0)
 ;;=5^Atrophic Vaginitis
 ;;^UTILITY(U,$J,358.3,6479,2)
 ;;=^270577
 ;;^UTILITY(U,$J,358.3,6480,0)
 ;;=607.1^^58^506^6
 ;;^UTILITY(U,$J,358.3,6480,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6480,1,4,0)
 ;;=4^607.1
 ;;^UTILITY(U,$J,358.3,6480,1,5,0)
 ;;=5^Balanitis
 ;;^UTILITY(U,$J,358.3,6480,2)
 ;;=^12530
 ;;^UTILITY(U,$J,358.3,6481,0)
 ;;=596.0^^58^506^7
 ;;^UTILITY(U,$J,358.3,6481,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6481,1,4,0)
 ;;=4^596.0
 ;;^UTILITY(U,$J,358.3,6481,1,5,0)
 ;;=5^Bladder Neck Obstruction
 ;;^UTILITY(U,$J,358.3,6481,2)
 ;;=^15144
 ;;^UTILITY(U,$J,358.3,6482,0)
 ;;=595.0^^58^506^10
 ;;^UTILITY(U,$J,358.3,6482,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6482,1,4,0)
 ;;=4^595.0
 ;;^UTILITY(U,$J,358.3,6482,1,5,0)
 ;;=5^Cystitis, Acute
 ;;^UTILITY(U,$J,358.3,6482,2)
 ;;=^259104
 ;;^UTILITY(U,$J,358.3,6483,0)
 ;;=595.82^^58^506^11
 ;;^UTILITY(U,$J,358.3,6483,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6483,1,4,0)
 ;;=4^595.82
 ;;^UTILITY(U,$J,358.3,6483,1,5,0)
 ;;=5^Cystitis, Radiation
 ;;^UTILITY(U,$J,358.3,6483,2)
 ;;=^270391
 ;;^UTILITY(U,$J,358.3,6484,0)
 ;;=596.59^^58^506^14
 ;;^UTILITY(U,$J,358.3,6484,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6484,1,4,0)
 ;;=4^596.59
 ;;^UTILITY(U,$J,358.3,6484,1,5,0)
 ;;=5^Detrusor Muscle Insuff
 ;;^UTILITY(U,$J,358.3,6484,2)
 ;;=^270393
 ;;^UTILITY(U,$J,358.3,6485,0)
 ;;=788.1^^58^506^15
 ;;^UTILITY(U,$J,358.3,6485,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6485,1,4,0)
 ;;=4^788.1
 ;;^UTILITY(U,$J,358.3,6485,1,5,0)
 ;;=5^Dysuria
 ;;^UTILITY(U,$J,358.3,6485,2)
 ;;=^37716
 ;;^UTILITY(U,$J,358.3,6486,0)
 ;;=604.90^^58^506^40
 ;;^UTILITY(U,$J,358.3,6486,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6486,1,4,0)
 ;;=4^604.90
 ;;^UTILITY(U,$J,358.3,6486,1,5,0)
 ;;=5^Orchitis/Epididymit
 ;;^UTILITY(U,$J,358.3,6486,2)
 ;;=^86178
 ;;^UTILITY(U,$J,358.3,6487,0)
 ;;=607.84^^58^506^31
 ;;^UTILITY(U,$J,358.3,6487,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6487,1,4,0)
 ;;=4^607.84
 ;;^UTILITY(U,$J,358.3,6487,1,5,0)
 ;;=5^Impotence, Organic Origin
 ;;^UTILITY(U,$J,358.3,6487,2)
 ;;=^270441
 ;;^UTILITY(U,$J,358.3,6488,0)
 ;;=098.0^^58^506^56
 ;;^UTILITY(U,$J,358.3,6488,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6488,1,4,0)
 ;;=4^098.0
 ;;^UTILITY(U,$J,358.3,6488,1,5,0)
 ;;=5^Urethritis, Gonococcal
 ;;^UTILITY(U,$J,358.3,6488,2)
 ;;=^52567
 ;;^UTILITY(U,$J,358.3,6489,0)
 ;;=550.92^^58^506^20
 ;;^UTILITY(U,$J,358.3,6489,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6489,1,4,0)
 ;;=4^550.92
 ;;^UTILITY(U,$J,358.3,6489,1,5,0)
 ;;=5^Hernia, Inguinal, Bilat
 ;;^UTILITY(U,$J,358.3,6489,2)
 ;;=^270212
 ;;^UTILITY(U,$J,358.3,6490,0)
 ;;=550.90^^58^506^21
 ;;^UTILITY(U,$J,358.3,6490,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6490,1,4,0)
 ;;=4^550.90
 ;;^UTILITY(U,$J,358.3,6490,1,5,0)
 ;;=5^Hernia, Inguinal, Unilat
 ;;^UTILITY(U,$J,358.3,6490,2)
 ;;=^63302
 ;;^UTILITY(U,$J,358.3,6491,0)
 ;;=302.72^^58^506^32
 ;;^UTILITY(U,$J,358.3,6491,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6491,1,4,0)
 ;;=4^302.72
 ;;^UTILITY(U,$J,358.3,6491,1,5,0)
 ;;=5^Impotence, Psychosocial
 ;;^UTILITY(U,$J,358.3,6491,2)
 ;;=^100632
 ;;^UTILITY(U,$J,358.3,6492,0)
 ;;=788.30^^58^506^62
 ;;^UTILITY(U,$J,358.3,6492,1,0)
 ;;=^358.31IA^5^2
