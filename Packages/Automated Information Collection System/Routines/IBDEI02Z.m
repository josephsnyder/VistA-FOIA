IBDEI02Z ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,3504,1,4,0)
 ;;=4^558.9
 ;;^UTILITY(U,$J,358.3,3504,1,5,0)
 ;;=5^Inflammatory Bowel Disease
 ;;^UTILITY(U,$J,358.3,3504,2)
 ;;=^87311
 ;;^UTILITY(U,$J,358.3,3505,0)
 ;;=211.3^^40^255^10
 ;;^UTILITY(U,$J,358.3,3505,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3505,1,4,0)
 ;;=4^211.3
 ;;^UTILITY(U,$J,358.3,3505,1,5,0)
 ;;=5^Colon Polyps (current)
 ;;^UTILITY(U,$J,358.3,3505,2)
 ;;=Colon Polyps (current)^13295
 ;;^UTILITY(U,$J,358.3,3506,0)
 ;;=V12.72^^40^255^11
 ;;^UTILITY(U,$J,358.3,3506,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3506,1,4,0)
 ;;=4^V12.72
 ;;^UTILITY(U,$J,358.3,3506,1,5,0)
 ;;=5^Colon Polyps (removed)
 ;;^UTILITY(U,$J,358.3,3506,2)
 ;;=Colon Polyps (removed)^303401
 ;;^UTILITY(U,$J,358.3,3507,0)
 ;;=789.01^^40^255^74
 ;;^UTILITY(U,$J,358.3,3507,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3507,1,4,0)
 ;;=4^789.01
 ;;^UTILITY(U,$J,358.3,3507,1,5,0)
 ;;=5^RUQ Abdominal Pain
 ;;^UTILITY(U,$J,358.3,3507,2)
 ;;=^303318
 ;;^UTILITY(U,$J,358.3,3508,0)
 ;;=789.02^^40^255^58
 ;;^UTILITY(U,$J,358.3,3508,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3508,1,4,0)
 ;;=4^789.02
 ;;^UTILITY(U,$J,358.3,3508,1,5,0)
 ;;=5^LUQ Abdominal Pain
 ;;^UTILITY(U,$J,358.3,3508,2)
 ;;=^303319
 ;;^UTILITY(U,$J,358.3,3509,0)
 ;;=789.03^^40^255^72
 ;;^UTILITY(U,$J,358.3,3509,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3509,1,4,0)
 ;;=4^789.03
 ;;^UTILITY(U,$J,358.3,3509,1,5,0)
 ;;=5^RLQ Abdominal Pain
 ;;^UTILITY(U,$J,358.3,3509,2)
 ;;=^303320
 ;;^UTILITY(U,$J,358.3,3510,0)
 ;;=789.04^^40^255^56
 ;;^UTILITY(U,$J,358.3,3510,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3510,1,4,0)
 ;;=4^789.04
 ;;^UTILITY(U,$J,358.3,3510,1,5,0)
 ;;=5^LLQ Abdominal Pain
 ;;^UTILITY(U,$J,358.3,3510,2)
 ;;=^303321
 ;;^UTILITY(U,$J,358.3,3511,0)
 ;;=789.05^^40^255^69
 ;;^UTILITY(U,$J,358.3,3511,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3511,1,4,0)
 ;;=4^789.05
 ;;^UTILITY(U,$J,358.3,3511,1,5,0)
 ;;=5^Periumbilical Pain
 ;;^UTILITY(U,$J,358.3,3511,2)
 ;;=^303322
 ;;^UTILITY(U,$J,358.3,3512,0)
 ;;=789.06^^40^255^21
 ;;^UTILITY(U,$J,358.3,3512,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3512,1,4,0)
 ;;=4^789.06
 ;;^UTILITY(U,$J,358.3,3512,1,5,0)
 ;;=5^Epigastric Pain
 ;;^UTILITY(U,$J,358.3,3512,2)
 ;;=^303323
 ;;^UTILITY(U,$J,358.3,3513,0)
 ;;=789.61^^40^255^75
 ;;^UTILITY(U,$J,358.3,3513,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3513,1,4,0)
 ;;=4^789.61
 ;;^UTILITY(U,$J,358.3,3513,1,5,0)
 ;;=5^RUQ Abdominal Tenderness
 ;;^UTILITY(U,$J,358.3,3513,2)
 ;;=^303343
 ;;^UTILITY(U,$J,358.3,3514,0)
 ;;=789.62^^40^255^59
 ;;^UTILITY(U,$J,358.3,3514,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3514,1,4,0)
 ;;=4^789.62
 ;;^UTILITY(U,$J,358.3,3514,1,5,0)
 ;;=5^LUQ Abdominal Tenderness
 ;;^UTILITY(U,$J,358.3,3514,2)
 ;;=^303344
 ;;^UTILITY(U,$J,358.3,3515,0)
 ;;=789.63^^40^255^73
 ;;^UTILITY(U,$J,358.3,3515,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3515,1,4,0)
 ;;=4^789.63
 ;;^UTILITY(U,$J,358.3,3515,1,5,0)
 ;;=5^RLQ Abdominal Tenderness
 ;;^UTILITY(U,$J,358.3,3515,2)
 ;;=^303345
 ;;^UTILITY(U,$J,358.3,3516,0)
 ;;=789.64^^40^255^57
 ;;^UTILITY(U,$J,358.3,3516,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3516,1,4,0)
 ;;=4^789.64
 ;;^UTILITY(U,$J,358.3,3516,1,5,0)
 ;;=5^LLQ Abdominal Tenderness
 ;;^UTILITY(U,$J,358.3,3516,2)
 ;;=^303346
 ;;^UTILITY(U,$J,358.3,3517,0)
 ;;=789.65^^40^255^70
 ;;^UTILITY(U,$J,358.3,3517,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3517,1,4,0)
 ;;=4^789.65
 ;;^UTILITY(U,$J,358.3,3517,1,5,0)
 ;;=5^Periumbilical Tenderness
 ;;^UTILITY(U,$J,358.3,3517,2)
 ;;=^303347
 ;;^UTILITY(U,$J,358.3,3518,0)
 ;;=789.66^^40^255^22
 ;;^UTILITY(U,$J,358.3,3518,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3518,1,4,0)
 ;;=4^789.66
 ;;^UTILITY(U,$J,358.3,3518,1,5,0)
 ;;=5^Epigastric Tenderness
 ;;^UTILITY(U,$J,358.3,3518,2)
 ;;=^303348
 ;;^UTILITY(U,$J,358.3,3519,0)
 ;;=070.1^^40^255^39
 ;;^UTILITY(U,$J,358.3,3519,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3519,1,4,0)
 ;;=4^070.1
 ;;^UTILITY(U,$J,358.3,3519,1,5,0)
 ;;=5^Hepatitis A
 ;;^UTILITY(U,$J,358.3,3519,2)
 ;;=^126486
 ;;^UTILITY(U,$J,358.3,3520,0)
 ;;=070.30^^40^255^40
 ;;^UTILITY(U,$J,358.3,3520,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3520,1,4,0)
 ;;=4^070.30
 ;;^UTILITY(U,$J,358.3,3520,1,5,0)
 ;;=5^Hepatitis B, Acute
 ;;^UTILITY(U,$J,358.3,3520,2)
 ;;=^266626
 ;;^UTILITY(U,$J,358.3,3521,0)
 ;;=070.32^^40^255^41
 ;;^UTILITY(U,$J,358.3,3521,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3521,1,4,0)
 ;;=4^070.32
 ;;^UTILITY(U,$J,358.3,3521,1,5,0)
 ;;=5^Hepatitis B, Chronic
 ;;^UTILITY(U,$J,358.3,3521,2)
 ;;=^303249
 ;;^UTILITY(U,$J,358.3,3522,0)
 ;;=070.51^^40^255^42
 ;;^UTILITY(U,$J,358.3,3522,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3522,1,4,0)
 ;;=4^070.51
 ;;^UTILITY(U,$J,358.3,3522,1,5,0)
 ;;=5^Hepatitis C, Acute
 ;;^UTILITY(U,$J,358.3,3522,2)
 ;;=^266632
 ;;^UTILITY(U,$J,358.3,3523,0)
 ;;=070.54^^40^255^43
 ;;^UTILITY(U,$J,358.3,3523,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3523,1,4,0)
 ;;=4^070.54
 ;;^UTILITY(U,$J,358.3,3523,1,5,0)
 ;;=5^Hepatitis C, Chronic
 ;;^UTILITY(U,$J,358.3,3523,2)
 ;;=^303252
 ;;^UTILITY(U,$J,358.3,3524,0)
 ;;=571.41^^40^255^44
 ;;^UTILITY(U,$J,358.3,3524,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3524,1,4,0)
 ;;=4^571.41
 ;;^UTILITY(U,$J,358.3,3524,1,5,0)
 ;;=5^Hepatitis, Chronic Persist
 ;;^UTILITY(U,$J,358.3,3524,2)
 ;;=^259093
 ;;^UTILITY(U,$J,358.3,3525,0)
 ;;=571.1^^40^255^45
 ;;^UTILITY(U,$J,358.3,3525,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3525,1,4,0)
 ;;=4^571.1
 ;;^UTILITY(U,$J,358.3,3525,1,5,0)
 ;;=5^Hepatitis, ETOH Acute
 ;;^UTILITY(U,$J,358.3,3525,2)
 ;;=^2597
 ;;^UTILITY(U,$J,358.3,3526,0)
 ;;=070.59^^40^255^47
 ;;^UTILITY(U,$J,358.3,3526,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3526,1,4,0)
 ;;=4^070.59
 ;;^UTILITY(U,$J,358.3,3526,1,5,0)
 ;;=5^Hepatitis, Other Viral
 ;;^UTILITY(U,$J,358.3,3526,2)
 ;;=^266631
 ;;^UTILITY(U,$J,358.3,3527,0)
 ;;=573.3^^40^255^46
 ;;^UTILITY(U,$J,358.3,3527,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3527,1,4,0)
 ;;=4^573.3
 ;;^UTILITY(U,$J,358.3,3527,1,5,0)
 ;;=5^Hepatitis, Other
 ;;^UTILITY(U,$J,358.3,3527,2)
 ;;=^56268
 ;;^UTILITY(U,$J,358.3,3528,0)
 ;;=555.9^^40^255^13
 ;;^UTILITY(U,$J,358.3,3528,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3528,1,4,0)
 ;;=4^555.9
 ;;^UTILITY(U,$J,358.3,3528,1,5,0)
 ;;=5^Crohn's Disease
 ;;^UTILITY(U,$J,358.3,3528,2)
 ;;=Crohn's Disease^29356
 ;;^UTILITY(U,$J,358.3,3529,0)
 ;;=787.91^^40^255^14
 ;;^UTILITY(U,$J,358.3,3529,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3529,1,4,0)
 ;;=4^787.91
 ;;^UTILITY(U,$J,358.3,3529,1,5,0)
 ;;=5^Diarrhea
 ;;^UTILITY(U,$J,358.3,3529,2)
 ;;=^33921
 ;;^UTILITY(U,$J,358.3,3530,0)
 ;;=562.11^^40^255^15
 ;;^UTILITY(U,$J,358.3,3530,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3530,1,4,0)
 ;;=4^562.11
 ;;^UTILITY(U,$J,358.3,3530,1,5,0)
 ;;=5^Diverticulitis, Colon
 ;;^UTILITY(U,$J,358.3,3530,2)
 ;;=^270274
 ;;^UTILITY(U,$J,358.3,3531,0)
 ;;=562.10^^40^255^16
 ;;^UTILITY(U,$J,358.3,3531,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3531,1,4,0)
 ;;=4^562.10
 ;;^UTILITY(U,$J,358.3,3531,1,5,0)
 ;;=5^Diverticulosis, Colon
 ;;^UTILITY(U,$J,358.3,3531,2)
 ;;=^35917
 ;;^UTILITY(U,$J,358.3,3532,0)
 ;;=532.90^^40^255^17
 ;;^UTILITY(U,$J,358.3,3532,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3532,1,4,0)
 ;;=4^532.90
 ;;^UTILITY(U,$J,358.3,3532,1,5,0)
 ;;=5^Duodenal Ulcer Nos
 ;;^UTILITY(U,$J,358.3,3532,2)
 ;;=^37311
 ;;^UTILITY(U,$J,358.3,3533,0)
 ;;=536.8^^40^255^18
 ;;^UTILITY(U,$J,358.3,3533,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3533,1,4,0)
 ;;=4^536.8
 ;;^UTILITY(U,$J,358.3,3533,1,5,0)
 ;;=5^Dyspepsia
 ;;^UTILITY(U,$J,358.3,3533,2)
 ;;=^37612
 ;;^UTILITY(U,$J,358.3,3534,0)
 ;;=571.0^^40^255^27
 ;;^UTILITY(U,$J,358.3,3534,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3534,1,4,0)
 ;;=4^571.0
 ;;^UTILITY(U,$J,358.3,3534,1,5,0)
 ;;=5^Fatty Liver W/ Alcohol
 ;;^UTILITY(U,$J,358.3,3534,2)
 ;;=^45182
 ;;^UTILITY(U,$J,358.3,3535,0)
 ;;=571.3^^40^255^20
 ;;^UTILITY(U,$J,358.3,3535,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3535,1,4,0)
 ;;=4^571.3
 ;;^UTILITY(U,$J,358.3,3535,1,5,0)
 ;;=5^ETOH Liver Disease
 ;;^UTILITY(U,$J,358.3,3535,2)
 ;;=ETOH Liver Disease^4638
 ;;^UTILITY(U,$J,358.3,3536,0)
 ;;=530.10^^40^255^24
 ;;^UTILITY(U,$J,358.3,3536,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3536,1,4,0)
 ;;=4^530.10
 ;;^UTILITY(U,$J,358.3,3536,1,5,0)
 ;;=5^Esophagitis, Unsp.
 ;;^UTILITY(U,$J,358.3,3536,2)
 ;;=^295809
 ;;^UTILITY(U,$J,358.3,3537,0)
 ;;=530.81^^40^255^30
 ;;^UTILITY(U,$J,358.3,3537,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3537,1,4,0)
 ;;=4^530.81
 ;;^UTILITY(U,$J,358.3,3537,1,5,0)
 ;;=5^GERD
 ;;^UTILITY(U,$J,358.3,3537,2)
 ;;=^295749
 ;;^UTILITY(U,$J,358.3,3538,0)
 ;;=456.1^^40^255^23
 ;;^UTILITY(U,$J,358.3,3538,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3538,1,4,0)
 ;;=4^456.1
 ;;^UTILITY(U,$J,358.3,3538,1,5,0)
 ;;=5^Esoph Varices W/O Bleed
 ;;^UTILITY(U,$J,358.3,3538,2)
 ;;=^269836
 ;;^UTILITY(U,$J,358.3,3539,0)
 ;;=571.8^^40^255^28
 ;;^UTILITY(U,$J,358.3,3539,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3539,1,4,0)
 ;;=4^571.8
 ;;^UTILITY(U,$J,358.3,3539,1,5,0)
 ;;=5^Fatty Liver W/O Alcohol
 ;;^UTILITY(U,$J,358.3,3539,2)
 ;;=^87404
 ;;^UTILITY(U,$J,358.3,3540,0)
 ;;=792.1^^40^255^37
 ;;^UTILITY(U,$J,358.3,3540,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3540,1,4,0)
 ;;=4^792.1
 ;;^UTILITY(U,$J,358.3,3540,1,5,0)
 ;;=5^Heme+Stool
 ;;^UTILITY(U,$J,358.3,3540,2)
 ;;=^273412
 ;;^UTILITY(U,$J,358.3,3541,0)
 ;;=564.5^^40^255^29
 ;;^UTILITY(U,$J,358.3,3541,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3541,1,4,0)
 ;;=4^564.5
 ;;^UTILITY(U,$J,358.3,3541,1,5,0)
 ;;=5^Functional Diarrhea
 ;;^UTILITY(U,$J,358.3,3541,2)
 ;;=^270281
 ;;^UTILITY(U,$J,358.3,3542,0)
 ;;=578.9^^40^255^31
 ;;^UTILITY(U,$J,358.3,3542,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3542,1,4,0)
 ;;=4^578.9
 ;;^UTILITY(U,$J,358.3,3542,1,5,0)
 ;;=5^GI Bleed
