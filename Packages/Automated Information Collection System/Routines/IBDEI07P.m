IBDEI07P ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,10121,1,3,0)
 ;;=3^Sarcoidosis
 ;;^UTILITY(U,$J,358.3,10121,1,4,0)
 ;;=4^135.
 ;;^UTILITY(U,$J,358.3,10121,2)
 ;;=Sarcoidosis^107916
 ;;^UTILITY(U,$J,358.3,10122,0)
 ;;=446.5^^79^677^21
 ;;^UTILITY(U,$J,358.3,10122,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10122,1,3,0)
 ;;=3^Temporal Arteritis
 ;;^UTILITY(U,$J,358.3,10122,1,4,0)
 ;;=4^446.5
 ;;^UTILITY(U,$J,358.3,10122,2)
 ;;=Temporal Arteritis^117658
 ;;^UTILITY(U,$J,358.3,10123,0)
 ;;=401.9^^79^677^12
 ;;^UTILITY(U,$J,358.3,10123,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10123,1,3,0)
 ;;=3^Hypertension
 ;;^UTILITY(U,$J,358.3,10123,1,4,0)
 ;;=4^401.9
 ;;^UTILITY(U,$J,358.3,10123,2)
 ;;=Hypertension^186630
 ;;^UTILITY(U,$J,358.3,10124,0)
 ;;=V72.0^^79^677^9
 ;;^UTILITY(U,$J,358.3,10124,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10124,1,3,0)
 ;;=3^Eye Exam
 ;;^UTILITY(U,$J,358.3,10124,1,4,0)
 ;;=4^V72.0
 ;;^UTILITY(U,$J,358.3,10124,2)
 ;;=Eye Exam^43432
 ;;^UTILITY(U,$J,358.3,10125,0)
 ;;=V41.0^^79^677^19
 ;;^UTILITY(U,$J,358.3,10125,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10125,1,3,0)
 ;;=3^Problems with Sight
 ;;^UTILITY(U,$J,358.3,10125,1,4,0)
 ;;=4^V41.0
 ;;^UTILITY(U,$J,358.3,10125,2)
 ;;=^295427
 ;;^UTILITY(U,$J,358.3,10126,0)
 ;;=998.59^^79^677^18
 ;;^UTILITY(U,$J,358.3,10126,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10126,1,3,0)
 ;;=3^Post Op Infection
 ;;^UTILITY(U,$J,358.3,10126,1,4,0)
 ;;=4^998.59
 ;;^UTILITY(U,$J,358.3,10126,2)
 ;;=Post Op Infection^97081
 ;;^UTILITY(U,$J,358.3,10127,0)
 ;;=365.11^^79^678^15
 ;;^UTILITY(U,$J,358.3,10127,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10127,1,3,0)
 ;;=3^Open Angle Glaucoma
 ;;^UTILITY(U,$J,358.3,10127,1,4,0)
 ;;=4^365.11
 ;;^UTILITY(U,$J,358.3,10127,2)
 ;;=Open Angle Glaucoma^51203
 ;;^UTILITY(U,$J,358.3,10128,0)
 ;;=365.12^^79^678^10
 ;;^UTILITY(U,$J,358.3,10128,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10128,1,3,0)
 ;;=3^Low Tension Glaucoma
 ;;^UTILITY(U,$J,358.3,10128,1,4,0)
 ;;=4^365.12
 ;;^UTILITY(U,$J,358.3,10128,2)
 ;;=Low Tension Glaucoma^265223
 ;;^UTILITY(U,$J,358.3,10129,0)
 ;;=365.63^^79^678^13
 ;;^UTILITY(U,$J,358.3,10129,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10129,1,3,0)
 ;;=3^Neovascular Glaucoma
 ;;^UTILITY(U,$J,358.3,10129,1,4,0)
 ;;=4^365.63
 ;;^UTILITY(U,$J,358.3,10129,2)
 ;;=Neovascular Glaucoma^268778
 ;;^UTILITY(U,$J,358.3,10130,0)
 ;;=365.10^^79^678^17
 ;;^UTILITY(U,$J,358.3,10130,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10130,1,3,0)
 ;;=3^Open Angle, Glaucoma Unspec
 ;;^UTILITY(U,$J,358.3,10130,1,4,0)
 ;;=4^365.10
 ;;^UTILITY(U,$J,358.3,10130,2)
 ;;=^51206
 ;;^UTILITY(U,$J,358.3,10131,0)
 ;;=365.13^^79^678^21
 ;;^UTILITY(U,$J,358.3,10131,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10131,1,3,0)
 ;;=3^Pigmentary Glaucoma
 ;;^UTILITY(U,$J,358.3,10131,1,4,0)
 ;;=4^365.13
 ;;^UTILITY(U,$J,358.3,10131,2)
 ;;=Pigmentary Glaucoma^51211
 ;;^UTILITY(U,$J,358.3,10132,0)
 ;;=365.20^^79^678^23
 ;;^UTILITY(U,$J,358.3,10132,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10132,1,3,0)
 ;;=3^Prim Angle Closure Glaucoma
 ;;^UTILITY(U,$J,358.3,10132,1,4,0)
 ;;=4^365.20
 ;;^UTILITY(U,$J,358.3,10132,2)
 ;;=^51195
 ;;^UTILITY(U,$J,358.3,10133,0)
 ;;=365.52^^79^678^24
 ;;^UTILITY(U,$J,358.3,10133,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10133,1,3,0)
 ;;=3^Pseudoexfoliation Glaucoma
 ;;^UTILITY(U,$J,358.3,10133,1,4,0)
 ;;=4^365.52
 ;;^UTILITY(U,$J,358.3,10133,2)
 ;;=Pseudoexfoliation Glaucoma^268771
 ;;^UTILITY(U,$J,358.3,10134,0)
 ;;=365.15^^79^678^27
 ;;^UTILITY(U,$J,358.3,10134,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10134,1,3,0)
 ;;=3^Residual Open Angle Glaucoma
 ;;^UTILITY(U,$J,358.3,10134,1,4,0)
 ;;=4^365.15
 ;;^UTILITY(U,$J,358.3,10134,2)
 ;;=Residual Open Angle Glaucoma^268751
 ;;^UTILITY(U,$J,358.3,10135,0)
 ;;=365.31^^79^678^31
 ;;^UTILITY(U,$J,358.3,10135,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10135,1,3,0)
 ;;=3^Steroid Induced Glaucoma
 ;;^UTILITY(U,$J,358.3,10135,1,4,0)
 ;;=4^365.31
 ;;^UTILITY(U,$J,358.3,10135,2)
 ;;=Steroid Induced Glaucoma^268761
 ;;^UTILITY(U,$J,358.3,10136,0)
 ;;=365.61^^79^678^8
 ;;^UTILITY(U,$J,358.3,10136,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10136,1,3,0)
 ;;=3^Glaucoma W/Pupillary Block
 ;;^UTILITY(U,$J,358.3,10136,1,4,0)
 ;;=4^365.61
 ;;^UTILITY(U,$J,358.3,10136,2)
 ;;=Glaucoma W/Pupillary Block^268776
 ;;^UTILITY(U,$J,358.3,10137,0)
 ;;=365.23^^79^678^4
 ;;^UTILITY(U,$J,358.3,10137,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10137,1,3,0)
 ;;=3^Chronic Angle Clos Glaucoma
 ;;^UTILITY(U,$J,358.3,10137,1,4,0)
 ;;=4^365.23
 ;;^UTILITY(U,$J,358.3,10137,2)
 ;;=^268756
 ;;^UTILITY(U,$J,358.3,10138,0)
 ;;=363.71^^79^678^29
 ;;^UTILITY(U,$J,358.3,10138,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10138,1,3,0)
 ;;=3^Serous Choroidal Detachment
 ;;^UTILITY(U,$J,358.3,10138,1,4,0)
 ;;=4^363.71
 ;;^UTILITY(U,$J,358.3,10138,2)
 ;;=Choroidal Detachment^268699
 ;;^UTILITY(U,$J,358.3,10139,0)
 ;;=365.51^^79^678^19
 ;;^UTILITY(U,$J,358.3,10139,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10139,1,3,0)
 ;;=3^Phacolytic Glaucoma
 ;;^UTILITY(U,$J,358.3,10139,1,4,0)
 ;;=4^365.51
 ;;^UTILITY(U,$J,358.3,10139,2)
 ;;=Phacolytic Glaucoma^265226
 ;;^UTILITY(U,$J,358.3,10140,0)
 ;;=365.01^^79^678^16
 ;;^UTILITY(U,$J,358.3,10140,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10140,1,3,0)
 ;;=3^Open Angle Glaucoma Suspect
 ;;^UTILITY(U,$J,358.3,10140,1,4,0)
 ;;=4^365.01
 ;;^UTILITY(U,$J,358.3,10140,2)
 ;;=Open Angle Glaucoma Suspect^268747
 ;;^UTILITY(U,$J,358.3,10141,0)
 ;;=365.04^^79^678^14
 ;;^UTILITY(U,$J,358.3,10141,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10141,1,3,0)
 ;;=3^Ocular Hypertension
 ;;^UTILITY(U,$J,358.3,10141,1,4,0)
 ;;=4^365.04
 ;;^UTILITY(U,$J,358.3,10141,2)
 ;;=Ocular Hypertension^85124
 ;;^UTILITY(U,$J,358.3,10142,0)
 ;;=365.03^^79^678^32
 ;;^UTILITY(U,$J,358.3,10142,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10142,1,3,0)
 ;;=3^Steroid Responder
 ;;^UTILITY(U,$J,358.3,10142,1,4,0)
 ;;=4^365.03
 ;;^UTILITY(U,$J,358.3,10142,2)
 ;;=^268749
 ;;^UTILITY(U,$J,358.3,10143,0)
 ;;=366.11^^79^678^25
 ;;^UTILITY(U,$J,358.3,10143,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10143,1,3,0)
 ;;=3^Pseudoexfoliation w/o Glaucoma
 ;;^UTILITY(U,$J,358.3,10143,1,4,0)
 ;;=4^366.11
 ;;^UTILITY(U,$J,358.3,10143,2)
 ;;=^265538
 ;;^UTILITY(U,$J,358.3,10144,0)
 ;;=365.02^^79^678^1
 ;;^UTILITY(U,$J,358.3,10144,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10144,1,3,0)
 ;;=3^Anatomic Narrow Angle
 ;;^UTILITY(U,$J,358.3,10144,1,4,0)
 ;;=4^365.02
 ;;^UTILITY(U,$J,358.3,10144,2)
 ;;=Anatomic Narrow Angle^268748
 ;;^UTILITY(U,$J,358.3,10145,0)
 ;;=364.53^^79^678^20
 ;;^UTILITY(U,$J,358.3,10145,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10145,1,3,0)
 ;;=3^Pigment Dispersion w/o Glauc
 ;;^UTILITY(U,$J,358.3,10145,1,4,0)
 ;;=4^364.53
 ;;^UTILITY(U,$J,358.3,10145,2)
 ;;=^268720
 ;;^UTILITY(U,$J,358.3,10146,0)
 ;;=364.42^^79^678^28
 ;;^UTILITY(U,$J,358.3,10146,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10146,1,3,0)
 ;;=3^Rubeosis Iridis w/o Glaucoma
 ;;^UTILITY(U,$J,358.3,10146,1,4,0)
 ;;=4^364.42
 ;;^UTILITY(U,$J,358.3,10146,2)
 ;;=Rubeosis Iridis w/o Glaucoma^268716
 ;;^UTILITY(U,$J,358.3,10147,0)
 ;;=364.77^^79^678^2
 ;;^UTILITY(U,$J,358.3,10147,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10147,1,3,0)
 ;;=3^Angle Recession w/o Glauc
 ;;^UTILITY(U,$J,358.3,10147,1,4,0)
 ;;=4^364.77
 ;;^UTILITY(U,$J,358.3,10147,2)
 ;;=Angle Recession w/o Glauc^268743
 ;;^UTILITY(U,$J,358.3,10148,0)
 ;;=368.40^^79^678^35
 ;;^UTILITY(U,$J,358.3,10148,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10148,1,3,0)
 ;;=3^Visual Field Defect
 ;;^UTILITY(U,$J,358.3,10148,1,4,0)
 ;;=4^368.40
 ;;^UTILITY(U,$J,358.3,10148,2)
 ;;=Visual Field Defect^126859
 ;;^UTILITY(U,$J,358.3,10149,0)
 ;;=363.70^^79^678^3
 ;;^UTILITY(U,$J,358.3,10149,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10149,1,3,0)
 ;;=3^Choroidal Detachment NOS
 ;;^UTILITY(U,$J,358.3,10149,1,4,0)
 ;;=4^363.70
 ;;^UTILITY(U,$J,358.3,10149,2)
 ;;=^276841
 ;;^UTILITY(U,$J,358.3,10150,0)
 ;;=365.24^^79^678^26
 ;;^UTILITY(U,$J,358.3,10150,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10150,1,3,0)
 ;;=3^Residual Angle-Closure Glaucoma
 ;;^UTILITY(U,$J,358.3,10150,1,4,0)
 ;;=4^365.24
 ;;^UTILITY(U,$J,358.3,10150,2)
 ;;=^268758
 ;;^UTILITY(U,$J,358.3,10151,0)
 ;;=365.65^^79^678^33
 ;;^UTILITY(U,$J,358.3,10151,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10151,1,3,0)
 ;;=3^Traumatic Glaucoma
 ;;^UTILITY(U,$J,358.3,10151,1,4,0)
 ;;=4^365.65
 ;;^UTILITY(U,$J,358.3,10151,2)
 ;;=^268780
 ;;^UTILITY(U,$J,358.3,10152,0)
 ;;=365.89^^79^678^34
 ;;^UTILITY(U,$J,358.3,10152,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10152,1,3,0)
 ;;=3^Uveitic Glaucoma
 ;;^UTILITY(U,$J,358.3,10152,1,4,0)
 ;;=4^365.89
 ;;^UTILITY(U,$J,358.3,10152,2)
 ;;=^88069
 ;;^UTILITY(U,$J,358.3,10153,0)
 ;;=365.05^^79^678^18
 ;;^UTILITY(U,$J,358.3,10153,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10153,1,3,0)
 ;;=3^Opn Ang w/ brdrlne fnd-Hi Risk
 ;;^UTILITY(U,$J,358.3,10153,1,4,0)
 ;;=4^365.05
 ;;^UTILITY(U,$J,358.3,10153,2)
 ;;=^340511
 ;;^UTILITY(U,$J,358.3,10154,0)
 ;;=365.06^^79^678^22
 ;;^UTILITY(U,$J,358.3,10154,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10154,1,3,0)
 ;;=3^Prim Ang Clos w/o Glauc Dmg
 ;;^UTILITY(U,$J,358.3,10154,1,4,0)
 ;;=4^365.06
 ;;^UTILITY(U,$J,358.3,10154,2)
 ;;=^340512
 ;;^UTILITY(U,$J,358.3,10155,0)
 ;;=365.70^^79^678^7
 ;;^UTILITY(U,$J,358.3,10155,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10155,1,3,0)
 ;;=3^Glaucoma Stage NOS
 ;;^UTILITY(U,$J,358.3,10155,1,4,0)
 ;;=4^365.70
 ;;^UTILITY(U,$J,358.3,10155,2)
 ;;=^340609
 ;;^UTILITY(U,$J,358.3,10156,0)
 ;;=365.71^^79^678^11
 ;;^UTILITY(U,$J,358.3,10156,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10156,1,3,0)
 ;;=3^Mild Stage Glaucoma
 ;;^UTILITY(U,$J,358.3,10156,1,4,0)
 ;;=4^365.71
 ;;^UTILITY(U,$J,358.3,10156,2)
 ;;=^340513
