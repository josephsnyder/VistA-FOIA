IBDEI01A ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,1081,1,5,0)
 ;;=5^Hypertension, NOS
 ;;^UTILITY(U,$J,358.3,1081,2)
 ;;=^186630
 ;;^UTILITY(U,$J,358.3,1082,0)
 ;;=796.2^^16^85^5
 ;;^UTILITY(U,$J,358.3,1082,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1082,1,4,0)
 ;;=4^796.2
 ;;^UTILITY(U,$J,358.3,1082,1,5,0)
 ;;=5^Elev BP W/O Hypertension
 ;;^UTILITY(U,$J,358.3,1082,2)
 ;;=^273464
 ;;^UTILITY(U,$J,358.3,1083,0)
 ;;=402.00^^16^85^13
 ;;^UTILITY(U,$J,358.3,1083,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1083,1,4,0)
 ;;=4^402.00
 ;;^UTILITY(U,$J,358.3,1083,1,5,0)
 ;;=5^Malignant HTN HRT Disease
 ;;^UTILITY(U,$J,358.3,1083,2)
 ;;=^269594
 ;;^UTILITY(U,$J,358.3,1084,0)
 ;;=402.01^^16^85^14
 ;;^UTILITY(U,$J,358.3,1084,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1084,1,4,0)
 ;;=4^402.01
 ;;^UTILITY(U,$J,358.3,1084,1,5,0)
 ;;=5^Malignant HTN HRT W/CHF
 ;;^UTILITY(U,$J,358.3,1084,2)
 ;;=^269595
 ;;^UTILITY(U,$J,358.3,1085,0)
 ;;=402.10^^16^85^1.5
 ;;^UTILITY(U,$J,358.3,1085,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1085,1,4,0)
 ;;=4^402.10
 ;;^UTILITY(U,$J,358.3,1085,1,5,0)
 ;;=5^Benign HTN HRT Disease
 ;;^UTILITY(U,$J,358.3,1085,2)
 ;;=^269598
 ;;^UTILITY(U,$J,358.3,1086,0)
 ;;=402.11^^16^85^1.7
 ;;^UTILITY(U,$J,358.3,1086,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1086,1,4,0)
 ;;=4^402.11
 ;;^UTILITY(U,$J,358.3,1086,1,5,0)
 ;;=5^Benign HTN HRT W/CHF
 ;;^UTILITY(U,$J,358.3,1086,2)
 ;;=^269599
 ;;^UTILITY(U,$J,358.3,1087,0)
 ;;=402.90^^16^85^7
 ;;^UTILITY(U,$J,358.3,1087,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1087,1,4,0)
 ;;=4^402.90
 ;;^UTILITY(U,$J,358.3,1087,1,5,0)
 ;;=5^HTN HRT Dis W/O CHF NOS
 ;;^UTILITY(U,$J,358.3,1087,2)
 ;;=^269601
 ;;^UTILITY(U,$J,358.3,1088,0)
 ;;=402.91^^16^85^6
 ;;^UTILITY(U,$J,358.3,1088,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1088,1,4,0)
 ;;=4^402.91
 ;;^UTILITY(U,$J,358.3,1088,1,5,0)
 ;;=5^HTN HRT Dis W/CHF
 ;;^UTILITY(U,$J,358.3,1088,2)
 ;;=^269602
 ;;^UTILITY(U,$J,358.3,1089,0)
 ;;=403.00^^16^85^16
 ;;^UTILITY(U,$J,358.3,1089,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1089,1,4,0)
 ;;=4^403.00
 ;;^UTILITY(U,$J,358.3,1089,1,5,0)
 ;;=5^Malignant HTN Ren W/O Renal Failure
 ;;^UTILITY(U,$J,358.3,1089,2)
 ;;=^269604
 ;;^UTILITY(U,$J,358.3,1090,0)
 ;;=403.01^^16^85^15
 ;;^UTILITY(U,$J,358.3,1090,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1090,1,4,0)
 ;;=4^403.01
 ;;^UTILITY(U,$J,358.3,1090,1,5,0)
 ;;=5^Malignant HTN Ren W/Ren Failure
 ;;^UTILITY(U,$J,358.3,1090,2)
 ;;=^269605
 ;;^UTILITY(U,$J,358.3,1091,0)
 ;;=403.10^^16^85^3
 ;;^UTILITY(U,$J,358.3,1091,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1091,1,4,0)
 ;;=4^403.10
 ;;^UTILITY(U,$J,358.3,1091,1,5,0)
 ;;=5^Benign HTN Ren W/O Renal Failure
 ;;^UTILITY(U,$J,358.3,1091,2)
 ;;=^269607
 ;;^UTILITY(U,$J,358.3,1092,0)
 ;;=403.11^^16^85^2
 ;;^UTILITY(U,$J,358.3,1092,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1092,1,4,0)
 ;;=4^403.11
 ;;^UTILITY(U,$J,358.3,1092,1,5,0)
 ;;=5^Benign HTN Ren W/Renal Failure
 ;;^UTILITY(U,$J,358.3,1092,2)
 ;;=^269608
 ;;^UTILITY(U,$J,358.3,1093,0)
 ;;=403.90^^16^85^9
 ;;^UTILITY(U,$J,358.3,1093,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1093,1,4,0)
 ;;=4^403.90
 ;;^UTILITY(U,$J,358.3,1093,1,5,0)
 ;;=5^HTN REN W/O Ren Fail
 ;;^UTILITY(U,$J,358.3,1093,2)
 ;;=^269609
 ;;^UTILITY(U,$J,358.3,1094,0)
 ;;=403.91^^16^85^8
 ;;^UTILITY(U,$J,358.3,1094,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1094,1,4,0)
 ;;=4^403.91
 ;;^UTILITY(U,$J,358.3,1094,1,5,0)
 ;;=5^HTN REN W Ren Fail
 ;;^UTILITY(U,$J,358.3,1094,2)
 ;;=^269610
 ;;^UTILITY(U,$J,358.3,1095,0)
 ;;=401.1^^16^85^1
 ;;^UTILITY(U,$J,358.3,1095,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1095,1,4,0)
 ;;=4^401.1
 ;;^UTILITY(U,$J,358.3,1095,1,5,0)
 ;;=5^Benign Hypertension
 ;;^UTILITY(U,$J,358.3,1095,2)
 ;;=^269591
 ;;^UTILITY(U,$J,358.3,1096,0)
 ;;=405.19^^16^85^3.5
 ;;^UTILITY(U,$J,358.3,1096,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1096,1,4,0)
 ;;=4^405.19
 ;;^UTILITY(U,$J,358.3,1096,1,5,0)
 ;;=5^Benign Ren HTN 2nd Ren Art Stenosis
 ;;^UTILITY(U,$J,358.3,1096,2)
 ;;=^269632
 ;;^UTILITY(U,$J,358.3,1097,0)
 ;;=405.99^^16^85^9.5
 ;;^UTILITY(U,$J,358.3,1097,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1097,1,4,0)
 ;;=4^405.99
 ;;^UTILITY(U,$J,358.3,1097,1,5,0)
 ;;=5^HTN Ren 2nd To Ren Art Stenosis
 ;;^UTILITY(U,$J,358.3,1097,2)
 ;;=^269635^440.1
 ;;^UTILITY(U,$J,358.3,1098,0)
 ;;=405.09^^16^85^17
 ;;^UTILITY(U,$J,358.3,1098,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1098,1,4,0)
 ;;=4^405.09
 ;;^UTILITY(U,$J,358.3,1098,1,5,0)
 ;;=5^Malig Ren HTN 2nd To Ren Art Stenosis
 ;;^UTILITY(U,$J,358.3,1098,2)
 ;;=^269629
 ;;^UTILITY(U,$J,358.3,1099,0)
 ;;=440.1^^16^85^10
 ;;^UTILITY(U,$J,358.3,1099,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1099,1,4,0)
 ;;=4^440.1
 ;;^UTILITY(U,$J,358.3,1099,1,5,0)
 ;;=5^      Renal Artery Stenosis (W/405.99)
 ;;^UTILITY(U,$J,358.3,1099,2)
 ;;=^269760
 ;;^UTILITY(U,$J,358.3,1100,0)
 ;;=424.1^^16^86^1
 ;;^UTILITY(U,$J,358.3,1100,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1100,1,4,0)
 ;;=4^424.1
 ;;^UTILITY(U,$J,358.3,1100,1,5,0)
 ;;=5^Aortic Stenosis
 ;;^UTILITY(U,$J,358.3,1100,2)
 ;;=^9330
 ;;^UTILITY(U,$J,358.3,1101,0)
 ;;=424.0^^16^86^1.2
 ;;^UTILITY(U,$J,358.3,1101,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1101,1,4,0)
 ;;=4^424.0
 ;;^UTILITY(U,$J,358.3,1101,1,5,0)
 ;;=5^Mitral Stenosis
 ;;^UTILITY(U,$J,358.3,1101,2)
 ;;=^78367
 ;;^UTILITY(U,$J,358.3,1102,0)
 ;;=424.3^^16^86^3
 ;;^UTILITY(U,$J,358.3,1102,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1102,1,4,0)
 ;;=4^424.3
 ;;^UTILITY(U,$J,358.3,1102,1,5,0)
 ;;=5^Non-Rheumatic Pulm Insuff/Stenosis
 ;;^UTILITY(U,$J,358.3,1102,2)
 ;;=Non-Rheumatic Pulm Insuff/Stenosis^101164
 ;;^UTILITY(U,$J,358.3,1103,0)
 ;;=424.2^^16^86^4
 ;;^UTILITY(U,$J,358.3,1103,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1103,1,4,0)
 ;;=4^424.2
 ;;^UTILITY(U,$J,358.3,1103,1,5,0)
 ;;=5^Non-Rheumatic Tricuspid Insuff
 ;;^UTILITY(U,$J,358.3,1103,2)
 ;;=^269715
 ;;^UTILITY(U,$J,358.3,1104,0)
 ;;=396.0^^16^86^5
 ;;^UTILITY(U,$J,358.3,1104,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1104,1,4,0)
 ;;=4^396.0
 ;;^UTILITY(U,$J,358.3,1104,1,5,0)
 ;;=5^Aortic and Mitral Stenosis
 ;;^UTILITY(U,$J,358.3,1104,2)
 ;;=Aortic and Mitral Stenosis^269580
 ;;^UTILITY(U,$J,358.3,1105,0)
 ;;=396.3^^16^86^6
 ;;^UTILITY(U,$J,358.3,1105,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1105,1,4,0)
 ;;=4^396.3
 ;;^UTILITY(U,$J,358.3,1105,1,5,0)
 ;;=5^Aortic and Mitral Insufficiency
 ;;^UTILITY(U,$J,358.3,1105,2)
 ;;=Aortic and Mitral Insufficiency^269583
 ;;^UTILITY(U,$J,358.3,1106,0)
 ;;=396.8^^16^86^7
 ;;^UTILITY(U,$J,358.3,1106,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1106,1,4,0)
 ;;=4^396.8
 ;;^UTILITY(U,$J,358.3,1106,1,5,0)
 ;;=5^Aortic and Mitral Insuff/Stenosis Combined
 ;;^UTILITY(U,$J,358.3,1106,2)
 ;;=Aortic and Mitral Insuff/Stenosis Combined^269584
 ;;^UTILITY(U,$J,358.3,1107,0)
 ;;=396.8^^16^87^1
 ;;^UTILITY(U,$J,358.3,1107,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1107,1,4,0)
 ;;=4^396.8
 ;;^UTILITY(U,$J,358.3,1107,1,5,0)
 ;;=5^Rhem Aortic & Mitral Stenosis/Insuff
 ;;^UTILITY(U,$J,358.3,1107,2)
 ;;=^269584
 ;;^UTILITY(U,$J,358.3,1108,0)
 ;;=395.2^^16^87^2
 ;;^UTILITY(U,$J,358.3,1108,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1108,1,4,0)
 ;;=4^395.2
 ;;^UTILITY(U,$J,358.3,1108,1,5,0)
 ;;=5^Rhem Aortic Stenosis W/Insuff
 ;;^UTILITY(U,$J,358.3,1108,2)
 ;;=^269577
 ;;^UTILITY(U,$J,358.3,1109,0)
 ;;=395.9^^16^87^3
 ;;^UTILITY(U,$J,358.3,1109,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1109,1,4,0)
 ;;=4^395.9
 ;;^UTILITY(U,$J,358.3,1109,1,5,0)
 ;;=5^Rhem Aortic Disease
 ;;^UTILITY(U,$J,358.3,1109,2)
 ;;=^269578
 ;;^UTILITY(U,$J,358.3,1110,0)
 ;;=395.1^^16^87^4
 ;;^UTILITY(U,$J,358.3,1110,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1110,1,4,0)
 ;;=4^395.1
 ;;^UTILITY(U,$J,358.3,1110,1,5,0)
 ;;=5^Rhem Aortic Insuff
 ;;^UTILITY(U,$J,358.3,1110,2)
 ;;=^269575
 ;;^UTILITY(U,$J,358.3,1111,0)
 ;;=394.1^^16^87^5
 ;;^UTILITY(U,$J,358.3,1111,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1111,1,4,0)
 ;;=4^394.1
 ;;^UTILITY(U,$J,358.3,1111,1,5,0)
 ;;=5^Rhem Mitral Insuff
 ;;^UTILITY(U,$J,358.3,1111,2)
 ;;=^269568
 ;;^UTILITY(U,$J,358.3,1112,0)
 ;;=395.0^^16^87^6
 ;;^UTILITY(U,$J,358.3,1112,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1112,1,4,0)
 ;;=4^395.0
 ;;^UTILITY(U,$J,358.3,1112,1,5,0)
 ;;=5^Rhem Aortic Stenosis
 ;;^UTILITY(U,$J,358.3,1112,2)
 ;;=^269573
 ;;^UTILITY(U,$J,358.3,1113,0)
 ;;=396.3^^16^87^7
 ;;^UTILITY(U,$J,358.3,1113,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1113,1,4,0)
 ;;=4^396.3
 ;;^UTILITY(U,$J,358.3,1113,1,5,0)
 ;;=5^Rhem Mitral/Aortic Insufficiency
 ;;^UTILITY(U,$J,358.3,1113,2)
 ;;=^269583
 ;;^UTILITY(U,$J,358.3,1114,0)
 ;;=396.2^^16^87^8
 ;;^UTILITY(U,$J,358.3,1114,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1114,1,4,0)
 ;;=4^396.2
 ;;^UTILITY(U,$J,358.3,1114,1,5,0)
 ;;=5^Rhem Mitral Insuff & Aortic Stenosis
 ;;^UTILITY(U,$J,358.3,1114,2)
 ;;=^269582
 ;;^UTILITY(U,$J,358.3,1115,0)
 ;;=394.0^^16^87^9
 ;;^UTILITY(U,$J,358.3,1115,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1115,1,4,0)
 ;;=4^394.0
 ;;^UTILITY(U,$J,358.3,1115,1,5,0)
 ;;=5^Rhem Mitral Stenosis
 ;;^UTILITY(U,$J,358.3,1115,2)
 ;;=^78404
 ;;^UTILITY(U,$J,358.3,1116,0)
 ;;=396.1^^16^87^10
 ;;^UTILITY(U,$J,358.3,1116,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1116,1,4,0)
 ;;=4^396.1
 ;;^UTILITY(U,$J,358.3,1116,1,5,0)
 ;;=5^Rhem Mitral Stenosis & Aortic Insuff
 ;;^UTILITY(U,$J,358.3,1116,2)
 ;;=^269581
 ;;^UTILITY(U,$J,358.3,1117,0)
 ;;=396.0^^16^87^11
 ;;^UTILITY(U,$J,358.3,1117,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1117,1,4,0)
 ;;=4^396.0
 ;;^UTILITY(U,$J,358.3,1117,1,5,0)
 ;;=5^Rhem Mitral & Aortic Stenosis
 ;;^UTILITY(U,$J,358.3,1117,2)
 ;;=^269580
 ;;^UTILITY(U,$J,358.3,1118,0)
 ;;=394.2^^16^87^12
 ;;^UTILITY(U,$J,358.3,1118,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1118,1,4,0)
 ;;=4^394.2
 ;;^UTILITY(U,$J,358.3,1118,1,5,0)
 ;;=5^Rhem Mitral Stenosis W/Insuff
