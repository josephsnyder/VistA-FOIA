IBDEI0F6 ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,20367,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20367,1,3,0)
 ;;=3^Hemiplegia,Nondominant Side
 ;;^UTILITY(U,$J,358.3,20367,1,4,0)
 ;;=4^342.92
 ;;^UTILITY(U,$J,358.3,20367,2)
 ;;=^303269
 ;;^UTILITY(U,$J,358.3,20368,0)
 ;;=V49.60^^151^1324^15
 ;;^UTILITY(U,$J,358.3,20368,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20368,1,3,0)
 ;;=3^Upper Limb Amput Status,Unsp
 ;;^UTILITY(U,$J,358.3,20368,1,4,0)
 ;;=4^V49.60
 ;;^UTILITY(U,$J,358.3,20368,2)
 ;;=^303427
 ;;^UTILITY(U,$J,358.3,20369,0)
 ;;=V49.61^^151^1324^14
 ;;^UTILITY(U,$J,358.3,20369,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20369,1,3,0)
 ;;=3^Thumb Amput Status
 ;;^UTILITY(U,$J,358.3,20369,1,4,0)
 ;;=4^V49.61
 ;;^UTILITY(U,$J,358.3,20369,2)
 ;;=^303428
 ;;^UTILITY(U,$J,358.3,20370,0)
 ;;=V49.62^^151^1324^11
 ;;^UTILITY(U,$J,358.3,20370,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20370,1,3,0)
 ;;=3^Oth Finger(s) Amput Status
 ;;^UTILITY(U,$J,358.3,20370,1,4,0)
 ;;=4^V49.62
 ;;^UTILITY(U,$J,358.3,20370,2)
 ;;=^303429
 ;;^UTILITY(U,$J,358.3,20371,0)
 ;;=V49.63^^151^1324^8
 ;;^UTILITY(U,$J,358.3,20371,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20371,1,3,0)
 ;;=3^Hand Amput Status
 ;;^UTILITY(U,$J,358.3,20371,1,4,0)
 ;;=4^V49.63
 ;;^UTILITY(U,$J,358.3,20371,2)
 ;;=^303430
 ;;^UTILITY(U,$J,358.3,20372,0)
 ;;=V49.64^^151^1324^16
 ;;^UTILITY(U,$J,358.3,20372,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20372,1,3,0)
 ;;=3^Wrist Amput Status
 ;;^UTILITY(U,$J,358.3,20372,1,4,0)
 ;;=4^V49.64
 ;;^UTILITY(U,$J,358.3,20372,2)
 ;;=^303431
 ;;^UTILITY(U,$J,358.3,20373,0)
 ;;=V49.65^^151^1324^4
 ;;^UTILITY(U,$J,358.3,20373,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20373,1,3,0)
 ;;=3^Below Elbow Amput Status
 ;;^UTILITY(U,$J,358.3,20373,1,4,0)
 ;;=4^V49.65
 ;;^UTILITY(U,$J,358.3,20373,2)
 ;;=^303432
 ;;^UTILITY(U,$J,358.3,20374,0)
 ;;=V49.66^^151^1324^1
 ;;^UTILITY(U,$J,358.3,20374,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20374,1,3,0)
 ;;=3^Above Elbow Amput Status
 ;;^UTILITY(U,$J,358.3,20374,1,4,0)
 ;;=4^V49.66
 ;;^UTILITY(U,$J,358.3,20374,2)
 ;;=^303433
 ;;^UTILITY(U,$J,358.3,20375,0)
 ;;=V49.67^^151^1324^13
 ;;^UTILITY(U,$J,358.3,20375,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20375,1,3,0)
 ;;=3^Shoulder Amput Status
 ;;^UTILITY(U,$J,358.3,20375,1,4,0)
 ;;=4^V49.67
 ;;^UTILITY(U,$J,358.3,20375,2)
 ;;=^303434
 ;;^UTILITY(U,$J,358.3,20376,0)
 ;;=V49.70^^151^1324^10
 ;;^UTILITY(U,$J,358.3,20376,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20376,1,3,0)
 ;;=3^Lower Limb Amput Status,Unsp
 ;;^UTILITY(U,$J,358.3,20376,1,4,0)
 ;;=4^V49.70
 ;;^UTILITY(U,$J,358.3,20376,2)
 ;;=^303438
 ;;^UTILITY(U,$J,358.3,20377,0)
 ;;=V49.71^^151^1324^7
 ;;^UTILITY(U,$J,358.3,20377,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20377,1,3,0)
 ;;=3^Great Toe Amput Status
 ;;^UTILITY(U,$J,358.3,20377,1,4,0)
 ;;=4^V49.71
 ;;^UTILITY(U,$J,358.3,20377,2)
 ;;=^303439
 ;;^UTILITY(U,$J,358.3,20378,0)
 ;;=V49.72^^151^1324^12
 ;;^UTILITY(U,$J,358.3,20378,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20378,1,3,0)
 ;;=3^Oth Toe(s) Amput Status
 ;;^UTILITY(U,$J,358.3,20378,1,4,0)
 ;;=4^V49.72
 ;;^UTILITY(U,$J,358.3,20378,2)
 ;;=^303440
 ;;^UTILITY(U,$J,358.3,20379,0)
 ;;=V49.73^^151^1324^6
 ;;^UTILITY(U,$J,358.3,20379,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20379,1,3,0)
 ;;=3^Foot Amput Status
 ;;^UTILITY(U,$J,358.3,20379,1,4,0)
 ;;=4^V49.73
 ;;^UTILITY(U,$J,358.3,20379,2)
 ;;=^303441
 ;;^UTILITY(U,$J,358.3,20380,0)
 ;;=V49.74^^151^1324^3
 ;;^UTILITY(U,$J,358.3,20380,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20380,1,3,0)
 ;;=3^Ankle Amput Status
 ;;^UTILITY(U,$J,358.3,20380,1,4,0)
 ;;=4^V49.74
 ;;^UTILITY(U,$J,358.3,20380,2)
 ;;=^303442
 ;;^UTILITY(U,$J,358.3,20381,0)
 ;;=V49.75^^151^1324^5
 ;;^UTILITY(U,$J,358.3,20381,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20381,1,3,0)
 ;;=3^Below Knee Amput Status
 ;;^UTILITY(U,$J,358.3,20381,1,4,0)
 ;;=4^V49.75
 ;;^UTILITY(U,$J,358.3,20381,2)
 ;;=^303443
 ;;^UTILITY(U,$J,358.3,20382,0)
 ;;=V49.76^^151^1324^2
 ;;^UTILITY(U,$J,358.3,20382,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20382,1,3,0)
 ;;=3^Above Knee Amput Status
 ;;^UTILITY(U,$J,358.3,20382,1,4,0)
 ;;=4^V49.76
 ;;^UTILITY(U,$J,358.3,20382,2)
 ;;=^303444
 ;;^UTILITY(U,$J,358.3,20383,0)
 ;;=V49.77^^151^1324^9
 ;;^UTILITY(U,$J,358.3,20383,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20383,1,3,0)
 ;;=3^Hip Amput Status
 ;;^UTILITY(U,$J,358.3,20383,1,4,0)
 ;;=4^V49.77
 ;;^UTILITY(U,$J,358.3,20383,2)
 ;;=^303445
 ;;^UTILITY(U,$J,358.3,20384,0)
 ;;=713.5^^152^1325^1
 ;;^UTILITY(U,$J,358.3,20384,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20384,1,3,0)
 ;;=3^Charcot'S Arthropathy
 ;;^UTILITY(U,$J,358.3,20384,1,4,0)
 ;;=4^713.5
 ;;^UTILITY(U,$J,358.3,20384,2)
 ;;=^10545
 ;;^UTILITY(U,$J,358.3,20385,0)
 ;;=337.1^^152^1325^9
 ;;^UTILITY(U,$J,358.3,20385,1,0)
 ;;=^358.31IA^4^3
 ;;^UTILITY(U,$J,358.3,20385,1,1,0)
 ;;=1
 ;;^UTILITY(U,$J,358.3,20385,1,3,0)
 ;;=3^Peripheral Neuropathy In Dm
 ;;^UTILITY(U,$J,358.3,20385,1,4,0)
 ;;=4^337.1
 ;;^UTILITY(U,$J,358.3,20385,2)
 ;;=^268435
 ;;^UTILITY(U,$J,358.3,20386,0)
 ;;=443.81^^152^1325^10
 ;;^UTILITY(U,$J,358.3,20386,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20386,1,3,0)
 ;;=3^Peripheral Vascular Dis In Dm
 ;;^UTILITY(U,$J,358.3,20386,1,4,0)
 ;;=4^443.81
 ;;^UTILITY(U,$J,358.3,20386,2)
 ;;=^92164
 ;;^UTILITY(U,$J,358.3,20387,0)
 ;;=V43.61^^152^1325^6
 ;;^UTILITY(U,$J,358.3,20387,1,0)
 ;;=^358.31IA^4^3
 ;;^UTILITY(U,$J,358.3,20387,1,1,0)
 ;;=1
 ;;^UTILITY(U,$J,358.3,20387,1,3,0)
 ;;=3^Joint Replacement - Shoulder
 ;;^UTILITY(U,$J,358.3,20387,1,4,0)
 ;;=4^V43.61
 ;;^UTILITY(U,$J,358.3,20387,2)
 ;;=^303410
 ;;^UTILITY(U,$J,358.3,20388,0)
 ;;=V43.62^^152^1325^3
 ;;^UTILITY(U,$J,358.3,20388,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20388,1,3,0)
 ;;=3^Joint Replacement - Elbow
 ;;^UTILITY(U,$J,358.3,20388,1,4,0)
 ;;=4^V43.62
 ;;^UTILITY(U,$J,358.3,20388,2)
 ;;=^303411
 ;;^UTILITY(U,$J,358.3,20389,0)
 ;;=V43.63^^152^1325^7
 ;;^UTILITY(U,$J,358.3,20389,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20389,1,3,0)
 ;;=3^Joint Replacement - Wrist
 ;;^UTILITY(U,$J,358.3,20389,1,4,0)
 ;;=4^V43.63
 ;;^UTILITY(U,$J,358.3,20389,2)
 ;;=^303412
 ;;^UTILITY(U,$J,358.3,20390,0)
 ;;=V43.64^^152^1325^4
 ;;^UTILITY(U,$J,358.3,20390,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20390,1,3,0)
 ;;=3^Joint Replacement - Hip
 ;;^UTILITY(U,$J,358.3,20390,1,4,0)
 ;;=4^V43.64
 ;;^UTILITY(U,$J,358.3,20390,2)
 ;;=^303413
 ;;^UTILITY(U,$J,358.3,20391,0)
 ;;=V43.65^^152^1325^5
 ;;^UTILITY(U,$J,358.3,20391,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20391,1,3,0)
 ;;=3^Joint Replacement - Knee
 ;;^UTILITY(U,$J,358.3,20391,1,4,0)
 ;;=4^V43.65
 ;;^UTILITY(U,$J,358.3,20391,2)
 ;;=^303414
 ;;^UTILITY(U,$J,358.3,20392,0)
 ;;=V43.66^^152^1325^2
 ;;^UTILITY(U,$J,358.3,20392,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20392,1,3,0)
 ;;=3^Joint Replacement - Ankle
 ;;^UTILITY(U,$J,358.3,20392,1,4,0)
 ;;=4^V43.66
 ;;^UTILITY(U,$J,358.3,20392,2)
 ;;=^303415
 ;;^UTILITY(U,$J,358.3,20393,0)
 ;;=V43.60^^152^1325^11
 ;;^UTILITY(U,$J,358.3,20393,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20393,1,3,0)
 ;;=3^Unspecified Joint Replaced
 ;;^UTILITY(U,$J,358.3,20393,1,4,0)
 ;;=4^V43.60
 ;;^UTILITY(U,$J,358.3,20393,2)
 ;;=^295443
 ;;^UTILITY(U,$J,358.3,20394,0)
 ;;=V43.7^^152^1325^8
 ;;^UTILITY(U,$J,358.3,20394,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20394,1,3,0)
 ;;=3^Limb Replacement 
 ;;^UTILITY(U,$J,358.3,20394,1,4,0)
 ;;=4^V43.7
 ;;^UTILITY(U,$J,358.3,20394,2)
 ;;=^295444
 ;;^UTILITY(U,$J,358.3,20395,0)
 ;;=V53.7^^152^1326^9
 ;;^UTILITY(U,$J,358.3,20395,1,0)
 ;;=^358.31IA^4^3
 ;;^UTILITY(U,$J,358.3,20395,1,1,0)
 ;;=1
 ;;^UTILITY(U,$J,358.3,20395,1,3,0)
 ;;=3^Fitting/Adjustment Ortho Device
 ;;^UTILITY(U,$J,358.3,20395,1,4,0)
 ;;=4^V53.7
 ;;^UTILITY(U,$J,358.3,20395,2)
 ;;=^295510
 ;;^UTILITY(U,$J,358.3,20396,0)
 ;;=V67.4^^152^1326^8
 ;;^UTILITY(U,$J,358.3,20396,1,0)
 ;;=^358.31IA^4^3
 ;;^UTILITY(U,$J,358.3,20396,1,1,0)
 ;;=1
 ;;^UTILITY(U,$J,358.3,20396,1,3,0)
 ;;=3^F/U for Healed Fx
 ;;^UTILITY(U,$J,358.3,20396,1,4,0)
 ;;=4^V67.4
 ;;^UTILITY(U,$J,358.3,20396,2)
 ;;=F/U for Healed Fx^295576
 ;;^UTILITY(U,$J,358.3,20397,0)
 ;;=V67.09^^152^1326^7
 ;;^UTILITY(U,$J,358.3,20397,1,0)
 ;;=^358.31IA^4^3
 ;;^UTILITY(U,$J,358.3,20397,1,1,0)
 ;;=1
 ;;^UTILITY(U,$J,358.3,20397,1,3,0)
 ;;=3^F/U Exam following Oth Surg
 ;;^UTILITY(U,$J,358.3,20397,1,4,0)
 ;;=4^V67.09
 ;;^UTILITY(U,$J,358.3,20397,2)
 ;;=F/U Exam Following Oth Surg^322080
 ;;^UTILITY(U,$J,358.3,20398,0)
 ;;=V54.10^^152^1326^1
 ;;^UTILITY(U,$J,358.3,20398,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20398,1,3,0)
 ;;=3^Aftercare for Healing Arm Fx
 ;;^UTILITY(U,$J,358.3,20398,1,4,0)
 ;;=4^V54.10
 ;;^UTILITY(U,$J,358.3,20398,2)
 ;;=Aftercare for Healing Arm Fx^295514
 ;;^UTILITY(U,$J,358.3,20399,0)
 ;;=V54.13^^152^1326^3
 ;;^UTILITY(U,$J,358.3,20399,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20399,1,3,0)
 ;;=3^Aftercare for Healing Fx of Hip
 ;;^UTILITY(U,$J,358.3,20399,1,4,0)
 ;;=4^V54.13
 ;;^UTILITY(U,$J,358.3,20399,2)
 ;;=Aftercare for Healing Fx of Hip^295514
 ;;^UTILITY(U,$J,358.3,20400,0)
 ;;=V54.15^^152^1326^2
 ;;^UTILITY(U,$J,358.3,20400,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20400,1,3,0)
 ;;=3^Aftercare for Healing Fx of Femur
 ;;^UTILITY(U,$J,358.3,20400,1,4,0)
 ;;=4^V54.15
 ;;^UTILITY(U,$J,358.3,20400,2)
 ;;=Aftercare for Healing Fx of Femur^295514
 ;;^UTILITY(U,$J,358.3,20401,0)
 ;;=V54.16^^152^1326^4
 ;;^UTILITY(U,$J,358.3,20401,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20401,1,3,0)
 ;;=3^Aftercare, Heal Fx of Low Leg/Foot
 ;;^UTILITY(U,$J,358.3,20401,1,4,0)
 ;;=4^V54.16
 ;;^UTILITY(U,$J,358.3,20401,2)
 ;;=Aftercare, Heal Fx of Low Leg/Foot^295514
 ;;^UTILITY(U,$J,358.3,20402,0)
 ;;=V54.81^^152^1326^6
 ;;^UTILITY(U,$J,358.3,20402,1,0)
 ;;=^358.31IA^4^3
 ;;^UTILITY(U,$J,358.3,20402,1,1,0)
 ;;=1
