IBDEI0HY ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,24225,2)
 ;;=^267781
 ;;^UTILITY(U,$J,358.3,24226,0)
 ;;=239.1^^193^1653^11
 ;;^UTILITY(U,$J,358.3,24226,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24226,1,1,0)
 ;;=1^239.1
 ;;^UTILITY(U,$J,358.3,24226,1,8,0)
 ;;=8^Respiratory Neoplasm NOS
 ;;^UTILITY(U,$J,358.3,24226,2)
 ;;=^267782
 ;;^UTILITY(U,$J,358.3,24227,0)
 ;;=239.2^^193^1653^2
 ;;^UTILITY(U,$J,358.3,24227,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24227,1,1,0)
 ;;=1^239.2
 ;;^UTILITY(U,$J,358.3,24227,1,8,0)
 ;;=8^Bone,Soft Tissue,Skin Neoplasm NOS
 ;;^UTILITY(U,$J,358.3,24227,2)
 ;;=^267783
 ;;^UTILITY(U,$J,358.3,24228,0)
 ;;=239.3^^193^1653^4
 ;;^UTILITY(U,$J,358.3,24228,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24228,1,1,0)
 ;;=1^239.3
 ;;^UTILITY(U,$J,358.3,24228,1,8,0)
 ;;=8^Breast Neoplasm NOS
 ;;^UTILITY(U,$J,358.3,24228,2)
 ;;=^81977
 ;;^UTILITY(U,$J,358.3,24229,0)
 ;;=239.4^^193^1653^1
 ;;^UTILITY(U,$J,358.3,24229,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24229,1,1,0)
 ;;=1^239.4
 ;;^UTILITY(U,$J,358.3,24229,1,8,0)
 ;;=8^Bladder Neoplasm NOS
 ;;^UTILITY(U,$J,358.3,24229,2)
 ;;=^81975
 ;;^UTILITY(U,$J,358.3,24230,0)
 ;;=239.5^^193^1653^9
 ;;^UTILITY(U,$J,358.3,24230,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24230,1,1,0)
 ;;=1^239.5
 ;;^UTILITY(U,$J,358.3,24230,1,8,0)
 ;;=8^Oth Genitourinary Org Neoplasm NOS
 ;;^UTILITY(U,$J,358.3,24230,2)
 ;;=^267784
 ;;^UTILITY(U,$J,358.3,24231,0)
 ;;=239.6^^193^1653^3
 ;;^UTILITY(U,$J,358.3,24231,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24231,1,1,0)
 ;;=1^239.6
 ;;^UTILITY(U,$J,358.3,24231,1,8,0)
 ;;=8^Brain Neoplasm NOS
 ;;^UTILITY(U,$J,358.3,24231,2)
 ;;=^16739
 ;;^UTILITY(U,$J,358.3,24232,0)
 ;;=239.7^^193^1653^6
 ;;^UTILITY(U,$J,358.3,24232,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24232,1,1,0)
 ;;=1^239.7
 ;;^UTILITY(U,$J,358.3,24232,1,8,0)
 ;;=8^Endocrine/Nerv Neoplasm NOS
 ;;^UTILITY(U,$J,358.3,24232,2)
 ;;=^267785
 ;;^UTILITY(U,$J,358.3,24233,0)
 ;;=239.81^^193^1653^12
 ;;^UTILITY(U,$J,358.3,24233,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24233,1,1,0)
 ;;=1^239.81
 ;;^UTILITY(U,$J,358.3,24233,1,8,0)
 ;;=8^Retina/Choroid Neoplasm NOS
 ;;^UTILITY(U,$J,358.3,24233,2)
 ;;=^338224
 ;;^UTILITY(U,$J,358.3,24234,0)
 ;;=239.89^^193^1653^7
 ;;^UTILITY(U,$J,358.3,24234,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24234,1,1,0)
 ;;=1^239.89
 ;;^UTILITY(U,$J,358.3,24234,1,8,0)
 ;;=8^Neoplasm,Oth Spec Sites
 ;;^UTILITY(U,$J,358.3,24234,2)
 ;;=^338225
 ;;^UTILITY(U,$J,358.3,24235,0)
 ;;=239.9^^193^1653^8
 ;;^UTILITY(U,$J,358.3,24235,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24235,1,1,0)
 ;;=1^239.9
 ;;^UTILITY(U,$J,358.3,24235,1,8,0)
 ;;=8^Neoplasm,Unspec Site 
 ;;^UTILITY(U,$J,358.3,24235,2)
 ;;=^82022
 ;;^UTILITY(U,$J,358.3,24236,0)
 ;;=288.9^^193^1653^15
 ;;^UTILITY(U,$J,358.3,24236,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24236,1,1,0)
 ;;=1^288.9
 ;;^UTILITY(U,$J,358.3,24236,1,8,0)
 ;;=8^Unspec Dis of WBC
 ;;^UTILITY(U,$J,358.3,24236,2)
 ;;=^267993
 ;;^UTILITY(U,$J,358.3,24237,0)
 ;;=289.0^^193^1653^13
 ;;^UTILITY(U,$J,358.3,24237,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24237,1,1,0)
 ;;=1^289.0
 ;;^UTILITY(U,$J,358.3,24237,1,8,0)
 ;;=8^Secondary Polycythemia
 ;;^UTILITY(U,$J,358.3,24237,2)
 ;;=^186856
 ;;^UTILITY(U,$J,358.3,24238,0)
 ;;=289.81^^193^1653^10
 ;;^UTILITY(U,$J,358.3,24238,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24238,1,1,0)
 ;;=1^289.81
 ;;^UTILITY(U,$J,358.3,24238,1,8,0)
 ;;=8^Primary Hypercoagulable State
 ;;^UTILITY(U,$J,358.3,24238,2)
 ;;=^329886
 ;;^UTILITY(U,$J,358.3,24239,0)
 ;;=289.9^^193^1653^14
 ;;^UTILITY(U,$J,358.3,24239,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,24239,1,1,0)
 ;;=1^289.9
 ;;^UTILITY(U,$J,358.3,24239,1,8,0)
 ;;=8^Unspec Blood Disease
 ;;^UTILITY(U,$J,358.3,24239,2)
 ;;=^55344
 ;;^UTILITY(U,$J,358.3,24240,0)
 ;;=A4220^^194^1654^1^^^^1
 ;;^UTILITY(U,$J,358.3,24240,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24240,1,1,0)
 ;;=1^A4220
 ;;^UTILITY(U,$J,358.3,24240,1,3,0)
 ;;=3^Infusion Pump Refill Kit
 ;;^UTILITY(U,$J,358.3,24241,0)
 ;;=99195^^194^1655^1^^^^1
 ;;^UTILITY(U,$J,358.3,24241,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24241,1,1,0)
 ;;=1^99195
 ;;^UTILITY(U,$J,358.3,24241,1,3,0)
 ;;=3^Phlebotomy
 ;;^UTILITY(U,$J,358.3,24242,0)
 ;;=20220^^194^1656^3^^^^1
 ;;^UTILITY(U,$J,358.3,24242,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24242,1,1,0)
 ;;=1^20220
 ;;^UTILITY(U,$J,358.3,24242,1,3,0)
 ;;=3^Bone Biopsy, Trocar/Needle
 ;;^UTILITY(U,$J,358.3,24243,0)
 ;;=62270^^194^1656^8^^^^1
 ;;^UTILITY(U,$J,358.3,24243,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24243,1,1,0)
 ;;=1^62270
 ;;^UTILITY(U,$J,358.3,24243,1,3,0)
 ;;=3^Lumbar Puncture
 ;;^UTILITY(U,$J,358.3,24244,0)
 ;;=45300^^194^1656^10^^^^1
 ;;^UTILITY(U,$J,358.3,24244,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24244,1,1,0)
 ;;=1^45300
 ;;^UTILITY(U,$J,358.3,24244,1,3,0)
 ;;=3^Proctosigmoidoscopy
 ;;^UTILITY(U,$J,358.3,24245,0)
 ;;=31575^^194^1656^7^^^^1
 ;;^UTILITY(U,$J,358.3,24245,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24245,1,1,0)
 ;;=1^31575
 ;;^UTILITY(U,$J,358.3,24245,1,3,0)
 ;;=3^Laryngoscopy,flex fibroptic,diag
 ;;^UTILITY(U,$J,358.3,24246,0)
 ;;=38220^^194^1656^4^^^^1
 ;;^UTILITY(U,$J,358.3,24246,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24246,1,1,0)
 ;;=1^38220
 ;;^UTILITY(U,$J,358.3,24246,1,3,0)
 ;;=3^Bone Marrow Aspiration
 ;;^UTILITY(U,$J,358.3,24247,0)
 ;;=38221^^194^1656^5^^^^1
 ;;^UTILITY(U,$J,358.3,24247,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24247,1,1,0)
 ;;=1^38221
 ;;^UTILITY(U,$J,358.3,24247,1,3,0)
 ;;=3^Bone Marrow Biopsy, Needle/Trocar
 ;;^UTILITY(U,$J,358.3,24248,0)
 ;;=36589^^194^1656^6^^^^1
 ;;^UTILITY(U,$J,358.3,24248,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24248,1,1,0)
 ;;=1^36589
 ;;^UTILITY(U,$J,358.3,24248,1,3,0)
 ;;=3^Hickman Cath Removal
 ;;^UTILITY(U,$J,358.3,24249,0)
 ;;=49082^^194^1656^2^^^^1
 ;;^UTILITY(U,$J,358.3,24249,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24249,1,1,0)
 ;;=1^49082
 ;;^UTILITY(U,$J,358.3,24249,1,3,0)
 ;;=3^Abd Paracentesis w/o Imag Guide
 ;;^UTILITY(U,$J,358.3,24250,0)
 ;;=49083^^194^1656^1^^^^1
 ;;^UTILITY(U,$J,358.3,24250,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24250,1,1,0)
 ;;=1^49083
 ;;^UTILITY(U,$J,358.3,24250,1,3,0)
 ;;=3^Abd Paracentesis w/ Imag Guide
 ;;^UTILITY(U,$J,358.3,24251,0)
 ;;=49084^^194^1656^9^^^^1
 ;;^UTILITY(U,$J,358.3,24251,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24251,1,1,0)
 ;;=1^49084
 ;;^UTILITY(U,$J,358.3,24251,1,3,0)
 ;;=3^Peritoneal Lavage,Inc Imag Guide
 ;;^UTILITY(U,$J,358.3,24252,0)
 ;;=32554^^194^1656^11^^^^1
 ;;^UTILITY(U,$J,358.3,24252,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24252,1,1,0)
 ;;=1^32554
 ;;^UTILITY(U,$J,358.3,24252,1,3,0)
 ;;=3^Thoracentesis w/o Imaging
 ;;^UTILITY(U,$J,358.3,24253,0)
 ;;=32555^^194^1656^12^^^^1
 ;;^UTILITY(U,$J,358.3,24253,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24253,1,1,0)
 ;;=1^32555
 ;;^UTILITY(U,$J,358.3,24253,1,3,0)
 ;;=3^Thoracentesis w/ Imaging
 ;;^UTILITY(U,$J,358.3,24254,0)
 ;;=90732^^194^1657^2^^^^1
 ;;^UTILITY(U,$J,358.3,24254,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24254,1,1,0)
 ;;=1^90732
 ;;^UTILITY(U,$J,358.3,24254,1,3,0)
 ;;=3^Pneumococcal Vaccine
 ;;^UTILITY(U,$J,358.3,24255,0)
 ;;=90658^^194^1657^1^^^^1
 ;;^UTILITY(U,$J,358.3,24255,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24255,1,1,0)
 ;;=1^90658
 ;;^UTILITY(U,$J,358.3,24255,1,3,0)
 ;;=3^Flu Vaccine
 ;;^UTILITY(U,$J,358.3,24256,0)
 ;;=99195^^194^1658^22^^^^1
 ;;^UTILITY(U,$J,358.3,24256,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24256,1,1,0)
 ;;=1^99195
 ;;^UTILITY(U,$J,358.3,24256,1,3,0)
 ;;=3^Phlebotomy
 ;;^UTILITY(U,$J,358.3,24257,0)
 ;;=96450^^194^1658^18^^^^1
 ;;^UTILITY(U,$J,358.3,24257,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24257,1,1,0)
 ;;=1^96450
 ;;^UTILITY(U,$J,358.3,24257,1,3,0)
 ;;=3^Chemotherapy, Into CNS
 ;;^UTILITY(U,$J,358.3,24258,0)
 ;;=96420^^194^1658^8^^^^1
 ;;^UTILITY(U,$J,358.3,24258,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24258,1,1,0)
 ;;=1^96420
 ;;^UTILITY(U,$J,358.3,24258,1,3,0)
 ;;=3^Chemo, IA push
 ;;^UTILITY(U,$J,358.3,24259,0)
 ;;=96422^^194^1658^6^^^^1
 ;;^UTILITY(U,$J,358.3,24259,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24259,1,1,0)
 ;;=1^96422
 ;;^UTILITY(U,$J,358.3,24259,1,3,0)
 ;;=3^Chemo, IA infusion,Init hr
 ;;^UTILITY(U,$J,358.3,24260,0)
 ;;=96405^^194^1658^3^^^^1
 ;;^UTILITY(U,$J,358.3,24260,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24260,1,1,0)
 ;;=1^96405
 ;;^UTILITY(U,$J,358.3,24260,1,3,0)
 ;;=3^Chemo Admin Intralesional,up to 7
 ;;^UTILITY(U,$J,358.3,24261,0)
 ;;=96440^^194^1658^4^^^^1
 ;;^UTILITY(U,$J,358.3,24261,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24261,1,1,0)
 ;;=1^96440
 ;;^UTILITY(U,$J,358.3,24261,1,3,0)
 ;;=3^Chemo Admin, Pleural Cavity
 ;;^UTILITY(U,$J,358.3,24262,0)
 ;;=96542^^194^1658^10^^^^1
 ;;^UTILITY(U,$J,358.3,24262,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24262,1,1,0)
 ;;=1^96542
 ;;^UTILITY(U,$J,358.3,24262,1,3,0)
 ;;=3^Chemo Inj Via Reservoir
 ;;^UTILITY(U,$J,358.3,24263,0)
 ;;=51720^^194^1658^1^^^^1
 ;;^UTILITY(U,$J,358.3,24263,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24263,1,1,0)
 ;;=1^51720
 ;;^UTILITY(U,$J,358.3,24263,1,3,0)
 ;;=3^Bladder Instill,anticarcinogenic
 ;;^UTILITY(U,$J,358.3,24264,0)
 ;;=96402^^194^1658^19^^^^1
 ;;^UTILITY(U,$J,358.3,24264,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24264,1,1,0)
 ;;=1^96402
 ;;^UTILITY(U,$J,358.3,24264,1,3,0)
 ;;=3^Chemotherapy,IM/SQ inj,Hormone
 ;;^UTILITY(U,$J,358.3,24265,0)
 ;;=96401^^194^1658^13^^^^1
 ;;^UTILITY(U,$J,358.3,24265,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24265,1,1,0)
 ;;=1^96401
 ;;^UTILITY(U,$J,358.3,24265,1,3,0)
 ;;=3^Chemo,IM/SQ inj,non-hormonal
 ;;^UTILITY(U,$J,358.3,24266,0)
 ;;=96409^^194^1658^16^^^^1
 ;;^UTILITY(U,$J,358.3,24266,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24266,1,1,0)
 ;;=1^96409
