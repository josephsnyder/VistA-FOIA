IBDEI01B ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,1118,2)
 ;;=^269570
 ;;^UTILITY(U,$J,358.3,1119,0)
 ;;=394.9^^16^87^13
 ;;^UTILITY(U,$J,358.3,1119,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1119,1,4,0)
 ;;=4^394.9
 ;;^UTILITY(U,$J,358.3,1119,1,5,0)
 ;;=5^Rhem Mitral Valve Dis
 ;;^UTILITY(U,$J,358.3,1119,2)
 ;;=^269571
 ;;^UTILITY(U,$J,358.3,1120,0)
 ;;=397.1^^16^87^14
 ;;^UTILITY(U,$J,358.3,1120,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1120,1,4,0)
 ;;=4^397.1
 ;;^UTILITY(U,$J,358.3,1120,1,5,0)
 ;;=5^Rhem Pulm Valve Disease
 ;;^UTILITY(U,$J,358.3,1120,2)
 ;;=^269587
 ;;^UTILITY(U,$J,358.3,1121,0)
 ;;=397.0^^16^87^15
 ;;^UTILITY(U,$J,358.3,1121,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1121,1,4,0)
 ;;=4^397.0
 ;;^UTILITY(U,$J,358.3,1121,1,5,0)
 ;;=5^Rhem Tricuspid Valve Disease
 ;;^UTILITY(U,$J,358.3,1121,2)
 ;;=^35528
 ;;^UTILITY(U,$J,358.3,1122,0)
 ;;=424.90^^16^88^1
 ;;^UTILITY(U,$J,358.3,1122,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1122,1,4,0)
 ;;=4^424.90
 ;;^UTILITY(U,$J,358.3,1122,1,5,0)
 ;;=5^Endocarditis
 ;;^UTILITY(U,$J,358.3,1122,2)
 ;;=^40327
 ;;^UTILITY(U,$J,358.3,1123,0)
 ;;=996.02^^16^88^2
 ;;^UTILITY(U,$J,358.3,1123,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1123,1,4,0)
 ;;=4^996.02
 ;;^UTILITY(U,$J,358.3,1123,1,5,0)
 ;;=5^Mech Comp Valve Prosth
 ;;^UTILITY(U,$J,358.3,1123,2)
 ;;=^276265
 ;;^UTILITY(U,$J,358.3,1124,0)
 ;;=V43.3^^16^88^3
 ;;^UTILITY(U,$J,358.3,1124,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1124,1,4,0)
 ;;=4^V43.3
 ;;^UTILITY(U,$J,358.3,1124,1,5,0)
 ;;=5^S/P Artif Valve Replac
 ;;^UTILITY(U,$J,358.3,1124,2)
 ;;=^295440
 ;;^UTILITY(U,$J,358.3,1125,0)
 ;;=V45.89^^16^88^4
 ;;^UTILITY(U,$J,358.3,1125,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1125,1,4,0)
 ;;=4^V45.89
 ;;^UTILITY(U,$J,358.3,1125,1,5,0)
 ;;=5^S/P Valve Surgery
 ;;^UTILITY(U,$J,358.3,1125,2)
 ;;=^276679
 ;;^UTILITY(U,$J,358.3,1126,0)
 ;;=996.71^^16^88^5
 ;;^UTILITY(U,$J,358.3,1126,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1126,1,4,0)
 ;;=4^996.71
 ;;^UTILITY(U,$J,358.3,1126,1,5,0)
 ;;=5^Thrombus Valve Pros
 ;;^UTILITY(U,$J,358.3,1126,2)
 ;;=^276294
 ;;^UTILITY(U,$J,358.3,1127,0)
 ;;=V58.61^^16^88^4.5
 ;;^UTILITY(U,$J,358.3,1127,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1127,1,4,0)
 ;;=4^V58.61
 ;;^UTILITY(U,$J,358.3,1127,1,5,0)
 ;;=5^Long Term Use Anticoagulants
 ;;^UTILITY(U,$J,358.3,1127,2)
 ;;=Long Term Use Anticoagulants^303459
 ;;^UTILITY(U,$J,358.3,1128,0)
 ;;=33206^^17^89^1^^^^1
 ;;^UTILITY(U,$J,358.3,1128,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1128,1,2,0)
 ;;=2^33206
 ;;^UTILITY(U,$J,358.3,1128,1,3,0)
 ;;=3^Pace Implant, Atrial
 ;;^UTILITY(U,$J,358.3,1129,0)
 ;;=33207^^17^89^2^^^^1
 ;;^UTILITY(U,$J,358.3,1129,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1129,1,2,0)
 ;;=2^33207
 ;;^UTILITY(U,$J,358.3,1129,1,3,0)
 ;;=3^Pace Implant, Vvi
 ;;^UTILITY(U,$J,358.3,1130,0)
 ;;=33208^^17^89^3^^^^1
 ;;^UTILITY(U,$J,358.3,1130,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1130,1,2,0)
 ;;=2^33208
 ;;^UTILITY(U,$J,358.3,1130,1,3,0)
 ;;=3^Pace Implant, Ddd
 ;;^UTILITY(U,$J,358.3,1131,0)
 ;;=33210^^17^89^4^^^^1
 ;;^UTILITY(U,$J,358.3,1131,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1131,1,2,0)
 ;;=2^33210
 ;;^UTILITY(U,$J,358.3,1131,1,3,0)
 ;;=3^Temp Pacer (Single)
 ;;^UTILITY(U,$J,358.3,1132,0)
 ;;=33211^^17^89^5^^^^1
 ;;^UTILITY(U,$J,358.3,1132,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1132,1,2,0)
 ;;=2^33211
 ;;^UTILITY(U,$J,358.3,1132,1,3,0)
 ;;=3^Temp Pacer (Dual)
 ;;^UTILITY(U,$J,358.3,1133,0)
 ;;=33212^^17^89^6^^^^1
 ;;^UTILITY(U,$J,358.3,1133,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1133,1,2,0)
 ;;=2^33212
 ;;^UTILITY(U,$J,358.3,1133,1,3,0)
 ;;=3^Insert Pacer, Pulse Gen (Sgl)
 ;;^UTILITY(U,$J,358.3,1134,0)
 ;;=33213^^17^89^7^^^^1
 ;;^UTILITY(U,$J,358.3,1134,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1134,1,2,0)
 ;;=2^33213
 ;;^UTILITY(U,$J,358.3,1134,1,3,0)
 ;;=3^Insert Pacer, Pulse Gen (Dual)
 ;;^UTILITY(U,$J,358.3,1135,0)
 ;;=33216^^17^89^8^^^^1
 ;;^UTILITY(U,$J,358.3,1135,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1135,1,2,0)
 ;;=2^33216
 ;;^UTILITY(U,$J,358.3,1135,1,3,0)
 ;;=3^Insert/Reposit Transv Elec (Sgl)
 ;;^UTILITY(U,$J,358.3,1136,0)
 ;;=33217^^17^89^9^^^^1
 ;;^UTILITY(U,$J,358.3,1136,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1136,1,2,0)
 ;;=2^33217
 ;;^UTILITY(U,$J,358.3,1136,1,3,0)
 ;;=3^Insert/Reposit Transv Elec(Dual)
 ;;^UTILITY(U,$J,358.3,1137,0)
 ;;=33218^^17^89^10^^^^1
 ;;^UTILITY(U,$J,358.3,1137,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1137,1,2,0)
 ;;=2^33218
 ;;^UTILITY(U,$J,358.3,1137,1,3,0)
 ;;=3^Repair Transv Elec (Single)
 ;;^UTILITY(U,$J,358.3,1138,0)
 ;;=33220^^17^89^11^^^^1
 ;;^UTILITY(U,$J,358.3,1138,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1138,1,2,0)
 ;;=2^33220
 ;;^UTILITY(U,$J,358.3,1138,1,3,0)
 ;;=3^Repair Transv Elec (Dual)
 ;;^UTILITY(U,$J,358.3,1139,0)
 ;;=33222^^17^89^12^^^^1
 ;;^UTILITY(U,$J,358.3,1139,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1139,1,2,0)
 ;;=2^33222
 ;;^UTILITY(U,$J,358.3,1139,1,3,0)
 ;;=3^Revis Or Reloc Skin Pckt
 ;;^UTILITY(U,$J,358.3,1140,0)
 ;;=33233^^17^89^18^^^^1
 ;;^UTILITY(U,$J,358.3,1140,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1140,1,2,0)
 ;;=2^33233
 ;;^UTILITY(U,$J,358.3,1140,1,3,0)
 ;;=3^Remove Pace Pulse Gen
 ;;^UTILITY(U,$J,358.3,1141,0)
 ;;=92960^^17^89^29^^^^1
 ;;^UTILITY(U,$J,358.3,1141,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1141,1,2,0)
 ;;=2^92960
 ;;^UTILITY(U,$J,358.3,1141,1,3,0)
 ;;=3^Cardioversion
 ;;^UTILITY(U,$J,358.3,1142,0)
 ;;=93650^^17^89^47^^^^1
 ;;^UTILITY(U,$J,358.3,1142,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1142,1,2,0)
 ;;=2^93650
 ;;^UTILITY(U,$J,358.3,1142,1,3,0)
 ;;=3^Abalation, Av Node
 ;;^UTILITY(U,$J,358.3,1143,0)
 ;;=93740^^17^89^49^^^^1
 ;;^UTILITY(U,$J,358.3,1143,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1143,1,2,0)
 ;;=2^93740
 ;;^UTILITY(U,$J,358.3,1143,1,3,0)
 ;;=3^Temp Gradient Studies
 ;;^UTILITY(U,$J,358.3,1144,0)
 ;;=33234^^17^89^20^^^^1
 ;;^UTILITY(U,$J,358.3,1144,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1144,1,2,0)
 ;;=2^33234
 ;;^UTILITY(U,$J,358.3,1144,1,3,0)
 ;;=3^Rem Transv Elec Atria/Vent(Sgl)
 ;;^UTILITY(U,$J,358.3,1145,0)
 ;;=33235^^17^89^21^^^^1
 ;;^UTILITY(U,$J,358.3,1145,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1145,1,2,0)
 ;;=2^33235
 ;;^UTILITY(U,$J,358.3,1145,1,3,0)
 ;;=3^Rem Transv Elec Atria/Vent(Dual)
 ;;^UTILITY(U,$J,358.3,1146,0)
 ;;=33240^^17^89^22^^^^1
 ;;^UTILITY(U,$J,358.3,1146,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1146,1,2,0)
 ;;=2^33240
 ;;^UTILITY(U,$J,358.3,1146,1,3,0)
 ;;=3^Insert Single/Dual Pulse Gen
 ;;^UTILITY(U,$J,358.3,1147,0)
 ;;=33241^^17^89^23^^^^1
 ;;^UTILITY(U,$J,358.3,1147,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1147,1,2,0)
 ;;=2^33241
 ;;^UTILITY(U,$J,358.3,1147,1,3,0)
 ;;=3^Subq Remove Sgl/Dual Pulse Gen
 ;;^UTILITY(U,$J,358.3,1148,0)
 ;;=33244^^17^89^24^^^^1
 ;;^UTILITY(U,$J,358.3,1148,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1148,1,2,0)
 ;;=2^33244
 ;;^UTILITY(U,$J,358.3,1148,1,3,0)
 ;;=3^Transv Remove Sgl/Dual Elec
 ;;^UTILITY(U,$J,358.3,1149,0)
 ;;=33249^^17^89^25^^^^1
 ;;^UTILITY(U,$J,358.3,1149,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1149,1,2,0)
 ;;=2^33249
 ;;^UTILITY(U,$J,358.3,1149,1,3,0)
 ;;=3^Ins/Reposit Lead, Insert Pulse Gen
 ;;^UTILITY(U,$J,358.3,1150,0)
 ;;=93285^^17^89^36^^^^1
 ;;^UTILITY(U,$J,358.3,1150,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1150,1,2,0)
 ;;=2^93285
 ;;^UTILITY(U,$J,358.3,1150,1,3,0)
 ;;=3^ILR Device Eval Progr
 ;;^UTILITY(U,$J,358.3,1151,0)
 ;;=93291^^17^89^42^^^^1
 ;;^UTILITY(U,$J,358.3,1151,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1151,1,2,0)
 ;;=2^93291
 ;;^UTILITY(U,$J,358.3,1151,1,3,0)
 ;;=3^ILR Device Interrogate
 ;;^UTILITY(U,$J,358.3,1152,0)
 ;;=93294^^17^89^45^^^^1
 ;;^UTILITY(U,$J,358.3,1152,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1152,1,2,0)
 ;;=2^93294
 ;;^UTILITY(U,$J,358.3,1152,1,3,0)
 ;;=3^PM Device Interrogate Remote
 ;;^UTILITY(U,$J,358.3,1153,0)
 ;;=93280^^17^89^31^^^^1
 ;;^UTILITY(U,$J,358.3,1153,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1153,1,2,0)
 ;;=2^93280
 ;;^UTILITY(U,$J,358.3,1153,1,3,0)
 ;;=3^PM Device Progr Eval,Dual
 ;;^UTILITY(U,$J,358.3,1154,0)
 ;;=93288^^17^89^39^^^^1
 ;;^UTILITY(U,$J,358.3,1154,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1154,1,2,0)
 ;;=2^93288
 ;;^UTILITY(U,$J,358.3,1154,1,3,0)
 ;;=3^PM Device Eval in Person
 ;;^UTILITY(U,$J,358.3,1155,0)
 ;;=93279^^17^89^30^^^^1
 ;;^UTILITY(U,$J,358.3,1155,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1155,1,2,0)
 ;;=2^93279
 ;;^UTILITY(U,$J,358.3,1155,1,3,0)
 ;;=3^PM Device Progr Eval,Sngl
 ;;^UTILITY(U,$J,358.3,1156,0)
 ;;=93282^^17^89^33^^^^1
 ;;^UTILITY(U,$J,358.3,1156,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1156,1,2,0)
 ;;=2^93282
 ;;^UTILITY(U,$J,358.3,1156,1,3,0)
 ;;=3^ICD Device Prog Eval,1 Sngl
 ;;^UTILITY(U,$J,358.3,1157,0)
 ;;=93289^^17^89^40^^^^1
 ;;^UTILITY(U,$J,358.3,1157,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1157,1,2,0)
 ;;=2^93289
 ;;^UTILITY(U,$J,358.3,1157,1,3,0)
 ;;=3^ICD Device Interrogatate
 ;;^UTILITY(U,$J,358.3,1158,0)
 ;;=93292^^17^89^43^^^^1
 ;;^UTILITY(U,$J,358.3,1158,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1158,1,2,0)
 ;;=2^93292
 ;;^UTILITY(U,$J,358.3,1158,1,3,0)
 ;;=3^WCD Device Interrogate
 ;;^UTILITY(U,$J,358.3,1159,0)
 ;;=93295^^17^89^46^^^^1
 ;;^UTILITY(U,$J,358.3,1159,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1159,1,2,0)
 ;;=2^93295
 ;;^UTILITY(U,$J,358.3,1159,1,3,0)
 ;;=3^ICD Device Interrogate Remote
 ;;^UTILITY(U,$J,358.3,1160,0)
 ;;=93283^^17^89^34^^^^1
 ;;^UTILITY(U,$J,358.3,1160,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1160,1,2,0)
 ;;=2^93283
 ;;^UTILITY(U,$J,358.3,1160,1,3,0)
 ;;=3^ICD Device Progr Eval,Dual
 ;;^UTILITY(U,$J,358.3,1161,0)
 ;;=93284^^17^89^35^^^^1
 ;;^UTILITY(U,$J,358.3,1161,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1161,1,2,0)
 ;;=2^93284