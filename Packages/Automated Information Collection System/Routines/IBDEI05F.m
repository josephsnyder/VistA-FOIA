IBDEI05F ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,6951,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6951,1,4,0)
 ;;=4^786.30
 ;;^UTILITY(U,$J,358.3,6951,1,5,0)
 ;;=5^Hemoptysis NOS
 ;;^UTILITY(U,$J,358.3,6951,2)
 ;;=^339669
 ;;^UTILITY(U,$J,358.3,6952,0)
 ;;=780.53^^58^510^85
 ;;^UTILITY(U,$J,358.3,6952,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6952,1,4,0)
 ;;=4^780.53
 ;;^UTILITY(U,$J,358.3,6952,1,5,0)
 ;;=5^Hypersomnia w/Sleep Apnea
 ;;^UTILITY(U,$J,358.3,6952,2)
 ;;=^332925
 ;;^UTILITY(U,$J,358.3,6953,0)
 ;;=491.22^^58^510^116
 ;;^UTILITY(U,$J,358.3,6953,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6953,1,4,0)
 ;;=4^491.22
 ;;^UTILITY(U,$J,358.3,6953,1,5,0)
 ;;=5^Obs Chr Bronc w/Acute Bronch
 ;;^UTILITY(U,$J,358.3,6953,2)
 ;;=^331455
 ;;^UTILITY(U,$J,358.3,6954,0)
 ;;=784.0^^58^511^18
 ;;^UTILITY(U,$J,358.3,6954,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6954,1,4,0)
 ;;=4^784.0
 ;;^UTILITY(U,$J,358.3,6954,1,5,0)
 ;;=5^Headache
 ;;^UTILITY(U,$J,358.3,6954,2)
 ;;=Headache^54133
 ;;^UTILITY(U,$J,358.3,6955,0)
 ;;=729.5^^58^511^16
 ;;^UTILITY(U,$J,358.3,6955,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6955,1,4,0)
 ;;=4^729.5
 ;;^UTILITY(U,$J,358.3,6955,1,5,0)
 ;;=5^Foot Pain
 ;;^UTILITY(U,$J,358.3,6955,2)
 ;;=Foot Pain^89086
 ;;^UTILITY(U,$J,358.3,6956,0)
 ;;=723.1^^58^511^24
 ;;^UTILITY(U,$J,358.3,6956,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6956,1,4,0)
 ;;=4^723.1
 ;;^UTILITY(U,$J,358.3,6956,1,5,0)
 ;;=5^Neck Pain
 ;;^UTILITY(U,$J,358.3,6956,2)
 ;;=Neck Pain^21917
 ;;^UTILITY(U,$J,358.3,6957,0)
 ;;=719.41^^58^511^33
 ;;^UTILITY(U,$J,358.3,6957,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6957,1,4,0)
 ;;=4^719.41
 ;;^UTILITY(U,$J,358.3,6957,1,5,0)
 ;;=5^Shoulder Pain
 ;;^UTILITY(U,$J,358.3,6957,2)
 ;;=^272398
 ;;^UTILITY(U,$J,358.3,6958,0)
 ;;=719.45^^58^511^19
 ;;^UTILITY(U,$J,358.3,6958,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6958,1,4,0)
 ;;=4^719.45
 ;;^UTILITY(U,$J,358.3,6958,1,5,0)
 ;;=5^Hip Pain
 ;;^UTILITY(U,$J,358.3,6958,2)
 ;;=Hip Pain^272402
 ;;^UTILITY(U,$J,358.3,6959,0)
 ;;=786.52^^58^511^10
 ;;^UTILITY(U,$J,358.3,6959,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6959,1,4,0)
 ;;=4^786.52
 ;;^UTILITY(U,$J,358.3,6959,1,5,0)
 ;;=5^Chest Wall Pain
 ;;^UTILITY(U,$J,358.3,6959,2)
 ;;=^89126
 ;;^UTILITY(U,$J,358.3,6960,0)
 ;;=719.46^^58^511^22
 ;;^UTILITY(U,$J,358.3,6960,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6960,1,4,0)
 ;;=4^719.46
 ;;^UTILITY(U,$J,358.3,6960,1,5,0)
 ;;=5^Knee Pain
 ;;^UTILITY(U,$J,358.3,6960,2)
 ;;=^272403
 ;;^UTILITY(U,$J,358.3,6961,0)
 ;;=346.90^^58^511^23
 ;;^UTILITY(U,$J,358.3,6961,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6961,1,4,0)
 ;;=4^346.90
 ;;^UTILITY(U,$J,358.3,6961,1,5,0)
 ;;=5^Migraine
 ;;^UTILITY(U,$J,358.3,6961,2)
 ;;=^293880
 ;;^UTILITY(U,$J,358.3,6962,0)
 ;;=729.1^^58^511^25
 ;;^UTILITY(U,$J,358.3,6962,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6962,1,4,0)
 ;;=4^729.1
 ;;^UTILITY(U,$J,358.3,6962,1,5,0)
 ;;=5^Neuropathic Pain
 ;;^UTILITY(U,$J,358.3,6962,2)
 ;;=Neuropathic Pain^80160
 ;;^UTILITY(U,$J,358.3,6963,0)
 ;;=625.9^^58^511^29
 ;;^UTILITY(U,$J,358.3,6963,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6963,1,4,0)
 ;;=4^625.9
 ;;^UTILITY(U,$J,358.3,6963,1,5,0)
 ;;=5^Pelvic Pain (Female)
 ;;^UTILITY(U,$J,358.3,6963,2)
 ;;=^123993
 ;;^UTILITY(U,$J,358.3,6964,0)
 ;;=388.70^^58^511^14
 ;;^UTILITY(U,$J,358.3,6964,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6964,1,4,0)
 ;;=4^388.70
 ;;^UTILITY(U,$J,358.3,6964,1,5,0)
 ;;=5^Ear Pain
 ;;^UTILITY(U,$J,358.3,6964,2)
 ;;=Ear Pain^37811
 ;;^UTILITY(U,$J,358.3,6965,0)
 ;;=526.9^^58^511^20
 ;;^UTILITY(U,$J,358.3,6965,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6965,1,4,0)
 ;;=4^526.9
 ;;^UTILITY(U,$J,358.3,6965,1,5,0)
 ;;=5^Jaw Pain
 ;;^UTILITY(U,$J,358.3,6965,2)
 ;;=Jaw Pain^66177
 ;;^UTILITY(U,$J,358.3,6966,0)
 ;;=789.01^^58^511^5
 ;;^UTILITY(U,$J,358.3,6966,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6966,1,4,0)
 ;;=4^789.01
 ;;^UTILITY(U,$J,358.3,6966,1,5,0)
 ;;=5^Abdominal Pain, RUQ
 ;;^UTILITY(U,$J,358.3,6966,2)
 ;;=Abdominal Pain, RUQ^303318
 ;;^UTILITY(U,$J,358.3,6967,0)
 ;;=789.02^^58^511^3
 ;;^UTILITY(U,$J,358.3,6967,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6967,1,4,0)
 ;;=4^789.02
 ;;^UTILITY(U,$J,358.3,6967,1,5,0)
 ;;=5^Abdominal Pain, LUQ
 ;;^UTILITY(U,$J,358.3,6967,2)
 ;;=Abdominal Pain, LUQ^303319
 ;;^UTILITY(U,$J,358.3,6968,0)
 ;;=789.03^^58^511^4
 ;;^UTILITY(U,$J,358.3,6968,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6968,1,4,0)
 ;;=4^789.03
 ;;^UTILITY(U,$J,358.3,6968,1,5,0)
 ;;=5^Abdominal Pain, RLQ
 ;;^UTILITY(U,$J,358.3,6968,2)
 ;;=Abdominal PainLLQ^303320
 ;;^UTILITY(U,$J,358.3,6969,0)
 ;;=789.04^^58^511^2
 ;;^UTILITY(U,$J,358.3,6969,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6969,1,4,0)
 ;;=4^789.04
 ;;^UTILITY(U,$J,358.3,6969,1,5,0)
 ;;=5^Abdominal Pain, LLQ
 ;;^UTILITY(U,$J,358.3,6969,2)
 ;;=^303321
 ;;^UTILITY(U,$J,358.3,6970,0)
 ;;=789.06^^58^511^15
 ;;^UTILITY(U,$J,358.3,6970,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6970,1,4,0)
 ;;=4^789.06
 ;;^UTILITY(U,$J,358.3,6970,1,5,0)
 ;;=5^Epigastric Pain
 ;;^UTILITY(U,$J,358.3,6970,2)
 ;;=Epigastric Pain^303323
 ;;^UTILITY(U,$J,358.3,6971,0)
 ;;=789.07^^58^511^1
 ;;^UTILITY(U,$J,358.3,6971,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6971,1,4,0)
 ;;=4^789.07
 ;;^UTILITY(U,$J,358.3,6971,1,5,0)
 ;;=5^Abdominal Pain, Generalized
 ;;^UTILITY(U,$J,358.3,6971,2)
 ;;=^303324
 ;;^UTILITY(U,$J,358.3,6972,0)
 ;;=788.0^^58^511^21
 ;;^UTILITY(U,$J,358.3,6972,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6972,1,4,0)
 ;;=4^788.0
 ;;^UTILITY(U,$J,358.3,6972,1,5,0)
 ;;=5^Kidney Pain
 ;;^UTILITY(U,$J,358.3,6972,2)
 ;;=^265306
 ;;^UTILITY(U,$J,358.3,6973,0)
 ;;=338.0^^58^511^9
 ;;^UTILITY(U,$J,358.3,6973,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6973,1,4,0)
 ;;=4^338.0
 ;;^UTILITY(U,$J,358.3,6973,1,5,0)
 ;;=5^Central Pain Syndrome
 ;;^UTILITY(U,$J,358.3,6973,2)
 ;;=^334189
 ;;^UTILITY(U,$J,358.3,6974,0)
 ;;=338.11^^58^511^6
 ;;^UTILITY(U,$J,358.3,6974,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6974,1,4,0)
 ;;=4^338.11
 ;;^UTILITY(U,$J,358.3,6974,1,5,0)
 ;;=5^Acute Pain due to Trauma
 ;;^UTILITY(U,$J,358.3,6974,2)
 ;;=^334070
 ;;^UTILITY(U,$J,358.3,6975,0)
 ;;=338.12^^58^511^7
 ;;^UTILITY(U,$J,358.3,6975,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6975,1,4,0)
 ;;=4^338.12
 ;;^UTILITY(U,$J,358.3,6975,1,5,0)
 ;;=5^Acute Post-Operative Pain
 ;;^UTILITY(U,$J,358.3,6975,2)
 ;;=^334071
 ;;^UTILITY(U,$J,358.3,6976,0)
 ;;=338.18^^58^511^31
 ;;^UTILITY(U,$J,358.3,6976,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6976,1,4,0)
 ;;=4^338.18
 ;;^UTILITY(U,$J,358.3,6976,1,5,0)
 ;;=5^Postoperative Pain NOS
 ;;^UTILITY(U,$J,358.3,6976,2)
 ;;=^334072
 ;;^UTILITY(U,$J,358.3,6977,0)
 ;;=338.19^^58^511^26
 ;;^UTILITY(U,$J,358.3,6977,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6977,1,4,0)
 ;;=4^338.19
 ;;^UTILITY(U,$J,358.3,6977,1,5,0)
 ;;=5^Other Acute Pain
 ;;^UTILITY(U,$J,358.3,6977,2)
 ;;=^334073
 ;;^UTILITY(U,$J,358.3,6978,0)
 ;;=338.21^^58^511^12
 ;;^UTILITY(U,$J,358.3,6978,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6978,1,4,0)
 ;;=4^338.21
 ;;^UTILITY(U,$J,358.3,6978,1,5,0)
 ;;=5^Chronic Pain due to Trauma
 ;;^UTILITY(U,$J,358.3,6978,2)
 ;;=^334074
 ;;^UTILITY(U,$J,358.3,6979,0)
 ;;=338.22^^58^511^13
 ;;^UTILITY(U,$J,358.3,6979,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6979,1,4,0)
 ;;=4^338.22
 ;;^UTILITY(U,$J,358.3,6979,1,5,0)
 ;;=5^Chronic Post-Thoracotomy Pain
 ;;^UTILITY(U,$J,358.3,6979,2)
 ;;=^334075
 ;;^UTILITY(U,$J,358.3,6980,0)
 ;;=338.28^^58^511^28
 ;;^UTILITY(U,$J,358.3,6980,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6980,1,4,0)
 ;;=4^338.28
 ;;^UTILITY(U,$J,358.3,6980,1,5,0)
 ;;=5^Other Chronic Postop Pain
 ;;^UTILITY(U,$J,358.3,6980,2)
 ;;=^334076
 ;;^UTILITY(U,$J,358.3,6981,0)
 ;;=338.29^^58^511^27
 ;;^UTILITY(U,$J,358.3,6981,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6981,1,4,0)
 ;;=4^338.29
 ;;^UTILITY(U,$J,358.3,6981,1,5,0)
 ;;=5^Other Chronic Pain
 ;;^UTILITY(U,$J,358.3,6981,2)
 ;;=^334077
 ;;^UTILITY(U,$J,358.3,6982,0)
 ;;=338.3^^58^511^8
 ;;^UTILITY(U,$J,358.3,6982,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6982,1,4,0)
 ;;=4^338.3
 ;;^UTILITY(U,$J,358.3,6982,1,5,0)
 ;;=5^Cancer Associated Pain
 ;;^UTILITY(U,$J,358.3,6982,2)
 ;;=^334078
 ;;^UTILITY(U,$J,358.3,6983,0)
 ;;=338.4^^58^511^11
 ;;^UTILITY(U,$J,358.3,6983,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6983,1,4,0)
 ;;=4^338.4
 ;;^UTILITY(U,$J,358.3,6983,1,5,0)
 ;;=5^Chronic Pain Syndrome
 ;;^UTILITY(U,$J,358.3,6983,2)
 ;;=^334079
 ;;^UTILITY(U,$J,358.3,6984,0)
 ;;=780.96^^58^511^17
 ;;^UTILITY(U,$J,358.3,6984,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6984,1,4,0)
 ;;=4^780.96
 ;;^UTILITY(U,$J,358.3,6984,1,5,0)
 ;;=5^Generalized Pain
 ;;^UTILITY(U,$J,358.3,6984,2)
 ;;=^334163
 ;;^UTILITY(U,$J,358.3,6985,0)
 ;;=607.9^^58^511^30
 ;;^UTILITY(U,$J,358.3,6985,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6985,1,4,0)
 ;;=4^607.9
 ;;^UTILITY(U,$J,358.3,6985,1,5,0)
 ;;=5^Penile Pain
 ;;^UTILITY(U,$J,358.3,6985,2)
 ;;=^270442
 ;;^UTILITY(U,$J,358.3,6986,0)
 ;;=608.9^^58^511^32
 ;;^UTILITY(U,$J,358.3,6986,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6986,1,4,0)
 ;;=4^608.9
 ;;^UTILITY(U,$J,358.3,6986,1,5,0)
 ;;=5^Scrotal Pain
 ;;^UTILITY(U,$J,358.3,6986,2)
 ;;=^123856
 ;;^UTILITY(U,$J,358.3,6987,0)
 ;;=V68.1^^58^512^6
 ;;^UTILITY(U,$J,358.3,6987,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6987,1,4,0)
 ;;=4^V68.1
 ;;^UTILITY(U,$J,358.3,6987,1,5,0)
 ;;=5^Rx Refill (Also mark Condition)
 ;;^UTILITY(U,$J,358.3,6987,2)
 ;;=RX Refill (also mark Condition)^295585
 ;;^UTILITY(U,$J,358.3,6988,0)
 ;;=V68.81^^58^512^7
 ;;^UTILITY(U,$J,358.3,6988,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6988,1,4,0)
 ;;=4^V68.81
 ;;^UTILITY(U,$J,358.3,6988,1,5,0)
 ;;=5^Transfer of Care (Also mark Conditions)
 ;;^UTILITY(U,$J,358.3,6988,2)
 ;;=Transfer of Care ^295587
 ;;^UTILITY(U,$J,358.3,6989,0)
 ;;=V58.83^^58^512^4
