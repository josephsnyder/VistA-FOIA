IBDEI0A4 ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,13431,0)
 ;;=338.11^^105^853^6
 ;;^UTILITY(U,$J,358.3,13431,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13431,1,4,0)
 ;;=4^338.11
 ;;^UTILITY(U,$J,358.3,13431,1,5,0)
 ;;=5^Acute Pain due to Trauma
 ;;^UTILITY(U,$J,358.3,13431,2)
 ;;=^334070
 ;;^UTILITY(U,$J,358.3,13432,0)
 ;;=338.12^^105^853^7
 ;;^UTILITY(U,$J,358.3,13432,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13432,1,4,0)
 ;;=4^338.12
 ;;^UTILITY(U,$J,358.3,13432,1,5,0)
 ;;=5^Acute Post-Operative Pain
 ;;^UTILITY(U,$J,358.3,13432,2)
 ;;=^334071
 ;;^UTILITY(U,$J,358.3,13433,0)
 ;;=338.18^^105^853^31
 ;;^UTILITY(U,$J,358.3,13433,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13433,1,4,0)
 ;;=4^338.18
 ;;^UTILITY(U,$J,358.3,13433,1,5,0)
 ;;=5^Postoperative Pain NOS
 ;;^UTILITY(U,$J,358.3,13433,2)
 ;;=^334072
 ;;^UTILITY(U,$J,358.3,13434,0)
 ;;=338.19^^105^853^26
 ;;^UTILITY(U,$J,358.3,13434,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13434,1,4,0)
 ;;=4^338.19
 ;;^UTILITY(U,$J,358.3,13434,1,5,0)
 ;;=5^Other Acute Pain
 ;;^UTILITY(U,$J,358.3,13434,2)
 ;;=^334073
 ;;^UTILITY(U,$J,358.3,13435,0)
 ;;=338.21^^105^853^12
 ;;^UTILITY(U,$J,358.3,13435,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13435,1,4,0)
 ;;=4^338.21
 ;;^UTILITY(U,$J,358.3,13435,1,5,0)
 ;;=5^Chronic Pain due to Trauma
 ;;^UTILITY(U,$J,358.3,13435,2)
 ;;=^334074
 ;;^UTILITY(U,$J,358.3,13436,0)
 ;;=338.22^^105^853^13
 ;;^UTILITY(U,$J,358.3,13436,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13436,1,4,0)
 ;;=4^338.22
 ;;^UTILITY(U,$J,358.3,13436,1,5,0)
 ;;=5^Chronic Post-Thoracotomy Pain
 ;;^UTILITY(U,$J,358.3,13436,2)
 ;;=^334075
 ;;^UTILITY(U,$J,358.3,13437,0)
 ;;=338.28^^105^853^28
 ;;^UTILITY(U,$J,358.3,13437,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13437,1,4,0)
 ;;=4^338.28
 ;;^UTILITY(U,$J,358.3,13437,1,5,0)
 ;;=5^Other Chronic Postop Pain
 ;;^UTILITY(U,$J,358.3,13437,2)
 ;;=^334076
 ;;^UTILITY(U,$J,358.3,13438,0)
 ;;=338.29^^105^853^27
 ;;^UTILITY(U,$J,358.3,13438,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13438,1,4,0)
 ;;=4^338.29
 ;;^UTILITY(U,$J,358.3,13438,1,5,0)
 ;;=5^Other Chronic Pain
 ;;^UTILITY(U,$J,358.3,13438,2)
 ;;=^334077
 ;;^UTILITY(U,$J,358.3,13439,0)
 ;;=338.3^^105^853^8
 ;;^UTILITY(U,$J,358.3,13439,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13439,1,4,0)
 ;;=4^338.3
 ;;^UTILITY(U,$J,358.3,13439,1,5,0)
 ;;=5^Cancer Associated Pain
 ;;^UTILITY(U,$J,358.3,13439,2)
 ;;=^334078
 ;;^UTILITY(U,$J,358.3,13440,0)
 ;;=338.4^^105^853^11
 ;;^UTILITY(U,$J,358.3,13440,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13440,1,4,0)
 ;;=4^338.4
 ;;^UTILITY(U,$J,358.3,13440,1,5,0)
 ;;=5^Chronic Pain Syndrome
 ;;^UTILITY(U,$J,358.3,13440,2)
 ;;=^334079
 ;;^UTILITY(U,$J,358.3,13441,0)
 ;;=780.96^^105^853^17
 ;;^UTILITY(U,$J,358.3,13441,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13441,1,4,0)
 ;;=4^780.96
 ;;^UTILITY(U,$J,358.3,13441,1,5,0)
 ;;=5^Generalized Pain
 ;;^UTILITY(U,$J,358.3,13441,2)
 ;;=^334163
 ;;^UTILITY(U,$J,358.3,13442,0)
 ;;=607.9^^105^853^30
 ;;^UTILITY(U,$J,358.3,13442,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13442,1,4,0)
 ;;=4^607.9
 ;;^UTILITY(U,$J,358.3,13442,1,5,0)
 ;;=5^Penile Pain
 ;;^UTILITY(U,$J,358.3,13442,2)
 ;;=^270442
 ;;^UTILITY(U,$J,358.3,13443,0)
 ;;=608.9^^105^853^32
 ;;^UTILITY(U,$J,358.3,13443,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13443,1,4,0)
 ;;=4^608.9
 ;;^UTILITY(U,$J,358.3,13443,1,5,0)
 ;;=5^Scrotal Pain
 ;;^UTILITY(U,$J,358.3,13443,2)
 ;;=^123856
 ;;^UTILITY(U,$J,358.3,13444,0)
 ;;=V68.1^^105^854^4
 ;;^UTILITY(U,$J,358.3,13444,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13444,1,4,0)
 ;;=4^V68.1
 ;;^UTILITY(U,$J,358.3,13444,1,5,0)
 ;;=5^Rx Refill (Also mark Condition)
 ;;^UTILITY(U,$J,358.3,13444,2)
 ;;=RX Refill (also mark Condition)^295585
 ;;^UTILITY(U,$J,358.3,13445,0)
 ;;=V68.81^^105^854^5
 ;;^UTILITY(U,$J,358.3,13445,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13445,1,4,0)
 ;;=4^V68.81
 ;;^UTILITY(U,$J,358.3,13445,1,5,0)
 ;;=5^Transfer of Care (Also mark Conditions)
 ;;^UTILITY(U,$J,358.3,13445,2)
 ;;=Transfer of Care ^295587
 ;;^UTILITY(U,$J,358.3,13446,0)
 ;;=V58.83^^105^854^2
 ;;^UTILITY(U,$J,358.3,13446,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13446,1,4,0)
 ;;=4^V58.83
 ;;^UTILITY(U,$J,358.3,13446,1,5,0)
 ;;=5^Encounter for Therapeutic Drug Monitoring
 ;;^UTILITY(U,$J,358.3,13446,2)
 ;;=Encounter for Therapeutic Drug Monitoring^322076
 ;;^UTILITY(U,$J,358.3,13447,0)
 ;;=V68.09^^105^854^3
 ;;^UTILITY(U,$J,358.3,13447,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13447,1,4,0)
 ;;=4^V68.09
 ;;^UTILITY(U,$J,358.3,13447,1,5,0)
 ;;=5^Forms Completion (Also include Condition
 ;;^UTILITY(U,$J,358.3,13447,2)
 ;;=^335321
 ;;^UTILITY(U,$J,358.3,13448,0)
 ;;=V65.40^^105^854^1
 ;;^UTILITY(U,$J,358.3,13448,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13448,1,4,0)
 ;;=4^V65.40
 ;;^UTILITY(U,$J,358.3,13448,1,5,0)
 ;;=5^Counseling NOS
 ;;^UTILITY(U,$J,358.3,13448,2)
 ;;=^87449
 ;;^UTILITY(U,$J,358.3,13449,0)
 ;;=E880.1^^105^855^14
 ;;^UTILITY(U,$J,358.3,13449,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13449,1,4,0)
 ;;=4^E880.1
 ;;^UTILITY(U,$J,358.3,13449,1,5,0)
 ;;=5^Fall on/from Sidewalk or Curb
 ;;^UTILITY(U,$J,358.3,13449,2)
 ;;=Fall on/from Sidewalk or Curb^303367
 ;;^UTILITY(U,$J,358.3,13450,0)
 ;;=E881.0^^105^855^5
 ;;^UTILITY(U,$J,358.3,13450,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13450,1,4,0)
 ;;=4^E881.0
 ;;^UTILITY(U,$J,358.3,13450,1,5,0)
 ;;=5^Fall From Ladder
 ;;^UTILITY(U,$J,358.3,13450,2)
 ;;=Fall From Ladder^294644
 ;;^UTILITY(U,$J,358.3,13451,0)
 ;;=E882.^^105^855^3
 ;;^UTILITY(U,$J,358.3,13451,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13451,1,4,0)
 ;;=4^E882.
 ;;^UTILITY(U,$J,358.3,13451,1,5,0)
 ;;=5^Fall From Building
 ;;^UTILITY(U,$J,358.3,13451,2)
 ;;=Fall From Building^294646
 ;;^UTILITY(U,$J,358.3,13452,0)
 ;;=E883.9^^105^855^7
 ;;^UTILITY(U,$J,358.3,13452,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13452,1,4,0)
 ;;=4^E883.9
 ;;^UTILITY(U,$J,358.3,13452,1,5,0)
 ;;=5^Fall Into Hole
 ;;^UTILITY(U,$J,358.3,13452,2)
 ;;=Fall Into Hole^294650
 ;;^UTILITY(U,$J,358.3,13453,0)
 ;;=E884.2^^105^855^4
 ;;^UTILITY(U,$J,358.3,13453,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13453,1,4,0)
 ;;=4^E884.2
 ;;^UTILITY(U,$J,358.3,13453,1,5,0)
 ;;=5^Fall From Chair
 ;;^UTILITY(U,$J,358.3,13453,2)
 ;;=Fall From Chair^294653
 ;;^UTILITY(U,$J,358.3,13454,0)
 ;;=E884.3^^105^855^6
 ;;^UTILITY(U,$J,358.3,13454,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13454,1,4,0)
 ;;=4^E884.3
 ;;^UTILITY(U,$J,358.3,13454,1,5,0)
 ;;=5^Fall From Wheelchair
 ;;^UTILITY(U,$J,358.3,13454,2)
 ;;=Fall From Wheelchair^303368
 ;;^UTILITY(U,$J,358.3,13455,0)
 ;;=E884.4^^105^855^2
 ;;^UTILITY(U,$J,358.3,13455,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13455,1,4,0)
 ;;=4^E884.4
 ;;^UTILITY(U,$J,358.3,13455,1,5,0)
 ;;=5^Fall From Bed
 ;;^UTILITY(U,$J,358.3,13455,2)
 ;;=Fall From Bed^303369
 ;;^UTILITY(U,$J,358.3,13456,0)
 ;;=E884.6^^105^855^8
 ;;^UTILITY(U,$J,358.3,13456,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13456,1,4,0)
 ;;=4^E884.6
 ;;^UTILITY(U,$J,358.3,13456,1,5,0)
 ;;=5^Fall from Commode
 ;;^UTILITY(U,$J,358.3,13456,2)
 ;;=Fall from Commode^303371
 ;;^UTILITY(U,$J,358.3,13457,0)
 ;;=E884.9^^105^855^18
 ;;^UTILITY(U,$J,358.3,13457,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13457,1,4,0)
 ;;=4^E884.9
 ;;^UTILITY(U,$J,358.3,13457,1,5,0)
 ;;=5^Other Fall, one level to another
 ;;^UTILITY(U,$J,358.3,13457,2)
 ;;=Other Fall^294654
 ;;^UTILITY(U,$J,358.3,13458,0)
 ;;=E885.1^^105^855^9
 ;;^UTILITY(U,$J,358.3,13458,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13458,1,4,0)
 ;;=4^E885.1
 ;;^UTILITY(U,$J,358.3,13458,1,5,0)
 ;;=5^Fall from Roller Skates
 ;;^UTILITY(U,$J,358.3,13458,2)
 ;;=Fall from Roller Skates^322100
 ;;^UTILITY(U,$J,358.3,13459,0)
 ;;=E885.2^^105^855^10
 ;;^UTILITY(U,$J,358.3,13459,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13459,1,4,0)
 ;;=4^E885.2
 ;;^UTILITY(U,$J,358.3,13459,1,5,0)
 ;;=5^Fall from Skateboard
 ;;^UTILITY(U,$J,358.3,13459,2)
 ;;=Fall from Skateboard^322102
 ;;^UTILITY(U,$J,358.3,13460,0)
 ;;=E885.3^^105^855^11
 ;;^UTILITY(U,$J,358.3,13460,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13460,1,4,0)
 ;;=4^E885.3
 ;;^UTILITY(U,$J,358.3,13460,1,5,0)
 ;;=5^Fall from Skis
 ;;^UTILITY(U,$J,358.3,13460,2)
 ;;=Fall from Skis^322103
 ;;^UTILITY(U,$J,358.3,13461,0)
 ;;=E885.4^^105^855^12
 ;;^UTILITY(U,$J,358.3,13461,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13461,1,4,0)
 ;;=4^E885.4
 ;;^UTILITY(U,$J,358.3,13461,1,5,0)
 ;;=5^Fall from Snowboard
 ;;^UTILITY(U,$J,358.3,13461,2)
 ;;=Fall from Snowboard^322104
 ;;^UTILITY(U,$J,358.3,13462,0)
 ;;=E885.9^^105^855^1
 ;;^UTILITY(U,$J,358.3,13462,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13462,1,4,0)
 ;;=4^E885.9
 ;;^UTILITY(U,$J,358.3,13462,1,5,0)
 ;;=5^Fall After Tripping or Slipping
 ;;^UTILITY(U,$J,358.3,13462,2)
 ;;=Fall after tripping or slipping^322105
 ;;^UTILITY(U,$J,358.3,13463,0)
 ;;=E886.0^^105^855^13
 ;;^UTILITY(U,$J,358.3,13463,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13463,1,4,0)
 ;;=4^E886.0
 ;;^UTILITY(U,$J,358.3,13463,1,5,0)
 ;;=5^Fall in Sports
 ;;^UTILITY(U,$J,358.3,13463,2)
 ;;=Fall in Sports^294656
 ;;^UTILITY(U,$J,358.3,13464,0)
 ;;=E886.9^^105^855^15
 ;;^UTILITY(U,$J,358.3,13464,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13464,1,4,0)
 ;;=4^E886.9
 ;;^UTILITY(U,$J,358.3,13464,1,5,0)
 ;;=5^Fall, Collision with another person
 ;;^UTILITY(U,$J,358.3,13464,2)
 ;;=Fall, Collision with another person^294657
 ;;^UTILITY(U,$J,358.3,13465,0)
 ;;=E888.9^^105^855^16
 ;;^UTILITY(U,$J,358.3,13465,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13465,1,4,0)
 ;;=4^E888.9
 ;;^UTILITY(U,$J,358.3,13465,1,5,0)
 ;;=5^Fall, Not Specified
 ;;^UTILITY(U,$J,358.3,13465,2)
 ;;=Fall, Not Specified^323639
 ;;^UTILITY(U,$J,358.3,13466,0)
 ;;=E819.0^^105^855^19
 ;;^UTILITY(U,$J,358.3,13466,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13466,1,4,0)
 ;;=4^E819.0
