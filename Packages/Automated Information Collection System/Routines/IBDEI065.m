IBDEI065 ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,7990,0)
 ;;=29126^^72^599^3^^^^1
 ;;^UTILITY(U,$J,358.3,7990,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,7990,1,2,0)
 ;;=2^29126
 ;;^UTILITY(U,$J,358.3,7990,1,3,0)
 ;;=3^Short Arm Splint; Dynamic
 ;;^UTILITY(U,$J,358.3,7991,0)
 ;;=29130^^72^599^1^^^^1
 ;;^UTILITY(U,$J,358.3,7991,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,7991,1,2,0)
 ;;=2^29130
 ;;^UTILITY(U,$J,358.3,7991,1,3,0)
 ;;=3^Finger Splint
 ;;^UTILITY(U,$J,358.3,7992,0)
 ;;=12001^^72^600^1^^^^1
 ;;^UTILITY(U,$J,358.3,7992,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,7992,1,2,0)
 ;;=2^12001
 ;;^UTILITY(U,$J,358.3,7992,1,3,0)
 ;;=3^Simple repair; 2.5 cm or less
 ;;^UTILITY(U,$J,358.3,7993,0)
 ;;=12002^^72^600^2^^^^1
 ;;^UTILITY(U,$J,358.3,7993,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,7993,1,2,0)
 ;;=2^12002
 ;;^UTILITY(U,$J,358.3,7993,1,3,0)
 ;;=3^Simple repair; 2.6 cm to 7.5 cm
 ;;^UTILITY(U,$J,358.3,7994,0)
 ;;=12004^^72^600^3^^^^1
 ;;^UTILITY(U,$J,358.3,7994,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,7994,1,2,0)
 ;;=2^12004
 ;;^UTILITY(U,$J,358.3,7994,1,3,0)
 ;;=3^Simple repair; 7.6 cm to 12.5 cm
 ;;^UTILITY(U,$J,358.3,7995,0)
 ;;=12005^^72^600^4^^^^1
 ;;^UTILITY(U,$J,358.3,7995,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,7995,1,2,0)
 ;;=2^12005
 ;;^UTILITY(U,$J,358.3,7995,1,3,0)
 ;;=3^Simple repair; 12.6 cm to 20 cm
 ;;^UTILITY(U,$J,358.3,7996,0)
 ;;=12006^^72^600^5^^^^1
 ;;^UTILITY(U,$J,358.3,7996,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,7996,1,2,0)
 ;;=2^12006
 ;;^UTILITY(U,$J,358.3,7996,1,3,0)
 ;;=3^Simple repair; 20.1 cm to 30 cm
 ;;^UTILITY(U,$J,358.3,7997,0)
 ;;=12007^^72^600^6^^^^1
 ;;^UTILITY(U,$J,358.3,7997,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,7997,1,2,0)
 ;;=2^12007
 ;;^UTILITY(U,$J,358.3,7997,1,3,0)
 ;;=3^Simple repair; over 30 cm
 ;;^UTILITY(U,$J,358.3,7998,0)
 ;;=12011^^72^601^1^^^^1
 ;;^UTILITY(U,$J,358.3,7998,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,7998,1,2,0)
 ;;=2^12011
 ;;^UTILITY(U,$J,358.3,7998,1,3,0)
 ;;=3^Simple repair; 2.5 cm or less
 ;;^UTILITY(U,$J,358.3,7999,0)
 ;;=12013^^72^601^2^^^^1
 ;;^UTILITY(U,$J,358.3,7999,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,7999,1,2,0)
 ;;=2^12013
 ;;^UTILITY(U,$J,358.3,7999,1,3,0)
 ;;=3^Simple repair; 2.6 cm to 5.0 cm
 ;;^UTILITY(U,$J,358.3,8000,0)
 ;;=12014^^72^601^3^^^^1
 ;;^UTILITY(U,$J,358.3,8000,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8000,1,2,0)
 ;;=2^12014
 ;;^UTILITY(U,$J,358.3,8000,1,3,0)
 ;;=3^Simple repair; 5.1 cm to 7.5 cm
 ;;^UTILITY(U,$J,358.3,8001,0)
 ;;=12015^^72^601^4^^^^1
 ;;^UTILITY(U,$J,358.3,8001,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8001,1,2,0)
 ;;=2^12015
 ;;^UTILITY(U,$J,358.3,8001,1,3,0)
 ;;=3^Simple repair; 7.6 cm to 12.5 cm
 ;;^UTILITY(U,$J,358.3,8002,0)
 ;;=12016^^72^601^5^^^^1
 ;;^UTILITY(U,$J,358.3,8002,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8002,1,2,0)
 ;;=2^12016
 ;;^UTILITY(U,$J,358.3,8002,1,3,0)
 ;;=3^Simple repair; 12.6 cm to 20 cm
 ;;^UTILITY(U,$J,358.3,8003,0)
 ;;=12017^^72^601^6^^^^1
 ;;^UTILITY(U,$J,358.3,8003,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8003,1,2,0)
 ;;=2^12017
 ;;^UTILITY(U,$J,358.3,8003,1,3,0)
 ;;=3^Simple repair; 20.1 cm to 30 cm
 ;;^UTILITY(U,$J,358.3,8004,0)
 ;;=12018^^72^601^7^^^^1
 ;;^UTILITY(U,$J,358.3,8004,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8004,1,2,0)
 ;;=2^12018
 ;;^UTILITY(U,$J,358.3,8004,1,3,0)
 ;;=3^Simple repair; over 30 cm
 ;;^UTILITY(U,$J,358.3,8005,0)
 ;;=12020^^72^601^8^^^^1
 ;;^UTILITY(U,$J,358.3,8005,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8005,1,2,0)
 ;;=2^12020
 ;;^UTILITY(U,$J,358.3,8005,1,3,0)
 ;;=3^TRXMT SUPERFICIAL WOUND DEHISCENCE;SIMPLE
 ;;^UTILITY(U,$J,358.3,8006,0)
 ;;=99291^^72^602^1^^^^1
 ;;^UTILITY(U,$J,358.3,8006,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8006,1,2,0)
 ;;=2^99291
 ;;^UTILITY(U,$J,358.3,8006,1,3,0)
 ;;=3^CRITICAL CARE 1ST HR
 ;;^UTILITY(U,$J,358.3,8007,0)
 ;;=99292^^72^602^2^^^^1
 ;;^UTILITY(U,$J,358.3,8007,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8007,1,2,0)
 ;;=2^99292
 ;;^UTILITY(U,$J,358.3,8007,1,3,0)
 ;;=3^CRITICAL CARE ADDL 30 MIN
 ;;^UTILITY(U,$J,358.3,8008,0)
 ;;=99288^^72^602^3^^^^1
 ;;^UTILITY(U,$J,358.3,8008,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8008,1,2,0)
 ;;=2^99288
 ;;^UTILITY(U,$J,358.3,8008,1,3,0)
 ;;=3^DIRECT ADVANCED LIFE SUPPORT
 ;;^UTILITY(U,$J,358.3,8009,0)
 ;;=92950^^72^602^4^^^^1
 ;;^UTILITY(U,$J,358.3,8009,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8009,1,2,0)
 ;;=2^92950
 ;;^UTILITY(U,$J,358.3,8009,1,3,0)
 ;;=3^HEART/LUNG RESUSCITATION CPR
 ;;^UTILITY(U,$J,358.3,8010,0)
 ;;=99144^^72^603^1^^^^1
 ;;^UTILITY(U,$J,358.3,8010,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8010,1,2,0)
 ;;=2^99144
 ;;^UTILITY(U,$J,358.3,8010,1,3,0)
 ;;=3^MOD SEDATION,1ST 30MIN
 ;;^UTILITY(U,$J,358.3,8011,0)
 ;;=99145^^72^603^2^^^^1
 ;;^UTILITY(U,$J,358.3,8011,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8011,1,2,0)
 ;;=2^99145
 ;;^UTILITY(U,$J,358.3,8011,1,3,0)
 ;;=3^MOD SEDATION,EA ADDL 15MIN
 ;;^UTILITY(U,$J,358.3,8012,0)
 ;;=10060^^72^604^1^^^^1
 ;;^UTILITY(U,$J,358.3,8012,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8012,1,2,0)
 ;;=2^10060
 ;;^UTILITY(U,$J,358.3,8012,1,3,0)
 ;;=3^ABSCESS I&D SIMP/SINGLE
 ;;^UTILITY(U,$J,358.3,8013,0)
 ;;=10061^^72^604^2^^^^1
 ;;^UTILITY(U,$J,358.3,8013,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8013,1,2,0)
 ;;=2^10061
 ;;^UTILITY(U,$J,358.3,8013,1,3,0)
 ;;=3^ABSCESS I&D COMP/MULTI
 ;;^UTILITY(U,$J,358.3,8014,0)
 ;;=10080^^72^604^3^^^^1
 ;;^UTILITY(U,$J,358.3,8014,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8014,1,2,0)
 ;;=2^10080
 ;;^UTILITY(U,$J,358.3,8014,1,3,0)
 ;;=3^PILONIDAL CYST,SIMPLE
 ;;^UTILITY(U,$J,358.3,8015,0)
 ;;=10120^^72^604^4^^^^1
 ;;^UTILITY(U,$J,358.3,8015,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8015,1,2,0)
 ;;=2^10120
 ;;^UTILITY(U,$J,358.3,8015,1,3,0)
 ;;=3^REMOVE FB SIMPLE,SKIN
 ;;^UTILITY(U,$J,358.3,8016,0)
 ;;=10121^^72^604^5^^^^1
 ;;^UTILITY(U,$J,358.3,8016,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8016,1,2,0)
 ;;=2^10121
 ;;^UTILITY(U,$J,358.3,8016,1,3,0)
 ;;=3^REMOVE FB COMPLEX
 ;;^UTILITY(U,$J,358.3,8017,0)
 ;;=10140^^72^604^6^^^^1
 ;;^UTILITY(U,$J,358.3,8017,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8017,1,2,0)
 ;;=2^10140
 ;;^UTILITY(U,$J,358.3,8017,1,3,0)
 ;;=3^HEMATOMA/SEROMA,SIMPLE
 ;;^UTILITY(U,$J,358.3,8018,0)
 ;;=10160^^72^604^7^^^^1
 ;;^UTILITY(U,$J,358.3,8018,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8018,1,2,0)
 ;;=2^10160
 ;;^UTILITY(U,$J,358.3,8018,1,3,0)
 ;;=3^ASPIRATE ABSCESS/HEMA/BULLA
 ;;^UTILITY(U,$J,358.3,8019,0)
 ;;=11000^^72^605^1^^^^1
 ;;^UTILITY(U,$J,358.3,8019,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8019,1,2,0)
 ;;=2^11000
 ;;^UTILITY(U,$J,358.3,8019,1,3,0)
 ;;=3^DEBRIDE SKIN UP TO 10%
 ;;^UTILITY(U,$J,358.3,8020,0)
 ;;=11042^^72^605^2^^^^1
 ;;^UTILITY(U,$J,358.3,8020,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8020,1,2,0)
 ;;=2^11042
 ;;^UTILITY(U,$J,358.3,8020,1,3,0)
 ;;=3^DEB SUBQ TISSUE 20 SQ CM/<
 ;;^UTILITY(U,$J,358.3,8021,0)
 ;;=11055^^72^605^3^^^^1
 ;;^UTILITY(U,$J,358.3,8021,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8021,1,2,0)
 ;;=2^11055
 ;;^UTILITY(U,$J,358.3,8021,1,3,0)
 ;;=3^TRIM CORN/CALLOUS,SINGLE
 ;;^UTILITY(U,$J,358.3,8022,0)
 ;;=11056^^72^605^4^^^^1
 ;;^UTILITY(U,$J,358.3,8022,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8022,1,2,0)
 ;;=2^11056
 ;;^UTILITY(U,$J,358.3,8022,1,3,0)
 ;;=3^TRIM CORN/CALLOUS,2-4 LESIONS
 ;;^UTILITY(U,$J,358.3,8023,0)
 ;;=11057^^72^605^5^^^^1
 ;;^UTILITY(U,$J,358.3,8023,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8023,1,2,0)
 ;;=2^11057
 ;;^UTILITY(U,$J,358.3,8023,1,3,0)
 ;;=3^TRIM CORN/CALLOUS,5 OR MORE
 ;;^UTILITY(U,$J,358.3,8024,0)
 ;;=11200^^72^605^6^^^^1
 ;;^UTILITY(U,$J,358.3,8024,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8024,1,2,0)
 ;;=2^11200
 ;;^UTILITY(U,$J,358.3,8024,1,3,0)
 ;;=3^REMOVAL OF SKIN TAGS UP TO 15
 ;;^UTILITY(U,$J,358.3,8025,0)
 ;;=11719^^72^605^7^^^^1
 ;;^UTILITY(U,$J,358.3,8025,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8025,1,2,0)
 ;;=2^11719
 ;;^UTILITY(U,$J,358.3,8025,1,3,0)
 ;;=3^TRIM NAILS (NONDYSTROPHIC)
 ;;^UTILITY(U,$J,358.3,8026,0)
 ;;=11730^^72^605^8^^^^1
 ;;^UTILITY(U,$J,358.3,8026,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8026,1,2,0)
 ;;=2^11730
 ;;^UTILITY(U,$J,358.3,8026,1,3,0)
 ;;=3^REMOVAL OF NAIL PLATE
 ;;^UTILITY(U,$J,358.3,8027,0)
 ;;=11740^^72^605^9^^^^1
 ;;^UTILITY(U,$J,358.3,8027,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8027,1,2,0)
 ;;=2^11740
 ;;^UTILITY(U,$J,358.3,8027,1,3,0)
 ;;=3^DRAIN BLOOD FROM UNDER NAIL
 ;;^UTILITY(U,$J,358.3,8028,0)
 ;;=11750^^72^605^10^^^^1
 ;;^UTILITY(U,$J,358.3,8028,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8028,1,2,0)
 ;;=2^11750
 ;;^UTILITY(U,$J,358.3,8028,1,3,0)
 ;;=3^REMOVAL OF NAIL BED
 ;;^UTILITY(U,$J,358.3,8029,0)
 ;;=11760^^72^605^11^^^^1
 ;;^UTILITY(U,$J,358.3,8029,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8029,1,2,0)
 ;;=2^11760
 ;;^UTILITY(U,$J,358.3,8029,1,3,0)
 ;;=3^REPAIR OF NAIL BED
 ;;^UTILITY(U,$J,358.3,8030,0)
 ;;=20600^^72^606^3^^^^1
 ;;^UTILITY(U,$J,358.3,8030,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8030,1,2,0)
 ;;=2^20600
 ;;^UTILITY(U,$J,358.3,8030,1,3,0)
 ;;=3^ARTHOCENTESIS,FINGERS/TOES
 ;;^UTILITY(U,$J,358.3,8031,0)
 ;;=20610^^72^606^4^^^^1
 ;;^UTILITY(U,$J,358.3,8031,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8031,1,2,0)
 ;;=2^20610
 ;;^UTILITY(U,$J,358.3,8031,1,3,0)
 ;;=3^ARTHOCENTESIS,KNEE/SHLDR/HIP
 ;;^UTILITY(U,$J,358.3,8032,0)
 ;;=20605^^72^606^5^^^^1
 ;;^UTILITY(U,$J,358.3,8032,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8032,1,2,0)
 ;;=2^20605
 ;;^UTILITY(U,$J,358.3,8032,1,3,0)
 ;;=3^ARTHROCENTESIS,WRIST/ELBOW/ANKLE/AC JT
 ;;^UTILITY(U,$J,358.3,8033,0)
 ;;=20612^^72^606^14^^^^1
 ;;^UTILITY(U,$J,358.3,8033,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8033,1,2,0)
 ;;=2^20612
 ;;^UTILITY(U,$J,358.3,8033,1,3,0)
 ;;=3^GANGLION CYST ASPIRATION/INJECTION
