IBDEI02M ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,3007,1,3,0)
 ;;=3^Joint Pain,Hip
 ;;^UTILITY(U,$J,358.3,3007,1,4,0)
 ;;=4^719.45
 ;;^UTILITY(U,$J,358.3,3007,2)
 ;;=^272402
 ;;^UTILITY(U,$J,358.3,3008,0)
 ;;=719.46^^34^236^47
 ;;^UTILITY(U,$J,358.3,3008,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3008,1,3,0)
 ;;=3^Joint Pain,Knee
 ;;^UTILITY(U,$J,358.3,3008,1,4,0)
 ;;=4^719.46
 ;;^UTILITY(U,$J,358.3,3008,2)
 ;;=^272403
 ;;^UTILITY(U,$J,358.3,3009,0)
 ;;=719.47^^34^236^43
 ;;^UTILITY(U,$J,358.3,3009,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3009,1,3,0)
 ;;=3^Joint Pain,Foot/Ankle
 ;;^UTILITY(U,$J,358.3,3009,1,4,0)
 ;;=4^719.47
 ;;^UTILITY(U,$J,358.3,3009,2)
 ;;=^272404
 ;;^UTILITY(U,$J,358.3,3010,0)
 ;;=721.1^^34^236^16
 ;;^UTILITY(U,$J,358.3,3010,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3010,1,3,0)
 ;;=3^Cerv Spondylosis w/o Myelopathy
 ;;^UTILITY(U,$J,358.3,3010,1,4,0)
 ;;=4^721.1
 ;;^UTILITY(U,$J,358.3,3010,2)
 ;;=^272453
 ;;^UTILITY(U,$J,358.3,3011,0)
 ;;=719.48^^34^236^48
 ;;^UTILITY(U,$J,358.3,3011,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3011,1,3,0)
 ;;=3^Joint Pain,Multiple
 ;;^UTILITY(U,$J,358.3,3011,1,4,0)
 ;;=4^719.48
 ;;^UTILITY(U,$J,358.3,3011,2)
 ;;=^272405
 ;;^UTILITY(U,$J,358.3,3012,0)
 ;;=719.51^^34^236^57
 ;;^UTILITY(U,$J,358.3,3012,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3012,1,3,0)
 ;;=3^Joint Stiffness,Shoulder
 ;;^UTILITY(U,$J,358.3,3012,1,4,0)
 ;;=4^719.51
 ;;^UTILITY(U,$J,358.3,3012,2)
 ;;=^272407
 ;;^UTILITY(U,$J,358.3,3013,0)
 ;;=719.52^^34^236^58
 ;;^UTILITY(U,$J,358.3,3013,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3013,1,3,0)
 ;;=3^Joint Stiffness,Upper Arm
 ;;^UTILITY(U,$J,358.3,3013,1,4,0)
 ;;=4^719.52
 ;;^UTILITY(U,$J,358.3,3013,2)
 ;;=^272408
 ;;^UTILITY(U,$J,358.3,3014,0)
 ;;=719.53^^34^236^52
 ;;^UTILITY(U,$J,358.3,3014,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3014,1,3,0)
 ;;=3^Joint Stiffness,Forearm
 ;;^UTILITY(U,$J,358.3,3014,1,4,0)
 ;;=4^719.53
 ;;^UTILITY(U,$J,358.3,3014,2)
 ;;=^272409
 ;;^UTILITY(U,$J,358.3,3015,0)
 ;;=719.54^^34^236^53
 ;;^UTILITY(U,$J,358.3,3015,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3015,1,3,0)
 ;;=3^Joint Stiffness,Hand
 ;;^UTILITY(U,$J,358.3,3015,1,4,0)
 ;;=4^719.54
 ;;^UTILITY(U,$J,358.3,3015,2)
 ;;=^272410
 ;;^UTILITY(U,$J,358.3,3016,0)
 ;;=719.55^^34^236^54
 ;;^UTILITY(U,$J,358.3,3016,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3016,1,3,0)
 ;;=3^Joint Stiffness,Hip
 ;;^UTILITY(U,$J,358.3,3016,1,4,0)
 ;;=4^719.55
 ;;^UTILITY(U,$J,358.3,3016,2)
 ;;=^272411
 ;;^UTILITY(U,$J,358.3,3017,0)
 ;;=719.56^^34^236^55
 ;;^UTILITY(U,$J,358.3,3017,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3017,1,3,0)
 ;;=3^Joint Stiffness,Knee
 ;;^UTILITY(U,$J,358.3,3017,1,4,0)
 ;;=4^719.56
 ;;^UTILITY(U,$J,358.3,3017,2)
 ;;=^272412
 ;;^UTILITY(U,$J,358.3,3018,0)
 ;;=719.57^^34^236^51
 ;;^UTILITY(U,$J,358.3,3018,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3018,1,3,0)
 ;;=3^Joint Stiffness,Ankle/Foot
 ;;^UTILITY(U,$J,358.3,3018,1,4,0)
 ;;=4^719.57
 ;;^UTILITY(U,$J,358.3,3018,2)
 ;;=^272413
 ;;^UTILITY(U,$J,358.3,3019,0)
 ;;=719.58^^34^236^56
 ;;^UTILITY(U,$J,358.3,3019,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3019,1,3,0)
 ;;=3^Joint Stiffness,Multiple Joints
 ;;^UTILITY(U,$J,358.3,3019,1,4,0)
 ;;=4^719.58
 ;;^UTILITY(U,$J,358.3,3019,2)
 ;;=^272414
 ;;^UTILITY(U,$J,358.3,3020,0)
 ;;=720.0^^34^236^4
 ;;^UTILITY(U,$J,358.3,3020,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3020,1,3,0)
 ;;=3^Ankylosing Spondylitis
 ;;^UTILITY(U,$J,358.3,3020,1,4,0)
 ;;=4^720.0
 ;;^UTILITY(U,$J,358.3,3020,2)
 ;;=^113484
 ;;^UTILITY(U,$J,358.3,3021,0)
 ;;=720.2^^34^236^108
 ;;^UTILITY(U,$J,358.3,3021,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3021,1,3,0)
 ;;=3^Sacroilitis NEC
 ;;^UTILITY(U,$J,358.3,3021,1,4,0)
 ;;=4^720.2
 ;;^UTILITY(U,$J,358.3,3021,2)
 ;;=^259118
 ;;^UTILITY(U,$J,358.3,3022,0)
 ;;=V72.0^^35^237^2
 ;;^UTILITY(U,$J,358.3,3022,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3022,1,3,0)
 ;;=3^EYE VISION EXAMINATION
 ;;^UTILITY(U,$J,358.3,3022,1,4,0)
 ;;=4^V72.0
 ;;^UTILITY(U,$J,358.3,3022,2)
 ;;=^43432
 ;;^UTILITY(U,$J,358.3,3023,0)
 ;;=V72.11^^35^237^4
 ;;^UTILITY(U,$J,358.3,3023,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3023,1,3,0)
 ;;=3^HEARING EXAM-FAIL SCREEN
 ;;^UTILITY(U,$J,358.3,3023,1,4,0)
 ;;=4^V72.11
 ;;^UTILITY(U,$J,358.3,3023,2)
 ;;=^334218
 ;;^UTILITY(U,$J,358.3,3024,0)
 ;;=V72.19^^35^237^1
 ;;^UTILITY(U,$J,358.3,3024,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3024,1,3,0)
 ;;=3^EXAM EARS & HEARING NEC
 ;;^UTILITY(U,$J,358.3,3024,1,4,0)
 ;;=4^V72.19
 ;;^UTILITY(U,$J,358.3,3024,2)
 ;;=^334219
 ;;^UTILITY(U,$J,358.3,3025,0)
 ;;=V72.7^^35^237^71
 ;;^UTILITY(U,$J,358.3,3025,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3025,1,3,0)
 ;;=3^SKIN/SENSITIZATION TESTS
 ;;^UTILITY(U,$J,358.3,3025,1,4,0)
 ;;=4^V72.7
 ;;^UTILITY(U,$J,358.3,3025,2)
 ;;=^295616
 ;;^UTILITY(U,$J,358.3,3026,0)
 ;;=V73.5^^35^237^66
 ;;^UTILITY(U,$J,358.3,3026,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3026,1,3,0)
 ;;=3^SCREENING-ARBOVIRUS DIS
 ;;^UTILITY(U,$J,358.3,3026,1,4,0)
 ;;=4^V73.5
 ;;^UTILITY(U,$J,358.3,3026,2)
 ;;=^295629
 ;;^UTILITY(U,$J,358.3,3027,0)
 ;;=V73.81^^35^237^83
 ;;^UTILITY(U,$J,358.3,3027,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3027,1,3,0)
 ;;=3^SPECIAL SCREEN EXAM HPV
 ;;^UTILITY(U,$J,358.3,3027,1,4,0)
 ;;=4^V73.81
 ;;^UTILITY(U,$J,358.3,3027,2)
 ;;=^335323
 ;;^UTILITY(U,$J,358.3,3028,0)
 ;;=V73.88^^35^237^75
 ;;^UTILITY(U,$J,358.3,3028,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3028,1,3,0)
 ;;=3^SP.SCR EXAM, OTH CHLA
 ;;^UTILITY(U,$J,358.3,3028,1,4,0)
 ;;=4^V73.88
 ;;^UTILITY(U,$J,358.3,3028,2)
 ;;=^295832
 ;;^UTILITY(U,$J,358.3,3029,0)
 ;;=V73.89^^35^237^72
 ;;^UTILITY(U,$J,358.3,3029,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3029,1,3,0)
 ;;=3^SP SCR EXAM, OTH SP VIR
 ;;^UTILITY(U,$J,358.3,3029,1,4,0)
 ;;=4^V73.89
 ;;^UTILITY(U,$J,358.3,3029,2)
 ;;=^295833
 ;;^UTILITY(U,$J,358.3,3030,0)
 ;;=V74.0^^35^237^55
 ;;^UTILITY(U,$J,358.3,3030,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3030,1,3,0)
 ;;=3^SCREENING FOR CHOLERA
 ;;^UTILITY(U,$J,358.3,3030,1,4,0)
 ;;=4^V74.0
 ;;^UTILITY(U,$J,358.3,3030,2)
 ;;=^295633
 ;;^UTILITY(U,$J,358.3,3031,0)
 ;;=V74.1^^35^237^68
 ;;^UTILITY(U,$J,358.3,3031,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3031,1,3,0)
 ;;=3^SCREENING-PULMONARY TB
 ;;^UTILITY(U,$J,358.3,3031,1,4,0)
 ;;=4^V74.1
 ;;^UTILITY(U,$J,358.3,3031,2)
 ;;=^108715
 ;;^UTILITY(U,$J,358.3,3032,0)
 ;;=V74.2^^35^237^61
 ;;^UTILITY(U,$J,358.3,3032,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3032,1,3,0)
 ;;=3^SCREENING FOR LEPROSY
 ;;^UTILITY(U,$J,358.3,3032,1,4,0)
 ;;=4^V74.2
 ;;^UTILITY(U,$J,358.3,3032,2)
 ;;=^295634
 ;;^UTILITY(U,$J,358.3,3033,0)
 ;;=V74.3^^35^237^57
 ;;^UTILITY(U,$J,358.3,3033,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3033,1,3,0)
 ;;=3^SCREENING FOR DIPHTHERIA
 ;;^UTILITY(U,$J,358.3,3033,1,4,0)
 ;;=4^V74.3
 ;;^UTILITY(U,$J,358.3,3033,2)
 ;;=^295635
 ;;^UTILITY(U,$J,358.3,3034,0)
 ;;=V74.4^^35^237^28
 ;;^UTILITY(U,$J,358.3,3034,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3034,1,3,0)
 ;;=3^SCREEN-BACT CONJUNCTIVIT
 ;;^UTILITY(U,$J,358.3,3034,1,4,0)
 ;;=4^V74.4
 ;;^UTILITY(U,$J,358.3,3034,2)
 ;;=^295636
 ;;^UTILITY(U,$J,358.3,3035,0)
 ;;=V74.5^^35^237^19
 ;;^UTILITY(U,$J,358.3,3035,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3035,1,3,0)
 ;;=3^SCREEN FOR VENERAL DIS
 ;;^UTILITY(U,$J,358.3,3035,1,4,0)
 ;;=4^V74.5
 ;;^UTILITY(U,$J,358.3,3035,2)
 ;;=^295637
 ;;^UTILITY(U,$J,358.3,3036,0)
 ;;=V74.6^^35^237^64
 ;;^UTILITY(U,$J,358.3,3036,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3036,1,3,0)
 ;;=3^SCREENING FOR YAWS
 ;;^UTILITY(U,$J,358.3,3036,1,4,0)
 ;;=4^V74.6
 ;;^UTILITY(U,$J,358.3,3036,2)
 ;;=^295638
 ;;^UTILITY(U,$J,358.3,3037,0)
 ;;=V74.8^^35^237^29
 ;;^UTILITY(U,$J,358.3,3037,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3037,1,3,0)
 ;;=3^SCREEN-BACTERIAL DIS NEC
 ;;^UTILITY(U,$J,358.3,3037,1,4,0)
 ;;=4^V74.8
 ;;^UTILITY(U,$J,358.3,3037,2)
 ;;=^295639
 ;;^UTILITY(U,$J,358.3,3038,0)
 ;;=V75.0^^35^237^48
 ;;^UTILITY(U,$J,358.3,3038,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3038,1,3,0)
 ;;=3^SCREEN-RICKETTSIAL DIS
 ;;^UTILITY(U,$J,358.3,3038,1,4,0)
 ;;=4^V75.0
 ;;^UTILITY(U,$J,358.3,3038,2)
 ;;=^295641
 ;;^UTILITY(U,$J,358.3,3039,0)
 ;;=V75.1^^35^237^62
 ;;^UTILITY(U,$J,358.3,3039,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3039,1,3,0)
 ;;=3^SCREENING FOR MALARIA
 ;;^UTILITY(U,$J,358.3,3039,1,4,0)
 ;;=4^V75.1
 ;;^UTILITY(U,$J,358.3,3039,2)
 ;;=^295642
 ;;^UTILITY(U,$J,358.3,3040,0)
 ;;=V75.2^^35^237^15
 ;;^UTILITY(U,$J,358.3,3040,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3040,1,3,0)
 ;;=3^SCREEN FOR LEISHMANIASIS
 ;;^UTILITY(U,$J,358.3,3040,1,4,0)
 ;;=4^V75.2
 ;;^UTILITY(U,$J,358.3,3040,2)
 ;;=^295643
 ;;^UTILITY(U,$J,358.3,3041,0)
 ;;=V75.3^^35^237^53
 ;;^UTILITY(U,$J,358.3,3041,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3041,1,3,0)
 ;;=3^SCREEN-TRYPANOSOMIASIS
 ;;^UTILITY(U,$J,358.3,3041,1,4,0)
 ;;=4^V75.3
 ;;^UTILITY(U,$J,358.3,3041,2)
 ;;=^295644
 ;;^UTILITY(U,$J,358.3,3042,0)
 ;;=V75.4^^35^237^41
 ;;^UTILITY(U,$J,358.3,3042,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3042,1,3,0)
 ;;=3^SCREEN-MYCOTIC INFECT
 ;;^UTILITY(U,$J,358.3,3042,1,4,0)
 ;;=4^V75.4
 ;;^UTILITY(U,$J,358.3,3042,2)
 ;;=^295645
 ;;^UTILITY(U,$J,358.3,3043,0)
 ;;=V75.5^^35^237^49
 ;;^UTILITY(U,$J,358.3,3043,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3043,1,3,0)
 ;;=3^SCREEN-SCHISTOSOMIASIS
 ;;^UTILITY(U,$J,358.3,3043,1,4,0)
 ;;=4^V75.5
 ;;^UTILITY(U,$J,358.3,3043,2)
 ;;=^295646
 ;;^UTILITY(U,$J,358.3,3044,0)
 ;;=V75.6^^35^237^10
 ;;^UTILITY(U,$J,358.3,3044,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3044,1,3,0)
 ;;=3^SCREEN FOR FILARIASIS
 ;;^UTILITY(U,$J,358.3,3044,1,4,0)
 ;;=4^V75.6
 ;;^UTILITY(U,$J,358.3,3044,2)
 ;;=^295647
 ;;^UTILITY(U,$J,358.3,3045,0)
 ;;=V75.7^^35^237^13
 ;;^UTILITY(U,$J,358.3,3045,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3045,1,3,0)
 ;;=3^SCREEN FOR HELMINTHIASIS
