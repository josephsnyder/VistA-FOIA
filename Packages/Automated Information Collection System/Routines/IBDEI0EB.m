IBDEI0EB ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,19120,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19120,1,2,0)
 ;;=2^51798
 ;;^UTILITY(U,$J,358.3,19120,1,3,0)
 ;;=3^US URINE CAPACITY MEASURE
 ;;^UTILITY(U,$J,358.3,19121,0)
 ;;=S9445^^144^1235^10^^^^1
 ;;^UTILITY(U,$J,358.3,19121,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19121,1,2,0)
 ;;=2^S9445
 ;;^UTILITY(U,$J,358.3,19121,1,3,0)
 ;;=3^PT ED,INDIV,PER ENCOUNTER
 ;;^UTILITY(U,$J,358.3,19122,0)
 ;;=2000F^^144^1235^3^^^^1
 ;;^UTILITY(U,$J,358.3,19122,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19122,1,2,0)
 ;;=2^2000F
 ;;^UTILITY(U,$J,358.3,19122,1,3,0)
 ;;=3^BP MEASURE (CAD,CKD,DF,HTN,DM)
 ;;^UTILITY(U,$J,358.3,19123,0)
 ;;=94640^^144^1235^9^^^^1
 ;;^UTILITY(U,$J,358.3,19123,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19123,1,2,0)
 ;;=2^94640
 ;;^UTILITY(U,$J,358.3,19123,1,3,0)
 ;;=3^NEBULIZER-AIRWAY INHAL TREATMENT
 ;;^UTILITY(U,$J,358.3,19124,0)
 ;;=96523^^144^1235^5^^^^1
 ;;^UTILITY(U,$J,358.3,19124,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19124,1,2,0)
 ;;=2^96523
 ;;^UTILITY(U,$J,358.3,19124,1,3,0)
 ;;=3^CATH FLUSH
 ;;^UTILITY(U,$J,358.3,19125,0)
 ;;=11719^^144^1235^14^^^^1
 ;;^UTILITY(U,$J,358.3,19125,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19125,1,2,0)
 ;;=2^11719
 ;;^UTILITY(U,$J,358.3,19125,1,3,0)
 ;;=3^TRIM NAIL(S)-NONDYSTROPHIC
 ;;^UTILITY(U,$J,358.3,19126,0)
 ;;=81002^^144^1235^15^^^^1
 ;;^UTILITY(U,$J,358.3,19126,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19126,1,2,0)
 ;;=2^81002
 ;;^UTILITY(U,$J,358.3,19126,1,3,0)
 ;;=3^UA DIPSTICK
 ;;^UTILITY(U,$J,358.3,19127,0)
 ;;=97602^^144^1235^17^^^^1
 ;;^UTILITY(U,$J,358.3,19127,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19127,1,2,0)
 ;;=2^97602
 ;;^UTILITY(U,$J,358.3,19127,1,3,0)
 ;;=3^WOUND(S) CARE NON-SELECTIVE
 ;;^UTILITY(U,$J,358.3,19128,0)
 ;;=S0630^^144^1235^12^^^^1
 ;;^UTILITY(U,$J,358.3,19128,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19128,1,2,0)
 ;;=2^S0630
 ;;^UTILITY(U,$J,358.3,19128,1,3,0)
 ;;=3^REMOVAL OF SUTURES
 ;;^UTILITY(U,$J,358.3,19129,0)
 ;;=95115^^144^1236^3^^^^1
 ;;^UTILITY(U,$J,358.3,19129,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19129,1,2,0)
 ;;=2^95115
 ;;^UTILITY(U,$J,358.3,19129,1,3,0)
 ;;=3^ALLERGY INJ, PTS OWN MED
 ;;^UTILITY(U,$J,358.3,19130,0)
 ;;=95117^^144^1236^1^^^^1
 ;;^UTILITY(U,$J,358.3,19130,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19130,1,2,0)
 ;;=2^95117
 ;;^UTILITY(U,$J,358.3,19130,1,3,0)
 ;;=3^ALLERGY INJ 2+, PTS OWN MED
 ;;^UTILITY(U,$J,358.3,19131,0)
 ;;=95120^^144^1236^4^^^^1
 ;;^UTILITY(U,$J,358.3,19131,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19131,1,2,0)
 ;;=2^95120
 ;;^UTILITY(U,$J,358.3,19131,1,3,0)
 ;;=3^ALLERGY INJ, WE PROVIDE MED
 ;;^UTILITY(U,$J,358.3,19132,0)
 ;;=95125^^144^1236^2^^^^1
 ;;^UTILITY(U,$J,358.3,19132,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19132,1,2,0)
 ;;=2^95125
 ;;^UTILITY(U,$J,358.3,19132,1,3,0)
 ;;=3^ALLERGY INJ 2+, WE PROVIDE MED
 ;;^UTILITY(U,$J,358.3,19133,0)
 ;;=96372^^144^1237^1^^^^1
 ;;^UTILITY(U,$J,358.3,19133,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19133,1,2,0)
 ;;=2^96372
 ;;^UTILITY(U,$J,358.3,19133,1,3,0)
 ;;=3^INJECTION ADMIN, SC/IM
 ;;^UTILITY(U,$J,358.3,19134,0)
 ;;=90471^^144^1238^3^^^^1
 ;;^UTILITY(U,$J,358.3,19134,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19134,1,2,0)
 ;;=2^90471
 ;;^UTILITY(U,$J,358.3,19134,1,3,0)
 ;;=3^IMMUNIZATION ADMIN,1 INJECTION
 ;;^UTILITY(U,$J,358.3,19135,0)
 ;;=90472^^144^1238^2^^^^1
 ;;^UTILITY(U,$J,358.3,19135,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19135,1,2,0)
 ;;=2^90472
 ;;^UTILITY(U,$J,358.3,19135,1,3,0)
 ;;=3^IMMUN ADMIN, EACH ADDITIONAL
 ;;^UTILITY(U,$J,358.3,19136,0)
 ;;=G9142^^144^1238^1^^^^1
 ;;^UTILITY(U,$J,358.3,19136,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19136,1,2,0)
 ;;=2^G9142
 ;;^UTILITY(U,$J,358.3,19136,1,3,0)
 ;;=3^ADMIN OF H1N1 VACCINE
 ;;^UTILITY(U,$J,358.3,19137,0)
 ;;=51102^^144^1239^1^^^^1
 ;;^UTILITY(U,$J,358.3,19137,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19137,1,2,0)
 ;;=2^51102
 ;;^UTILITY(U,$J,358.3,19137,1,3,0)
 ;;=3^DRAIN BL W/CATH INSERTION
 ;;^UTILITY(U,$J,358.3,19138,0)
 ;;=J1645^^144^1240^3^^^^1
 ;;^UTILITY(U,$J,358.3,19138,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19138,1,2,0)
 ;;=2^J1645
 ;;^UTILITY(U,$J,358.3,19138,1,3,0)
 ;;=3^HEPARIN (FOR FLUSHING)
 ;;^UTILITY(U,$J,358.3,19139,0)
 ;;=36593^^144^1240^2^^^^1
 ;;^UTILITY(U,$J,358.3,19139,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19139,1,2,0)
 ;;=2^36593
 ;;^UTILITY(U,$J,358.3,19139,1,3,0)
 ;;=3^DECLOT VASCULAR DEVICE
 ;;^UTILITY(U,$J,358.3,19140,0)
 ;;=96523^^144^1240^1^^^^1
 ;;^UTILITY(U,$J,358.3,19140,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19140,1,2,0)
 ;;=2^96523
 ;;^UTILITY(U,$J,358.3,19140,1,3,0)
 ;;=3^CATH FLUSH
 ;;^UTILITY(U,$J,358.3,19141,0)
 ;;=94150^^144^1241^1^^^^1
 ;;^UTILITY(U,$J,358.3,19141,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19141,1,2,0)
 ;;=2^94150
 ;;^UTILITY(U,$J,358.3,19141,1,3,0)
 ;;=3^PFT TEST
 ;;^UTILITY(U,$J,358.3,19142,0)
 ;;=93005^^144^1242^1^^^^1
 ;;^UTILITY(U,$J,358.3,19142,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19142,1,2,0)
 ;;=2^93005
 ;;^UTILITY(U,$J,358.3,19142,1,3,0)
 ;;=3^EKG, TRACING ONLY
 ;;^UTILITY(U,$J,358.3,19143,0)
 ;;=93225^^144^1242^2^^^^1
 ;;^UTILITY(U,$J,358.3,19143,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19143,1,2,0)
 ;;=2^93225
 ;;^UTILITY(U,$J,358.3,19143,1,3,0)
 ;;=3^HOLTER MONITOR PLACEMENT/REMVL
 ;;^UTILITY(U,$J,358.3,19144,0)
 ;;=90732^^144^1243^29^^^^1
 ;;^UTILITY(U,$J,358.3,19144,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19144,1,2,0)
 ;;=2^90732
 ;;^UTILITY(U,$J,358.3,19144,1,3,0)
 ;;=3^PNEUMOCOCCAL VACCINE
 ;;^UTILITY(U,$J,358.3,19145,0)
 ;;=90658^^144^1243^10^^^^1
 ;;^UTILITY(U,$J,358.3,19145,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19145,1,2,0)
 ;;=2^90658
 ;;^UTILITY(U,$J,358.3,19145,1,3,0)
 ;;=3^FLU VACCINE, IM
 ;;^UTILITY(U,$J,358.3,19146,0)
 ;;=90585^^144^1243^1^^^^1
 ;;^UTILITY(U,$J,358.3,19146,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19146,1,2,0)
 ;;=2^90585
 ;;^UTILITY(U,$J,358.3,19146,1,3,0)
 ;;=3^BCG VACCINE, PERCUT
 ;;^UTILITY(U,$J,358.3,19147,0)
 ;;=90632^^144^1243^12^^^^1
 ;;^UTILITY(U,$J,358.3,19147,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19147,1,2,0)
 ;;=2^90632
 ;;^UTILITY(U,$J,358.3,19147,1,3,0)
 ;;=3^HEP A VACCINE, ADULT IM
 ;;^UTILITY(U,$J,358.3,19148,0)
 ;;=90636^^144^1243^13^^^^1
 ;;^UTILITY(U,$J,358.3,19148,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19148,1,2,0)
 ;;=2^90636
 ;;^UTILITY(U,$J,358.3,19148,1,3,0)
 ;;=3^HEP A/HEP B VACC, ADULT IM
 ;;^UTILITY(U,$J,358.3,19149,0)
 ;;=90645^^144^1243^16^^^^1
 ;;^UTILITY(U,$J,358.3,19149,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19149,1,2,0)
 ;;=2^90645
 ;;^UTILITY(U,$J,358.3,19149,1,3,0)
 ;;=3^HIB VACCINE, HBOC, IM
 ;;^UTILITY(U,$J,358.3,19150,0)
 ;;=90646^^144^1243^17^^^^1
 ;;^UTILITY(U,$J,358.3,19150,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19150,1,2,0)
 ;;=2^90646
 ;;^UTILITY(U,$J,358.3,19150,1,3,0)
 ;;=3^HIB VACCINE, PRP-D, IM
 ;;^UTILITY(U,$J,358.3,19151,0)
 ;;=90647^^144^1243^18^^^^1
 ;;^UTILITY(U,$J,358.3,19151,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19151,1,2,0)
 ;;=2^90647
 ;;^UTILITY(U,$J,358.3,19151,1,3,0)
 ;;=3^HIB VACCINE, PRP-OMP, IM
 ;;^UTILITY(U,$J,358.3,19152,0)
 ;;=90648^^144^1243^19^^^^1
 ;;^UTILITY(U,$J,358.3,19152,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19152,1,2,0)
 ;;=2^90648
 ;;^UTILITY(U,$J,358.3,19152,1,3,0)
 ;;=3^HIB VACCINE, PRP-T, IM
 ;;^UTILITY(U,$J,358.3,19153,0)
 ;;=90649^^144^1243^11^^^^1
 ;;^UTILITY(U,$J,358.3,19153,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19153,1,2,0)
 ;;=2^90649
 ;;^UTILITY(U,$J,358.3,19153,1,3,0)
 ;;=3^H PAPILLOMA VACC 3 DOSE IM
 ;;^UTILITY(U,$J,358.3,19154,0)
 ;;=90675^^144^1243^32^^^^1
 ;;^UTILITY(U,$J,358.3,19154,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19154,1,2,0)
 ;;=2^90675
 ;;^UTILITY(U,$J,358.3,19154,1,3,0)
 ;;=3^RABIES VACCINE, IM
 ;;^UTILITY(U,$J,358.3,19155,0)
 ;;=90676^^144^1243^31^^^^1
 ;;^UTILITY(U,$J,358.3,19155,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19155,1,2,0)
 ;;=2^90676
 ;;^UTILITY(U,$J,358.3,19155,1,3,0)
 ;;=3^RABIES VACCINE, ID
 ;;^UTILITY(U,$J,358.3,19156,0)
 ;;=90680^^144^1243^33^^^^1
 ;;^UTILITY(U,$J,358.3,19156,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19156,1,2,0)
 ;;=2^90680
 ;;^UTILITY(U,$J,358.3,19156,1,3,0)
 ;;=3^ROTOVIRUS VACC 3 DOSE, ORAL
 ;;^UTILITY(U,$J,358.3,19157,0)
 ;;=90698^^144^1243^5^^^^1
 ;;^UTILITY(U,$J,358.3,19157,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19157,1,2,0)
 ;;=2^90698
 ;;^UTILITY(U,$J,358.3,19157,1,3,0)
 ;;=3^DTAP-HIB-IP VACCINE, IM
 ;;^UTILITY(U,$J,358.3,19158,0)
 ;;=90703^^144^1243^38^^^^1
 ;;^UTILITY(U,$J,358.3,19158,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19158,1,2,0)
 ;;=2^90703
 ;;^UTILITY(U,$J,358.3,19158,1,3,0)
 ;;=3^TETANUS VACCINE, IM
 ;;^UTILITY(U,$J,358.3,19159,0)
 ;;=90704^^144^1243^27^^^^1
 ;;^UTILITY(U,$J,358.3,19159,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19159,1,2,0)
 ;;=2^90704
 ;;^UTILITY(U,$J,358.3,19159,1,3,0)
 ;;=3^MUMPS VACCINE, SC
 ;;^UTILITY(U,$J,358.3,19160,0)
 ;;=90705^^144^1243^21^^^^1
 ;;^UTILITY(U,$J,358.3,19160,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19160,1,2,0)
 ;;=2^90705
 ;;^UTILITY(U,$J,358.3,19160,1,3,0)
 ;;=3^MEASLES VACCINE, SC
 ;;^UTILITY(U,$J,358.3,19161,0)
 ;;=90706^^144^1243^34^^^^1
 ;;^UTILITY(U,$J,358.3,19161,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19161,1,2,0)
 ;;=2^90706
 ;;^UTILITY(U,$J,358.3,19161,1,3,0)
 ;;=3^RUBELLA VACCINE, SC
 ;;^UTILITY(U,$J,358.3,19162,0)
 ;;=90707^^144^1243^25^^^^1
 ;;^UTILITY(U,$J,358.3,19162,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19162,1,2,0)
 ;;=2^90707
 ;;^UTILITY(U,$J,358.3,19162,1,3,0)
 ;;=3^MMR VACCINE, SC
 ;;^UTILITY(U,$J,358.3,19163,0)
 ;;=90708^^144^1243^22^^^^1
 ;;^UTILITY(U,$J,358.3,19163,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19163,1,2,0)
 ;;=2^90708
 ;;^UTILITY(U,$J,358.3,19163,1,3,0)
 ;;=3^MEASLES-RUBELLA VACCINE, SC
