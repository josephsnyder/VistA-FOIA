IBDEI03G ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,4155,1,2,0)
 ;;=2^36561
 ;;^UTILITY(U,$J,358.3,4155,1,3,0)
 ;;=3^INSERT TUNNELED CV CATH W/PORT
 ;;^UTILITY(U,$J,358.3,4156,0)
 ;;=36620^^41^274^7^^^^1
 ;;^UTILITY(U,$J,358.3,4156,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4156,1,2,0)
 ;;=2^36620
 ;;^UTILITY(U,$J,358.3,4156,1,3,0)
 ;;=3^INS ART LINE/MONITOR,PERC
 ;;^UTILITY(U,$J,358.3,4157,0)
 ;;=36625^^41^274^6^^^^1
 ;;^UTILITY(U,$J,358.3,4157,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4157,1,2,0)
 ;;=2^36625
 ;;^UTILITY(U,$J,358.3,4157,1,3,0)
 ;;=3^INS ART LINE/MONITOR,CUTDOWN
 ;;^UTILITY(U,$J,358.3,4158,0)
 ;;=20600^^41^274^3^^^^1
 ;;^UTILITY(U,$J,358.3,4158,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4158,1,2,0)
 ;;=2^20600
 ;;^UTILITY(U,$J,358.3,4158,1,3,0)
 ;;=3^ARTHROCENTESIS,SM JOINT
 ;;^UTILITY(U,$J,358.3,4159,0)
 ;;=20605^^41^274^2^^^^1
 ;;^UTILITY(U,$J,358.3,4159,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4159,1,2,0)
 ;;=2^20605
 ;;^UTILITY(U,$J,358.3,4159,1,3,0)
 ;;=3^ARTHROCENTESIS,MED JOINT
 ;;^UTILITY(U,$J,358.3,4160,0)
 ;;=20610^^41^274^1^^^^1
 ;;^UTILITY(U,$J,358.3,4160,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4160,1,2,0)
 ;;=2^20610
 ;;^UTILITY(U,$J,358.3,4160,1,3,0)
 ;;=3^ARTHROCENTESIS,LG JOINT
 ;;^UTILITY(U,$J,358.3,4161,0)
 ;;=62270^^41^274^16^^^^1
 ;;^UTILITY(U,$J,358.3,4161,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4161,1,2,0)
 ;;=2^62270
 ;;^UTILITY(U,$J,358.3,4161,1,3,0)
 ;;=3^SPINAL FLUID TAP DIAGNOSTIC
 ;;^UTILITY(U,$J,358.3,4162,0)
 ;;=33010^^41^274^12^^^^1
 ;;^UTILITY(U,$J,358.3,4162,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4162,1,2,0)
 ;;=2^33010
 ;;^UTILITY(U,$J,358.3,4162,1,3,0)
 ;;=3^PARICARDIOCENTESIS,INIT
 ;;^UTILITY(U,$J,358.3,4163,0)
 ;;=33011^^41^274^13^^^^1
 ;;^UTILITY(U,$J,358.3,4163,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4163,1,2,0)
 ;;=2^33011
 ;;^UTILITY(U,$J,358.3,4163,1,3,0)
 ;;=3^PERICARDIOCENTESIS,SUBSEQ
 ;;^UTILITY(U,$J,358.3,4164,0)
 ;;=76930^^41^274^5^^^^1
 ;;^UTILITY(U,$J,358.3,4164,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4164,1,2,0)
 ;;=2^76930
 ;;^UTILITY(U,$J,358.3,4164,1,3,0)
 ;;=3^ECHO GUIDE CARDIOCENTESIS
 ;;^UTILITY(U,$J,358.3,4165,0)
 ;;=36013^^41^274^4^^^^1
 ;;^UTILITY(U,$J,358.3,4165,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4165,1,2,0)
 ;;=2^36013
 ;;^UTILITY(U,$J,358.3,4165,1,3,0)
 ;;=3^CATH,RT HRT/MAIN PULM ART
 ;;^UTILITY(U,$J,358.3,4166,0)
 ;;=36014^^41^274^15^^^^1
 ;;^UTILITY(U,$J,358.3,4166,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4166,1,2,0)
 ;;=2^36014
 ;;^UTILITY(U,$J,358.3,4166,1,3,0)
 ;;=3^SELECTIVE CATH PULM ART
 ;;^UTILITY(U,$J,358.3,4167,0)
 ;;=36015^^41^274^14^^^^1
 ;;^UTILITY(U,$J,358.3,4167,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4167,1,2,0)
 ;;=2^36015
 ;;^UTILITY(U,$J,358.3,4167,1,3,0)
 ;;=3^SELECT CATH,SEGMENTAL,PULM
 ;;^UTILITY(U,$J,358.3,4168,0)
 ;;=36600^^41^275^2^^^^1
 ;;^UTILITY(U,$J,358.3,4168,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4168,1,2,0)
 ;;=2^36600
 ;;^UTILITY(U,$J,358.3,4168,1,3,0)
 ;;=3^ABG
 ;;^UTILITY(U,$J,358.3,4169,0)
 ;;=36620^^41^275^1^^^^1
 ;;^UTILITY(U,$J,358.3,4169,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4169,1,2,0)
 ;;=2^36620
 ;;^UTILITY(U,$J,358.3,4169,1,3,0)
 ;;=3^A-LINE
 ;;^UTILITY(U,$J,358.3,4170,0)
 ;;=36415^^41^275^7^^^^1
 ;;^UTILITY(U,$J,358.3,4170,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4170,1,2,0)
 ;;=2^36415
 ;;^UTILITY(U,$J,358.3,4170,1,3,0)
 ;;=3^VENOUS PUNCTURE
 ;;^UTILITY(U,$J,358.3,4171,0)
 ;;=36000^^41^275^6^^^^1
 ;;^UTILITY(U,$J,358.3,4171,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4171,1,2,0)
 ;;=2^36000
 ;;^UTILITY(U,$J,358.3,4171,1,3,0)
 ;;=3^HEP LOCK W/O IV INFUSION
 ;;^UTILITY(U,$J,358.3,4172,0)
 ;;=82803^^41^275^3^^^^1
 ;;^UTILITY(U,$J,358.3,4172,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4172,1,2,0)
 ;;=2^82803
 ;;^UTILITY(U,$J,358.3,4172,1,3,0)
 ;;=3^ABG ANALYZED,pO2,pCO2,HCO3
 ;;^UTILITY(U,$J,358.3,4173,0)
 ;;=82805^^41^275^4^^^^1
 ;;^UTILITY(U,$J,358.3,4173,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4173,1,2,0)
 ;;=2^82805
 ;;^UTILITY(U,$J,358.3,4173,1,3,0)
 ;;=3^ARTERIAL O2 SATURATION
 ;;^UTILITY(U,$J,358.3,4174,0)
 ;;=82375^^41^275^5^^^^1
 ;;^UTILITY(U,$J,358.3,4174,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4174,1,2,0)
 ;;=2^82375
 ;;^UTILITY(U,$J,358.3,4174,1,3,0)
 ;;=3^BLOOD GAS, CO ANALYSIS
 ;;^UTILITY(U,$J,358.3,4175,0)
 ;;=31622^^41^276^1^^^^1
 ;;^UTILITY(U,$J,358.3,4175,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4175,1,2,0)
 ;;=2^31622
 ;;^UTILITY(U,$J,358.3,4175,1,3,0)
 ;;=3^BRONCHOSCOPY DIAG W/WASH
 ;;^UTILITY(U,$J,358.3,4176,0)
 ;;=31623^^41^276^3^^^^1
 ;;^UTILITY(U,$J,358.3,4176,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4176,1,2,0)
 ;;=2^31623
 ;;^UTILITY(U,$J,358.3,4176,1,3,0)
 ;;=3^BRONCHOSCOPY W/BRUSHINGS
 ;;^UTILITY(U,$J,358.3,4177,0)
 ;;=31624^^41^276^5^^^^1
 ;;^UTILITY(U,$J,358.3,4177,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4177,1,2,0)
 ;;=2^31624
 ;;^UTILITY(U,$J,358.3,4177,1,3,0)
 ;;=3^BRONCHOSCOPY W/LAVAGE
 ;;^UTILITY(U,$J,358.3,4178,0)
 ;;=31625^^41^276^2^^^^1
 ;;^UTILITY(U,$J,358.3,4178,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4178,1,2,0)
 ;;=2^31625
 ;;^UTILITY(U,$J,358.3,4178,1,3,0)
 ;;=3^BRONCHOSCOPY W/BIOPSY(S)
 ;;^UTILITY(U,$J,358.3,4179,0)
 ;;=31628^^41^276^6^^^^1
 ;;^UTILITY(U,$J,358.3,4179,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4179,1,2,0)
 ;;=2^31628
 ;;^UTILITY(U,$J,358.3,4179,1,3,0)
 ;;=3^BRONCHOSCOPY W/TRANSBRONCH BX
 ;;^UTILITY(U,$J,358.3,4180,0)
 ;;=31629^^41^276^7^^^^1
 ;;^UTILITY(U,$J,358.3,4180,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4180,1,2,0)
 ;;=2^31629
 ;;^UTILITY(U,$J,358.3,4180,1,3,0)
 ;;=3^BRONCHOSCOPY W/TRANSBRONCH NEEDLE ASPIR
 ;;^UTILITY(U,$J,358.3,4181,0)
 ;;=31635^^41^276^4^^^^1
 ;;^UTILITY(U,$J,358.3,4181,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4181,1,2,0)
 ;;=2^31635
 ;;^UTILITY(U,$J,358.3,4181,1,3,0)
 ;;=3^BRONCHOSCOPY W/FB REMOVAL
 ;;^UTILITY(U,$J,358.3,4182,0)
 ;;=93015^^41^277^1^^^^1
 ;;^UTILITY(U,$J,358.3,4182,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4182,1,2,0)
 ;;=2^93015
 ;;^UTILITY(U,$J,358.3,4182,1,3,0)
 ;;=3^CARDIOVASCULAR STRESS TST,COMPLETE
 ;;^UTILITY(U,$J,358.3,4183,0)
 ;;=93017^^41^277^2^^^^1
 ;;^UTILITY(U,$J,358.3,4183,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4183,1,2,0)
 ;;=2^93017
 ;;^UTILITY(U,$J,358.3,4183,1,3,0)
 ;;=3^CARDIOVASCULAR STRESS TST,TRACING
 ;;^UTILITY(U,$J,358.3,4184,0)
 ;;=94620^^41^277^4^^^^1
 ;;^UTILITY(U,$J,358.3,4184,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4184,1,2,0)
 ;;=2^94620
 ;;^UTILITY(U,$J,358.3,4184,1,3,0)
 ;;=3^PULMONARY STRESS TST,SIMPLE
 ;;^UTILITY(U,$J,358.3,4185,0)
 ;;=94621^^41^277^3^^^^1
 ;;^UTILITY(U,$J,358.3,4185,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4185,1,2,0)
 ;;=2^94621
 ;;^UTILITY(U,$J,358.3,4185,1,3,0)
 ;;=3^PULM STRESS TST,COMPLEX
 ;;^UTILITY(U,$J,358.3,4186,0)
 ;;=94760^^41^278^1^^^^1
 ;;^UTILITY(U,$J,358.3,4186,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4186,1,2,0)
 ;;=2^94760
 ;;^UTILITY(U,$J,358.3,4186,1,3,0)
 ;;=3^EAR/PULSE OX,RESTING ONLY
 ;;^UTILITY(U,$J,358.3,4187,0)
 ;;=94761^^41^278^2^^^^1
 ;;^UTILITY(U,$J,358.3,4187,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4187,1,2,0)
 ;;=2^94761
 ;;^UTILITY(U,$J,358.3,4187,1,3,0)
 ;;=3^EAR/PULSE OX,RESTING & WALKING
 ;;^UTILITY(U,$J,358.3,4188,0)
 ;;=94010^^41^279^12^^^^1
 ;;^UTILITY(U,$J,358.3,4188,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4188,1,2,0)
 ;;=2^94010
 ;;^UTILITY(U,$J,358.3,4188,1,3,0)
 ;;=3^SPIROMETRY/FORCED VITAL CAPACITY
 ;;^UTILITY(U,$J,358.3,4189,0)
 ;;=94060^^41^279^11^^^^1
 ;;^UTILITY(U,$J,358.3,4189,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4189,1,2,0)
 ;;=2^94060
 ;;^UTILITY(U,$J,358.3,4189,1,3,0)
 ;;=3^SPIROMETRY,PRE/POST BRONCHODILATOR
 ;;^UTILITY(U,$J,358.3,4190,0)
 ;;=94750^^41^279^3^^^^1
 ;;^UTILITY(U,$J,358.3,4190,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4190,1,2,0)
 ;;=2^94750
 ;;^UTILITY(U,$J,358.3,4190,1,3,0)
 ;;=3^COMPLIANCE STUDY
 ;;^UTILITY(U,$J,358.3,4191,0)
 ;;=94375^^41^279^4^^^^1
 ;;^UTILITY(U,$J,358.3,4191,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4191,1,2,0)
 ;;=2^94375
 ;;^UTILITY(U,$J,358.3,4191,1,3,0)
 ;;=3^FLOW VOLUME LOOP
 ;;^UTILITY(U,$J,358.3,4192,0)
 ;;=94070^^41^279^7^^^^1
 ;;^UTILITY(U,$J,358.3,4192,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4192,1,2,0)
 ;;=2^94070
 ;;^UTILITY(U,$J,358.3,4192,1,3,0)
 ;;=3^PROLONG EVAL OF BRONCHOSPASM
 ;;^UTILITY(U,$J,358.3,4193,0)
 ;;=94200^^41^279^5^^^^1
 ;;^UTILITY(U,$J,358.3,4193,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4193,1,2,0)
 ;;=2^94200
 ;;^UTILITY(U,$J,358.3,4193,1,3,0)
 ;;=3^MAX VOLUNTARY VENTILATION
 ;;^UTILITY(U,$J,358.3,4194,0)
 ;;=94014^^41^279^1^^^^1
 ;;^UTILITY(U,$J,358.3,4194,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4194,1,2,0)
 ;;=2^94014
 ;;^UTILITY(U,$J,358.3,4194,1,3,0)
 ;;=3^30 DAY SPIROMETRY
 ;;^UTILITY(U,$J,358.3,4195,0)
 ;;=94729^^41^279^2^^^^1
 ;;^UTILITY(U,$J,358.3,4195,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4195,1,2,0)
 ;;=2^94729
 ;;^UTILITY(U,$J,358.3,4195,1,3,0)
 ;;=3^C0/MEMBRANE DIFFUSE CAPACITY
 ;;^UTILITY(U,$J,358.3,4196,0)
 ;;=94726^^41^279^9^^^^1
 ;;^UTILITY(U,$J,358.3,4196,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4196,1,2,0)
 ;;=2^94726
 ;;^UTILITY(U,$J,358.3,4196,1,3,0)
 ;;=3^PULM FUNCT TST PLETHYMOGRAP
 ;;^UTILITY(U,$J,358.3,4197,0)
 ;;=94727^^41^279^10^^^^1
 ;;^UTILITY(U,$J,358.3,4197,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4197,1,2,0)
 ;;=2^94727
 ;;^UTILITY(U,$J,358.3,4197,1,3,0)
 ;;=3^PULM FUNCTION TEST BY GAS
 ;;^UTILITY(U,$J,358.3,4198,0)
 ;;=94728^^41^279^8^^^^1
 ;;^UTILITY(U,$J,358.3,4198,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4198,1,2,0)
 ;;=2^94728
 ;;^UTILITY(U,$J,358.3,4198,1,3,0)
 ;;=3^PULM FUNCT TEST OSCILLOMETRY
 ;;^UTILITY(U,$J,358.3,4199,0)
 ;;=94799^^41^279^6^^^^1
 ;;^UTILITY(U,$J,358.3,4199,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4199,1,2,0)
 ;;=2^94799
 ;;^UTILITY(U,$J,358.3,4199,1,3,0)
 ;;=3^MIP and MEP
