IBDEI0BN ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,15493,0)
 ;;=90837^^113^956^3^^^^1
 ;;^UTILITY(U,$J,358.3,15493,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15493,1,2,0)
 ;;=2^90837
 ;;^UTILITY(U,$J,358.3,15493,1,3,0)
 ;;=3^Psychotherapy 53+ min
 ;;^UTILITY(U,$J,358.3,15494,0)
 ;;=90853^^113^956^4^^^^1
 ;;^UTILITY(U,$J,358.3,15494,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15494,1,2,0)
 ;;=2^90853
 ;;^UTILITY(U,$J,358.3,15494,1,3,0)
 ;;=3^Group Psychotherapy
 ;;^UTILITY(U,$J,358.3,15495,0)
 ;;=90846^^113^956^5^^^^1
 ;;^UTILITY(U,$J,358.3,15495,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15495,1,2,0)
 ;;=2^90846
 ;;^UTILITY(U,$J,358.3,15495,1,3,0)
 ;;=3^Family Psychotherapy w/o Pt
 ;;^UTILITY(U,$J,358.3,15496,0)
 ;;=90847^^113^956^6^^^^1
 ;;^UTILITY(U,$J,358.3,15496,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15496,1,2,0)
 ;;=2^90847
 ;;^UTILITY(U,$J,358.3,15496,1,3,0)
 ;;=3^Family Psychotherapy w/ Pt
 ;;^UTILITY(U,$J,358.3,15497,0)
 ;;=90875^^113^956^7^^^^1
 ;;^UTILITY(U,$J,358.3,15497,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15497,1,2,0)
 ;;=2^90875
 ;;^UTILITY(U,$J,358.3,15497,1,3,0)
 ;;=3^With Biofeedback 20-30 min
 ;;^UTILITY(U,$J,358.3,15498,0)
 ;;=90876^^113^956^8^^^^1
 ;;^UTILITY(U,$J,358.3,15498,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15498,1,2,0)
 ;;=2^90876
 ;;^UTILITY(U,$J,358.3,15498,1,3,0)
 ;;=3^With Biofeedback 45-50 min
 ;;^UTILITY(U,$J,358.3,15499,0)
 ;;=90839^^113^957^1^^^^1
 ;;^UTILITY(U,$J,358.3,15499,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15499,1,2,0)
 ;;=2^90839
 ;;^UTILITY(U,$J,358.3,15499,1,3,0)
 ;;=3^Psychotherapy Crisis Init Hr
 ;;^UTILITY(U,$J,358.3,15500,0)
 ;;=90840^^113^957^2^^^^1
 ;;^UTILITY(U,$J,358.3,15500,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15500,1,2,0)
 ;;=2^90840
 ;;^UTILITY(U,$J,358.3,15500,1,3,0)
 ;;=3^Psychotherapy Crisis;Ea Addl 30 min
 ;;^UTILITY(U,$J,358.3,15501,0)
 ;;=97545^^113^958^24^^^^1
 ;;^UTILITY(U,$J,358.3,15501,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15501,1,2,0)
 ;;=2^97545
 ;;^UTILITY(U,$J,358.3,15501,1,3,0)
 ;;=3^Work Therapy, Init 2 hrs
 ;;^UTILITY(U,$J,358.3,15502,0)
 ;;=97546^^113^958^23^^^^1
 ;;^UTILITY(U,$J,358.3,15502,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15502,1,2,0)
 ;;=2^97546
 ;;^UTILITY(U,$J,358.3,15502,1,3,0)
 ;;=3^Work Ther, addl hrs after 2
 ;;^UTILITY(U,$J,358.3,15503,0)
 ;;=97537^^113^958^3^^^^1
 ;;^UTILITY(U,$J,358.3,15503,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15503,1,2,0)
 ;;=2^97537
 ;;^UTILITY(U,$J,358.3,15503,1,3,0)
 ;;=3^Community/Work Reintegration per 15 min
 ;;^UTILITY(U,$J,358.3,15504,0)
 ;;=97532^^113^958^2^^^^1
 ;;^UTILITY(U,$J,358.3,15504,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15504,1,2,0)
 ;;=2^97532
 ;;^UTILITY(U,$J,358.3,15504,1,3,0)
 ;;=3^Cognitive Sk Dev (PhD/OT)per 15min
 ;;^UTILITY(U,$J,358.3,15505,0)
 ;;=97533^^113^958^17^^^^1
 ;;^UTILITY(U,$J,358.3,15505,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15505,1,2,0)
 ;;=2^97533
 ;;^UTILITY(U,$J,358.3,15505,1,3,0)
 ;;=3^Sensory Integrat per 15 min
 ;;^UTILITY(U,$J,358.3,15506,0)
 ;;=97535^^113^958^1^^^^1
 ;;^UTILITY(U,$J,358.3,15506,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15506,1,2,0)
 ;;=2^97535
 ;;^UTILITY(U,$J,358.3,15506,1,3,0)
 ;;=3^ADL Train per 15 min
 ;;^UTILITY(U,$J,358.3,15507,0)
 ;;=H0004^^113^958^19^^^^1
 ;;^UTILITY(U,$J,358.3,15507,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15507,1,2,0)
 ;;=2^H0004
 ;;^UTILITY(U,$J,358.3,15507,1,3,0)
 ;;=3^Subs Abuse Ind Counseling,per 15 min
 ;;^UTILITY(U,$J,358.3,15508,0)
 ;;=H0046^^113^958^11^^^^1
 ;;^UTILITY(U,$J,358.3,15508,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15508,1,2,0)
 ;;=2^H0046
 ;;^UTILITY(U,$J,358.3,15508,1,3,0)
 ;;=3^PTSD Group
 ;;^UTILITY(U,$J,358.3,15509,0)
 ;;=96119^^113^958^8^^^^1
 ;;^UTILITY(U,$J,358.3,15509,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15509,1,2,0)
 ;;=2^96119
 ;;^UTILITY(U,$J,358.3,15509,1,3,0)
 ;;=3^Neuropsych Test by tech,per hr
 ;;^UTILITY(U,$J,358.3,15510,0)
 ;;=96102^^113^958^14^^^^1
 ;;^UTILITY(U,$J,358.3,15510,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15510,1,2,0)
 ;;=2^96102
 ;;^UTILITY(U,$J,358.3,15510,1,3,0)
 ;;=3^Psych Test by Tech,per hr
 ;;^UTILITY(U,$J,358.3,15511,0)
 ;;=96103^^113^958^15^^^^1
 ;;^UTILITY(U,$J,358.3,15511,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15511,1,2,0)
 ;;=2^96103
 ;;^UTILITY(U,$J,358.3,15511,1,3,0)
 ;;=3^Psych Test by computer
 ;;^UTILITY(U,$J,358.3,15512,0)
 ;;=96120^^113^958^9^^^^1
 ;;^UTILITY(U,$J,358.3,15512,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15512,1,2,0)
 ;;=2^96120
 ;;^UTILITY(U,$J,358.3,15512,1,3,0)
 ;;=3^Neuropsych Tst Admin w/Comp
 ;;^UTILITY(U,$J,358.3,15513,0)
 ;;=96125^^113^958^18^^^^1
 ;;^UTILITY(U,$J,358.3,15513,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15513,1,2,0)
 ;;=2^96125
 ;;^UTILITY(U,$J,358.3,15513,1,3,0)
 ;;=3^Stan Cog Perf Tst, per hr
 ;;^UTILITY(U,$J,358.3,15514,0)
 ;;=Q3014^^113^958^20^^^^1
 ;;^UTILITY(U,$J,358.3,15514,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15514,1,2,0)
 ;;=2^Q3014
 ;;^UTILITY(U,$J,358.3,15514,1,3,0)
 ;;=3^Telehealth Facility Fee
 ;;^UTILITY(U,$J,358.3,15515,0)
 ;;=90887^^113^958^4^^^^1
 ;;^UTILITY(U,$J,358.3,15515,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15515,1,2,0)
 ;;=2^90887
 ;;^UTILITY(U,$J,358.3,15515,1,3,0)
 ;;=3^Consultation w/Family
 ;;^UTILITY(U,$J,358.3,15516,0)
 ;;=90885^^113^958^13^^^^1
 ;;^UTILITY(U,$J,358.3,15516,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15516,1,2,0)
 ;;=2^90885
 ;;^UTILITY(U,$J,358.3,15516,1,3,0)
 ;;=3^Psych Eval of Records
 ;;^UTILITY(U,$J,358.3,15517,0)
 ;;=90889^^113^958^12^^^^1
 ;;^UTILITY(U,$J,358.3,15517,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15517,1,2,0)
 ;;=2^90889
 ;;^UTILITY(U,$J,358.3,15517,1,3,0)
 ;;=3^Preparation of Report
 ;;^UTILITY(U,$J,358.3,15518,0)
 ;;=96118^^113^958^10^^^^1
 ;;^UTILITY(U,$J,358.3,15518,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15518,1,2,0)
 ;;=2^96118
 ;;^UTILITY(U,$J,358.3,15518,1,3,0)
 ;;=3^Neuropsych Tst/Hr of Psychologist/MD Time
 ;;^UTILITY(U,$J,358.3,15519,0)
 ;;=97150^^113^958^5^^^^1
 ;;^UTILITY(U,$J,358.3,15519,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15519,1,2,0)
 ;;=2^97150
 ;;^UTILITY(U,$J,358.3,15519,1,3,0)
 ;;=3^Group Therapeutic Procedures
 ;;^UTILITY(U,$J,358.3,15520,0)
 ;;=97530^^113^958^21^^^^1
 ;;^UTILITY(U,$J,358.3,15520,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15520,1,2,0)
 ;;=2^97530
 ;;^UTILITY(U,$J,358.3,15520,1,3,0)
 ;;=3^Therapeutic Activ,Dir Prov Contact,ea 15 min
 ;;^UTILITY(U,$J,358.3,15521,0)
 ;;=G0177^^113^958^22^^^^1
 ;;^UTILITY(U,$J,358.3,15521,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15521,1,2,0)
 ;;=2^G0177
 ;;^UTILITY(U,$J,358.3,15521,1,3,0)
 ;;=3^Train/Ed for Disability > 44 Min
 ;;^UTILITY(U,$J,358.3,15522,0)
 ;;=H0038^^113^958^16^^^^1
 ;;^UTILITY(U,$J,358.3,15522,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15522,1,2,0)
 ;;=2^H0038
 ;;^UTILITY(U,$J,358.3,15522,1,3,0)
 ;;=3^Self-Help/Peer Svc per 15 Min
 ;;^UTILITY(U,$J,358.3,15523,0)
 ;;=90899^^113^958^6^^^^1
 ;;^UTILITY(U,$J,358.3,15523,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15523,1,2,0)
 ;;=2^90899
 ;;^UTILITY(U,$J,358.3,15523,1,3,0)
 ;;=3^NOS Psych Service
 ;;^UTILITY(U,$J,358.3,15524,0)
 ;;=96116^^113^958^7^^^^1
 ;;^UTILITY(U,$J,358.3,15524,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15524,1,2,0)
 ;;=2^96116
 ;;^UTILITY(U,$J,358.3,15524,1,3,0)
 ;;=3^Neurobehavioral Status Exam
 ;;^UTILITY(U,$J,358.3,15525,0)
 ;;=96150^^113^959^1^^^^1
 ;;^UTILITY(U,$J,358.3,15525,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15525,1,2,0)
 ;;=2^96150
 ;;^UTILITY(U,$J,358.3,15525,1,3,0)
 ;;=3^Behavior Assess,Initial,ea 15min
 ;;^UTILITY(U,$J,358.3,15526,0)
 ;;=96151^^113^959^2^^^^1
 ;;^UTILITY(U,$J,358.3,15526,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15526,1,2,0)
 ;;=2^96151
 ;;^UTILITY(U,$J,358.3,15526,1,3,0)
 ;;=3^Behavior Reassessment,ea 15min
 ;;^UTILITY(U,$J,358.3,15527,0)
 ;;=96152^^113^959^3^^^^1
 ;;^UTILITY(U,$J,358.3,15527,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15527,1,2,0)
 ;;=2^96152
 ;;^UTILITY(U,$J,358.3,15527,1,3,0)
 ;;=3^Behavior Intervention,Ind,ea 15min
 ;;^UTILITY(U,$J,358.3,15528,0)
 ;;=96153^^113^959^4^^^^1
 ;;^UTILITY(U,$J,358.3,15528,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15528,1,2,0)
 ;;=2^96153
 ;;^UTILITY(U,$J,358.3,15528,1,3,0)
 ;;=3^Behavior Intervention,Grp,ea 15min
 ;;^UTILITY(U,$J,358.3,15529,0)
 ;;=96154^^113^959^5^^^^1
 ;;^UTILITY(U,$J,358.3,15529,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15529,1,2,0)
 ;;=2^96154
 ;;^UTILITY(U,$J,358.3,15529,1,3,0)
 ;;=3^Behav Intervent,Fam w/Pt,ea 15min
 ;;^UTILITY(U,$J,358.3,15530,0)
 ;;=96155^^113^959^6^^^^1
 ;;^UTILITY(U,$J,358.3,15530,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15530,1,2,0)
 ;;=2^96155
 ;;^UTILITY(U,$J,358.3,15530,1,3,0)
 ;;=3^Behav Intervent,Fam w/o Pt,ea 15min
 ;;^UTILITY(U,$J,358.3,15531,0)
 ;;=99367^^113^960^1^^^^1
 ;;^UTILITY(U,$J,358.3,15531,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15531,1,2,0)
 ;;=2^99367
 ;;^UTILITY(U,$J,358.3,15531,1,3,0)
 ;;=3^Team Conf w/o Pt By Phys>29min
 ;;^UTILITY(U,$J,358.3,15532,0)
 ;;=99368^^113^960^2^^^^1
 ;;^UTILITY(U,$J,358.3,15532,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15532,1,2,0)
 ;;=2^99368
 ;;^UTILITY(U,$J,358.3,15532,1,3,0)
 ;;=3^Team Conf w/o Pt by HC Pro>29min
 ;;^UTILITY(U,$J,358.3,15533,0)
 ;;=90875^^113^961^1^^^^1
 ;;^UTILITY(U,$J,358.3,15533,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15533,1,2,0)
 ;;=2^90875
 ;;^UTILITY(U,$J,358.3,15533,1,3,0)
 ;;=3^Interactive Complexity,Add-On
 ;;^UTILITY(U,$J,358.3,15534,0)
 ;;=H0001^^113^962^1^^^^1
 ;;^UTILITY(U,$J,358.3,15534,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15534,1,2,0)
 ;;=2^H0001
 ;;^UTILITY(U,$J,358.3,15534,1,3,0)
 ;;=3^Addictions Assessment
 ;;^UTILITY(U,$J,358.3,15535,0)
 ;;=H0002^^113^962^10^^^^1
 ;;^UTILITY(U,$J,358.3,15535,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,15535,1,2,0)
 ;;=2^H0002
