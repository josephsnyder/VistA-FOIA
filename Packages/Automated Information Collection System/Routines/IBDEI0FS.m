IBDEI0FS ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,21208,1,3,0)
 ;;=3^96125
 ;;^UTILITY(U,$J,358.3,21209,0)
 ;;=97024^^162^1390^14^^^^1
 ;;^UTILITY(U,$J,358.3,21209,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21209,1,2,0)
 ;;=2^Diathermy
 ;;^UTILITY(U,$J,358.3,21209,1,3,0)
 ;;=3^97024
 ;;^UTILITY(U,$J,358.3,21210,0)
 ;;=97039^^162^1390^47^^^^1
 ;;^UTILITY(U,$J,358.3,21210,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21210,1,2,0)
 ;;=2^Unlisted Modality (Spec Type/Time)
 ;;^UTILITY(U,$J,358.3,21210,1,3,0)
 ;;=3^97039
 ;;^UTILITY(U,$J,358.3,21211,0)
 ;;=97139^^162^1390^48^^^^1
 ;;^UTILITY(U,$J,358.3,21211,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21211,1,2,0)
 ;;=2^Unlisted Therapeutic Proc (Specify)
 ;;^UTILITY(U,$J,358.3,21211,1,3,0)
 ;;=3^97139
 ;;^UTILITY(U,$J,358.3,21212,0)
 ;;=97533^^162^1390^38^^^^1
 ;;^UTILITY(U,$J,358.3,21212,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21212,1,2,0)
 ;;=2^Sensory Integration,ea 15min
 ;;^UTILITY(U,$J,358.3,21212,1,3,0)
 ;;=3^97533
 ;;^UTILITY(U,$J,358.3,21213,0)
 ;;=97537^^162^1390^11^^^^1
 ;;^UTILITY(U,$J,358.3,21213,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21213,1,2,0)
 ;;=2^Community/Work Reintegration,ea 15min
 ;;^UTILITY(U,$J,358.3,21213,1,3,0)
 ;;=3^97537
 ;;^UTILITY(U,$J,358.3,21214,0)
 ;;=97755^^162^1390^7^^^^1
 ;;^UTILITY(U,$J,358.3,21214,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21214,1,2,0)
 ;;=2^Assistive Tech Assess,ea 15min
 ;;^UTILITY(U,$J,358.3,21214,1,3,0)
 ;;=3^97755
 ;;^UTILITY(U,$J,358.3,21215,0)
 ;;=97810^^162^1390^3^^^^1
 ;;^UTILITY(U,$J,358.3,21215,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21215,1,2,0)
 ;;=2^Acupunct w/o Stimul ea 15min
 ;;^UTILITY(U,$J,358.3,21215,1,3,0)
 ;;=3^97810
 ;;^UTILITY(U,$J,358.3,21216,0)
 ;;=97811^^162^1390^4^^^^1
 ;;^UTILITY(U,$J,358.3,21216,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21216,1,2,0)
 ;;=2^Acupunct w/o Stimul ea addl 15min
 ;;^UTILITY(U,$J,358.3,21216,1,3,0)
 ;;=3^97811
 ;;^UTILITY(U,$J,358.3,21217,0)
 ;;=97813^^162^1390^1^^^^1
 ;;^UTILITY(U,$J,358.3,21217,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21217,1,2,0)
 ;;=2^Acupunct w/ Stimul ea 15min
 ;;^UTILITY(U,$J,358.3,21217,1,3,0)
 ;;=3^97813
 ;;^UTILITY(U,$J,358.3,21218,0)
 ;;=97814^^162^1390^2^^^^1
 ;;^UTILITY(U,$J,358.3,21218,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21218,1,2,0)
 ;;=2^Acupunct w/ Stimul ea addl 15min
 ;;^UTILITY(U,$J,358.3,21218,1,3,0)
 ;;=3^97814
 ;;^UTILITY(U,$J,358.3,21219,0)
 ;;=95831^^162^1390^31^^^^1
 ;;^UTILITY(U,$J,358.3,21219,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21219,1,2,0)
 ;;=2^Muscle Testing,Manual;Extrem or Trunk
 ;;^UTILITY(U,$J,358.3,21219,1,3,0)
 ;;=3^95831
 ;;^UTILITY(U,$J,358.3,21220,0)
 ;;=20600^^162^1391^5^^^^1
 ;;^UTILITY(U,$J,358.3,21220,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21220,1,2,0)
 ;;=2^Drain/Inject, Joint/Bursa
 ;;^UTILITY(U,$J,358.3,21220,1,3,0)
 ;;=3^20600
 ;;^UTILITY(U,$J,358.3,21221,0)
 ;;=20605^^162^1391^1^^^^1
 ;;^UTILITY(U,$J,358.3,21221,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21221,1,2,0)
 ;;=2^Asp/Inj Interm Jt(Ac/Wrist/Ankle
 ;;^UTILITY(U,$J,358.3,21221,1,3,0)
 ;;=3^20605
 ;;^UTILITY(U,$J,358.3,21222,0)
 ;;=20610^^162^1391^2^^^^1
 ;;^UTILITY(U,$J,358.3,21222,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21222,1,2,0)
 ;;=2^Asp/Inj Maj Jt (Should/Hip/Knee
 ;;^UTILITY(U,$J,358.3,21222,1,3,0)
 ;;=3^20610
 ;;^UTILITY(U,$J,358.3,21223,0)
 ;;=20550^^162^1391^6^^^^1
 ;;^UTILITY(U,$J,358.3,21223,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21223,1,2,0)
 ;;=2^Inject Tendon/Ligament/Cyst 
 ;;^UTILITY(U,$J,358.3,21223,1,3,0)
 ;;=3^20550
 ;;^UTILITY(U,$J,358.3,21224,0)
 ;;=J1100^^162^1391^4^^^^1
 ;;^UTILITY(U,$J,358.3,21224,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21224,1,2,0)
 ;;=2^Dexamethasone Sodium Phos 1 mg
 ;;^UTILITY(U,$J,358.3,21224,1,3,0)
 ;;=3^J1100
 ;;^UTILITY(U,$J,358.3,21225,0)
 ;;=J0800^^162^1391^3^^^^1
 ;;^UTILITY(U,$J,358.3,21225,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21225,1,2,0)
 ;;=2^Corticotropin Inj up to 40 units
 ;;^UTILITY(U,$J,358.3,21225,1,3,0)
 ;;=3^J0800
 ;;^UTILITY(U,$J,358.3,21226,0)
 ;;=J1040^^162^1391^7^^^^1
 ;;^UTILITY(U,$J,358.3,21226,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21226,1,2,0)
 ;;=2^Methylprednisolone 80 Mg Inj
 ;;^UTILITY(U,$J,358.3,21226,1,3,0)
 ;;=3^J1040
 ;;^UTILITY(U,$J,358.3,21227,0)
 ;;=97762^^162^1392^1^^^^1
 ;;^UTILITY(U,$J,358.3,21227,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21227,1,2,0)
 ;;=2^C/O for Orthotic/Prosth Use,Est Pt,ea 15min
 ;;^UTILITY(U,$J,358.3,21227,1,3,0)
 ;;=3^97762
 ;;^UTILITY(U,$J,358.3,21228,0)
 ;;=97760^^162^1392^2^^^^1
 ;;^UTILITY(U,$J,358.3,21228,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21228,1,2,0)
 ;;=2^Orthotic Mgmt and Training,ea 15min
 ;;^UTILITY(U,$J,358.3,21228,1,3,0)
 ;;=3^97760
 ;;^UTILITY(U,$J,358.3,21229,0)
 ;;=97761^^162^1392^3^^^^1
 ;;^UTILITY(U,$J,358.3,21229,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21229,1,2,0)
 ;;=2^Prosthetic Training,ea 15min
 ;;^UTILITY(U,$J,358.3,21229,1,3,0)
 ;;=3^97761
 ;;^UTILITY(U,$J,358.3,21230,0)
 ;;=97110^^162^1393^7^^^^1
 ;;^UTILITY(U,$J,358.3,21230,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21230,1,2,0)
 ;;=2^Therapeutic Exercises, Ea 15 Min
 ;;^UTILITY(U,$J,358.3,21230,1,3,0)
 ;;=3^97110
 ;;^UTILITY(U,$J,358.3,21231,0)
 ;;=97750^^162^1393^5^^^^1
 ;;^UTILITY(U,$J,358.3,21231,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21231,1,2,0)
 ;;=2^Physical Perform Test, Ea 15 Min
 ;;^UTILITY(U,$J,358.3,21231,1,3,0)
 ;;=3^97750
 ;;^UTILITY(U,$J,358.3,21232,0)
 ;;=97112^^162^1393^4^^^^1
 ;;^UTILITY(U,$J,358.3,21232,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21232,1,2,0)
 ;;=2^Neuromuscular Reeduc,  Ea 15 Min
 ;;^UTILITY(U,$J,358.3,21232,1,3,0)
 ;;=3^97112
 ;;^UTILITY(U,$J,358.3,21233,0)
 ;;=97113^^162^1393^1^^^^1
 ;;^UTILITY(U,$J,358.3,21233,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21233,1,2,0)
 ;;=2^Aquatic Exercises,ea 15min
 ;;^UTILITY(U,$J,358.3,21233,1,3,0)
 ;;=3^97113
 ;;^UTILITY(U,$J,358.3,21234,0)
 ;;=97116^^162^1393^3^^^^1
 ;;^UTILITY(U,$J,358.3,21234,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21234,1,2,0)
 ;;=2^Gait Training, Ea 15 Min
 ;;^UTILITY(U,$J,358.3,21234,1,3,0)
 ;;=3^97116
 ;;^UTILITY(U,$J,358.3,21235,0)
 ;;=97150^^162^1393^8^^^^1
 ;;^UTILITY(U,$J,358.3,21235,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21235,1,2,0)
 ;;=2^Therapeutic Proc, Group, 2+ Ind
 ;;^UTILITY(U,$J,358.3,21235,1,3,0)
 ;;=3^97150
 ;;^UTILITY(U,$J,358.3,21236,0)
 ;;=97530^^162^1393^6^^^^1
 ;;^UTILITY(U,$J,358.3,21236,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21236,1,2,0)
 ;;=2^Therapeutic Dynamic Activity,1-1,ea 15min
 ;;^UTILITY(U,$J,358.3,21236,1,3,0)
 ;;=3^97530
 ;;^UTILITY(U,$J,358.3,21237,0)
 ;;=97537^^162^1393^2^^^^1
 ;;^UTILITY(U,$J,358.3,21237,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21237,1,2,0)
 ;;=2^Community/Work Reintegration, Ea 15 Min
 ;;^UTILITY(U,$J,358.3,21237,1,3,0)
 ;;=3^97537
 ;;^UTILITY(U,$J,358.3,21238,0)
 ;;=97542^^162^1393^9^^^^1
 ;;^UTILITY(U,$J,358.3,21238,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21238,1,2,0)
 ;;=2^Wheelchair Training,ea 15 Min
 ;;^UTILITY(U,$J,358.3,21238,1,3,0)
 ;;=3^97542
 ;;^UTILITY(U,$J,358.3,21239,0)
 ;;=97001^^162^1394^4^^^^1
 ;;^UTILITY(U,$J,358.3,21239,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21239,1,2,0)
 ;;=2^Pt Evaluation
 ;;^UTILITY(U,$J,358.3,21239,1,3,0)
 ;;=3^97001
 ;;^UTILITY(U,$J,358.3,21240,0)
 ;;=97002^^162^1394^5^^^^1
 ;;^UTILITY(U,$J,358.3,21240,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21240,1,2,0)
 ;;=2^Pt Re-Evaluation
 ;;^UTILITY(U,$J,358.3,21240,1,3,0)
 ;;=3^97002
 ;;^UTILITY(U,$J,358.3,21241,0)
 ;;=97003^^162^1394^1^^^^1
 ;;^UTILITY(U,$J,358.3,21241,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21241,1,2,0)
 ;;=2^Ot Evaluation
 ;;^UTILITY(U,$J,358.3,21241,1,3,0)
 ;;=3^97003
 ;;^UTILITY(U,$J,358.3,21242,0)
 ;;=97004^^162^1394^2^^^^1
 ;;^UTILITY(U,$J,358.3,21242,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21242,1,2,0)
 ;;=2^Ot Re-Evaluation
 ;;^UTILITY(U,$J,358.3,21242,1,3,0)
 ;;=3^97004
 ;;^UTILITY(U,$J,358.3,21243,0)
 ;;=97750^^162^1394^3^^^^1
 ;;^UTILITY(U,$J,358.3,21243,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21243,1,2,0)
 ;;=2^Physical Performance Test (KT Eval)
 ;;^UTILITY(U,$J,358.3,21243,1,3,0)
 ;;=3^97750
 ;;^UTILITY(U,$J,358.3,21244,0)
 ;;=97597^^162^1395^1^^^^1
 ;;^UTILITY(U,$J,358.3,21244,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21244,1,2,0)
 ;;=2^Active Wound Care/20 Cm Or <
 ;;^UTILITY(U,$J,358.3,21244,1,3,0)
 ;;=3^97597
 ;;^UTILITY(U,$J,358.3,21245,0)
 ;;=97598^^162^1395^2^^^^1
 ;;^UTILITY(U,$J,358.3,21245,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21245,1,2,0)
 ;;=2^Active Wound Care ea addl 20 Cm (add on)
 ;;^UTILITY(U,$J,358.3,21245,1,3,0)
 ;;=3^97598
 ;;^UTILITY(U,$J,358.3,21246,0)
 ;;=97605^^162^1395^6^^^^1
 ;;^UTILITY(U,$J,358.3,21246,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21246,1,2,0)
 ;;=2^Neg Press Wound Tx </= 50 Cm
 ;;^UTILITY(U,$J,358.3,21246,1,3,0)
 ;;=3^97605
 ;;^UTILITY(U,$J,358.3,21247,0)
 ;;=97606^^162^1395^7^^^^1
 ;;^UTILITY(U,$J,358.3,21247,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21247,1,2,0)
 ;;=2^Neg Press Wound Tx, > 50 Cm
 ;;^UTILITY(U,$J,358.3,21247,1,3,0)
 ;;=3^97606
 ;;^UTILITY(U,$J,358.3,21248,0)
 ;;=97602^^162^1395^8^^^^1
 ;;^UTILITY(U,$J,358.3,21248,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21248,1,2,0)
 ;;=2^Removal devitalized tissue w/o anesth
 ;;^UTILITY(U,$J,358.3,21248,1,3,0)
 ;;=3^97602
 ;;^UTILITY(U,$J,358.3,21249,0)
 ;;=G0281^^162^1395^4^^^^1
 ;;^UTILITY(U,$J,358.3,21249,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21249,1,2,0)
 ;;=2^Electrical Stimulation,Wound Care
 ;;^UTILITY(U,$J,358.3,21249,1,3,0)
 ;;=3^G0281
 ;;^UTILITY(U,$J,358.3,21250,0)
 ;;=G0283^^162^1395^3^^^^1
 ;;^UTILITY(U,$J,358.3,21250,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21250,1,2,0)
 ;;=2^Electrical Stimulation,Oth than Wnd Care
 ;;^UTILITY(U,$J,358.3,21250,1,3,0)
 ;;=3^G0283
