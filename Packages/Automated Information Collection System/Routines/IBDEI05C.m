IBDEI05C ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,6837,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6837,1,4,0)
 ;;=4^791.9
 ;;^UTILITY(U,$J,358.3,6837,1,5,0)
 ;;=5^Abnormal UA
 ;;^UTILITY(U,$J,358.3,6837,2)
 ;;=Abnormal UA^273408
 ;;^UTILITY(U,$J,358.3,6838,0)
 ;;=789.01^^58^510^7
 ;;^UTILITY(U,$J,358.3,6838,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6838,1,4,0)
 ;;=4^789.01
 ;;^UTILITY(U,$J,358.3,6838,1,5,0)
 ;;=5^Abdominal pain, RUQ
 ;;^UTILITY(U,$J,358.3,6838,2)
 ;;=^303318
 ;;^UTILITY(U,$J,358.3,6839,0)
 ;;=789.02^^58^510^4
 ;;^UTILITY(U,$J,358.3,6839,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6839,1,4,0)
 ;;=4^789.02
 ;;^UTILITY(U,$J,358.3,6839,1,5,0)
 ;;=5^Abdominal pain, LUQ
 ;;^UTILITY(U,$J,358.3,6839,2)
 ;;=^303319
 ;;^UTILITY(U,$J,358.3,6840,0)
 ;;=789.03^^58^510^6
 ;;^UTILITY(U,$J,358.3,6840,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6840,1,4,0)
 ;;=4^789.03
 ;;^UTILITY(U,$J,358.3,6840,1,5,0)
 ;;=5^Abdominal pain, RLQ
 ;;^UTILITY(U,$J,358.3,6840,2)
 ;;=^303320
 ;;^UTILITY(U,$J,358.3,6841,0)
 ;;=789.04^^58^510^3
 ;;^UTILITY(U,$J,358.3,6841,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6841,1,4,0)
 ;;=4^789.04
 ;;^UTILITY(U,$J,358.3,6841,1,5,0)
 ;;=5^Abdominal pain, LLQ
 ;;^UTILITY(U,$J,358.3,6841,2)
 ;;=^303321
 ;;^UTILITY(U,$J,358.3,6842,0)
 ;;=789.06^^58^510^2
 ;;^UTILITY(U,$J,358.3,6842,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6842,1,4,0)
 ;;=4^789.06
 ;;^UTILITY(U,$J,358.3,6842,1,5,0)
 ;;=5^Abdominal pain, Epigastric
 ;;^UTILITY(U,$J,358.3,6842,2)
 ;;=^303323
 ;;^UTILITY(U,$J,358.3,6843,0)
 ;;=789.05^^58^510^5
 ;;^UTILITY(U,$J,358.3,6843,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6843,1,4,0)
 ;;=4^789.05
 ;;^UTILITY(U,$J,358.3,6843,1,5,0)
 ;;=5^Abdominal pain, Periumbilical
 ;;^UTILITY(U,$J,358.3,6843,2)
 ;;=^303322
 ;;^UTILITY(U,$J,358.3,6844,0)
 ;;=789.40^^58^510^8
 ;;^UTILITY(U,$J,358.3,6844,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6844,1,4,0)
 ;;=4^789.40
 ;;^UTILITY(U,$J,358.3,6844,1,5,0)
 ;;=5^Abdominal rigidity, unsp site
 ;;^UTILITY(U,$J,358.3,6844,2)
 ;;=^273393
 ;;^UTILITY(U,$J,358.3,6845,0)
 ;;=789.1^^58^510^80
 ;;^UTILITY(U,$J,358.3,6845,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6845,1,4,0)
 ;;=4^789.1
 ;;^UTILITY(U,$J,358.3,6845,1,5,0)
 ;;=5^Hepatomegaly
 ;;^UTILITY(U,$J,358.3,6845,2)
 ;;=Hepatomegaly^56494
 ;;^UTILITY(U,$J,358.3,6846,0)
 ;;=789.30^^58^510^1
 ;;^UTILITY(U,$J,358.3,6846,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6846,1,4,0)
 ;;=4^789.30
 ;;^UTILITY(U,$J,358.3,6846,1,5,0)
 ;;=5^Abdominal Mass/Lump
 ;;^UTILITY(U,$J,358.3,6846,2)
 ;;=Abdominal Mass/Lump^917
 ;;^UTILITY(U,$J,358.3,6847,0)
 ;;=789.2^^58^510^139
 ;;^UTILITY(U,$J,358.3,6847,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6847,1,4,0)
 ;;=4^789.2
 ;;^UTILITY(U,$J,358.3,6847,1,5,0)
 ;;=5^Splenomegaly
 ;;^UTILITY(U,$J,358.3,6847,2)
 ;;=Splenomegaly^113452
 ;;^UTILITY(U,$J,358.3,6848,0)
 ;;=785.2^^58^510^36
 ;;^UTILITY(U,$J,358.3,6848,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6848,1,4,0)
 ;;=4^785.2
 ;;^UTILITY(U,$J,358.3,6848,1,5,0)
 ;;=5^Cardiac murmurs, undiagnosed
 ;;^UTILITY(U,$J,358.3,6848,2)
 ;;=^295854
 ;;^UTILITY(U,$J,358.3,6849,0)
 ;;=786.50^^58^510^39
 ;;^UTILITY(U,$J,358.3,6849,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6849,1,4,0)
 ;;=4^786.50
 ;;^UTILITY(U,$J,358.3,6849,1,5,0)
 ;;=5^Chest pain/Discomfort (nonsp) chest pain diff from discomfort
 ;;^UTILITY(U,$J,358.3,6849,2)
 ;;=^22485
 ;;^UTILITY(U,$J,358.3,6850,0)
 ;;=786.51^^58^510^130
 ;;^UTILITY(U,$J,358.3,6850,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6850,1,4,0)
 ;;=4^786.51
 ;;^UTILITY(U,$J,358.3,6850,1,5,0)
 ;;=5^Precordial Pain
 ;;^UTILITY(U,$J,358.3,6850,2)
 ;;=Precordial Pain^276877
 ;;^UTILITY(U,$J,358.3,6851,0)
 ;;=786.2^^58^510^45
 ;;^UTILITY(U,$J,358.3,6851,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6851,1,4,0)
 ;;=4^786.2
 ;;^UTILITY(U,$J,358.3,6851,1,5,0)
 ;;=5^Cough
 ;;^UTILITY(U,$J,358.3,6851,2)
 ;;=Cough^28905
 ;;^UTILITY(U,$J,358.3,6852,0)
 ;;=396.0^^58^510^42
 ;;^UTILITY(U,$J,358.3,6852,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6852,1,4,0)
 ;;=4^396.0
 ;;^UTILITY(U,$J,358.3,6852,1,5,0)
 ;;=5^Combined Aortic&Mitral Valve stenosis
 ;;^UTILITY(U,$J,358.3,6852,2)
 ;;=^269580
 ;;^UTILITY(U,$J,358.3,6853,0)
 ;;=786.09^^58^510^59
 ;;^UTILITY(U,$J,358.3,6853,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6853,1,4,0)
 ;;=4^786.09
 ;;^UTILITY(U,$J,358.3,6853,1,5,0)
 ;;=5^Dyspnea
 ;;^UTILITY(U,$J,358.3,6853,2)
 ;;=Dyspnea^87547
 ;;^UTILITY(U,$J,358.3,6854,0)
 ;;=786.8^^58^510^81
 ;;^UTILITY(U,$J,358.3,6854,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6854,1,4,0)
 ;;=4^786.8
 ;;^UTILITY(U,$J,358.3,6854,1,5,0)
 ;;=5^Hiccough
 ;;^UTILITY(U,$J,358.3,6854,2)
 ;;=Hiccough^57197
 ;;^UTILITY(U,$J,358.3,6855,0)
 ;;=786.01^^58^510^86
 ;;^UTILITY(U,$J,358.3,6855,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6855,1,4,0)
 ;;=4^786.01
 ;;^UTILITY(U,$J,358.3,6855,1,5,0)
 ;;=5^Hyperventilation
 ;;^UTILITY(U,$J,358.3,6855,2)
 ;;=Hyperventilation^60480
 ;;^UTILITY(U,$J,358.3,6856,0)
 ;;=786.6^^58^510^106
 ;;^UTILITY(U,$J,358.3,6856,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6856,1,4,0)
 ;;=4^786.6
 ;;^UTILITY(U,$J,358.3,6856,1,5,0)
 ;;=5^Mass, Lump of chest
 ;;^UTILITY(U,$J,358.3,6856,2)
 ;;=^273380
 ;;^UTILITY(U,$J,358.3,6857,0)
 ;;=786.02^^58^510^121
 ;;^UTILITY(U,$J,358.3,6857,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6857,1,4,0)
 ;;=4^786.02
 ;;^UTILITY(U,$J,358.3,6857,1,5,0)
 ;;=5^Orthopnea
 ;;^UTILITY(U,$J,358.3,6857,2)
 ;;=Orthopnea^186737
 ;;^UTILITY(U,$J,358.3,6858,0)
 ;;=786.52^^58^510^122
 ;;^UTILITY(U,$J,358.3,6858,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6858,1,4,0)
 ;;=4^786.52
 ;;^UTILITY(U,$J,358.3,6858,1,5,0)
 ;;=5^Painful Respiration
 ;;^UTILITY(U,$J,358.3,6858,2)
 ;;=^89126
 ;;^UTILITY(U,$J,358.3,6859,0)
 ;;=785.1^^58^510^124
 ;;^UTILITY(U,$J,358.3,6859,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6859,1,4,0)
 ;;=4^785.1
 ;;^UTILITY(U,$J,358.3,6859,1,5,0)
 ;;=5^Palpitations
 ;;^UTILITY(U,$J,358.3,6859,2)
 ;;=Palpitations^89281
 ;;^UTILITY(U,$J,358.3,6860,0)
 ;;=786.4^^58^510^140
 ;;^UTILITY(U,$J,358.3,6860,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6860,1,4,0)
 ;;=4^786.4
 ;;^UTILITY(U,$J,358.3,6860,1,5,0)
 ;;=5^Sputum production, abnormal
 ;;^UTILITY(U,$J,358.3,6860,2)
 ;;=^273377
 ;;^UTILITY(U,$J,358.3,6861,0)
 ;;=786.1^^58^510^141
 ;;^UTILITY(U,$J,358.3,6861,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6861,1,4,0)
 ;;=4^786.1
 ;;^UTILITY(U,$J,358.3,6861,1,5,0)
 ;;=5^Stridor
 ;;^UTILITY(U,$J,358.3,6861,2)
 ;;=Stridor^114767
 ;;^UTILITY(U,$J,358.3,6862,0)
 ;;=785.0^^58^510^144
 ;;^UTILITY(U,$J,358.3,6862,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6862,1,4,0)
 ;;=4^785.0
 ;;^UTILITY(U,$J,358.3,6862,1,5,0)
 ;;=5^Tachycardia
 ;;^UTILITY(U,$J,358.3,6862,2)
 ;;=Tachycardia^117041
 ;;^UTILITY(U,$J,358.3,6863,0)
 ;;=786.06^^58^510^145
 ;;^UTILITY(U,$J,358.3,6863,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6863,1,4,0)
 ;;=4^786.06
 ;;^UTILITY(U,$J,358.3,6863,1,5,0)
 ;;=5^Tachypnea
 ;;^UTILITY(U,$J,358.3,6863,2)
 ;;=Tachypnea^321213
 ;;^UTILITY(U,$J,358.3,6864,0)
 ;;=305.1^^58^510^149
 ;;^UTILITY(U,$J,358.3,6864,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6864,1,4,0)
 ;;=4^305.1
 ;;^UTILITY(U,$J,358.3,6864,1,5,0)
 ;;=5^Tobacco Use
 ;;^UTILITY(U,$J,358.3,6864,2)
 ;;=Tobacco Use^119899
 ;;^UTILITY(U,$J,358.3,6865,0)
 ;;=786.07^^58^510^158
 ;;^UTILITY(U,$J,358.3,6865,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6865,1,4,0)
 ;;=4^786.07
 ;;^UTILITY(U,$J,358.3,6865,1,5,0)
 ;;=5^Wheezing
 ;;^UTILITY(U,$J,358.3,6865,2)
 ;;=Wheezing^127848
 ;;^UTILITY(U,$J,358.3,6866,0)
 ;;=787.7^^58^510^11
 ;;^UTILITY(U,$J,358.3,6866,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6866,1,4,0)
 ;;=4^787.7
 ;;^UTILITY(U,$J,358.3,6866,1,5,0)
 ;;=5^Abnormal Feces
 ;;^UTILITY(U,$J,358.3,6866,2)
 ;;=Abdominal Feces^276857
 ;;^UTILITY(U,$J,358.3,6867,0)
 ;;=787.99^^58^510^146
 ;;^UTILITY(U,$J,358.3,6867,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6867,1,4,0)
 ;;=4^787.99
 ;;^UTILITY(U,$J,358.3,6867,1,5,0)
 ;;=5^Tenesmus
 ;;^UTILITY(U,$J,358.3,6867,2)
 ;;=Tenesmus^273388
 ;;^UTILITY(U,$J,358.3,6868,0)
 ;;=787.91^^58^510^50
 ;;^UTILITY(U,$J,358.3,6868,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6868,1,4,0)
 ;;=4^787.91
 ;;^UTILITY(U,$J,358.3,6868,1,5,0)
 ;;=5^Diarrhea
 ;;^UTILITY(U,$J,358.3,6868,2)
 ;;=Diarrhea^33921
 ;;^UTILITY(U,$J,358.3,6869,0)
 ;;=787.3^^58^510^71
 ;;^UTILITY(U,$J,358.3,6869,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6869,1,4,0)
 ;;=4^787.3
 ;;^UTILITY(U,$J,358.3,6869,1,5,0)
 ;;=5^Flatulence/Eructation/Gas pain
 ;;^UTILITY(U,$J,358.3,6869,2)
 ;;=^46766
 ;;^UTILITY(U,$J,358.3,6870,0)
 ;;=787.1^^58^510^77
 ;;^UTILITY(U,$J,358.3,6870,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6870,1,4,0)
 ;;=4^787.1
 ;;^UTILITY(U,$J,358.3,6870,1,5,0)
 ;;=5^Heartburn
 ;;^UTILITY(U,$J,358.3,6870,2)
 ;;=Heartburn^54996
 ;;^UTILITY(U,$J,358.3,6871,0)
 ;;=786.59^^58^510^38
 ;;^UTILITY(U,$J,358.3,6871,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6871,1,4,0)
 ;;=4^786.59
 ;;^UTILITY(U,$J,358.3,6871,1,5,0)
 ;;=5^Chest Pain
 ;;^UTILITY(U,$J,358.3,6871,2)
 ;;=Chest Pain^87384
 ;;^UTILITY(U,$J,358.3,6872,0)
 ;;=787.02^^58^510^111
 ;;^UTILITY(U,$J,358.3,6872,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6872,1,4,0)
 ;;=4^787.02
 ;;^UTILITY(U,$J,358.3,6872,1,5,0)
 ;;=5^Nausea Alone
 ;;^UTILITY(U,$J,358.3,6872,2)
 ;;=Nausea Alone^81639
 ;;^UTILITY(U,$J,358.3,6873,0)
 ;;=787.01^^58^510^110
 ;;^UTILITY(U,$J,358.3,6873,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6873,1,4,0)
 ;;=4^787.01
 ;;^UTILITY(U,$J,358.3,6873,1,5,0)
 ;;=5^Nausea & vomiting
 ;;^UTILITY(U,$J,358.3,6873,2)
 ;;=nausea and vomiting^81644
 ;;^UTILITY(U,$J,358.3,6874,0)
 ;;=787.03^^58^510^155
 ;;^UTILITY(U,$J,358.3,6874,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,6874,1,4,0)
 ;;=4^787.03
 ;;^UTILITY(U,$J,358.3,6874,1,5,0)
 ;;=5^Vomiting Alone
 ;;^UTILITY(U,$J,358.3,6874,2)
 ;;=Vomiting Alone^127237