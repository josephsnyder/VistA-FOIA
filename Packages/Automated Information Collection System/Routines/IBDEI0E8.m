IBDEI0E8 ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,18990,1,3,0)
 ;;=3^Headache
 ;;^UTILITY(U,$J,358.3,18990,2)
 ;;=Headache^54133
 ;;^UTILITY(U,$J,358.3,18991,0)
 ;;=92532^^142^1226^11
 ;;^UTILITY(U,$J,358.3,18991,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18991,1,2,0)
 ;;=2^92532
 ;;^UTILITY(U,$J,358.3,18991,1,3,0)
 ;;=3^Positional Nystagmus Study
 ;;^UTILITY(U,$J,358.3,18992,0)
 ;;=95857^^142^1226^12
 ;;^UTILITY(U,$J,358.3,18992,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18992,1,2,0)
 ;;=2^95857
 ;;^UTILITY(U,$J,358.3,18992,1,3,0)
 ;;=3^Tensilon Test
 ;;^UTILITY(U,$J,358.3,18993,0)
 ;;=64612^^142^1226^7^^^^1
 ;;^UTILITY(U,$J,358.3,18993,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18993,1,2,0)
 ;;=2^64612
 ;;^UTILITY(U,$J,358.3,18993,1,3,0)
 ;;=3^Botox Inj, Facial Nerve
 ;;^UTILITY(U,$J,358.3,18994,0)
 ;;=64613^^142^1226^8^^^^1
 ;;^UTILITY(U,$J,358.3,18994,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18994,1,2,0)
 ;;=2^64613
 ;;^UTILITY(U,$J,358.3,18994,1,3,0)
 ;;=3^Botox Inj, Cervical Muscles
 ;;^UTILITY(U,$J,358.3,18995,0)
 ;;=64614^^142^1226^9^^^^1
 ;;^UTILITY(U,$J,358.3,18995,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18995,1,2,0)
 ;;=2^64614
 ;;^UTILITY(U,$J,358.3,18995,1,3,0)
 ;;=3^Botox Inj, Extremity/Trunk
 ;;^UTILITY(U,$J,358.3,18996,0)
 ;;=J0585^^142^1226^1^^^^1
 ;;^UTILITY(U,$J,358.3,18996,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18996,1,2,0)
 ;;=2^J0585
 ;;^UTILITY(U,$J,358.3,18996,1,3,0)
 ;;=3^Botox Supply, per unit
 ;;^UTILITY(U,$J,358.3,18997,0)
 ;;=20552^^142^1226^2^^^^1
 ;;^UTILITY(U,$J,358.3,18997,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18997,1,2,0)
 ;;=2^20552
 ;;^UTILITY(U,$J,358.3,18997,1,3,0)
 ;;=3^Trigger Point, 1 or 2 muscles
 ;;^UTILITY(U,$J,358.3,18998,0)
 ;;=20553^^142^1226^3^^^^1
 ;;^UTILITY(U,$J,358.3,18998,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18998,1,2,0)
 ;;=2^20553
 ;;^UTILITY(U,$J,358.3,18998,1,3,0)
 ;;=3^Trigger Point, 3 or more muscles
 ;;^UTILITY(U,$J,358.3,18999,0)
 ;;=20612^^142^1226^4^^^^1
 ;;^UTILITY(U,$J,358.3,18999,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18999,1,2,0)
 ;;=2^20612
 ;;^UTILITY(U,$J,358.3,18999,1,3,0)
 ;;=3^Ganglion Cyst Aspriation/Injection
 ;;^UTILITY(U,$J,358.3,19000,0)
 ;;=64550^^142^1226^6^^^^1
 ;;^UTILITY(U,$J,358.3,19000,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19000,1,2,0)
 ;;=2^64550
 ;;^UTILITY(U,$J,358.3,19000,1,3,0)
 ;;=3^TENS Device Training and Issue
 ;;^UTILITY(U,$J,358.3,19001,0)
 ;;=64450^^142^1226^5^^^^1
 ;;^UTILITY(U,$J,358.3,19001,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19001,1,2,0)
 ;;=2^64450
 ;;^UTILITY(U,$J,358.3,19001,1,3,0)
 ;;=3^Nerve Block, peripheral nerve
 ;;^UTILITY(U,$J,358.3,19002,0)
 ;;=95990^^142^1226^13^^^^1
 ;;^UTILITY(U,$J,358.3,19002,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19002,1,2,0)
 ;;=2^95990
 ;;^UTILITY(U,$J,358.3,19002,1,3,0)
 ;;=3^Refill Spinal Implant Pump
 ;;^UTILITY(U,$J,358.3,19003,0)
 ;;=96402^^142^1226^14^^^^1
 ;;^UTILITY(U,$J,358.3,19003,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19003,1,2,0)
 ;;=2^96402
 ;;^UTILITY(U,$J,358.3,19003,1,3,0)
 ;;=3^Injec,IM,anti-neplastic horm
 ;;^UTILITY(U,$J,358.3,19004,0)
 ;;=96372^^142^1226^10^^^^1
 ;;^UTILITY(U,$J,358.3,19004,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19004,1,2,0)
 ;;=2^96372
 ;;^UTILITY(U,$J,358.3,19004,1,3,0)
 ;;=3^Ther/Proph/Diag Inj, SC/IM
 ;;^UTILITY(U,$J,358.3,19005,0)
 ;;=95816^^142^1227^3^^^^1
 ;;^UTILITY(U,$J,358.3,19005,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19005,1,2,0)
 ;;=2^95816
 ;;^UTILITY(U,$J,358.3,19005,1,3,0)
 ;;=3^EEG, Awake and Drowsy
 ;;^UTILITY(U,$J,358.3,19006,0)
 ;;=95819^^142^1227^4^^^^1
 ;;^UTILITY(U,$J,358.3,19006,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19006,1,2,0)
 ;;=2^95819
 ;;^UTILITY(U,$J,358.3,19006,1,3,0)
 ;;=3^EEG, Awake and Asleep
 ;;^UTILITY(U,$J,358.3,19007,0)
 ;;=95822^^142^1227^5^^^^1
 ;;^UTILITY(U,$J,358.3,19007,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19007,1,2,0)
 ;;=2^95822
 ;;^UTILITY(U,$J,358.3,19007,1,3,0)
 ;;=3^EEG, Sleep or Coma only
 ;;^UTILITY(U,$J,358.3,19008,0)
 ;;=95827^^142^1227^6^^^^1
 ;;^UTILITY(U,$J,358.3,19008,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19008,1,2,0)
 ;;=2^95827
 ;;^UTILITY(U,$J,358.3,19008,1,3,0)
 ;;=3^EEG, all night monitor
 ;;^UTILITY(U,$J,358.3,19009,0)
 ;;=95812^^142^1227^1^^^^1
 ;;^UTILITY(U,$J,358.3,19009,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19009,1,2,0)
 ;;=2^95812
 ;;^UTILITY(U,$J,358.3,19009,1,3,0)
 ;;=3^EEG, 41-60 minutes
 ;;^UTILITY(U,$J,358.3,19010,0)
 ;;=95813^^142^1227^2^^^^1
 ;;^UTILITY(U,$J,358.3,19010,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19010,1,2,0)
 ;;=2^95813
 ;;^UTILITY(U,$J,358.3,19010,1,3,0)
 ;;=3^EEG, Over 1 hour
 ;;^UTILITY(U,$J,358.3,19011,0)
 ;;=95806^^142^1228^4^^^^1
 ;;^UTILITY(U,$J,358.3,19011,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19011,1,2,0)
 ;;=2^95806
 ;;^UTILITY(U,$J,358.3,19011,1,3,0)
 ;;=3^Sleep Study/Unattended
 ;;^UTILITY(U,$J,358.3,19012,0)
 ;;=95807^^142^1228^3^^^^1
 ;;^UTILITY(U,$J,358.3,19012,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19012,1,2,0)
 ;;=2^95807
 ;;^UTILITY(U,$J,358.3,19012,1,3,0)
 ;;=3^Sleep Study in Hosp /Clinic
 ;;^UTILITY(U,$J,358.3,19013,0)
 ;;=95805^^142^1228^1^^^^1
 ;;^UTILITY(U,$J,358.3,19013,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19013,1,2,0)
 ;;=2^95805
 ;;^UTILITY(U,$J,358.3,19013,1,3,0)
 ;;=3^Multiple Sleep Latency Test
 ;;^UTILITY(U,$J,358.3,19014,0)
 ;;=95808^^142^1228^2^^^^1
 ;;^UTILITY(U,$J,358.3,19014,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19014,1,2,0)
 ;;=2^95808
 ;;^UTILITY(U,$J,358.3,19014,1,3,0)
 ;;=3^POLYSOMNOGRAPHY, 1-3
 ;;^UTILITY(U,$J,358.3,19015,0)
 ;;=95860^^142^1229^13^^^^1
 ;;^UTILITY(U,$J,358.3,19015,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19015,1,2,0)
 ;;=2^95860
 ;;^UTILITY(U,$J,358.3,19015,1,3,0)
 ;;=3^EMG, one extremity
 ;;^UTILITY(U,$J,358.3,19016,0)
 ;;=95861^^142^1229^2^^^^1
 ;;^UTILITY(U,$J,358.3,19016,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19016,1,2,0)
 ;;=2^95861
 ;;^UTILITY(U,$J,358.3,19016,1,3,0)
 ;;=3^EMG, 2 extremities
 ;;^UTILITY(U,$J,358.3,19017,0)
 ;;=95863^^142^1229^3^^^^1
 ;;^UTILITY(U,$J,358.3,19017,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19017,1,2,0)
 ;;=2^95863
 ;;^UTILITY(U,$J,358.3,19017,1,3,0)
 ;;=3^EMG, 3 extremities
 ;;^UTILITY(U,$J,358.3,19018,0)
 ;;=95864^^142^1229^4^^^^1
 ;;^UTILITY(U,$J,358.3,19018,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19018,1,2,0)
 ;;=2^95864
 ;;^UTILITY(U,$J,358.3,19018,1,3,0)
 ;;=3^EMG, 4 extremities
 ;;^UTILITY(U,$J,358.3,19019,0)
 ;;=95869^^142^1229^12^^^^1
 ;;^UTILITY(U,$J,358.3,19019,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19019,1,2,0)
 ;;=2^95869
 ;;^UTILITY(U,$J,358.3,19019,1,3,0)
 ;;=3^EMG, Thoracic Paraspinal Muscles
 ;;^UTILITY(U,$J,358.3,19020,0)
 ;;=95867^^142^1229^7^^^^1
 ;;^UTILITY(U,$J,358.3,19020,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19020,1,2,0)
 ;;=2^95867
 ;;^UTILITY(U,$J,358.3,19020,1,3,0)
 ;;=3^EMG, Cranial Nerve supplied Muscles, unilat
 ;;^UTILITY(U,$J,358.3,19021,0)
 ;;=95868^^142^1229^1^^^^1
 ;;^UTILITY(U,$J,358.3,19021,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19021,1,2,0)
 ;;=2^95868
 ;;^UTILITY(U,$J,358.3,19021,1,3,0)
 ;;=3^EMG , Cranial Nerve Supplied muscles, bilat
 ;;^UTILITY(U,$J,358.3,19022,0)
 ;;=51785^^142^1229^5^^^^1
 ;;^UTILITY(U,$J,358.3,19022,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19022,1,2,0)
 ;;=2^51785
 ;;^UTILITY(U,$J,358.3,19022,1,3,0)
 ;;=3^EMG, Anal/Urinary Muscle
 ;;^UTILITY(U,$J,358.3,19023,0)
 ;;=51792^^142^1229^28^^^^1
 ;;^UTILITY(U,$J,358.3,19023,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19023,1,2,0)
 ;;=2^51792
 ;;^UTILITY(U,$J,358.3,19023,1,3,0)
 ;;=3^Urinary Reflex Study
 ;;^UTILITY(U,$J,358.3,19024,0)
 ;;=95865^^142^1229^9^^^^1
 ;;^UTILITY(U,$J,358.3,19024,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19024,1,2,0)
 ;;=2^95865
 ;;^UTILITY(U,$J,358.3,19024,1,3,0)
 ;;=3^EMG, Larynx
 ;;^UTILITY(U,$J,358.3,19025,0)
 ;;=95866^^142^1229^8^^^^1
 ;;^UTILITY(U,$J,358.3,19025,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19025,1,2,0)
 ;;=2^95866
 ;;^UTILITY(U,$J,358.3,19025,1,3,0)
 ;;=3^EMG, Hemidiaphragm
 ;;^UTILITY(U,$J,358.3,19026,0)
 ;;=95870^^142^1229^10^^^^1
 ;;^UTILITY(U,$J,358.3,19026,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19026,1,2,0)
 ;;=2^95870
 ;;^UTILITY(U,$J,358.3,19026,1,3,0)
 ;;=3^EMG, Limited-One extremity
 ;;^UTILITY(U,$J,358.3,19027,0)
 ;;=95872^^142^1229^11^^^^1
 ;;^UTILITY(U,$J,358.3,19027,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19027,1,2,0)
 ;;=2^95872
 ;;^UTILITY(U,$J,358.3,19027,1,3,0)
 ;;=3^EMG, Single Fiber Electrode
 ;;^UTILITY(U,$J,358.3,19028,0)
 ;;=95873^^142^1229^17^^^^1
 ;;^UTILITY(U,$J,358.3,19028,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19028,1,2,0)
 ;;=2^95873
 ;;^UTILITY(U,$J,358.3,19028,1,3,0)
 ;;=3^Elec Stim,Guide chemodenervation
 ;;^UTILITY(U,$J,358.3,19029,0)
 ;;=95874^^142^1229^27^^^^1
 ;;^UTILITY(U,$J,358.3,19029,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19029,1,2,0)
 ;;=2^95874
 ;;^UTILITY(U,$J,358.3,19029,1,3,0)
 ;;=3^Needle EMG,Guide Chemodenervation
 ;;^UTILITY(U,$J,358.3,19030,0)
 ;;=95875^^142^1229^18^^^^1
 ;;^UTILITY(U,$J,358.3,19030,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19030,1,2,0)
 ;;=2^95875
 ;;^UTILITY(U,$J,358.3,19030,1,3,0)
 ;;=3^Ischemic limb exercise test
 ;;^UTILITY(U,$J,358.3,19031,0)
 ;;=95885^^142^1229^15^^^^1
 ;;^UTILITY(U,$J,358.3,19031,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19031,1,2,0)
 ;;=2^95885
 ;;^UTILITY(U,$J,358.3,19031,1,3,0)
 ;;=3^EMG,ea extrem w/nerve conduction;limited
 ;;^UTILITY(U,$J,358.3,19032,0)
 ;;=95886^^142^1229^14^^^^1
 ;;^UTILITY(U,$J,358.3,19032,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,19032,1,2,0)
 ;;=2^95886
 ;;^UTILITY(U,$J,358.3,19032,1,3,0)
 ;;=3^EMG,>4 muscles,>2 nerves/>3 spinal levels
 ;;^UTILITY(U,$J,358.3,19033,0)
 ;;=95887^^142^1229^16^^^^1
 ;;^UTILITY(U,$J,358.3,19033,1,0)
 ;;=^358.31IA^3^2
