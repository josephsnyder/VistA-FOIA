IBDEI013 ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,810,0)
 ;;=369.4^^13^64^35
 ;;^UTILITY(U,$J,358.3,810,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,810,1,3,0)
 ;;=3^369.4
 ;;^UTILITY(U,$J,358.3,810,1,4,0)
 ;;=4^Legal Blindness (USA definition)
 ;;^UTILITY(U,$J,358.3,810,2)
 ;;=^268887
 ;;^UTILITY(U,$J,358.3,811,0)
 ;;=369.01^^13^64^25
 ;;^UTILITY(U,$J,358.3,811,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,811,1,3,0)
 ;;=3^369.01
 ;;^UTILITY(U,$J,358.3,811,1,4,0)
 ;;=4^BE:NLP; LE:NLP
 ;;^UTILITY(U,$J,358.3,811,2)
 ;;=Totally Blind, NLP or LPO^268861
 ;;^UTILITY(U,$J,358.3,812,0)
 ;;=369.02^^13^64^22
 ;;^UTILITY(U,$J,358.3,812,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,812,1,3,0)
 ;;=3^369.02
 ;;^UTILITY(U,$J,358.3,812,1,4,0)
 ;;=4^BE:CF,HM,LP,VF<5;LE:NOS
 ;;^UTILITY(U,$J,358.3,812,2)
 ;;=^268862
 ;;^UTILITY(U,$J,358.3,813,0)
 ;;=369.05^^13^64^18
 ;;^UTILITY(U,$J,358.3,813,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,813,1,3,0)
 ;;=3^369.05
 ;;^UTILITY(U,$J,358.3,813,1,4,0)
 ;;=4^BE:<10 or 20/500-20/1000;LE:Not Specified
 ;;^UTILITY(U,$J,358.3,813,2)
 ;;=^268865
 ;;^UTILITY(U,$J,358.3,814,0)
 ;;=369.11^^13^64^19
 ;;^UTILITY(U,$J,358.3,814,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,814,1,3,0)
 ;;=3^369.11
 ;;^UTILITY(U,$J,358.3,814,1,4,0)
 ;;=4^BE:<20 or 20/200-20/400;LE:Not Specified
 ;;^UTILITY(U,$J,358.3,814,2)
 ;;=^268871
 ;;^UTILITY(U,$J,358.3,815,0)
 ;;=369.16^^13^64^16
 ;;^UTILITY(U,$J,358.3,815,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,815,1,3,0)
 ;;=3^369.16
 ;;^UTILITY(U,$J,358.3,815,1,4,0)
 ;;=4^BE:20/70-20/160;LE:NLP
 ;;^UTILITY(U,$J,358.3,815,2)
 ;;=^268876
 ;;^UTILITY(U,$J,358.3,816,0)
 ;;=369.24^^13^64^13
 ;;^UTILITY(U,$J,358.3,816,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,816,1,3,0)
 ;;=3^369.24
 ;;^UTILITY(U,$J,358.3,816,1,4,0)
 ;;=4^BE:20/70-20/160;LE:20/200-20/400,VF<20
 ;;^UTILITY(U,$J,358.3,816,2)
 ;;=^268884
 ;;^UTILITY(U,$J,358.3,817,0)
 ;;=369.25^^13^64^17
 ;;^UTILITY(U,$J,358.3,817,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,817,1,3,0)
 ;;=3^369.25
 ;;^UTILITY(U,$J,358.3,817,1,4,0)
 ;;=4^BE:20/70-20/160;LE:Same
 ;;^UTILITY(U,$J,358.3,817,2)
 ;;=^268885
 ;;^UTILITY(U,$J,358.3,818,0)
 ;;=369.03^^13^64^21
 ;;^UTILITY(U,$J,358.3,818,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,818,1,3,0)
 ;;=3^369.03
 ;;^UTILITY(U,$J,358.3,818,1,4,0)
 ;;=4^BE:CF,HM,LP,VF<5;LE:NLP
 ;;^UTILITY(U,$J,358.3,818,2)
 ;;=^268863
 ;;^UTILITY(U,$J,358.3,819,0)
 ;;=369.04^^13^64^23
 ;;^UTILITY(U,$J,358.3,819,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,819,1,3,0)
 ;;=3^369.04
 ;;^UTILITY(U,$J,358.3,819,1,4,0)
 ;;=4^BE:CF,HM,LP,VF<5;LE:Same
 ;;^UTILITY(U,$J,358.3,819,2)
 ;;=^268864
 ;;^UTILITY(U,$J,358.3,820,0)
 ;;=369.06^^13^64^11
 ;;^UTILITY(U,$J,358.3,820,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,820,1,3,0)
 ;;=3^369.06
 ;;^UTILITY(U,$J,358.3,820,1,4,0)
 ;;=4^BE:20/500-20/1000,VF<10;LE:NLP
 ;;^UTILITY(U,$J,358.3,820,2)
 ;;=^268866
 ;;^UTILITY(U,$J,358.3,821,0)
 ;;=369.07^^13^64^10
 ;;^UTILITY(U,$J,358.3,821,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,821,1,3,0)
 ;;=3^369.07
 ;;^UTILITY(U,$J,358.3,821,1,4,0)
 ;;=4^BE:20/500-20/1000,VF<10;LE:CF,HM,LP,VF<5
 ;;^UTILITY(U,$J,358.3,821,2)
 ;;=^268867
 ;;^UTILITY(U,$J,358.3,822,0)
 ;;=369.08^^13^64^12
 ;;^UTILITY(U,$J,358.3,822,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,822,1,3,0)
 ;;=3^369.08
 ;;^UTILITY(U,$J,358.3,822,1,4,0)
 ;;=4^BE:20/500-20/1000,VF<10;LE:Same
 ;;^UTILITY(U,$J,358.3,822,2)
 ;;=^268868
 ;;^UTILITY(U,$J,358.3,823,0)
 ;;=369.12^^13^64^8
 ;;^UTILITY(U,$J,358.3,823,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,823,1,3,0)
 ;;=3^369.12
 ;;^UTILITY(U,$J,358.3,823,1,4,0)
 ;;=4^BE:20/200-20/400,VF<20;LE:NLP
 ;;^UTILITY(U,$J,358.3,823,2)
 ;;=^268872
 ;;^UTILITY(U,$J,358.3,824,0)
 ;;=369.13^^13^64^7
 ;;^UTILITY(U,$J,358.3,824,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,824,1,3,0)
 ;;=3^369.13
 ;;^UTILITY(U,$J,358.3,824,1,4,0)
 ;;=4^BE:20/200-20/400,VF<20;LE:CF,HM,LP,VF<5
 ;;^UTILITY(U,$J,358.3,824,2)
 ;;=^268873
 ;;^UTILITY(U,$J,358.3,825,0)
 ;;=369.14^^13^64^6
 ;;^UTILITY(U,$J,358.3,825,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,825,1,3,0)
 ;;=3^369.14
 ;;^UTILITY(U,$J,358.3,825,1,4,0)
 ;;=4^BE:20/200-20/400,VF<20;LE:20/500-20/1000,VF<10
 ;;^UTILITY(U,$J,358.3,825,2)
 ;;=^268874
 ;;^UTILITY(U,$J,358.3,826,0)
 ;;=369.22^^13^64^9
 ;;^UTILITY(U,$J,358.3,826,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,826,1,3,0)
 ;;=3^369.22
 ;;^UTILITY(U,$J,358.3,826,1,4,0)
 ;;=4^BE:20/200-20/400,VF<20;LE:Same
 ;;^UTILITY(U,$J,358.3,826,2)
 ;;=^268882
 ;;^UTILITY(U,$J,358.3,827,0)
 ;;=369.17^^13^64^15
 ;;^UTILITY(U,$J,358.3,827,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,827,1,3,0)
 ;;=3^369.17
 ;;^UTILITY(U,$J,358.3,827,1,4,0)
 ;;=4^BE:20/70-20/160;LE:CF,HM,LP,VF<5
 ;;^UTILITY(U,$J,358.3,827,2)
 ;;=^268877
 ;;^UTILITY(U,$J,358.3,828,0)
 ;;=369.18^^13^64^14
 ;;^UTILITY(U,$J,358.3,828,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,828,1,3,0)
 ;;=3^369.18
 ;;^UTILITY(U,$J,358.3,828,1,4,0)
 ;;=4^BE:20/70-20/160;LE:20/500-20/1000,VF<10
 ;;^UTILITY(U,$J,358.3,828,2)
 ;;=^268878
 ;;^UTILITY(U,$J,358.3,829,0)
 ;;=368.41^^13^64^27
 ;;^UTILITY(U,$J,358.3,829,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,829,1,3,0)
 ;;=3^368.41
 ;;^UTILITY(U,$J,358.3,829,1,4,0)
 ;;=4^Central Scotoma
 ;;^UTILITY(U,$J,358.3,829,2)
 ;;=^265366
 ;;^UTILITY(U,$J,358.3,830,0)
 ;;=368.44^^13^64^36
 ;;^UTILITY(U,$J,358.3,830,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,830,1,3,0)
 ;;=3^368.44
 ;;^UTILITY(U,$J,358.3,830,1,4,0)
 ;;=4^Localized Visual Field Defect
 ;;^UTILITY(U,$J,358.3,830,2)
 ;;=^87688
 ;;^UTILITY(U,$J,358.3,831,0)
 ;;=368.45^^13^64^32
 ;;^UTILITY(U,$J,358.3,831,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,831,1,3,0)
 ;;=3^368.45
 ;;^UTILITY(U,$J,358.3,831,1,4,0)
 ;;=4^Field Defect-Contract/Constrict
 ;;^UTILITY(U,$J,358.3,831,2)
 ;;=^268846
 ;;^UTILITY(U,$J,358.3,832,0)
 ;;=368.30^^13^64^26
 ;;^UTILITY(U,$J,358.3,832,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,832,1,3,0)
 ;;=3^368.30
 ;;^UTILITY(U,$J,358.3,832,1,4,0)
 ;;=4^Binocular Vision Disorder, NOS
 ;;^UTILITY(U,$J,358.3,832,2)
 ;;=^14407
 ;;^UTILITY(U,$J,358.3,833,0)
 ;;=368.31^^13^64^41
 ;;^UTILITY(U,$J,358.3,833,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,833,1,3,0)
 ;;=3^368.31
 ;;^UTILITY(U,$J,358.3,833,1,4,0)
 ;;=4^Suppression of Binocular Vision
 ;;^UTILITY(U,$J,358.3,833,2)
 ;;=^265391
 ;;^UTILITY(U,$J,358.3,834,0)
 ;;=368.32^^13^64^40
 ;;^UTILITY(U,$J,358.3,834,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,834,1,3,0)
 ;;=3^368.32
 ;;^UTILITY(U,$J,358.3,834,1,4,0)
 ;;=4^Simultaneous Vis Percept w/o Fusion
 ;;^UTILITY(U,$J,358.3,834,2)
 ;;=^268841
 ;;^UTILITY(U,$J,358.3,835,0)
 ;;=368.34^^13^64^2
 ;;^UTILITY(U,$J,358.3,835,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,835,1,3,0)
 ;;=3^368.34
 ;;^UTILITY(U,$J,358.3,835,1,4,0)
 ;;=4^Abnormal Retinal Correspondence
 ;;^UTILITY(U,$J,358.3,835,2)
 ;;=^268844
 ;;^UTILITY(U,$J,358.3,836,0)
 ;;=368.40^^13^64^46
 ;;^UTILITY(U,$J,358.3,836,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,836,1,3,0)
 ;;=3^368.40
 ;;^UTILITY(U,$J,358.3,836,1,4,0)
 ;;=4^Visual Field Defect, NOS
 ;;^UTILITY(U,$J,358.3,836,2)
 ;;=^126859
 ;;^UTILITY(U,$J,358.3,837,0)
 ;;=368.60^^13^64^37
 ;;^UTILITY(U,$J,358.3,837,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,837,1,3,0)
 ;;=3^368.60
 ;;^UTILITY(U,$J,358.3,837,1,4,0)
 ;;=4^Night Blindness, NOS
 ;;^UTILITY(U,$J,358.3,837,2)
 ;;=^83350
 ;;^UTILITY(U,$J,358.3,838,0)
 ;;=368.62^^13^64^4
 ;;^UTILITY(U,$J,358.3,838,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,838,1,3,0)
 ;;=3^368.62
 ;;^UTILITY(U,$J,358.3,838,1,4,0)
 ;;=4^Acquired Night Blindness
 ;;^UTILITY(U,$J,358.3,838,2)
 ;;=^265401
 ;;^UTILITY(U,$J,358.3,839,0)
 ;;=368.63^^13^64^1
 ;;^UTILITY(U,$J,358.3,839,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,839,1,3,0)
 ;;=3^368.63
 ;;^UTILITY(U,$J,358.3,839,1,4,0)
 ;;=4^Abnormal Dark Adaptation Curve
 ;;^UTILITY(U,$J,358.3,839,2)
 ;;=^268858
 ;;^UTILITY(U,$J,358.3,840,0)
 ;;=368.69^^13^64^38
 ;;^UTILITY(U,$J,358.3,840,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,840,1,3,0)
 ;;=3^368.69
 ;;^UTILITY(U,$J,358.3,840,1,4,0)
 ;;=4^Other Night Blindness
 ;;^UTILITY(U,$J,358.3,840,2)
 ;;=^87726
 ;;^UTILITY(U,$J,358.3,841,0)
 ;;=378.83^^13^64^29
 ;;^UTILITY(U,$J,358.3,841,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,841,1,3,0)
 ;;=3^378.83
 ;;^UTILITY(U,$J,358.3,841,1,4,0)
 ;;=4^Convergence Insufficiency/Palsy
 ;;^UTILITY(U,$J,358.3,841,2)
 ;;=^269277
 ;;^UTILITY(U,$J,358.3,842,0)
 ;;=378.84^^13^64^28
 ;;^UTILITY(U,$J,358.3,842,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,842,1,3,0)
 ;;=3^378.84
 ;;^UTILITY(U,$J,358.3,842,1,4,0)
 ;;=4^Convergence Excess or Spasm
 ;;^UTILITY(U,$J,358.3,842,2)
 ;;=^269278
 ;;^UTILITY(U,$J,358.3,843,0)
 ;;=379.58^^13^64^30
 ;;^UTILITY(U,$J,358.3,843,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,843,1,3,0)
 ;;=3^379.58
 ;;^UTILITY(U,$J,358.3,843,1,4,0)
 ;;=4^Deficiencies of Smooth Pursuit Movements
 ;;^UTILITY(U,$J,358.3,843,2)
 ;;=^269329
 ;;^UTILITY(U,$J,358.3,844,0)
 ;;=379.57^^13^64^39
 ;;^UTILITY(U,$J,358.3,844,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,844,1,3,0)
 ;;=3^379.57
 ;;^UTILITY(U,$J,358.3,844,1,4,0)
 ;;=4^Saccadic Dysfunction
 ;;^UTILITY(U,$J,358.3,844,2)
 ;;=^269327
 ;;^UTILITY(U,$J,358.3,845,0)
 ;;=368.2^^13^64^31
 ;;^UTILITY(U,$J,358.3,845,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,845,1,3,0)
 ;;=3^368.2
 ;;^UTILITY(U,$J,358.3,845,1,4,0)
 ;;=4^Diplopia
 ;;^UTILITY(U,$J,358.3,845,2)
 ;;=^35208
 ;;^UTILITY(U,$J,358.3,846,0)
 ;;=368.13^^13^64^44
 ;;^UTILITY(U,$J,358.3,846,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,846,1,3,0)
 ;;=3^368.13
 ;;^UTILITY(U,$J,358.3,846,1,4,0)
 ;;=4^Visual Discomfort
 ;;^UTILITY(U,$J,358.3,846,2)
 ;;=^126851
 ;;^UTILITY(U,$J,358.3,847,0)
 ;;=368.15^^13^64^45
 ;;^UTILITY(U,$J,358.3,847,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,847,1,3,0)
 ;;=3^368.15
 ;;^UTILITY(U,$J,358.3,847,1,4,0)
 ;;=4^Visual Distortions
