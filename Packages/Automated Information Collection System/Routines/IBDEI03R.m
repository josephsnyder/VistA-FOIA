IBDEI03R ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,4604,1,5,0)
 ;;=5^789.00
 ;;^UTILITY(U,$J,358.3,4604,2)
 ;;=Abdominal Pain, Unspec^303317
 ;;^UTILITY(U,$J,358.3,4605,0)
 ;;=550.90^^44^321^61
 ;;^UTILITY(U,$J,358.3,4605,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4605,1,4,0)
 ;;=4^Hernia,Inguinal Unilateral
 ;;^UTILITY(U,$J,358.3,4605,1,5,0)
 ;;=5^550.90
 ;;^UTILITY(U,$J,358.3,4605,2)
 ;;=^63302
 ;;^UTILITY(U,$J,358.3,4606,0)
 ;;=153.9^^44^321^8
 ;;^UTILITY(U,$J,358.3,4606,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4606,1,4,0)
 ;;=4^CA of Colon
 ;;^UTILITY(U,$J,358.3,4606,1,5,0)
 ;;=5^153.9
 ;;^UTILITY(U,$J,358.3,4606,2)
 ;;=CA of Colon^267078
 ;;^UTILITY(U,$J,358.3,4607,0)
 ;;=455.6^^44^321^56
 ;;^UTILITY(U,$J,358.3,4607,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4607,1,4,0)
 ;;=4^Hemorrhoids,Unspecified
 ;;^UTILITY(U,$J,358.3,4607,1,5,0)
 ;;=5^455.6
 ;;^UTILITY(U,$J,358.3,4607,2)
 ;;=^123922
 ;;^UTILITY(U,$J,358.3,4608,0)
 ;;=553.1^^44^321^62
 ;;^UTILITY(U,$J,358.3,4608,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4608,1,4,0)
 ;;=4^Hernia,Umbilical
 ;;^UTILITY(U,$J,358.3,4608,1,5,0)
 ;;=5^553.1
 ;;^UTILITY(U,$J,358.3,4608,2)
 ;;=^123475
 ;;^UTILITY(U,$J,358.3,4609,0)
 ;;=553.20^^44^321^63
 ;;^UTILITY(U,$J,358.3,4609,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4609,1,4,0)
 ;;=4^Hernia,Ventral
 ;;^UTILITY(U,$J,358.3,4609,1,5,0)
 ;;=5^553.20
 ;;^UTILITY(U,$J,358.3,4609,2)
 ;;=^123997
 ;;^UTILITY(U,$J,358.3,4610,0)
 ;;=574.20^^44^321^39
 ;;^UTILITY(U,$J,358.3,4610,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4610,1,4,0)
 ;;=4^Cholelithiasis
 ;;^UTILITY(U,$J,358.3,4610,1,5,0)
 ;;=5^574.20
 ;;^UTILITY(U,$J,358.3,4610,2)
 ;;=^18282
 ;;^UTILITY(U,$J,358.3,4611,0)
 ;;=154.1^^44^321^27
 ;;^UTILITY(U,$J,358.3,4611,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4611,1,4,0)
 ;;=4^CA of Rectum
 ;;^UTILITY(U,$J,358.3,4611,1,5,0)
 ;;=5^154.1
 ;;^UTILITY(U,$J,358.3,4611,2)
 ;;=^267090
 ;;^UTILITY(U,$J,358.3,4612,0)
 ;;=562.10^^44^321^50
 ;;^UTILITY(U,$J,358.3,4612,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4612,1,4,0)
 ;;=4^Diverticulosis
 ;;^UTILITY(U,$J,358.3,4612,1,5,0)
 ;;=5^562.10
 ;;^UTILITY(U,$J,358.3,4612,2)
 ;;=^35917
 ;;^UTILITY(U,$J,358.3,4613,0)
 ;;=562.11^^44^321^49
 ;;^UTILITY(U,$J,358.3,4613,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4613,1,4,0)
 ;;=4^Diverticulitis
 ;;^UTILITY(U,$J,358.3,4613,1,5,0)
 ;;=5^562.11
 ;;^UTILITY(U,$J,358.3,4613,2)
 ;;=^270274
 ;;^UTILITY(U,$J,358.3,4614,0)
 ;;=575.10^^44^321^38
 ;;^UTILITY(U,$J,358.3,4614,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4614,1,4,0)
 ;;=4^Cholecystitis
 ;;^UTILITY(U,$J,358.3,4614,1,5,0)
 ;;=5^575.10
 ;;^UTILITY(U,$J,358.3,4614,2)
 ;;=^23341
 ;;^UTILITY(U,$J,358.3,4615,0)
 ;;=566.^^44^321^1
 ;;^UTILITY(U,$J,358.3,4615,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4615,1,4,0)
 ;;=4^Abscess,Anal/Rectal
 ;;^UTILITY(U,$J,358.3,4615,1,5,0)
 ;;=5^566.
 ;;^UTILITY(U,$J,358.3,4615,2)
 ;;=^270285
 ;;^UTILITY(U,$J,358.3,4616,0)
 ;;=550.92^^44^321^60
 ;;^UTILITY(U,$J,358.3,4616,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4616,1,4,0)
 ;;=4^Hernia,Inguinal Bilat
 ;;^UTILITY(U,$J,358.3,4616,1,5,0)
 ;;=5^550.92
 ;;^UTILITY(U,$J,358.3,4616,2)
 ;;=^270212
 ;;^UTILITY(U,$J,358.3,4617,0)
 ;;=555.9^^44^321^46
 ;;^UTILITY(U,$J,358.3,4617,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4617,1,4,0)
 ;;=4^Crohn's/Enteritis, Unspec
 ;;^UTILITY(U,$J,358.3,4617,1,5,0)
 ;;=5^555.9
 ;;^UTILITY(U,$J,358.3,4617,2)
 ;;=^29356
 ;;^UTILITY(U,$J,358.3,4618,0)
 ;;=530.81^^44^321^52
 ;;^UTILITY(U,$J,358.3,4618,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4618,1,4,0)
 ;;=4^GERD
 ;;^UTILITY(U,$J,358.3,4618,1,5,0)
 ;;=5^530.81
 ;;^UTILITY(U,$J,358.3,4618,2)
 ;;=GERD^295749
 ;;^UTILITY(U,$J,358.3,4619,0)
 ;;=455.0^^44^321^55
 ;;^UTILITY(U,$J,358.3,4619,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4619,1,4,0)
 ;;=4^Hemorrhoid,Internal
 ;;^UTILITY(U,$J,358.3,4619,1,5,0)
 ;;=5^455.0
 ;;^UTILITY(U,$J,358.3,4619,2)
 ;;=^269822
 ;;^UTILITY(U,$J,358.3,4620,0)
 ;;=578.1^^44^321^71
 ;;^UTILITY(U,$J,358.3,4620,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4620,1,4,0)
 ;;=4^Melena
 ;;^UTILITY(U,$J,358.3,4620,1,5,0)
 ;;=5^578.1
 ;;^UTILITY(U,$J,358.3,4620,2)
 ;;=^276839
 ;;^UTILITY(U,$J,358.3,4621,0)
 ;;=556.9^^44^321^42
 ;;^UTILITY(U,$J,358.3,4621,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4621,1,4,0)
 ;;=4^Colitis,Ulcerative
 ;;^UTILITY(U,$J,358.3,4621,1,5,0)
 ;;=5^556.9
 ;;^UTILITY(U,$J,358.3,4621,2)
 ;;=^26044
 ;;^UTILITY(U,$J,358.3,4622,0)
 ;;=787.91^^44^321^48
 ;;^UTILITY(U,$J,358.3,4622,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4622,1,4,0)
 ;;=4^Diarrhea
 ;;^UTILITY(U,$J,358.3,4622,1,5,0)
 ;;=5^787.91
 ;;^UTILITY(U,$J,358.3,4622,2)
 ;;=^33921
 ;;^UTILITY(U,$J,358.3,4623,0)
 ;;=564.1^^44^321^68
 ;;^UTILITY(U,$J,358.3,4623,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4623,1,4,0)
 ;;=4^Irritable Colon
 ;;^UTILITY(U,$J,358.3,4623,1,5,0)
 ;;=5^564.1
 ;;^UTILITY(U,$J,358.3,4623,2)
 ;;=^65682
 ;;^UTILITY(U,$J,358.3,4624,0)
 ;;=530.3^^44^321^51
 ;;^UTILITY(U,$J,358.3,4624,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4624,1,4,0)
 ;;=4^Esophageal Stricture
 ;;^UTILITY(U,$J,358.3,4624,1,5,0)
 ;;=5^530.3
 ;;^UTILITY(U,$J,358.3,4624,2)
 ;;=^114760
 ;;^UTILITY(U,$J,358.3,4625,0)
 ;;=571.2^^44^321^40
 ;;^UTILITY(U,$J,358.3,4625,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4625,1,4,0)
 ;;=4^Cirrhosis,Alcohol Liver
 ;;^UTILITY(U,$J,358.3,4625,1,5,0)
 ;;=5^571.2
 ;;^UTILITY(U,$J,358.3,4625,2)
 ;;=^71505
 ;;^UTILITY(U,$J,358.3,4626,0)
 ;;=533.90^^44^321^79
 ;;^UTILITY(U,$J,358.3,4626,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4626,1,4,0)
 ;;=4^Ulcer,Peptic
 ;;^UTILITY(U,$J,358.3,4626,1,5,0)
 ;;=5^533.90
 ;;^UTILITY(U,$J,358.3,4626,2)
 ;;=^93051
 ;;^UTILITY(U,$J,358.3,4627,0)
 ;;=553.3^^44^321^58
 ;;^UTILITY(U,$J,358.3,4627,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4627,1,4,0)
 ;;=4^Hernia,Hiatal
 ;;^UTILITY(U,$J,358.3,4627,1,5,0)
 ;;=5^553.3
 ;;^UTILITY(U,$J,358.3,4627,2)
 ;;=^33903
 ;;^UTILITY(U,$J,358.3,4628,0)
 ;;=153.5^^44^321^5
 ;;^UTILITY(U,$J,358.3,4628,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4628,1,4,0)
 ;;=4^CA of Appendix
 ;;^UTILITY(U,$J,358.3,4628,1,5,0)
 ;;=5^153.5
 ;;^UTILITY(U,$J,358.3,4628,2)
 ;;=CA of Appendix^267084
 ;;^UTILITY(U,$J,358.3,4629,0)
 ;;=153.4^^44^321^17
 ;;^UTILITY(U,$J,358.3,4629,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4629,1,4,0)
 ;;=4^CA of Ileocecal Valve
 ;;^UTILITY(U,$J,358.3,4629,1,5,0)
 ;;=5^153.4
 ;;^UTILITY(U,$J,358.3,4629,2)
 ;;=CA of Ileocecal Valve^267083
 ;;^UTILITY(U,$J,358.3,4630,0)
 ;;=154.0^^44^321^9
 ;;^UTILITY(U,$J,358.3,4630,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4630,1,4,0)
 ;;=4^CA of Colon and Rectum
 ;;^UTILITY(U,$J,358.3,4630,1,5,0)
 ;;=5^154.0
 ;;^UTILITY(U,$J,358.3,4630,2)
 ;;=CA of Colon and Rectum^267089
 ;;^UTILITY(U,$J,358.3,4631,0)
 ;;=153.6^^44^321^6
 ;;^UTILITY(U,$J,358.3,4631,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4631,1,4,0)
 ;;=4^CA of Ascending Colon
 ;;^UTILITY(U,$J,358.3,4631,1,5,0)
 ;;=5^153.6
 ;;^UTILITY(U,$J,358.3,4631,2)
 ;;=CA of Ascending Colon^267085
 ;;^UTILITY(U,$J,358.3,4632,0)
 ;;=153.8^^44^321^12
 ;;^UTILITY(U,$J,358.3,4632,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4632,1,4,0)
 ;;=4^CA of Colon/Contiguous Sites
 ;;^UTILITY(U,$J,358.3,4632,1,5,0)
 ;;=5^153.8
 ;;^UTILITY(U,$J,358.3,4632,2)
 ;;=CA of Colon/Contiguous Sites^267087
 ;;^UTILITY(U,$J,358.3,4633,0)
 ;;=153.2^^44^321^13
 ;;^UTILITY(U,$J,358.3,4633,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4633,1,4,0)
 ;;=4^CA of Descending Colon
 ;;^UTILITY(U,$J,358.3,4633,1,5,0)
 ;;=5^153.2
 ;;^UTILITY(U,$J,358.3,4633,2)
 ;;=CA of Descending Colon^267081
 ;;^UTILITY(U,$J,358.3,4634,0)
 ;;=153.3^^44^321^35
 ;;^UTILITY(U,$J,358.3,4634,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4634,1,4,0)
 ;;=4^Ca of Sigmoid Colon
 ;;^UTILITY(U,$J,358.3,4634,1,5,0)
 ;;=5^153.3
 ;;^UTILITY(U,$J,358.3,4634,2)
 ;;=Ca of Sigmoid Colon^267082
 ;;^UTILITY(U,$J,358.3,4635,0)
 ;;=153.1^^44^321^32
 ;;^UTILITY(U,$J,358.3,4635,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4635,1,4,0)
 ;;=4^CA of Transverse Colon
 ;;^UTILITY(U,$J,358.3,4635,1,5,0)
 ;;=5^153.1
 ;;^UTILITY(U,$J,358.3,4635,2)
 ;;=CA of Transverse Colon^267080
 ;;^UTILITY(U,$J,358.3,4636,0)
 ;;=153.0^^44^321^11
 ;;^UTILITY(U,$J,358.3,4636,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4636,1,4,0)
 ;;=4^CA of Colon, Hepatic Flexure.
 ;;^UTILITY(U,$J,358.3,4636,1,5,0)
 ;;=5^153.0
 ;;^UTILITY(U,$J,358.3,4636,2)
 ;;=CA of Colon at Hepatic Flexure^267079
 ;;^UTILITY(U,$J,358.3,4637,0)
 ;;=153.7^^44^321^10
 ;;^UTILITY(U,$J,358.3,4637,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4637,1,4,0)
 ;;=4^CA of Colon at Splenic Flexure
 ;;^UTILITY(U,$J,358.3,4637,1,5,0)
 ;;=5^153.7
 ;;^UTILITY(U,$J,358.3,4637,2)
 ;;=CA of Colon at Splenic Flexure^267086
 ;;^UTILITY(U,$J,358.3,4638,0)
 ;;=151.9^^44^321^28
 ;;^UTILITY(U,$J,358.3,4638,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4638,1,4,0)
 ;;=4^CA of Stomach
 ;;^UTILITY(U,$J,358.3,4638,1,5,0)
 ;;=5^151.9
 ;;^UTILITY(U,$J,358.3,4638,2)
 ;;=CA of Stomach^73532
 ;;^UTILITY(U,$J,358.3,4639,0)
 ;;=151.2^^44^321^4
 ;;^UTILITY(U,$J,358.3,4639,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4639,1,4,0)
 ;;=4^CA of Antrum of Stomach
 ;;^UTILITY(U,$J,358.3,4639,1,5,0)
 ;;=5^151.2
 ;;^UTILITY(U,$J,358.3,4639,2)
 ;;=CA of Antrum of Stomach^267065
 ;;^UTILITY(U,$J,358.3,4640,0)
 ;;=151.4^^44^321^29
 ;;^UTILITY(U,$J,358.3,4640,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4640,1,4,0)
 ;;=4^CA of Stomach Body
 ;;^UTILITY(U,$J,358.3,4640,1,5,0)
 ;;=5^151.4
 ;;^UTILITY(U,$J,358.3,4640,2)
 ;;=CA of Stomach Body^267067
 ;;^UTILITY(U,$J,358.3,4641,0)
 ;;=151.0^^44^321^30
 ;;^UTILITY(U,$J,358.3,4641,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4641,1,4,0)
 ;;=4^CA of Stomach Cardia
 ;;^UTILITY(U,$J,358.3,4641,1,5,0)
 ;;=5^151.0
 ;;^UTILITY(U,$J,358.3,4641,2)
 ;;=CA of Stomach Cardia^267063
 ;;^UTILITY(U,$J,358.3,4642,0)
 ;;=151.3^^44^321^15
