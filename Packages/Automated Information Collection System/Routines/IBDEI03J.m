IBDEI03J ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,4289,0)
 ;;=11640^^43^292^1^^^^1
 ;;^UTILITY(U,$J,358.3,4289,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4289,1,2,0)
 ;;=2^Malig Excision < 0.5 cm or less
 ;;^UTILITY(U,$J,358.3,4289,1,4,0)
 ;;=4^11640
 ;;^UTILITY(U,$J,358.3,4290,0)
 ;;=11641^^43^292^1^^^^1
 ;;^UTILITY(U,$J,358.3,4290,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4290,1,2,0)
 ;;=2^Malig Excision 0.6-1.0 cm
 ;;^UTILITY(U,$J,358.3,4290,1,4,0)
 ;;=4^11641
 ;;^UTILITY(U,$J,358.3,4291,0)
 ;;=11642^^43^292^2^^^^1
 ;;^UTILITY(U,$J,358.3,4291,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4291,1,2,0)
 ;;=2^Malig Excision 1.1-2.0 cm
 ;;^UTILITY(U,$J,358.3,4291,1,4,0)
 ;;=4^11642
 ;;^UTILITY(U,$J,358.3,4292,0)
 ;;=11643^^43^292^3^^^^1
 ;;^UTILITY(U,$J,358.3,4292,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4292,1,2,0)
 ;;=2^Malig Excision 2.1-3.0 cm
 ;;^UTILITY(U,$J,358.3,4292,1,4,0)
 ;;=4^11643
 ;;^UTILITY(U,$J,358.3,4293,0)
 ;;=11644^^43^292^4^^^^1
 ;;^UTILITY(U,$J,358.3,4293,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4293,1,2,0)
 ;;=2^Malig Excision 3.1-4.0 cm
 ;;^UTILITY(U,$J,358.3,4293,1,4,0)
 ;;=4^11644
 ;;^UTILITY(U,$J,358.3,4294,0)
 ;;=11646^^43^292^6^^^^1
 ;;^UTILITY(U,$J,358.3,4294,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4294,1,2,0)
 ;;=2^Malig Excision > 4.0 cm
 ;;^UTILITY(U,$J,358.3,4294,1,4,0)
 ;;=4^11646
 ;;^UTILITY(U,$J,358.3,4295,0)
 ;;=11620^^43^293^1^^^^1
 ;;^UTILITY(U,$J,358.3,4295,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4295,1,2,0)
 ;;=2^Malig Excision < 0.5 cm or less
 ;;^UTILITY(U,$J,358.3,4295,1,4,0)
 ;;=4^11620
 ;;^UTILITY(U,$J,358.3,4296,0)
 ;;=11621^^43^293^2^^^^1
 ;;^UTILITY(U,$J,358.3,4296,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4296,1,2,0)
 ;;=2^Malig Excision 0.6-1.0 cm
 ;;^UTILITY(U,$J,358.3,4296,1,4,0)
 ;;=4^11621
 ;;^UTILITY(U,$J,358.3,4297,0)
 ;;=11622^^43^293^3^^^^1
 ;;^UTILITY(U,$J,358.3,4297,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4297,1,2,0)
 ;;=2^Malig Excision 1.1-2.0 cm
 ;;^UTILITY(U,$J,358.3,4297,1,4,0)
 ;;=4^11622
 ;;^UTILITY(U,$J,358.3,4298,0)
 ;;=11623^^43^293^4^^^^1
 ;;^UTILITY(U,$J,358.3,4298,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4298,1,2,0)
 ;;=2^Malig Excision 2.1-3.0 cm
 ;;^UTILITY(U,$J,358.3,4298,1,4,0)
 ;;=4^11623
 ;;^UTILITY(U,$J,358.3,4299,0)
 ;;=11624^^43^293^5^^^^1
 ;;^UTILITY(U,$J,358.3,4299,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4299,1,2,0)
 ;;=2^Malig Excision 3.1-4.0 cm
 ;;^UTILITY(U,$J,358.3,4299,1,4,0)
 ;;=4^11624
 ;;^UTILITY(U,$J,358.3,4300,0)
 ;;=11626^^43^293^6^^^^1
 ;;^UTILITY(U,$J,358.3,4300,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4300,1,2,0)
 ;;=2^Malig Excision > 4.0 cm
 ;;^UTILITY(U,$J,358.3,4300,1,4,0)
 ;;=4^11626
 ;;^UTILITY(U,$J,358.3,4301,0)
 ;;=11600^^43^294^1^^^^1
 ;;^UTILITY(U,$J,358.3,4301,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4301,1,2,0)
 ;;=2^Malig Excision, Trunk/Arm/Leg, <0.5cm
 ;;^UTILITY(U,$J,358.3,4301,1,4,0)
 ;;=4^11600
 ;;^UTILITY(U,$J,358.3,4302,0)
 ;;=11601^^43^294^2^^^^1
 ;;^UTILITY(U,$J,358.3,4302,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4302,1,2,0)
 ;;=2^Malig Excision, Trunk/Arm/Leg, 0.6-1.0 cm 
 ;;^UTILITY(U,$J,358.3,4302,1,4,0)
 ;;=4^11601
 ;;^UTILITY(U,$J,358.3,4303,0)
 ;;=11602^^43^294^3^^^^1
 ;;^UTILITY(U,$J,358.3,4303,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4303,1,2,0)
 ;;=2^Malig Excision, Trunk/Arm/Leg, 1.1-2.0 cm
 ;;^UTILITY(U,$J,358.3,4303,1,4,0)
 ;;=4^11602
 ;;^UTILITY(U,$J,358.3,4304,0)
 ;;=11603^^43^294^4^^^^1
 ;;^UTILITY(U,$J,358.3,4304,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4304,1,2,0)
 ;;=2^Malig Excision, Trunk/Arm/Leg, 2.1-3.0cm
 ;;^UTILITY(U,$J,358.3,4304,1,4,0)
 ;;=4^11603
 ;;^UTILITY(U,$J,358.3,4305,0)
 ;;=11604^^43^294^5^^^^1
 ;;^UTILITY(U,$J,358.3,4305,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4305,1,2,0)
 ;;=2^Malig Excision, Trunk/Arm/Leg, 3.1-4.0 cm
 ;;^UTILITY(U,$J,358.3,4305,1,4,0)
 ;;=4^11604
 ;;^UTILITY(U,$J,358.3,4306,0)
 ;;=11606^^43^294^6^^^^1
 ;;^UTILITY(U,$J,358.3,4306,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4306,1,2,0)
 ;;=2^Malig Excision, Trunk/Arm/Leg, >4.0
 ;;^UTILITY(U,$J,358.3,4306,1,4,0)
 ;;=4^11606
 ;;^UTILITY(U,$J,358.3,4307,0)
 ;;=12001^^43^295^1^^^^1
 ;;^UTILITY(U,$J,358.3,4307,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4307,1,2,0)
 ;;=2^Repair, Simple < 2.5cm or less
 ;;^UTILITY(U,$J,358.3,4307,1,4,0)
 ;;=4^12001
 ;;^UTILITY(U,$J,358.3,4308,0)
 ;;=12002^^43^295^2^^^^1
 ;;^UTILITY(U,$J,358.3,4308,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4308,1,2,0)
 ;;=2^Repair, Simple, 2.6-7.5cm
 ;;^UTILITY(U,$J,358.3,4308,1,4,0)
 ;;=4^12002
 ;;^UTILITY(U,$J,358.3,4309,0)
 ;;=12004^^43^295^3^^^^1
 ;;^UTILITY(U,$J,358.3,4309,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4309,1,2,0)
 ;;=2^Repair, Simple, 7.6-12.5cm
 ;;^UTILITY(U,$J,358.3,4309,1,4,0)
 ;;=4^12004
 ;;^UTILITY(U,$J,358.3,4310,0)
 ;;=12005^^43^295^4^^^^1
 ;;^UTILITY(U,$J,358.3,4310,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4310,1,2,0)
 ;;=2^Repair, Simple, 12.6-20.0cm
 ;;^UTILITY(U,$J,358.3,4310,1,4,0)
 ;;=4^12005
 ;;^UTILITY(U,$J,358.3,4311,0)
 ;;=12006^^43^295^5^^^^1
 ;;^UTILITY(U,$J,358.3,4311,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4311,1,2,0)
 ;;=2^Repair, Simple, 20.1-30.0cm
 ;;^UTILITY(U,$J,358.3,4311,1,4,0)
 ;;=4^12006
 ;;^UTILITY(U,$J,358.3,4312,0)
 ;;=12007^^43^295^6^^^^1
 ;;^UTILITY(U,$J,358.3,4312,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4312,1,2,0)
 ;;=2^Repair, Simple, >30cm
 ;;^UTILITY(U,$J,358.3,4312,1,4,0)
 ;;=4^12007
 ;;^UTILITY(U,$J,358.3,4313,0)
 ;;=12011^^43^296^1^^^^1
 ;;^UTILITY(U,$J,358.3,4313,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4313,1,2,0)
 ;;=2^Repair Simple 2.5 cm or less
 ;;^UTILITY(U,$J,358.3,4313,1,4,0)
 ;;=4^12011
 ;;^UTILITY(U,$J,358.3,4314,0)
 ;;=12013^^43^296^2^^^^1
 ;;^UTILITY(U,$J,358.3,4314,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4314,1,2,0)
 ;;=2^Repair Simple 2.6-5.0 cm
 ;;^UTILITY(U,$J,358.3,4314,1,4,0)
 ;;=4^12013
 ;;^UTILITY(U,$J,358.3,4315,0)
 ;;=12014^^43^296^3^^^^1
 ;;^UTILITY(U,$J,358.3,4315,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4315,1,2,0)
 ;;=2^Repair Simple 5.1-7.5 cm
 ;;^UTILITY(U,$J,358.3,4315,1,4,0)
 ;;=4^12014
 ;;^UTILITY(U,$J,358.3,4316,0)
 ;;=12015^^43^296^4^^^^1
 ;;^UTILITY(U,$J,358.3,4316,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4316,1,2,0)
 ;;=2^Repair Simple 7.6-12.5 cm
 ;;^UTILITY(U,$J,358.3,4316,1,4,0)
 ;;=4^12015
 ;;^UTILITY(U,$J,358.3,4317,0)
 ;;=12016^^43^296^5^^^^1
 ;;^UTILITY(U,$J,358.3,4317,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4317,1,2,0)
 ;;=2^Repair Simple 12.6-20.0 cm
 ;;^UTILITY(U,$J,358.3,4317,1,4,0)
 ;;=4^12016
 ;;^UTILITY(U,$J,358.3,4318,0)
 ;;=12017^^43^296^6^^^^1
 ;;^UTILITY(U,$J,358.3,4318,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4318,1,2,0)
 ;;=2^Repair Simple 20.1-30.0 cm
 ;;^UTILITY(U,$J,358.3,4318,1,4,0)
 ;;=4^12017
 ;;^UTILITY(U,$J,358.3,4319,0)
 ;;=12018^^43^296^7^^^^1
 ;;^UTILITY(U,$J,358.3,4319,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4319,1,2,0)
 ;;=2^Repair Simple > 30 cm
 ;;^UTILITY(U,$J,358.3,4319,1,4,0)
 ;;=4^12018
 ;;^UTILITY(U,$J,358.3,4320,0)
 ;;=12020^^43^296^8^^^^1
 ;;^UTILITY(U,$J,358.3,4320,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4320,1,2,0)
 ;;=2^Rx wound dehisc,simple close
 ;;^UTILITY(U,$J,358.3,4320,1,4,0)
 ;;=4^12020
 ;;^UTILITY(U,$J,358.3,4321,0)
 ;;=12021^^43^296^9^^^^1
 ;;^UTILITY(U,$J,358.3,4321,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4321,1,2,0)
 ;;=2^Rx wound dehisc, w/ packing
 ;;^UTILITY(U,$J,358.3,4321,1,4,0)
 ;;=4^12021
 ;;^UTILITY(U,$J,358.3,4322,0)
 ;;=12031^^43^297^1^^^^1
 ;;^UTILITY(U,$J,358.3,4322,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4322,1,2,0)
 ;;=2^Repair Intermediate 2.5 cm or less
 ;;^UTILITY(U,$J,358.3,4322,1,4,0)
 ;;=4^12031
 ;;^UTILITY(U,$J,358.3,4323,0)
 ;;=12032^^43^297^2^^^^1
 ;;^UTILITY(U,$J,358.3,4323,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4323,1,2,0)
 ;;=2^Repair Intermediate 2.6-7.5 cm
 ;;^UTILITY(U,$J,358.3,4323,1,4,0)
 ;;=4^12032
 ;;^UTILITY(U,$J,358.3,4324,0)
 ;;=12034^^43^297^3^^^^1
 ;;^UTILITY(U,$J,358.3,4324,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4324,1,2,0)
 ;;=2^Repair Intermediate 7.6-12.5 cm
 ;;^UTILITY(U,$J,358.3,4324,1,4,0)
 ;;=4^12034
 ;;^UTILITY(U,$J,358.3,4325,0)
 ;;=12035^^43^297^4^^^^1
 ;;^UTILITY(U,$J,358.3,4325,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4325,1,2,0)
 ;;=2^Repair Intermediate 12.6-20.0 cm
 ;;^UTILITY(U,$J,358.3,4325,1,4,0)
 ;;=4^12035
 ;;^UTILITY(U,$J,358.3,4326,0)
 ;;=12036^^43^297^5^^^^1
 ;;^UTILITY(U,$J,358.3,4326,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4326,1,2,0)
 ;;=2^Repair Intermediate 20.1-30.0 cm
 ;;^UTILITY(U,$J,358.3,4326,1,4,0)
 ;;=4^12036
 ;;^UTILITY(U,$J,358.3,4327,0)
 ;;=12037^^43^297^6^^^^1
 ;;^UTILITY(U,$J,358.3,4327,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4327,1,2,0)
 ;;=2^Repair Intermediate > 30 cm
 ;;^UTILITY(U,$J,358.3,4327,1,4,0)
 ;;=4^12037
 ;;^UTILITY(U,$J,358.3,4328,0)
 ;;=12051^^43^298^1^^^^1
 ;;^UTILITY(U,$J,358.3,4328,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4328,1,2,0)
 ;;=2^Repair, Intermediate < 2.5 cm or less
 ;;^UTILITY(U,$J,358.3,4328,1,4,0)
 ;;=4^12051
 ;;^UTILITY(U,$J,358.3,4329,0)
 ;;=12052^^43^298^2^^^^1
 ;;^UTILITY(U,$J,358.3,4329,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4329,1,2,0)
 ;;=2^Repair, Intermediate 1.6-5.0 cm
 ;;^UTILITY(U,$J,358.3,4329,1,4,0)
 ;;=4^12052
 ;;^UTILITY(U,$J,358.3,4330,0)
 ;;=12053^^43^298^3^^^^1
 ;;^UTILITY(U,$J,358.3,4330,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4330,1,2,0)
 ;;=2^Repair, Intermediate 5.1-7.5 cm
 ;;^UTILITY(U,$J,358.3,4330,1,4,0)
 ;;=4^12053
 ;;^UTILITY(U,$J,358.3,4331,0)
 ;;=12054^^43^298^4^^^^1
 ;;^UTILITY(U,$J,358.3,4331,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4331,1,2,0)
 ;;=2^Repair, Intermediate 7.6-12.5 cm
 ;;^UTILITY(U,$J,358.3,4331,1,4,0)
 ;;=4^12054
 ;;^UTILITY(U,$J,358.3,4332,0)
 ;;=12055^^43^298^5^^^^1
 ;;^UTILITY(U,$J,358.3,4332,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,4332,1,2,0)
 ;;=2^Repair, Intermediate 12.6-20.0 cm
