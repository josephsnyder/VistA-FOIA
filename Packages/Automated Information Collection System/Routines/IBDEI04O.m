IBDEI04O ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,5905,1,2,0)
 ;;=2^90937
 ;;^UTILITY(U,$J,358.3,5905,1,3,0)
 ;;=3^HEMODIALYSIS REPEATED EVAL
 ;;^UTILITY(U,$J,358.3,5906,0)
 ;;=E1590^^56^484^17^^^^1
 ;;^UTILITY(U,$J,358.3,5906,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5906,1,2,0)
 ;;=2^E1590
 ;;^UTILITY(U,$J,358.3,5906,1,3,0)
 ;;=3^HEMODIALYSIS MACHINE
 ;;^UTILITY(U,$J,358.3,5907,0)
 ;;=49421^^56^484^20^^^^1
 ;;^UTILITY(U,$J,358.3,5907,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5907,1,2,0)
 ;;=2^49421
 ;;^UTILITY(U,$J,358.3,5907,1,3,0)
 ;;=3^INS TUN IP CATH FOR DIAL OPN
 ;;^UTILITY(U,$J,358.3,5908,0)
 ;;=36556^^56^484^21^^^^1
 ;;^UTILITY(U,$J,358.3,5908,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5908,1,2,0)
 ;;=2^36556
 ;;^UTILITY(U,$J,358.3,5908,1,3,0)
 ;;=3^INSERT NON-TUNNEL CV CATH
 ;;^UTILITY(U,$J,358.3,5909,0)
 ;;=99195^^56^484^22^^^^1
 ;;^UTILITY(U,$J,358.3,5909,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5909,1,2,0)
 ;;=2^99195
 ;;^UTILITY(U,$J,358.3,5909,1,3,0)
 ;;=3^PHLEBOTOMY
 ;;^UTILITY(U,$J,358.3,5910,0)
 ;;=S0630^^56^484^23^^^^1
 ;;^UTILITY(U,$J,358.3,5910,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5910,1,2,0)
 ;;=2^S0630
 ;;^UTILITY(U,$J,358.3,5910,1,3,0)
 ;;=3^REMOVAL OF SUTURES
 ;;^UTILITY(U,$J,358.3,5911,0)
 ;;=49422^^56^484^24^^^^1
 ;;^UTILITY(U,$J,358.3,5911,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5911,1,2,0)
 ;;=2^49422
 ;;^UTILITY(U,$J,358.3,5911,1,3,0)
 ;;=3^REMOVE PERM CANNULA/CATHETER
 ;;^UTILITY(U,$J,358.3,5912,0)
 ;;=49422^^56^484^25^^^^1
 ;;^UTILITY(U,$J,358.3,5912,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5912,1,2,0)
 ;;=2^49422
 ;;^UTILITY(U,$J,358.3,5912,1,3,0)
 ;;=3^REMOVE TUNNELED IP CATH
 ;;^UTILITY(U,$J,358.3,5913,0)
 ;;=A4740^^56^484^26^^^^1
 ;;^UTILITY(U,$J,358.3,5913,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5913,1,2,0)
 ;;=2^A4740
 ;;^UTILITY(U,$J,358.3,5913,1,3,0)
 ;;=3^SHUNT ACCESSORY
 ;;^UTILITY(U,$J,358.3,5914,0)
 ;;=A4714^^56^484^27^^^^1
 ;;^UTILITY(U,$J,358.3,5914,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5914,1,2,0)
 ;;=2^A4714
 ;;^UTILITY(U,$J,358.3,5914,1,3,0)
 ;;=3^TREATED WATER PER GALLON
 ;;^UTILITY(U,$J,358.3,5915,0)
 ;;=90965^^56^485^1^^^^1
 ;;^UTILITY(U,$J,358.3,5915,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5915,1,2,0)
 ;;=2^90965
 ;;^UTILITY(U,$J,358.3,5915,1,3,0)
 ;;=3^ESRD HOME PT SERV P MO 12-19
 ;;^UTILITY(U,$J,358.3,5916,0)
 ;;=90966^^56^485^2^^^^1
 ;;^UTILITY(U,$J,358.3,5916,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5916,1,2,0)
 ;;=2^90966
 ;;^UTILITY(U,$J,358.3,5916,1,3,0)
 ;;=3^ESRD HOME PT SERV P MO 20+
 ;;^UTILITY(U,$J,358.3,5917,0)
 ;;=90957^^56^486^5^^^^1
 ;;^UTILITY(U,$J,358.3,5917,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5917,1,2,0)
 ;;=2^90957
 ;;^UTILITY(U,$J,358.3,5917,1,3,0)
 ;;=3^ESRD SRV 4 VSTS P MO 12-19
 ;;^UTILITY(U,$J,358.3,5918,0)
 ;;=90958^^56^486^3^^^^1
 ;;^UTILITY(U,$J,358.3,5918,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5918,1,2,0)
 ;;=2^90958
 ;;^UTILITY(U,$J,358.3,5918,1,3,0)
 ;;=3^ESRD SRV 2-3 VSTS P MO 12-19
 ;;^UTILITY(U,$J,358.3,5919,0)
 ;;=90959^^56^486^1^^^^1
 ;;^UTILITY(U,$J,358.3,5919,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5919,1,2,0)
 ;;=2^90959
 ;;^UTILITY(U,$J,358.3,5919,1,3,0)
 ;;=3^ESRD SERV 1 VST P MO 12-19
 ;;^UTILITY(U,$J,358.3,5920,0)
 ;;=90960^^56^486^6^^^^1
 ;;^UTILITY(U,$J,358.3,5920,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5920,1,2,0)
 ;;=2^90960
 ;;^UTILITY(U,$J,358.3,5920,1,3,0)
 ;;=3^ESRD SRV 4 VISITS P MO 20+
 ;;^UTILITY(U,$J,358.3,5921,0)
 ;;=90961^^56^486^4^^^^1
 ;;^UTILITY(U,$J,358.3,5921,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5921,1,2,0)
 ;;=2^90961
 ;;^UTILITY(U,$J,358.3,5921,1,3,0)
 ;;=3^ESRD SRV 2-3 VSTS P MO 20+
 ;;^UTILITY(U,$J,358.3,5922,0)
 ;;=90962^^56^486^2^^^^1
 ;;^UTILITY(U,$J,358.3,5922,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5922,1,2,0)
 ;;=2^90962
 ;;^UTILITY(U,$J,358.3,5922,1,3,0)
 ;;=3^ESRD SERV 1 VISIT P MO 20+
 ;;^UTILITY(U,$J,358.3,5923,0)
 ;;=99195^^56^487^6^^^^1
 ;;^UTILITY(U,$J,358.3,5923,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5923,1,2,0)
 ;;=2^99195
 ;;^UTILITY(U,$J,358.3,5923,1,3,0)
 ;;=3^PHLEBOTOMY
 ;;^UTILITY(U,$J,358.3,5924,0)
 ;;=A4253^^56^487^5^^^^1
 ;;^UTILITY(U,$J,358.3,5924,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5924,1,2,0)
 ;;=2^A4253
 ;;^UTILITY(U,$J,358.3,5924,1,3,0)
 ;;=3^BLOOD GLUCOSE/REAGENT STRIPS
 ;;^UTILITY(U,$J,358.3,5925,0)
 ;;=80202^^56^487^3^^^^1
 ;;^UTILITY(U,$J,358.3,5925,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5925,1,2,0)
 ;;=2^80202
 ;;^UTILITY(U,$J,358.3,5925,1,3,0)
 ;;=3^ASSAY OF VANCOMYCIN
 ;;^UTILITY(U,$J,358.3,5926,0)
 ;;=87040^^56^487^4^^^^1
 ;;^UTILITY(U,$J,358.3,5926,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5926,1,2,0)
 ;;=2^87040
 ;;^UTILITY(U,$J,358.3,5926,1,3,0)
 ;;=3^BLOOD CULTURE FOR BACTERIA
 ;;^UTILITY(U,$J,358.3,5927,0)
 ;;=82003^^56^487^1^^^^1
 ;;^UTILITY(U,$J,358.3,5927,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5927,1,2,0)
 ;;=2^82003
 ;;^UTILITY(U,$J,358.3,5927,1,3,0)
 ;;=3^ASSAY OF ACETAMINOPHEN
 ;;^UTILITY(U,$J,358.3,5928,0)
 ;;=82507^^56^487^2^^^^1
 ;;^UTILITY(U,$J,358.3,5928,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5928,1,2,0)
 ;;=2^82507
 ;;^UTILITY(U,$J,358.3,5928,1,3,0)
 ;;=3^ASSAY OF CITRATE
 ;;^UTILITY(U,$J,358.3,5929,0)
 ;;=A4216^^56^488^18^^^^1
 ;;^UTILITY(U,$J,358.3,5929,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5929,1,2,0)
 ;;=2^A4216
 ;;^UTILITY(U,$J,358.3,5929,1,3,0)
 ;;=3^STERILE WATER/SALINE, 10 ML
 ;;^UTILITY(U,$J,358.3,5930,0)
 ;;=B4164^^56^488^16^^^^1
 ;;^UTILITY(U,$J,358.3,5930,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5930,1,2,0)
 ;;=2^B4164
 ;;^UTILITY(U,$J,358.3,5930,1,3,0)
 ;;=3^PARENTERAL 50% DEXTROSE SOLU
 ;;^UTILITY(U,$J,358.3,5931,0)
 ;;=B9006^^56^488^17^^^^1
 ;;^UTILITY(U,$J,358.3,5931,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5931,1,2,0)
 ;;=2^B9006
 ;;^UTILITY(U,$J,358.3,5931,1,3,0)
 ;;=3^PARENTERAL INFUS PUMP STATIO
 ;;^UTILITY(U,$J,358.3,5932,0)
 ;;=E1520^^56^488^11^^^^1
 ;;^UTILITY(U,$J,358.3,5932,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5932,1,2,0)
 ;;=2^E1520
 ;;^UTILITY(U,$J,358.3,5932,1,3,0)
 ;;=3^HEPARIN INFUSION PUMP
 ;;^UTILITY(U,$J,358.3,5933,0)
 ;;=J0713^^56^488^4^^^^1
 ;;^UTILITY(U,$J,358.3,5933,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5933,1,2,0)
 ;;=2^J0713
 ;;^UTILITY(U,$J,358.3,5933,1,3,0)
 ;;=3^CEFTAZIDIME INJ PER 500 MG
 ;;^UTILITY(U,$J,358.3,5934,0)
 ;;=J0735^^56^488^5^^^^1
 ;;^UTILITY(U,$J,358.3,5934,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5934,1,2,0)
 ;;=2^J0735
 ;;^UTILITY(U,$J,358.3,5934,1,3,0)
 ;;=3^CLONIDINE HCL INJ PER 1 MG
 ;;^UTILITY(U,$J,358.3,5935,0)
 ;;=J1200^^56^488^7^^^^1
 ;;^UTILITY(U,$J,358.3,5935,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5935,1,2,0)
 ;;=2^J1200
 ;;^UTILITY(U,$J,358.3,5935,1,3,0)
 ;;=3^DIPHENHYDRAMINE HCL PER 50 MG
 ;;^UTILITY(U,$J,358.3,5936,0)
 ;;=J1270^^56^488^8^^^^1
 ;;^UTILITY(U,$J,358.3,5936,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5936,1,2,0)
 ;;=2^J1270
 ;;^UTILITY(U,$J,358.3,5936,1,3,0)
 ;;=3^DOXERCALCIFEROL INJ PER 1 MCG
 ;;^UTILITY(U,$J,358.3,5937,0)
 ;;=J1580^^56^488^10^^^^1
 ;;^UTILITY(U,$J,358.3,5937,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5937,1,2,0)
 ;;=2^J1580
 ;;^UTILITY(U,$J,358.3,5937,1,3,0)
 ;;=3^GARAMYCIN GENTAMICIN PER 80 MG
 ;;^UTILITY(U,$J,358.3,5938,0)
 ;;=J1642^^56^488^12^^^^1
 ;;^UTILITY(U,$J,358.3,5938,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5938,1,2,0)
 ;;=2^J1642
 ;;^UTILITY(U,$J,358.3,5938,1,3,0)
 ;;=3^HEPARIN SODIUM INJ PER 10 U
 ;;^UTILITY(U,$J,358.3,5939,0)
 ;;=J1644^^56^488^13^^^^1
 ;;^UTILITY(U,$J,358.3,5939,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5939,1,2,0)
 ;;=2^J1644
 ;;^UTILITY(U,$J,358.3,5939,1,3,0)
 ;;=3^HEPARIN SODIUM INJ PER 1000 U
 ;;^UTILITY(U,$J,358.3,5940,0)
 ;;=J2060^^56^488^14^^^^1
 ;;^UTILITY(U,$J,358.3,5940,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5940,1,2,0)
 ;;=2^J2060
 ;;^UTILITY(U,$J,358.3,5940,1,3,0)
 ;;=3^LORAZEPAM INJ PER 2 MG
 ;;^UTILITY(U,$J,358.3,5941,0)
 ;;=J2405^^56^488^15^^^^1
 ;;^UTILITY(U,$J,358.3,5941,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5941,1,2,0)
 ;;=2^J2405
 ;;^UTILITY(U,$J,358.3,5941,1,3,0)
 ;;=3^ONDANSETRON HCL INJ PER 1 MG
 ;;^UTILITY(U,$J,358.3,5942,0)
 ;;=J2997^^56^488^2^^^^1
 ;;^UTILITY(U,$J,358.3,5942,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5942,1,2,0)
 ;;=2^J2997
 ;;^UTILITY(U,$J,358.3,5942,1,3,0)
 ;;=3^ALTEPLASE RECOMBINANT PER 1 MG
 ;;^UTILITY(U,$J,358.3,5943,0)
 ;;=J3260^^56^488^20^^^^1
 ;;^UTILITY(U,$J,358.3,5943,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5943,1,2,0)
 ;;=2^J3260
 ;;^UTILITY(U,$J,358.3,5943,1,3,0)
 ;;=3^TOBRAMYCIN SULFATE PER 80 MG
 ;;^UTILITY(U,$J,358.3,5944,0)
 ;;=J7682^^56^488^19^^^^1
 ;;^UTILITY(U,$J,358.3,5944,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5944,1,2,0)
 ;;=2^J7682
 ;;^UTILITY(U,$J,358.3,5944,1,3,0)
 ;;=3^TOBRAMYCIN NON-COM UNIT 300 MG
 ;;^UTILITY(U,$J,358.3,5945,0)
 ;;=P9047^^56^488^1^^^^1
 ;;^UTILITY(U,$J,358.3,5945,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5945,1,2,0)
 ;;=2^P9047
 ;;^UTILITY(U,$J,358.3,5945,1,3,0)
 ;;=3^ALBUMIN (HUMAN), 25%, 50ML
 ;;^UTILITY(U,$J,358.3,5946,0)
 ;;=J0886^^56^488^9^^^^1
 ;;^UTILITY(U,$J,358.3,5946,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5946,1,2,0)
 ;;=2^J0886
 ;;^UTILITY(U,$J,358.3,5946,1,3,0)
 ;;=3^EPOETIN ALFA 1000 UNITS ESRD
 ;;^UTILITY(U,$J,358.3,5947,0)
 ;;=J3370^^56^488^21^^^^1
 ;;^UTILITY(U,$J,358.3,5947,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5947,1,2,0)
 ;;=2^J3370
 ;;^UTILITY(U,$J,358.3,5947,1,3,0)
 ;;=3^VANCOMYCIN HCL 500 MG
 ;;^UTILITY(U,$J,358.3,5948,0)
 ;;=J0636^^56^488^3^^^^1
 ;;^UTILITY(U,$J,358.3,5948,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5948,1,2,0)
 ;;=2^J0636
 ;;^UTILITY(U,$J,358.3,5948,1,3,0)
 ;;=3^CALCITRIOL INJ PER 0.1 MCG
 ;;^UTILITY(U,$J,358.3,5949,0)
 ;;=J0882^^56^488^6^^^^1
 ;;^UTILITY(U,$J,358.3,5949,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5949,1,2,0)
 ;;=2^J0882
 ;;^UTILITY(U,$J,358.3,5949,1,3,0)
 ;;=3^DARBEPOETIN ALFA,ESRD USE 1MCG
