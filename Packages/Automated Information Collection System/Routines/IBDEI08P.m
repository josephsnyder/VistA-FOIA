IBDEI08P ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,11501,1,5,0)
 ;;=5^Allergic Urticaria
 ;;^UTILITY(U,$J,358.3,11501,2)
 ;;=^259112
 ;;^UTILITY(U,$J,358.3,11502,0)
 ;;=707.00^^97^774^12
 ;;^UTILITY(U,$J,358.3,11502,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11502,1,4,0)
 ;;=4^707.00
 ;;^UTILITY(U,$J,358.3,11502,1,5,0)
 ;;=5^Decubitus Ulcer,Site NOS
 ;;^UTILITY(U,$J,358.3,11502,2)
 ;;=^336880
 ;;^UTILITY(U,$J,358.3,11503,0)
 ;;=707.06^^97^774^6
 ;;^UTILITY(U,$J,358.3,11503,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11503,1,4,0)
 ;;=4^707.06
 ;;^UTILITY(U,$J,358.3,11503,1,5,0)
 ;;=5^Decubitus Ulcer,Ankle
 ;;^UTILITY(U,$J,358.3,11503,2)
 ;;=^336839
 ;;^UTILITY(U,$J,358.3,11504,0)
 ;;=707.05^^97^774^7
 ;;^UTILITY(U,$J,358.3,11504,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11504,1,4,0)
 ;;=4^707.05
 ;;^UTILITY(U,$J,358.3,11504,1,5,0)
 ;;=5^Decubitus Ulcer,Buttock
 ;;^UTILITY(U,$J,358.3,11504,2)
 ;;=^336838
 ;;^UTILITY(U,$J,358.3,11505,0)
 ;;=707.07^^97^774^8
 ;;^UTILITY(U,$J,358.3,11505,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11505,1,4,0)
 ;;=4^707.07
 ;;^UTILITY(U,$J,358.3,11505,1,5,0)
 ;;=5^Decubitus Ulcer,Heel
 ;;^UTILITY(U,$J,358.3,11505,2)
 ;;=^336840
 ;;^UTILITY(U,$J,358.3,11506,0)
 ;;=707.04^^97^774^9
 ;;^UTILITY(U,$J,358.3,11506,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11506,1,4,0)
 ;;=4^707.04
 ;;^UTILITY(U,$J,358.3,11506,1,5,0)
 ;;=5^Decubitus Ulcer,Hip
 ;;^UTILITY(U,$J,358.3,11506,2)
 ;;=^336837
 ;;^UTILITY(U,$J,358.3,11507,0)
 ;;=707.03^^97^774^10
 ;;^UTILITY(U,$J,358.3,11507,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11507,1,4,0)
 ;;=4^707.03
 ;;^UTILITY(U,$J,358.3,11507,1,5,0)
 ;;=5^Decubitus Ulcer,Low Back
 ;;^UTILITY(U,$J,358.3,11507,2)
 ;;=^336836
 ;;^UTILITY(U,$J,358.3,11508,0)
 ;;=707.09^^97^774^11
 ;;^UTILITY(U,$J,358.3,11508,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11508,1,4,0)
 ;;=4^707.09
 ;;^UTILITY(U,$J,358.3,11508,1,5,0)
 ;;=5^Decubitus Ulcer,Site NEC
 ;;^UTILITY(U,$J,358.3,11508,2)
 ;;=^336841
 ;;^UTILITY(U,$J,358.3,11509,0)
 ;;=707.02^^97^774^13
 ;;^UTILITY(U,$J,358.3,11509,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11509,1,4,0)
 ;;=4^707.02
 ;;^UTILITY(U,$J,358.3,11509,1,5,0)
 ;;=5^Decubitus Ulcer,Upper Back
 ;;^UTILITY(U,$J,358.3,11509,2)
 ;;=^336835
 ;;^UTILITY(U,$J,358.3,11510,0)
 ;;=782.3^^97^774^15
 ;;^UTILITY(U,$J,358.3,11510,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11510,1,4,0)
 ;;=4^782.3
 ;;^UTILITY(U,$J,358.3,11510,1,5,0)
 ;;=5^Edema,Local Swelling
 ;;^UTILITY(U,$J,358.3,11510,2)
 ;;=^38340
 ;;^UTILITY(U,$J,358.3,11511,0)
 ;;=704.8^^97^774^16
 ;;^UTILITY(U,$J,358.3,11511,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11511,1,4,0)
 ;;=4^704.8
 ;;^UTILITY(U,$J,358.3,11511,1,5,0)
 ;;=5^Folliculitis
 ;;^UTILITY(U,$J,358.3,11511,2)
 ;;=^87969
 ;;^UTILITY(U,$J,358.3,11512,0)
 ;;=703.0^^97^774^17
 ;;^UTILITY(U,$J,358.3,11512,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11512,1,4,0)
 ;;=4^703.0
 ;;^UTILITY(U,$J,358.3,11512,1,5,0)
 ;;=5^Ingrown Nail
 ;;^UTILITY(U,$J,358.3,11512,2)
 ;;=^81221
 ;;^UTILITY(U,$J,358.3,11513,0)
 ;;=685.1^^97^774^19
 ;;^UTILITY(U,$J,358.3,11513,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11513,1,4,0)
 ;;=4^685.1
 ;;^UTILITY(U,$J,358.3,11513,1,5,0)
 ;;=5^Pilonidal Cyst w/o Absc
 ;;^UTILITY(U,$J,358.3,11513,2)
 ;;=^94453
 ;;^UTILITY(U,$J,358.3,11514,0)
 ;;=706.3^^97^774^23
 ;;^UTILITY(U,$J,358.3,11514,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11514,1,4,0)
 ;;=4^706.3
 ;;^UTILITY(U,$J,358.3,11514,1,5,0)
 ;;=5^Seborrhea
 ;;^UTILITY(U,$J,358.3,11514,2)
 ;;=^108864
 ;;^UTILITY(U,$J,358.3,11515,0)
 ;;=111.0^^97^774^26
 ;;^UTILITY(U,$J,358.3,11515,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11515,1,4,0)
 ;;=4^111.0
 ;;^UTILITY(U,$J,358.3,11515,1,5,0)
 ;;=5^Tinea Vesicolor
 ;;^UTILITY(U,$J,358.3,11515,2)
 ;;=^119758
 ;;^UTILITY(U,$J,358.3,11516,0)
 ;;=707.19^^97^774^32
 ;;^UTILITY(U,$J,358.3,11516,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11516,1,4,0)
 ;;=4^707.19
 ;;^UTILITY(U,$J,358.3,11516,1,5,0)
 ;;=5^Ulcer Other Part Lwr Limb
 ;;^UTILITY(U,$J,358.3,11516,2)
 ;;=^322150
 ;;^UTILITY(U,$J,358.3,11517,0)
 ;;=692.9^^97^774^14
 ;;^UTILITY(U,$J,358.3,11517,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11517,1,4,0)
 ;;=4^692.9
 ;;^UTILITY(U,$J,358.3,11517,1,5,0)
 ;;=5^Dermatitis/Eczema
 ;;^UTILITY(U,$J,358.3,11517,2)
 ;;=^27800
 ;;^UTILITY(U,$J,358.3,11518,0)
 ;;=110.1^^97^774^18
 ;;^UTILITY(U,$J,358.3,11518,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11518,1,4,0)
 ;;=4^110.1
 ;;^UTILITY(U,$J,358.3,11518,1,5,0)
 ;;=5^Onychomycosis
 ;;^UTILITY(U,$J,358.3,11518,2)
 ;;=^33173
 ;;^UTILITY(U,$J,358.3,11519,0)
 ;;=696.1^^97^774^20
 ;;^UTILITY(U,$J,358.3,11519,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11519,1,4,0)
 ;;=4^696.1
 ;;^UTILITY(U,$J,358.3,11519,1,5,0)
 ;;=5^Psoriasis NOS
 ;;^UTILITY(U,$J,358.3,11519,2)
 ;;=^271917
 ;;^UTILITY(U,$J,358.3,11520,0)
 ;;=782.1^^97^774^21
 ;;^UTILITY(U,$J,358.3,11520,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11520,1,4,0)
 ;;=4^782.1
 ;;^UTILITY(U,$J,358.3,11520,1,5,0)
 ;;=5^Rash NOS/Skin Erupt NEC
 ;;^UTILITY(U,$J,358.3,11520,2)
 ;;=^102948
 ;;^UTILITY(U,$J,358.3,11521,0)
 ;;=695.3^^97^774^22
 ;;^UTILITY(U,$J,358.3,11521,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11521,1,4,0)
 ;;=4^695.3
 ;;^UTILITY(U,$J,358.3,11521,1,5,0)
 ;;=5^Rosacea
 ;;^UTILITY(U,$J,358.3,11521,2)
 ;;=^107114
 ;;^UTILITY(U,$J,358.3,11522,0)
 ;;=702.11^^97^774^24
 ;;^UTILITY(U,$J,358.3,11522,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11522,1,4,0)
 ;;=4^702.11
 ;;^UTILITY(U,$J,358.3,11522,1,5,0)
 ;;=5^Seborrhea,Keratosis Inflam
 ;;^UTILITY(U,$J,358.3,11522,2)
 ;;=^303311
 ;;^UTILITY(U,$J,358.3,11523,0)
 ;;=707.13^^97^774^27
 ;;^UTILITY(U,$J,358.3,11523,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11523,1,4,0)
 ;;=4^707.13
 ;;^UTILITY(U,$J,358.3,11523,1,5,0)
 ;;=5^Ulcer Ankle
 ;;^UTILITY(U,$J,358.3,11523,2)
 ;;=^322145
 ;;^UTILITY(U,$J,358.3,11524,0)
 ;;=707.12^^97^774^28
 ;;^UTILITY(U,$J,358.3,11524,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11524,1,4,0)
 ;;=4^707.12
 ;;^UTILITY(U,$J,358.3,11524,1,5,0)
 ;;=5^Ulcer Calf
 ;;^UTILITY(U,$J,358.3,11524,2)
 ;;=^322144
 ;;^UTILITY(U,$J,358.3,11525,0)
 ;;=707.14^^97^774^29
 ;;^UTILITY(U,$J,358.3,11525,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11525,1,4,0)
 ;;=4^707.14
 ;;^UTILITY(U,$J,358.3,11525,1,5,0)
 ;;=5^Ulcer Heel and Midfoot
 ;;^UTILITY(U,$J,358.3,11525,2)
 ;;=^322146
 ;;^UTILITY(U,$J,358.3,11526,0)
 ;;=707.15^^97^774^31
 ;;^UTILITY(U,$J,358.3,11526,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11526,1,4,0)
 ;;=4^707.15
 ;;^UTILITY(U,$J,358.3,11526,1,5,0)
 ;;=5^Ulcer Other Part Foot
 ;;^UTILITY(U,$J,358.3,11526,2)
 ;;=^322148
 ;;^UTILITY(U,$J,358.3,11527,0)
 ;;=707.11^^97^774^33
 ;;^UTILITY(U,$J,358.3,11527,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11527,1,4,0)
 ;;=4^707.11
 ;;^UTILITY(U,$J,358.3,11527,1,5,0)
 ;;=5^Ulcer Thigh
 ;;^UTILITY(U,$J,358.3,11527,2)
 ;;=^322143
 ;;^UTILITY(U,$J,358.3,11528,0)
 ;;=707.10^^97^774^30
 ;;^UTILITY(U,$J,358.3,11528,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11528,1,4,0)
 ;;=4^707.10
 ;;^UTILITY(U,$J,358.3,11528,1,5,0)
 ;;=5^Ulcer Lower Limb Unsp
 ;;^UTILITY(U,$J,358.3,11528,2)
 ;;=^322142
 ;;^UTILITY(U,$J,358.3,11529,0)
 ;;=780.4^^97^775^6
 ;;^UTILITY(U,$J,358.3,11529,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11529,1,4,0)
 ;;=4^780.4
 ;;^UTILITY(U,$J,358.3,11529,1,5,0)
 ;;=5^Dizziness and Giddiness
 ;;^UTILITY(U,$J,358.3,11529,2)
 ;;=^35946
 ;;^UTILITY(U,$J,358.3,11530,0)
 ;;=780.79^^97^775^9
 ;;^UTILITY(U,$J,358.3,11530,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11530,1,4,0)
 ;;=4^780.79
 ;;^UTILITY(U,$J,358.3,11530,1,5,0)
 ;;=5^Malaise
 ;;^UTILITY(U,$J,358.3,11530,2)
 ;;=Malaise^73344
 ;;^UTILITY(U,$J,358.3,11531,0)
 ;;=780.2^^97^775^12
 ;;^UTILITY(U,$J,358.3,11531,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11531,1,4,0)
 ;;=4^780.2
 ;;^UTILITY(U,$J,358.3,11531,1,5,0)
 ;;=5^Syncope and Collapse
 ;;^UTILITY(U,$J,358.3,11531,2)
 ;;=^116707
 ;;^UTILITY(U,$J,358.3,11532,0)
 ;;=783.1^^97^775^2
 ;;^UTILITY(U,$J,358.3,11532,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11532,1,4,0)
 ;;=4^783.1
 ;;^UTILITY(U,$J,358.3,11532,1,5,0)
 ;;=5^Abnormal Weight Gain
 ;;^UTILITY(U,$J,358.3,11532,2)
 ;;=^998
 ;;^UTILITY(U,$J,358.3,11533,0)
 ;;=783.21^^97^775^8
 ;;^UTILITY(U,$J,358.3,11533,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11533,1,4,0)
 ;;=4^783.21
 ;;^UTILITY(U,$J,358.3,11533,1,5,0)
 ;;=5^Loss of Weight
 ;;^UTILITY(U,$J,358.3,11533,2)
 ;;=^322005
 ;;^UTILITY(U,$J,358.3,11534,0)
 ;;=790.6^^97^775^1
 ;;^UTILITY(U,$J,358.3,11534,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11534,1,4,0)
 ;;=4^790.6
 ;;^UTILITY(U,$J,358.3,11534,1,5,0)
 ;;=5^Abnormal Blood Chemistry
 ;;^UTILITY(U,$J,358.3,11534,2)
 ;;=^87228
 ;;^UTILITY(U,$J,358.3,11535,0)
 ;;=V70.0^^97^775^4
 ;;^UTILITY(U,$J,358.3,11535,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11535,1,4,0)
 ;;=4^V70.0
 ;;^UTILITY(U,$J,358.3,11535,1,5,0)
 ;;=5^Annual Health Exam
 ;;^UTILITY(U,$J,358.3,11535,2)
 ;;=^295590
 ;;^UTILITY(U,$J,358.3,11536,0)
 ;;=995.20^^97^775^5
 ;;^UTILITY(U,$J,358.3,11536,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11536,1,4,0)
 ;;=4^995.20
 ;;^UTILITY(U,$J,358.3,11536,1,5,0)
 ;;=5^Arthus Phenomenon
 ;;^UTILITY(U,$J,358.3,11536,2)
 ;;=^334208
 ;;^UTILITY(U,$J,358.3,11537,0)
 ;;=995.23^^97^775^3
 ;;^UTILITY(U,$J,358.3,11537,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11537,1,4,0)
 ;;=4^995.23
 ;;^UTILITY(U,$J,358.3,11537,1,5,0)
 ;;=5^Adverse Effect Insulin NOS
 ;;^UTILITY(U,$J,358.3,11537,2)
 ;;=^334179
 ;;^UTILITY(U,$J,358.3,11538,0)
 ;;=V60.0^^97^775^7
 ;;^UTILITY(U,$J,358.3,11538,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11538,1,4,0)
 ;;=4^V60.0
 ;;^UTILITY(U,$J,358.3,11538,1,5,0)
 ;;=5^Lack of Housing
 ;;^UTILITY(U,$J,358.3,11538,2)
 ;;=^295539
 ;;^UTILITY(U,$J,358.3,11539,0)
 ;;=V68.81^^97^775^10
 ;;^UTILITY(U,$J,358.3,11539,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11539,1,4,0)
 ;;=4^V68.81
