IBDEI0AA ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,13651,1,5,0)
 ;;=5^Hypertriglyceridemia, Pure
 ;;^UTILITY(U,$J,358.3,13651,2)
 ;;=Hypertriglyceridemia, Pure^101303
 ;;^UTILITY(U,$J,358.3,13652,0)
 ;;=272.2^^107^862^24
 ;;^UTILITY(U,$J,358.3,13652,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13652,1,4,0)
 ;;=4^272.2
 ;;^UTILITY(U,$J,358.3,13652,1,5,0)
 ;;=5^Hyperlipidemia, Mixed
 ;;^UTILITY(U,$J,358.3,13652,2)
 ;;=^78424
 ;;^UTILITY(U,$J,358.3,13653,0)
 ;;=275.42^^107^862^21
 ;;^UTILITY(U,$J,358.3,13653,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13653,1,4,0)
 ;;=4^275.42
 ;;^UTILITY(U,$J,358.3,13653,1,5,0)
 ;;=5^Hypercalcemia
 ;;^UTILITY(U,$J,358.3,13653,2)
 ;;=^59932
 ;;^UTILITY(U,$J,358.3,13654,0)
 ;;=275.41^^107^862^28
 ;;^UTILITY(U,$J,358.3,13654,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13654,1,4,0)
 ;;=4^275.41
 ;;^UTILITY(U,$J,358.3,13654,1,5,0)
 ;;=5^Hypocalcemia
 ;;^UTILITY(U,$J,358.3,13654,2)
 ;;=^60542
 ;;^UTILITY(U,$J,358.3,13655,0)
 ;;=276.7^^107^862^23
 ;;^UTILITY(U,$J,358.3,13655,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13655,1,4,0)
 ;;=4^276.7
 ;;^UTILITY(U,$J,358.3,13655,1,5,0)
 ;;=5^Hyperkalemia
 ;;^UTILITY(U,$J,358.3,13655,2)
 ;;=^60042
 ;;^UTILITY(U,$J,358.3,13656,0)
 ;;=275.2^^107^862^19
 ;;^UTILITY(U,$J,358.3,13656,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13656,1,4,0)
 ;;=4^275.2
 ;;^UTILITY(U,$J,358.3,13656,1,5,0)
 ;;=5^Hyper Or Hypomagnesemia
 ;;^UTILITY(U,$J,358.3,13656,2)
 ;;=^35626
 ;;^UTILITY(U,$J,358.3,13657,0)
 ;;=276.0^^107^862^25
 ;;^UTILITY(U,$J,358.3,13657,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13657,1,4,0)
 ;;=4^276.0
 ;;^UTILITY(U,$J,358.3,13657,1,5,0)
 ;;=5^Hypernatremia
 ;;^UTILITY(U,$J,358.3,13657,2)
 ;;=^60144
 ;;^UTILITY(U,$J,358.3,13658,0)
 ;;=276.1^^107^862^31
 ;;^UTILITY(U,$J,358.3,13658,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13658,1,4,0)
 ;;=4^276.1
 ;;^UTILITY(U,$J,358.3,13658,1,5,0)
 ;;=5^Hyponatremia
 ;;^UTILITY(U,$J,358.3,13658,2)
 ;;=Hyponatremia^60722
 ;;^UTILITY(U,$J,358.3,13659,0)
 ;;=275.3^^107^862^20
 ;;^UTILITY(U,$J,358.3,13659,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13659,1,4,0)
 ;;=4^275.3
 ;;^UTILITY(U,$J,358.3,13659,1,5,0)
 ;;=5^Hyper Or Hypophosphatemia
 ;;^UTILITY(U,$J,358.3,13659,2)
 ;;=^93796
 ;;^UTILITY(U,$J,358.3,13660,0)
 ;;=240.0^^107^862^14
 ;;^UTILITY(U,$J,358.3,13660,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13660,1,4,0)
 ;;=4^240.0
 ;;^UTILITY(U,$J,358.3,13660,1,5,0)
 ;;=5^Goiter, Simple
 ;;^UTILITY(U,$J,358.3,13660,2)
 ;;=^259806
 ;;^UTILITY(U,$J,358.3,13661,0)
 ;;=241.1^^107^862^13
 ;;^UTILITY(U,$J,358.3,13661,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13661,1,4,0)
 ;;=4^241.1
 ;;^UTILITY(U,$J,358.3,13661,1,5,0)
 ;;=5^Goiter, Nontox, Multinod
 ;;^UTILITY(U,$J,358.3,13661,2)
 ;;=^267790
 ;;^UTILITY(U,$J,358.3,13662,0)
 ;;=241.0^^107^862^50
 ;;^UTILITY(U,$J,358.3,13662,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13662,1,4,0)
 ;;=4^241.0
 ;;^UTILITY(U,$J,358.3,13662,1,5,0)
 ;;=5^Thyroid Nodule, Nontoxic
 ;;^UTILITY(U,$J,358.3,13662,2)
 ;;=^83865
 ;;^UTILITY(U,$J,358.3,13663,0)
 ;;=242.00^^107^862^15
 ;;^UTILITY(U,$J,358.3,13663,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13663,1,4,0)
 ;;=4^242.00
 ;;^UTILITY(U,$J,358.3,13663,1,5,0)
 ;;=5^Graves' Disease
 ;;^UTILITY(U,$J,358.3,13663,2)
 ;;=^267793
 ;;^UTILITY(U,$J,358.3,13664,0)
 ;;=242.01^^107^862^12
 ;;^UTILITY(U,$J,358.3,13664,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13664,1,4,0)
 ;;=4^242.01
 ;;^UTILITY(U,$J,358.3,13664,1,5,0)
 ;;=5^Goiter Diff Tox W Strm
 ;;^UTILITY(U,$J,358.3,13664,2)
 ;;=^267794
 ;;^UTILITY(U,$J,358.3,13665,0)
 ;;=252.1^^107^862^32
 ;;^UTILITY(U,$J,358.3,13665,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13665,1,4,0)
 ;;=4^252.1
 ;;^UTILITY(U,$J,358.3,13665,1,5,0)
 ;;=5^Hypoparathyroidism
 ;;^UTILITY(U,$J,358.3,13665,2)
 ;;=^60635
 ;;^UTILITY(U,$J,358.3,13666,0)
 ;;=242.90^^107^862^26
 ;;^UTILITY(U,$J,358.3,13666,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13666,1,4,0)
 ;;=4^242.90
 ;;^UTILITY(U,$J,358.3,13666,1,5,0)
 ;;=5^Hyperthyroid W/O Goiter Or Strm
 ;;^UTILITY(U,$J,358.3,13666,2)
 ;;=^267811
 ;;^UTILITY(U,$J,358.3,13667,0)
 ;;=242.91^^107^862^37
 ;;^UTILITY(U,$J,358.3,13667,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13667,1,4,0)
 ;;=4^242.91
 ;;^UTILITY(U,$J,358.3,13667,1,5,0)
 ;;=5^Hyprthy W/O Goit W Strm
 ;;^UTILITY(U,$J,358.3,13667,2)
 ;;=^267812
 ;;^UTILITY(U,$J,358.3,13668,0)
 ;;=244.0^^107^862^35
 ;;^UTILITY(U,$J,358.3,13668,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13668,1,4,0)
 ;;=4^244.0
 ;;^UTILITY(U,$J,358.3,13668,1,5,0)
 ;;=5^Hypothyroid, Postsurgical
 ;;^UTILITY(U,$J,358.3,13668,2)
 ;;=^267814
 ;;^UTILITY(U,$J,358.3,13669,0)
 ;;=244.2^^107^862^34
 ;;^UTILITY(U,$J,358.3,13669,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13669,1,4,0)
 ;;=4^244.2
 ;;^UTILITY(U,$J,358.3,13669,1,5,0)
 ;;=5^Hypothyroid Due To Iodine Rx
 ;;^UTILITY(U,$J,358.3,13669,2)
 ;;=^267817
 ;;^UTILITY(U,$J,358.3,13670,0)
 ;;=244.9^^107^862^36
 ;;^UTILITY(U,$J,358.3,13670,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13670,1,4,0)
 ;;=4^244.9
 ;;^UTILITY(U,$J,358.3,13670,1,5,0)
 ;;=5^Hypothyroid, Unspec Cause
 ;;^UTILITY(U,$J,358.3,13670,2)
 ;;=^123752
 ;;^UTILITY(U,$J,358.3,13671,0)
 ;;=245.0^^107^862^51
 ;;^UTILITY(U,$J,358.3,13671,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13671,1,4,0)
 ;;=4^245.0
 ;;^UTILITY(U,$J,358.3,13671,1,5,0)
 ;;=5^Thyroiditis, Acute
 ;;^UTILITY(U,$J,358.3,13671,2)
 ;;=^2692
 ;;^UTILITY(U,$J,358.3,13672,0)
 ;;=245.1^^107^862^52
 ;;^UTILITY(U,$J,358.3,13672,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13672,1,4,0)
 ;;=4^245.1
 ;;^UTILITY(U,$J,358.3,13672,1,5,0)
 ;;=5^Thyroiditis, Subacute
 ;;^UTILITY(U,$J,358.3,13672,2)
 ;;=^119376
 ;;^UTILITY(U,$J,358.3,13673,0)
 ;;=733.01^^107^862^45
 ;;^UTILITY(U,$J,358.3,13673,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13673,1,4,0)
 ;;=4^733.01
 ;;^UTILITY(U,$J,358.3,13673,1,5,0)
 ;;=5^Osteoporosis, Senile
 ;;^UTILITY(U,$J,358.3,13673,2)
 ;;=Osteoporosis, Senile^87188
 ;;^UTILITY(U,$J,358.3,13674,0)
 ;;=733.02^^107^862^44
 ;;^UTILITY(U,$J,358.3,13674,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13674,1,4,0)
 ;;=4^733.02
 ;;^UTILITY(U,$J,358.3,13674,1,5,0)
 ;;=5^Osteoporosis, Idiopathic
 ;;^UTILITY(U,$J,358.3,13674,2)
 ;;=Osteoporosis, Idiopathic^272692
 ;;^UTILITY(U,$J,358.3,13675,0)
 ;;=268.2^^107^862^41
 ;;^UTILITY(U,$J,358.3,13675,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13675,1,4,0)
 ;;=4^268.2
 ;;^UTILITY(U,$J,358.3,13675,1,5,0)
 ;;=5^Osteomalacia
 ;;^UTILITY(U,$J,358.3,13675,2)
 ;;=Osteomalacia^87103
 ;;^UTILITY(U,$J,358.3,13676,0)
 ;;=733.90^^107^862^42
 ;;^UTILITY(U,$J,358.3,13676,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13676,1,4,0)
 ;;=4^733.90
 ;;^UTILITY(U,$J,358.3,13676,1,5,0)
 ;;=5^Osteopenia
 ;;^UTILITY(U,$J,358.3,13676,2)
 ;;=Osteopenia^35593
 ;;^UTILITY(U,$J,358.3,13677,0)
 ;;=275.49^^107^862^46
 ;;^UTILITY(U,$J,358.3,13677,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13677,1,4,0)
 ;;=4^275.49
 ;;^UTILITY(U,$J,358.3,13677,1,5,0)
 ;;=5^Pseudohypoparathyroidism
 ;;^UTILITY(U,$J,358.3,13677,2)
 ;;=Pseudohypparathyroidism^317904
 ;;^UTILITY(U,$J,358.3,13678,0)
 ;;=266.2^^107^862^53
 ;;^UTILITY(U,$J,358.3,13678,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13678,1,4,0)
 ;;=4^266.2
 ;;^UTILITY(U,$J,358.3,13678,1,5,0)
 ;;=5^Vitamin B12 Deficiency
 ;;^UTILITY(U,$J,358.3,13678,2)
 ;;=Vitamin B12 Deficiency^87347
 ;;^UTILITY(U,$J,358.3,13679,0)
 ;;=268.9^^107^862^55
 ;;^UTILITY(U,$J,358.3,13679,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13679,1,4,0)
 ;;=4^268.9
 ;;^UTILITY(U,$J,358.3,13679,1,5,0)
 ;;=5^Vitamin D Deficiency
 ;;^UTILITY(U,$J,358.3,13679,2)
 ;;=Vitamin D Deficiency^126968
 ;;^UTILITY(U,$J,358.3,13680,0)
 ;;=266.1^^107^862^54
 ;;^UTILITY(U,$J,358.3,13680,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13680,1,4,0)
 ;;=4^266.1
 ;;^UTILITY(U,$J,358.3,13680,1,5,0)
 ;;=5^Vitamin B6 Deficiency
 ;;^UTILITY(U,$J,358.3,13680,2)
 ;;=^101683
 ;;^UTILITY(U,$J,358.3,13681,0)
 ;;=780.99^^107^862^2
 ;;^UTILITY(U,$J,358.3,13681,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13681,1,4,0)
 ;;=4^780.99
 ;;^UTILITY(U,$J,358.3,13681,1,5,0)
 ;;=5^Cold Intolerance
 ;;^UTILITY(U,$J,358.3,13681,2)
 ;;=Cold Intolerance^328568
 ;;^UTILITY(U,$J,358.3,13682,0)
 ;;=255.41^^107^862^10
 ;;^UTILITY(U,$J,358.3,13682,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13682,1,4,0)
 ;;=4^255.41
 ;;^UTILITY(U,$J,358.3,13682,1,5,0)
 ;;=5^Glucocorticoid Deficient
 ;;^UTILITY(U,$J,358.3,13682,2)
 ;;=^335240
 ;;^UTILITY(U,$J,358.3,13683,0)
 ;;=255.42^^107^862^38
 ;;^UTILITY(U,$J,358.3,13683,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13683,1,4,0)
 ;;=4^255.42
 ;;^UTILITY(U,$J,358.3,13683,1,5,0)
 ;;=5^Mineralcorticoid Defcnt
 ;;^UTILITY(U,$J,358.3,13683,2)
 ;;=^335241
 ;;^UTILITY(U,$J,358.3,13684,0)
 ;;=259.50^^107^862^1
 ;;^UTILITY(U,$J,358.3,13684,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13684,1,4,0)
 ;;=4^259.50
 ;;^UTILITY(U,$J,358.3,13684,1,5,0)
 ;;=5^Androgen Insensitivity, Unsp
 ;;^UTILITY(U,$J,358.3,13684,2)
 ;;=^336738
 ;;^UTILITY(U,$J,358.3,13685,0)
 ;;=275.5^^107^862^18
 ;;^UTILITY(U,$J,358.3,13685,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13685,1,4,0)
 ;;=4^275.5
 ;;^UTILITY(U,$J,358.3,13685,1,5,0)
 ;;=5^Hungry Bone Syndrome
 ;;^UTILITY(U,$J,358.3,13685,2)
 ;;=^336538
 ;;^UTILITY(U,$J,358.3,13686,0)
 ;;=249.00^^107^862^49
 ;;^UTILITY(U,$J,358.3,13686,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13686,1,4,0)
 ;;=4^249.00
 ;;^UTILITY(U,$J,358.3,13686,1,5,0)
 ;;=5^Secondary DM w/o Complication
 ;;^UTILITY(U,$J,358.3,13686,2)
 ;;=^336728
 ;;^UTILITY(U,$J,358.3,13687,0)
 ;;=249.40^^107^862^48
 ;;^UTILITY(U,$J,358.3,13687,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13687,1,4,0)
 ;;=4^249.40
 ;;^UTILITY(U,$J,358.3,13687,1,5,0)
 ;;=5^Secondary DM w/ Renal Complication
 ;;^UTILITY(U,$J,358.3,13687,2)
 ;;=^336732
 ;;^UTILITY(U,$J,358.3,13688,0)
 ;;=249.60^^107^862^47
 ;;^UTILITY(U,$J,358.3,13688,1,0)
 ;;=^358.31IA^5^2
