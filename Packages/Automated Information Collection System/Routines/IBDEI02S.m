IBDEI02S ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,3237,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3237,1,4,0)
 ;;=4^276.1
 ;;^UTILITY(U,$J,358.3,3237,1,5,0)
 ;;=5^Hyponatremia
 ;;^UTILITY(U,$J,358.3,3237,2)
 ;;=Hyponatremia^60722
 ;;^UTILITY(U,$J,358.3,3238,0)
 ;;=275.3^^40^249^19
 ;;^UTILITY(U,$J,358.3,3238,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3238,1,4,0)
 ;;=4^275.3
 ;;^UTILITY(U,$J,358.3,3238,1,5,0)
 ;;=5^Hyper Or Hypophosphatemia
 ;;^UTILITY(U,$J,358.3,3238,2)
 ;;=^93796
 ;;^UTILITY(U,$J,358.3,3239,0)
 ;;=240.0^^40^249^14
 ;;^UTILITY(U,$J,358.3,3239,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3239,1,4,0)
 ;;=4^240.0
 ;;^UTILITY(U,$J,358.3,3239,1,5,0)
 ;;=5^Goiter, Simple
 ;;^UTILITY(U,$J,358.3,3239,2)
 ;;=^259806
 ;;^UTILITY(U,$J,358.3,3240,0)
 ;;=241.1^^40^249^13
 ;;^UTILITY(U,$J,358.3,3240,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3240,1,4,0)
 ;;=4^241.1
 ;;^UTILITY(U,$J,358.3,3240,1,5,0)
 ;;=5^Goiter, Nontox, Multinod
 ;;^UTILITY(U,$J,358.3,3240,2)
 ;;=^267790
 ;;^UTILITY(U,$J,358.3,3241,0)
 ;;=241.0^^40^249^45
 ;;^UTILITY(U,$J,358.3,3241,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3241,1,4,0)
 ;;=4^241.0
 ;;^UTILITY(U,$J,358.3,3241,1,5,0)
 ;;=5^Thyroid Nodule, Nontoxic
 ;;^UTILITY(U,$J,358.3,3241,2)
 ;;=^83865
 ;;^UTILITY(U,$J,358.3,3242,0)
 ;;=242.00^^40^249^15
 ;;^UTILITY(U,$J,358.3,3242,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3242,1,4,0)
 ;;=4^242.00
 ;;^UTILITY(U,$J,358.3,3242,1,5,0)
 ;;=5^Graves' Disease
 ;;^UTILITY(U,$J,358.3,3242,2)
 ;;=^267793
 ;;^UTILITY(U,$J,358.3,3243,0)
 ;;=242.01^^40^249^12
 ;;^UTILITY(U,$J,358.3,3243,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3243,1,4,0)
 ;;=4^242.01
 ;;^UTILITY(U,$J,358.3,3243,1,5,0)
 ;;=5^Goiter Diff Tox W Strm
 ;;^UTILITY(U,$J,358.3,3243,2)
 ;;=^267794
 ;;^UTILITY(U,$J,358.3,3244,0)
 ;;=252.1^^40^249^31
 ;;^UTILITY(U,$J,358.3,3244,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3244,1,4,0)
 ;;=4^252.1
 ;;^UTILITY(U,$J,358.3,3244,1,5,0)
 ;;=5^Hypoparathyroidism
 ;;^UTILITY(U,$J,358.3,3244,2)
 ;;=^60635
 ;;^UTILITY(U,$J,358.3,3245,0)
 ;;=242.90^^40^249^25
 ;;^UTILITY(U,$J,358.3,3245,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3245,1,4,0)
 ;;=4^242.90
 ;;^UTILITY(U,$J,358.3,3245,1,5,0)
 ;;=5^Hyperthyroid w/o Goiter Or Strm
 ;;^UTILITY(U,$J,358.3,3245,2)
 ;;=^267811
 ;;^UTILITY(U,$J,358.3,3246,0)
 ;;=242.91^^40^249^36
 ;;^UTILITY(U,$J,358.3,3246,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3246,1,4,0)
 ;;=4^242.91
 ;;^UTILITY(U,$J,358.3,3246,1,5,0)
 ;;=5^Hyprthy W/O Goit W Strm
 ;;^UTILITY(U,$J,358.3,3246,2)
 ;;=^267812
 ;;^UTILITY(U,$J,358.3,3247,0)
 ;;=244.0^^40^249^34
 ;;^UTILITY(U,$J,358.3,3247,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3247,1,4,0)
 ;;=4^244.0
 ;;^UTILITY(U,$J,358.3,3247,1,5,0)
 ;;=5^Hypothyroid, Postsurgical
 ;;^UTILITY(U,$J,358.3,3247,2)
 ;;=^267814
 ;;^UTILITY(U,$J,358.3,3248,0)
 ;;=244.2^^40^249^33
 ;;^UTILITY(U,$J,358.3,3248,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3248,1,4,0)
 ;;=4^244.2
 ;;^UTILITY(U,$J,358.3,3248,1,5,0)
 ;;=5^Hypothyroid Due To Iodine Rx
 ;;^UTILITY(U,$J,358.3,3248,2)
 ;;=^267817
 ;;^UTILITY(U,$J,358.3,3249,0)
 ;;=244.9^^40^249^35
 ;;^UTILITY(U,$J,358.3,3249,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3249,1,4,0)
 ;;=4^244.9
 ;;^UTILITY(U,$J,358.3,3249,1,5,0)
 ;;=5^Hypothyroid, Unspec Cause
 ;;^UTILITY(U,$J,358.3,3249,2)
 ;;=^123752
 ;;^UTILITY(U,$J,358.3,3250,0)
 ;;=245.0^^40^249^46
 ;;^UTILITY(U,$J,358.3,3250,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3250,1,4,0)
 ;;=4^245.0
 ;;^UTILITY(U,$J,358.3,3250,1,5,0)
 ;;=5^Thyroiditis, Acute
 ;;^UTILITY(U,$J,358.3,3250,2)
 ;;=^2692
 ;;^UTILITY(U,$J,358.3,3251,0)
 ;;=245.1^^40^249^47
 ;;^UTILITY(U,$J,358.3,3251,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3251,1,4,0)
 ;;=4^245.1
 ;;^UTILITY(U,$J,358.3,3251,1,5,0)
 ;;=5^Thyroiditis, Subacute
 ;;^UTILITY(U,$J,358.3,3251,2)
 ;;=^119376
 ;;^UTILITY(U,$J,358.3,3252,0)
 ;;=733.01^^40^249^43
 ;;^UTILITY(U,$J,358.3,3252,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3252,1,4,0)
 ;;=4^733.01
 ;;^UTILITY(U,$J,358.3,3252,1,5,0)
 ;;=5^Osteoporosis, Senile
 ;;^UTILITY(U,$J,358.3,3252,2)
 ;;=Osteoporosis, Senile^87188
 ;;^UTILITY(U,$J,358.3,3253,0)
 ;;=733.02^^40^249^42
 ;;^UTILITY(U,$J,358.3,3253,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3253,1,4,0)
 ;;=4^733.02
 ;;^UTILITY(U,$J,358.3,3253,1,5,0)
 ;;=5^Osteoporosis, Idiopathic
 ;;^UTILITY(U,$J,358.3,3253,2)
 ;;=Osteoporosis, Idiopathic^272692
 ;;^UTILITY(U,$J,358.3,3254,0)
 ;;=268.2^^40^249^39
 ;;^UTILITY(U,$J,358.3,3254,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3254,1,4,0)
 ;;=4^268.2
 ;;^UTILITY(U,$J,358.3,3254,1,5,0)
 ;;=5^Osteomalacia
 ;;^UTILITY(U,$J,358.3,3254,2)
 ;;=Osteomalacia^87103
 ;;^UTILITY(U,$J,358.3,3255,0)
 ;;=733.90^^40^249^40
 ;;^UTILITY(U,$J,358.3,3255,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3255,1,4,0)
 ;;=4^733.90
 ;;^UTILITY(U,$J,358.3,3255,1,5,0)
 ;;=5^Osteopenia
 ;;^UTILITY(U,$J,358.3,3255,2)
 ;;=Osteopenia^35593
 ;;^UTILITY(U,$J,358.3,3256,0)
 ;;=275.49^^40^249^44
 ;;^UTILITY(U,$J,358.3,3256,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3256,1,4,0)
 ;;=4^275.49
 ;;^UTILITY(U,$J,358.3,3256,1,5,0)
 ;;=5^Pseudohypoparathyroidism
 ;;^UTILITY(U,$J,358.3,3256,2)
 ;;=Pseudohypparathyroidism^317904
 ;;^UTILITY(U,$J,358.3,3257,0)
 ;;=266.2^^40^249^48
 ;;^UTILITY(U,$J,358.3,3257,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3257,1,4,0)
 ;;=4^266.2
 ;;^UTILITY(U,$J,358.3,3257,1,5,0)
 ;;=5^Vitamin B12 Deficiency
 ;;^UTILITY(U,$J,358.3,3257,2)
 ;;=Vitamin B12 Deficiency^87347
 ;;^UTILITY(U,$J,358.3,3258,0)
 ;;=268.9^^40^249^50
 ;;^UTILITY(U,$J,358.3,3258,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3258,1,4,0)
 ;;=4^268.9
 ;;^UTILITY(U,$J,358.3,3258,1,5,0)
 ;;=5^Vitamin D Deficiency
 ;;^UTILITY(U,$J,358.3,3258,2)
 ;;=Vitamin D Deficiency^126968
 ;;^UTILITY(U,$J,358.3,3259,0)
 ;;=266.1^^40^249^49
 ;;^UTILITY(U,$J,358.3,3259,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3259,1,4,0)
 ;;=4^266.1
 ;;^UTILITY(U,$J,358.3,3259,1,5,0)
 ;;=5^Vitamin B6 Deficiency
 ;;^UTILITY(U,$J,358.3,3259,2)
 ;;=^101683
 ;;^UTILITY(U,$J,358.3,3260,0)
 ;;=780.99^^40^249^2
 ;;^UTILITY(U,$J,358.3,3260,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3260,1,4,0)
 ;;=4^780.99
 ;;^UTILITY(U,$J,358.3,3260,1,5,0)
 ;;=5^Cold Intolerance
 ;;^UTILITY(U,$J,358.3,3260,2)
 ;;=Cold Intolerance^328568
 ;;^UTILITY(U,$J,358.3,3261,0)
 ;;=255.41^^40^249^1
 ;;^UTILITY(U,$J,358.3,3261,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3261,1,4,0)
 ;;=4^255.41
 ;;^UTILITY(U,$J,358.3,3261,1,5,0)
 ;;=5^Adrenal Insuff
 ;;^UTILITY(U,$J,358.3,3261,2)
 ;;=^335240
 ;;^UTILITY(U,$J,358.3,3262,0)
 ;;=276.9^^40^249^10
 ;;^UTILITY(U,$J,358.3,3262,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3262,1,4,0)
 ;;=4^276.9
 ;;^UTILITY(U,$J,358.3,3262,1,5,0)
 ;;=5^FLUID/ELECTROLYTE ABNORMALITY
 ;;^UTILITY(U,$J,358.3,3262,2)
 ;;=^267949
 ;;^UTILITY(U,$J,358.3,3263,0)
 ;;=793.2^^40^250^3
 ;;^UTILITY(U,$J,358.3,3263,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3263,1,4,0)
 ;;=4^793.2
 ;;^UTILITY(U,$J,358.3,3263,1,5,0)
 ;;=5^Abnormal Chest x-ray, other
 ;;^UTILITY(U,$J,358.3,3263,2)
 ;;=^273419
 ;;^UTILITY(U,$J,358.3,3264,0)
 ;;=277.6^^40^250^22
 ;;^UTILITY(U,$J,358.3,3264,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3264,1,4,0)
 ;;=4^277.6
 ;;^UTILITY(U,$J,358.3,3264,1,5,0)
 ;;=5^Hereditary Angioedema
 ;;^UTILITY(U,$J,358.3,3264,2)
 ;;=^87463
 ;;^UTILITY(U,$J,358.3,3265,0)
 ;;=493.92^^40^250^6
 ;;^UTILITY(U,$J,358.3,3265,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3265,1,4,0)
 ;;=4^493.92
 ;;^UTILITY(U,$J,358.3,3265,1,5,0)
 ;;=5^Asthma, Acute Exacerbation
 ;;^UTILITY(U,$J,358.3,3265,2)
 ;;=^322001
 ;;^UTILITY(U,$J,358.3,3266,0)
 ;;=493.20^^40^250^11
 ;;^UTILITY(U,$J,358.3,3266,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3266,1,4,0)
 ;;=4^493.20
 ;;^UTILITY(U,$J,358.3,3266,1,5,0)
 ;;=5^COPD with Asthma
 ;;^UTILITY(U,$J,358.3,3266,2)
 ;;=COPD with Asthma^269964
 ;;^UTILITY(U,$J,358.3,3267,0)
 ;;=493.91^^40^250^7
 ;;^UTILITY(U,$J,358.3,3267,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3267,1,4,0)
 ;;=4^493.91
 ;;^UTILITY(U,$J,358.3,3267,1,5,0)
 ;;=5^Asthma, with Status Asthmat
 ;;^UTILITY(U,$J,358.3,3267,2)
 ;;=^269967
 ;;^UTILITY(U,$J,358.3,3268,0)
 ;;=491.21^^40^250^10
 ;;^UTILITY(U,$J,358.3,3268,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3268,1,4,0)
 ;;=4^491.21
 ;;^UTILITY(U,$J,358.3,3268,1,5,0)
 ;;=5^COPD Exacerbation
 ;;^UTILITY(U,$J,358.3,3268,2)
 ;;=COPD Exacerbation^269954
 ;;^UTILITY(U,$J,358.3,3269,0)
 ;;=494.0^^40^250^9
 ;;^UTILITY(U,$J,358.3,3269,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3269,1,4,0)
 ;;=4^494.0
 ;;^UTILITY(U,$J,358.3,3269,1,5,0)
 ;;=5^Bronchiectasis, chronic
 ;;^UTILITY(U,$J,358.3,3269,2)
 ;;=^321990
 ;;^UTILITY(U,$J,358.3,3270,0)
 ;;=494.1^^40^250^8
 ;;^UTILITY(U,$J,358.3,3270,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3270,1,4,0)
 ;;=4^494.1
 ;;^UTILITY(U,$J,358.3,3270,1,5,0)
 ;;=5^Bronchiectasis with exacerb
 ;;^UTILITY(U,$J,358.3,3270,2)
 ;;=^321991
 ;;^UTILITY(U,$J,358.3,3271,0)
 ;;=496.^^40^250^12
 ;;^UTILITY(U,$J,358.3,3271,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3271,1,4,0)
 ;;=4^496.
 ;;^UTILITY(U,$J,358.3,3271,1,5,0)
 ;;=5^COPD, General
 ;;^UTILITY(U,$J,358.3,3271,2)
 ;;=COPD, General^24355
 ;;^UTILITY(U,$J,358.3,3272,0)
 ;;=491.20^^40^250^13
 ;;^UTILITY(U,$J,358.3,3272,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3272,1,4,0)
 ;;=4^491.20
 ;;^UTILITY(U,$J,358.3,3272,1,5,0)
 ;;=5^Chronic Asthmatic Bronchitis
 ;;^UTILITY(U,$J,358.3,3272,2)
 ;;=Chronic Asthmatic Bronchitis^269953
 ;;^UTILITY(U,$J,358.3,3273,0)
 ;;=491.9^^40^250^14
 ;;^UTILITY(U,$J,358.3,3273,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3273,1,4,0)
 ;;=4^491.9
 ;;^UTILITY(U,$J,358.3,3273,1,5,0)
 ;;=5^Chronic Bronchitis
 ;;^UTILITY(U,$J,358.3,3273,2)
 ;;=^24359
 ;;^UTILITY(U,$J,358.3,3274,0)
 ;;=786.2^^40^250^15
 ;;^UTILITY(U,$J,358.3,3274,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3274,1,4,0)
 ;;=4^786.2
