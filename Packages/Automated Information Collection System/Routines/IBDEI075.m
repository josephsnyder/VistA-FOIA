IBDEI075 ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,9382,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9382,1,2,0)
 ;;=2^Visual Field - Threshold
 ;;^UTILITY(U,$J,358.3,9382,1,3,0)
 ;;=3^92083
 ;;^UTILITY(U,$J,358.3,9383,0)
 ;;=92100^^76^650^19^^^^1
 ;;^UTILITY(U,$J,358.3,9383,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9383,1,2,0)
 ;;=2^Serial Tonometry
 ;;^UTILITY(U,$J,358.3,9383,1,3,0)
 ;;=3^92100
 ;;^UTILITY(U,$J,358.3,9384,0)
 ;;=76519^^76^650^1^^^^1
 ;;^UTILITY(U,$J,358.3,9384,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9384,1,2,0)
 ;;=2^A-Scan, one eye
 ;;^UTILITY(U,$J,358.3,9384,1,3,0)
 ;;=3^76519
 ;;^UTILITY(U,$J,358.3,9384,3,0)
 ;;=^358.33^174^3
 ;;^UTILITY(U,$J,358.3,9384,3,1,0)
 ;;=LT
 ;;^UTILITY(U,$J,358.3,9384,3,172,0)
 ;;=RT
 ;;^UTILITY(U,$J,358.3,9384,3,174,0)
 ;;=50
 ;;^UTILITY(U,$J,358.3,9385,0)
 ;;=76512^^76^650^2^^^^1
 ;;^UTILITY(U,$J,358.3,9385,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9385,1,2,0)
 ;;=2^B-Scan one eye
 ;;^UTILITY(U,$J,358.3,9385,1,3,0)
 ;;=3^76512
 ;;^UTILITY(U,$J,358.3,9385,3,0)
 ;;=^358.33^178^3
 ;;^UTILITY(U,$J,358.3,9385,3,1,0)
 ;;=LT
 ;;^UTILITY(U,$J,358.3,9385,3,176,0)
 ;;=RT
 ;;^UTILITY(U,$J,358.3,9385,3,178,0)
 ;;=50
 ;;^UTILITY(U,$J,358.3,9386,0)
 ;;=92226^^76^650^8^^^^1
 ;;^UTILITY(U,$J,358.3,9386,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9386,1,2,0)
 ;;=2^Ext Ophthalmoscopy, Subseq
 ;;^UTILITY(U,$J,358.3,9386,1,3,0)
 ;;=3^92226
 ;;^UTILITY(U,$J,358.3,9387,0)
 ;;=92060^^76^650^18^^^^1
 ;;^UTILITY(U,$J,358.3,9387,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9387,1,2,0)
 ;;=2^Sensorimotor Exam
 ;;^UTILITY(U,$J,358.3,9387,1,3,0)
 ;;=3^92060
 ;;^UTILITY(U,$J,358.3,9388,0)
 ;;=92240^^76^650^16^^^^1
 ;;^UTILITY(U,$J,358.3,9388,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9388,1,2,0)
 ;;=2^ICG Angiography
 ;;^UTILITY(U,$J,358.3,9388,1,3,0)
 ;;=3^92240
 ;;^UTILITY(U,$J,358.3,9389,0)
 ;;=92065^^76^650^17^^^^1
 ;;^UTILITY(U,$J,358.3,9389,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9389,1,2,0)
 ;;=2^Orthoptic/Pleoptic Training
 ;;^UTILITY(U,$J,358.3,9389,1,3,0)
 ;;=3^92065
 ;;^UTILITY(U,$J,358.3,9390,0)
 ;;=G0117^^76^650^13^^^^1
 ;;^UTILITY(U,$J,358.3,9390,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9390,1,2,0)
 ;;=2^Glaucoma Screen by MD/OD
 ;;^UTILITY(U,$J,358.3,9390,1,3,0)
 ;;=3^G0117
 ;;^UTILITY(U,$J,358.3,9391,0)
 ;;=G0118^^76^650^14^^^^1
 ;;^UTILITY(U,$J,358.3,9391,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9391,1,2,0)
 ;;=2^Glaucoma Screen by Tech
 ;;^UTILITY(U,$J,358.3,9391,1,3,0)
 ;;=3^G0118
 ;;^UTILITY(U,$J,358.3,9392,0)
 ;;=S9150^^76^650^7^^^^1
 ;;^UTILITY(U,$J,358.3,9392,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9392,1,2,0)
 ;;=2^Evaluation by ocularist
 ;;^UTILITY(U,$J,358.3,9392,1,3,0)
 ;;=3^S9150
 ;;^UTILITY(U,$J,358.3,9393,0)
 ;;=76514^^76^650^5^^^^1
 ;;^UTILITY(U,$J,358.3,9393,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9393,1,2,0)
 ;;=2^Corneal Pachymetry
 ;;^UTILITY(U,$J,358.3,9393,1,3,0)
 ;;=3^76514
 ;;^UTILITY(U,$J,358.3,9394,0)
 ;;=92060^^76^650^20^^^^1
 ;;^UTILITY(U,$J,358.3,9394,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9394,1,2,0)
 ;;=2^Special Eye Evaluation
 ;;^UTILITY(U,$J,358.3,9394,1,3,0)
 ;;=3^92060
 ;;^UTILITY(U,$J,358.3,9395,0)
 ;;=92133^^76^650^4^^^^1
 ;;^UTILITY(U,$J,358.3,9395,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9395,1,2,0)
 ;;=2^Cmptr Ophth Img Optic Nerve
 ;;^UTILITY(U,$J,358.3,9395,1,3,0)
 ;;=3^92133
 ;;^UTILITY(U,$J,358.3,9396,0)
 ;;=92134^^76^650^3^^^^1
 ;;^UTILITY(U,$J,358.3,9396,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9396,1,2,0)
 ;;=2^Cmptr Ophth Dx Img Post Segmt
 ;;^UTILITY(U,$J,358.3,9396,1,3,0)
 ;;=3^92134
 ;;^UTILITY(U,$J,358.3,9397,0)
 ;;=67800^^76^651^6^^^^1
 ;;^UTILITY(U,$J,358.3,9397,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9397,1,2,0)
 ;;=2^Chalazion - excision - single
 ;;^UTILITY(U,$J,358.3,9397,1,3,0)
 ;;=3^67800
 ;;^UTILITY(U,$J,358.3,9398,0)
 ;;=65435^^76^651^7^^^^1
 ;;^UTILITY(U,$J,358.3,9398,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9398,1,2,0)
 ;;=2^Corneal debridement* (tx)
 ;;^UTILITY(U,$J,358.3,9398,1,3,0)
 ;;=3^65435
 ;;^UTILITY(U,$J,358.3,9399,0)
 ;;=67700^^76^651^9^^^^1
 ;;^UTILITY(U,$J,358.3,9399,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9399,1,2,0)
 ;;=2^Cyst-drainage* (eyelid)
 ;;^UTILITY(U,$J,358.3,9399,1,3,0)
 ;;=3^67700
 ;;^UTILITY(U,$J,358.3,9400,0)
 ;;=67825^^76^651^13^^^^1
 ;;^UTILITY(U,$J,358.3,9400,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9400,1,2,0)
 ;;=2^Epilation - Electro surgical*
 ;;^UTILITY(U,$J,358.3,9400,1,3,0)
 ;;=3^67825
 ;;^UTILITY(U,$J,358.3,9401,0)
 ;;=67820^^76^651^14^^^^1
 ;;^UTILITY(U,$J,358.3,9401,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9401,1,2,0)
 ;;=2^Epilation - Forceps*
 ;;^UTILITY(U,$J,358.3,9401,1,3,0)
 ;;=3^67820
 ;;^UTILITY(U,$J,358.3,9402,0)
 ;;=67810^^76^651^4^^^^1
 ;;^UTILITY(U,$J,358.3,9402,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9402,1,2,0)
 ;;=2^Biopsy of Eyelid
 ;;^UTILITY(U,$J,358.3,9402,1,3,0)
 ;;=3^67810
 ;;^UTILITY(U,$J,358.3,9403,0)
 ;;=67850^^76^651^15^^^^1
 ;;^UTILITY(U,$J,358.3,9403,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9403,1,2,0)
 ;;=2^Eyelid lesion,destruction, up to 1 cm
 ;;^UTILITY(U,$J,358.3,9403,1,3,0)
 ;;=3^67850
 ;;^UTILITY(U,$J,358.3,9404,0)
 ;;=67840^^76^651^16^^^^1
 ;;^UTILITY(U,$J,358.3,9404,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9404,1,2,0)
 ;;=2^Eyelid lesion,excision, Simple
 ;;^UTILITY(U,$J,358.3,9404,1,3,0)
 ;;=3^67840
 ;;^UTILITY(U,$J,358.3,9405,0)
 ;;=67930^^76^651^17^^^^1
 ;;^UTILITY(U,$J,358.3,9405,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9405,1,2,0)
 ;;=2^Eyelid wound - repair
 ;;^UTILITY(U,$J,358.3,9405,1,3,0)
 ;;=3^67930
 ;;^UTILITY(U,$J,358.3,9406,0)
 ;;=65205^^76^651^20^^^^1
 ;;^UTILITY(U,$J,358.3,9406,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9406,1,2,0)
 ;;=2^FB removal conj superficial*(incl Concretions)
 ;;^UTILITY(U,$J,358.3,9406,1,3,0)
 ;;=3^65205
 ;;^UTILITY(U,$J,358.3,9407,0)
 ;;=65210^^76^651^19^^^^1
 ;;^UTILITY(U,$J,358.3,9407,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9407,1,2,0)
 ;;=2^FB removal conj embedded
 ;;^UTILITY(U,$J,358.3,9407,1,3,0)
 ;;=3^65210
 ;;^UTILITY(U,$J,358.3,9408,0)
 ;;=65222^^76^651^21^^^^1
 ;;^UTILITY(U,$J,358.3,9408,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9408,1,2,0)
 ;;=2^FB removal corneal w/slit lamp*
 ;;^UTILITY(U,$J,358.3,9408,1,3,0)
 ;;=3^65222
 ;;^UTILITY(U,$J,358.3,9409,0)
 ;;=68020^^76^651^8^^^^1
 ;;^UTILITY(U,$J,358.3,9409,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9409,1,2,0)
 ;;=2^Cyst I&D (Conj)
 ;;^UTILITY(U,$J,358.3,9409,1,3,0)
 ;;=3^68020
 ;;^UTILITY(U,$J,358.3,9410,0)
 ;;=65220^^76^651^18^^^^1
 ;;^UTILITY(U,$J,358.3,9410,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9410,1,2,0)
 ;;=2^FB Removal, Cornea w/o Slit Lamp
 ;;^UTILITY(U,$J,358.3,9410,1,3,0)
 ;;=3^65220
 ;;^UTILITY(U,$J,358.3,9411,0)
 ;;=65410^^76^651^3^^^^1
 ;;^UTILITY(U,$J,358.3,9411,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9411,1,2,0)
 ;;=2^Biopsy of Cornea
 ;;^UTILITY(U,$J,358.3,9411,1,3,0)
 ;;=3^65410
 ;;^UTILITY(U,$J,358.3,9412,0)
 ;;=67710^^76^651^31^^^^1
 ;;^UTILITY(U,$J,358.3,9412,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9412,1,2,0)
 ;;=2^Severing of Tarsorrhaphy
 ;;^UTILITY(U,$J,358.3,9412,1,3,0)
 ;;=3^67710
 ;;^UTILITY(U,$J,358.3,9413,0)
 ;;=67715^^76^651^5^^^^1
 ;;^UTILITY(U,$J,358.3,9413,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9413,1,2,0)
 ;;=2^Canthotomy
 ;;^UTILITY(U,$J,358.3,9413,1,3,0)
 ;;=3^67715
 ;;^UTILITY(U,$J,358.3,9414,0)
 ;;=67515^^76^651^24^^^^1
 ;;^UTILITY(U,$J,358.3,9414,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9414,1,2,0)
 ;;=2^Inject Tenon's capsule
 ;;^UTILITY(U,$J,358.3,9414,1,3,0)
 ;;=3^67515
 ;;^UTILITY(U,$J,358.3,9415,0)
 ;;=68135^^76^651^10^^^^1
 ;;^UTILITY(U,$J,358.3,9415,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9415,1,2,0)
 ;;=2^Destruction, Conj Lesion
 ;;^UTILITY(U,$J,358.3,9415,1,3,0)
 ;;=3^68135
 ;;^UTILITY(U,$J,358.3,9416,0)
 ;;=68760^^76^651^1^^^^1
 ;;^UTILITY(U,$J,358.3,9416,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9416,1,2,0)
 ;;=2^Ablation, Lacrimal Punctum
 ;;^UTILITY(U,$J,358.3,9416,1,3,0)
 ;;=3^68760
 ;;^UTILITY(U,$J,358.3,9417,0)
 ;;=68761^^76^651^25^^^^1
 ;;^UTILITY(U,$J,358.3,9417,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9417,1,2,0)
 ;;=2^Insert Plug, Lacrimal Punctum, each
 ;;^UTILITY(U,$J,358.3,9417,1,3,0)
 ;;=3^68761
 ;;^UTILITY(U,$J,358.3,9418,0)
 ;;=68801^^76^651^11^^^^1
 ;;^UTILITY(U,$J,358.3,9418,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9418,1,2,0)
 ;;=2^Dilate Lacrimal Punctum
 ;;^UTILITY(U,$J,358.3,9418,1,3,0)
 ;;=3^68801
 ;;^UTILITY(U,$J,358.3,9419,0)
 ;;=68110^^76^651^26^^^^1
 ;;^UTILITY(U,$J,358.3,9419,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9419,1,2,0)
 ;;=2^Probe Nasolacr Duct, w or w/o Irr
 ;;^UTILITY(U,$J,358.3,9419,1,3,0)
 ;;=3^68110
 ;;^UTILITY(U,$J,358.3,9420,0)
 ;;=65400^^76^651^32^^^^1
 ;;^UTILITY(U,$J,358.3,9420,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9420,1,2,0)
 ;;=2^Superficial Keratectomy
 ;;^UTILITY(U,$J,358.3,9420,1,3,0)
 ;;=3^65400
 ;;^UTILITY(U,$J,358.3,9421,0)
 ;;=65436^^76^651^27^^^^1
 ;;^UTILITY(U,$J,358.3,9421,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9421,1,2,0)
 ;;=2^Remove Corneal Epith, EDTA
 ;;^UTILITY(U,$J,358.3,9421,1,3,0)
 ;;=3^65436
 ;;^UTILITY(U,$J,358.3,9422,0)
 ;;=65772^^76^651^23^^^^1
 ;;^UTILITY(U,$J,358.3,9422,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9422,1,2,0)
 ;;=2^Incision, Cornea, Relaxation
 ;;^UTILITY(U,$J,358.3,9422,1,3,0)
 ;;=3^65772
 ;;^UTILITY(U,$J,358.3,9423,0)
 ;;=37609^^76^651^33^^^^1
 ;;^UTILITY(U,$J,358.3,9423,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9423,1,2,0)
 ;;=2^Temporal Artery Biopsy
 ;;^UTILITY(U,$J,358.3,9423,1,3,0)
 ;;=3^37609
 ;;^UTILITY(U,$J,358.3,9424,0)
 ;;=66821^^76^651^2^^^^1
 ;;^UTILITY(U,$J,358.3,9424,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,9424,1,2,0)
 ;;=2^After Cataract Laser Surgery
 ;;^UTILITY(U,$J,358.3,9424,1,3,0)
 ;;=3^66821
