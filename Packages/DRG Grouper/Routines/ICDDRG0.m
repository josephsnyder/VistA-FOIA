ICDDRG0 ;ALB/GRR/EG/ADL - DRG GROUPER PROCESSING BEGINS ; 11/13/07 4:06pm
 ;;18.0;DRG Grouper;**1,2,7,10,14,17,20,24,27,30,31,32,33,37,45,50,53,56,65*;Oct 20, 2000;Build 12
 ;GROUPING PROCESS BEGINS
 ;
GROUP ;
 I $D(ICDSEX(1))&($D(ICDSEX(2))) S ICDRTC=4,ICDDRG=$S(ICDDATE>3070930.9:999,1:470) G KILL^ICDDRG
 I ICDMDC'=14,ICDMDC'=17,ICDMDC'=18,ICDMDC'=19,ICDMDC'=20,ICDMDC'=23,ICDMDC'=15 D:ICDOPCT<2  I ((ICDDATE'>3070930.9)&("468^476^477"[ICDRG))!((ICDDATE>3070930.9)&("983^986^989"[ICDRG)) G END
 . I $D(ICDF) Q
 . I ICDPD["M",ICDOR'["y" S ICDOPCT=0 Q
 . I ICDDATE>3070930.9 D
 . . I ICDOR["O",ICDNOR=ICDONR,ICDNOR>0,'$D(ICDPDRG(769)),ICDORNI'["p" S ICDRG=$S($D(ICDORNI("O")):983,ICDORNI["y":986,ICDORNI["z":989,1:983) Q
 . . I ICDOPNR S ICDRG=$S(ICDORNI["y":986,1:983),ICDOPNR=0 Q
 . E  D
 . . I ICDOR["O",ICDNOR=ICDONR,ICDNOR>0,'$D(ICDPDRG(377)),ICDORNI'["p" S ICDRG=$S($D(ICDORNI("O")):468,ICDORNI["y":476,ICDORNI["z":477,1:468) Q
 . . I ICDOPNR S ICDRG=$S(ICDORNI["y":476,1:468),ICDOPNR=0 Q
 ;
 ;if number of non-extensive ORs eqs # OR, 477
 ;
 I ICDMDC'=14,ICDMDC'=17,ICDMDC'=18,ICDMDC'=19,ICDMDC'=20,ICDMDC'=23,ICDMDC'=15,ICDORNI'["y"&(ICDORNI'="")&(ICDORNI["z") D  I ((ICDDATE'>3070930.9)&(ICDRG=477))!((ICDDATE>3070930.9)&(ICDRG=989)) G END
 . I $D(ICDF) Q
 . NEW K S K=$$ORNI(ICDORNI) I K=ICDOPCT S ICDRG=$S(ICDDATE>3070930.9:989,1:477) Q
 ;
 ;if number of non-extensive ORs+prostatics eqs # OR, 476
 ;
 I ICDMDC'=14,ICDMDC'=17,ICDMDC'=18,ICDMDC'=19,ICDMDC'=20,ICDMDC'=23,ICDMDC'=15,ICDORNI["y"&(ICDORNI'="") D  I ((ICDDATE'>3070930.9)&(ICDRG=476))!((ICDDATE>3070930.9)&(ICDRG=986)) G END
 .N K S K=$$ORNI(ICDORNI) I K=ICDOPCT&(ICDNOR=ICDONR) S ICDRG=$S(ICDDATE>3070930.9:986,1:476) Q
 I ICDMDC'=14,ICDMDC'=17,ICDMDC'=18,ICDMDC'=19,ICDMDC'=20,ICDMDC'=23,ICDMDC'=15,ICDNOR=ICDONR&(ICDOPCT>0) S ICDRG=$S(ICDDATE>3070930.9:983,1:468) G END
 I ICDMDC=5,ICDOR'["O" S ICDRTC=$S(ICDEXP="":5,1:"") S:ICDRTC'="" ICDRG=$S(ICDDATE>3070930.9:999,1:470) D:ICDRTC="" MI G END
 ;I ICDMDC=18,ICDOR["O"!(ICDORNI["O") S ICDRG=415 G END  ;;disabled by ICD*18*24 and new DRGs 578/579 - see ICDTLB6C
 I ICDMDC=19,ICDOCNT>0,ICDOR["O" S (ICDRG,HICDRG)=$S(ICDDATE>3070930.9:876,1:424) D CKDRG
 I ICDMDC=23,ICDOR["O"!(ICDORNI["O") S ICDRG=$S(ICDDATE>3070930.9:941,1:461) G END
 I ICDMDC=14 D ^ICDDRG14 I ICDRG]"" G END
 I ICDMDC=20 S ICDRTC=$S(ICDDMS="":7,1:"") I ICDDMS'=0 D  G END
 . I ICDDATE>3070930.9 S ICDRG=$S(ICDDMS="":999,1:894) Q
 . S ICDRG=$S(ICDDMS="":470,1:433)
 I ICDMDC=22 S ICDRTC=$S(ICDTRS="":6,1:"") S:ICDRTC'="" ICDRG=$S(ICDDATE>3070930.9:999,1:470) D:ICDRTC="" CKBURN G END
 I ICDMDC=15 S ICDRTC=$S(ICDEXP="":5,ICDTRS="":6,1:"") I ICDTRS'=0 D  G END
 . I ICDDATE>3070930.9 S ICDRG=$S(ICDRTC'="":999,1:789) Q
 . S ICDRG=$S(ICDRTC'="":470,1:385)
NEONATE I 'ICDNOR!('$D(ICDODRG)) S ICDRG=$O(ICDPDRG(0)) X "I ICDMDC=15,$D(ICDSDRG),$O(ICDSDRG(0))<ICDRG D NEONATF^ICDDRG0" D  D DODRG G GETMOR:ICDRG="",END
 . N X,X1,X2,%
 . S X1=$S($G(DGADM):$G(DGADM),1:DT),X2=$G(DOB) I X1,X2 D ^%DTC I X<29 D NBCOMP Q
 . I ICDDATE'>3070930.9 I ICDRG<385!(ICDRG>391) Q
 . I ICDDATE>3070930.9 I ICDRG<789!(ICDRG>795) Q
 .; I "^11917^11918^11921^"[("^"_ICDDX(1)_"^") S ICDRG=395 Q
 . I ICDDATE'>3070930.9 I $O(ICDRG(391)) S ICDRG=$O(ICDRG(391)) Q
 . I ICDDATE>3070930.9 I $O(ICDRG(795)) S ICDRG=$O(ICDRG(795)) Q
 . I 'ICDRG S ICDRG=$S(ICDDATE>3070930.9:999,1:470),ICDRTC=8
 I AGE="",ICDMDC=3 S ICDRTC=3 S ICDRG=$S(ICDDATE>3070930.9:999,1:470) G END
 D ^ICDDRG1:ICDMDC=1,^ICDDRG2:ICDMDC=2,^ICDDRG3:ICDMDC=3,^ICDDRG5:ICDMDC=5,^ICDDRG6:ICDMDC=6,^ICDDRG7:ICDMDC=7,^ICDDRG8:ICDMDC=8,^ICDDRG9:ICDMDC=9,^ICDDRG10:ICDMDC=10,^ICDDRG11:ICDMDC=11,^ICDDRG12:ICDMDC=12,^ICDDRG13:ICDMDC=13
 D ^ICDDRG17:ICDMDC=17
CONT G:ICDMDC=15 GETMOR S (ICDRG,HICDRG)=$O(ICDODRG(0)) G:ICDRG'>0 ENTER
 D DODRG
 G:ICDRG'>0 LOOK8:ICDMDC=8,AGAIN G END
ENTER I 'ICDNOR,ICDORNR'=0,ICDMDC'=20,ICDMDC'=15 S ICDRG=$S(ICDDATE>3070930.9:983,1:468) G END
GETMOR S (ICDRG,HICDRG)=$O(ICDPDRG(0)) S:ICDRG'>0 (ICDRG,HICDRG)=$S(ICDDATE>3070930.9:998,1:469)
CKDRG D DODRG
 I ICDRG="" K ICDPDRG(HICDRG) G GETMOR
DODRG ;Go to DRG file and retrieve table entry to use if defined
 N ICDMCV,ICDMCV1,ICDMCV2
 N DRGFY,ICDREF S (DRGFY,ICDREF)=""
 I ICDRG S DRGFY=$O(^ICD(ICDRG,2,"B",$P(+$G(ICDDATE),".")_.01),-1)
 I 'DRGFY S DRGFY=3121001 ;default to current fiscal year
 S ICDREF=$O(^ICD(+ICDRG,2,"B",+DRGFY,ICDREF))
 I ICDREF'="" D
 . S ICDREF=$P($G(^ICD(+ICDRG,2,ICDREF,0)),U,3)
 . S ICDREF="DRG"_ICDRG_"^"_ICDREF D @ICDREF K ICDREF
 I ICDOR["4"&(ICDDATE<3071001) D DRG232^ICDTLB3
 Q
ORNI(X) ;
 N I,K
 S K=0 F I=1:1:$L(ICDORNI) I $E(ICDORNI,I,I)="z"!($E(ICDORNI,I,I)="y") S K=K+1
 Q K
END ;
 D:ICDP24'=""!($D(ICDS24)) CKMST^ICDDRGX S ICDDRG=ICDRG
 ;ICD*18*24 check for higher numbered DRG (such as new DRG 561) before checking for 489 in CKHIV^ICDDRGX
 I ICDDATE<3071001 I ICDRG=489!(ICDRG=490)!(ICDRG=543&($G(ICDOR)="")) S ICDRG=$P($G(ICDPDRG),U,2) I ICDRG=543 S ICDRG=561
 I ICDDATE'<3071001 I ICDRG=976!(ICDRG=977)!(ICDRG=24&($G(ICDOR)="")) S ICDRG=$P($G(ICDPDRG),U,2) I ICDRG=24 S ICDRG=99
 D:$G(ICDP25)=1!(($G(ICDP25)>1)&($D(ICDS25(1)))) CKHIV^ICDDRGX S ICDDRG=ICDRG
 ; this will effectively make DRG 103 into a pre-MDC (ICD*18*1)
 I $D(ICDOP(" 33.6"))!$D(ICDOP(" 37.5"))!(ICDDATE>3030930.9&($D(ICDOP(" 37.51"))!$D(ICDOP(" 37.66")))) S ICDRG=$S(ICDDATE>3070930.9:2,1:103),ICDNMDC(1)="" D DODRG
 I (ICDDATE>3050930.9)&($D(ICDOP(" 37.64")))&($D(ICDOP(" 37.65"))) S ICDRG=$S(ICDDATE>3070930.9:2,1:103),ICDNMDC(1)="" D DODRG
 I (ICDDATE>3060930.9)&($D(ICDOP(" 37.63")))&($D(ICDOP(" 37.64"))) S ICDRG=$S(ICDDATE>3070930.9:2,1:103),ICDNMDC(1)="" D DODRG
 I $D(ICDOP(" 39.65")) S ICDRG=$S(ICDDATE>3070930.9:3,1:541),ICDNMDC(1)=""
 I (ICDDATE>3070930.9)&($D(ICDOP(" 46.97"))) S ICDRG=5,ICDNMDC(1)=""
 ; this will create DRGs 512/513 as pre-MDC
 I $D(ICDOP(" 52.80"))!$D(ICDOP(" 52.82")) S ICDRG=$S(ICDDATE>3070930.9:10,1:513),ICDNMDC(1)=""
 I (ICDDATE>3070930.9) D
 . I ICDRG=10 I $D(ICDOP(" 55.69")) S ICDRG=8
 E  I ICDRG=513 I $D(ICDOP(" 55.69")) S ICDRG=512
 ; this will create DRG 481 as pre-MDC - loops thru 41.00 thru .09
 N X S X=0 F  S X=$O(ICDOP(X)) Q:X=""  I X["41.0" S ICDRG=$S(ICDDATE>3070930.9&ICDDATE<3101001:9,ICDDATE>3100930.9:14,1:481),ICDNMDC(1)=""
 I $D(ICDNMDC(1)) I ICDNMDC(1)="" D CKNMDC^ICDDRGX S ICDDRG=ICDRG K ICDNMDC
 I $D(ICDOP(" 37.52"))&(ICDDATE>3070930.9)&(ICDDATE<3081001) D
   .S ICDRG=215
   .S ICDDRG=215 Q
 I ICDDATE>3070930.9 D
 . I ICDRG=983 D CHKMDC4^ICDDRGX
 . D DODRG S ICDDRG=ICDRG ;check for MCC/CC
 E  I ICDRG=468 D CHKMDC4^ICDDRGX D DODRG S ICDDRG=ICDRG
 S:ICDRTC="" ICDRTC=0
 S ICDTMP=$$DRG^ICDGTDRG(ICDDRG,ICDDATE) I '$P(ICDTMP,U,14) S ICDDRG=$S(ICDDATE>3070930.9:999,1:470)
 G KILL^ICDDRG
MI ;
 ; if PTCA and not a bypass
 I ICDOR["1"!($D(ICDOP(" 37.90"))) I ICDOR'["b"&(ICDOR'["6") D  Q
 . I ICDDATE>3070930.9 D CMS516^ICDTBL2 Q
 . E  D DRG516^ICDTLB6B
 I ICDPD["A" D EN1^ICDDRG5 I ICDCC3 S ICDRG=$O(ICDODRG(0)) D DODRG Q
 I ICDPD["AI"!(ICDSD["AI") D  Q
 . I ICDDATE>3070930.9 D
 . . S ICDRG=$S($S($D(ICDEXP):ICDEXP,1:0):285,ICDPD["V"!(ICDSD["V"):280,1:282)
 . E  D 
 . . I $D(ICDOP(" 36.07")) I $D(ICDOP(" 37.26"))!($D(ICDOP(" 37.27"))) S ICDRG=526 Q
 . . S ICDRG=$S($S($D(ICDEXP):ICDEXP,1:0):123,ICDPD["s"!ICDPD["V"!(ICDSD["s")!(ICDSD["V"):121,1:122)
 I $D(ICDOP(" 37.26"))&($D(ICDOP(" 39.61"))) S ICDRG=$S(ICDDATE>3070930.9:230,1:108) Q
 ;I $D(ICDOP(" 37.26")) S ICDRG=112 Q
 I ICDDATE<3071001 I $D(ICDOP(" 36.07")) I $D(ICDOP(" 37.26"))!($D(ICDOP(" 37.27"))) S ICDRG=527 Q
 I ICDDATE<3071001 I $D(ICDOP(" 36.06")) I $D(ICDOP(" 37.26"))!$D(ICDOP(" 37.27")) S ICDRG=517 Q
 ;I $D(ICDOP(" 37.26"))!$D(ICDOP(" 37.27")) S ICDRG=$S(ICDDATE>3070930.9:251,1:518) Q
 I ICDOR["H" D  Q
 . I ICDDATE>3070930.9  S ICDRG=$S(ICDPD["X"!(ICDSD["X"):286,1:287) Q
 . E  S ICDRG=$S(ICDPD["X"!(ICDSD["X"):124,1:125) Q
 I ICDDATE>3070930.9 K ICDPDRG(286),ICDPDRG(287)
 E  K ICDPDRG(124)
 I ICDOR["p" S ICDRG=$O(ICDODRG(0)) D DODRG Q
 I ICDOR["F" S ICDRG=$O(ICDODRG(0)) D DODRG Q
 E  D  Q
 . I ICDDATE>3070930.9 K ICDPDRG(280),ICDPDRG(281),ICDPDRG(282) S ICDRG=$O(ICDPDRG(0)) D DODRG Q
 . E  K ICDPDRG(121) S ICDRG=$O(ICDPDRG(0)) D DODRG Q
 ;
CKBURN ; MDC22 - Burns (extensive, full thickness, or non-extensive)
 D
 . I ICDPD["*"!(ICDSD["*") D  Q
 . . I ICDDATE>3070930.9 S ICDRG=$S(ICDOR["k":927,1:933) Q
 . . E  S ICDRG=$S(ICDOR["k":504,1:505) Q
 . I ICDPD["b"!(ICDSD["b") D FTBURN Q
 . I ICDDATE>3070930.9 S ICDRG=$S(ICDCC!(ICDPD["T")!(ICDSD["T"):935,1:935)
 . E  S ICDRG=$S(ICDCC!(ICDPD["T")!(ICDSD["T"):510,1:511)
 Q
 ;
AGAIN G:'$D(ICDODRG) ENTER
 K ICDODRG(HICDRG) I $O(ICDODRG(HICDRG))'>0 K ICDODRG G GROUP
 S ICDRG=$O(ICDODRG(HICDRG)) G GROUP
 ;
 ;
LOOK8 G:'$D(ICDJ) GETMOR
 S ICDJ=$O(ICDJ(0)) G:ICDJ'>0 GETMOR
 K ICDJ(ICDJ),ICDODRG D END^ICDDRG8 G GETMOR:'$D(ICDODRG),CONT
 Q
 ;
NBCOMP ; check for complication related to NB
 I ICDSD'["J"!'$D(ICDSDRG) Q
 N ICDSDXCK
 S ICDSDXCK=$O(ICDSDRG(0))
 I ICDDATE>3070930.9 D
 . I ICDSDXCK<ICDRG,ICDSDXCK>788,ICDSDXCK<796 D
 .. S ICDRG=$S($D(ICDPDRG(795)):795,$D(ICDPDRG(791)):791,1:$O(ICDSDRG(0)))
 E  D
 . I ICDSDXCK<ICDRG,ICDSDXCK>384,ICDSDXCK<392 D
 .. S ICDRG=$S($D(ICDPDRG(391)):391,$D(ICDPDRG(387)):387,1:$O(ICDSDRG(0)))
 Q
 ;
FTBURN ; full thickness burn check
 I ICDSD["j"!(ICDOR["k") D
 . I ICDCC!(ICDPD["T")!(ICDSD["T") S ICDRG=$S(ICDDATE>3070930.9:928,1:506)
 . E  S ICDRG=$S(ICDDATE>3070930.9:929,1:507)
 E  D
 . I ICDCC!(ICDPD["T")!(ICDSD["T") S ICDRG=$S(ICDDATE>3070930.9:934,1:508)
 . E  S ICDRG=$S(ICDDATE>3070930.9:934,1:509)
 Q
 ;
NEONATF ;NEONATE - Continuation of xecute line
 I ICDDATE>3070930.9 S ICDRG=$S($D(ICDPDRG(795)):795,$D(ICDPDRG(791)):791,1:$O(ICDSDRG(0))) Q
 S ICDRG=$S($D(ICDPDRG(391)):391,$D(ICDPDRG(387)):387,1:$O(ICDSDRG(0)))
 Q
