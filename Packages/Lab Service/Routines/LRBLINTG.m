LRBLINTG ;DALLAS CIOFO/RLM/CYM - INTEGRITY CHECKER FOR BLOOD BANK ROUTINES ;08/20/2001 4:35 PM
 ;;5.2;LAB SERVICE;**247,267,275**;Sep 27, 1994
 ;Per VHA Directive 97-033 this routine should not be modified.  Medical Device # BK970021
START ; Routine called from LRBLSRV
 S X=$T(+0) X ^%ZOSF("RSUM") S ^TMP("LRBL",$J,2,0)="LRBLINTG at "_LRBLSITE_" = "_Y
 F LRI=1:1 S LRA=$T(ROU+LRI) Q:LRA["***"  S X=$P(LRA,",",2) D
  . X ^%ZOSF("TEST") I '$T S ^TMP("LRBL",$J,LRI+3,0)=X_" is missing." Q
  . X ^%ZOSF("RSUM")  I Y'=$P(LRA,",",3) S ^TMP("LRBL",$J,LRI+3,0)=X_" should be "_$P(LRA,",",3)_" is "_Y
 K XMY S XMY("G.bloodbank@ISC-CHICAGO.domain.ext")=""
 S %DT="T",X="NOW" D ^%DT,DD^LRX S LRBLNOW=Y
 S XMSUB="BB Checksum data at "_LRBLSITE_" run on "_LRBLNOW
 S XMTEXT="^TMP(""LRBL"",$J,",XMDUZ="Blood Bank Monitor" D ^XMD
 K %DT,LRA,LRBLNOW,LRBLSITE,LRI,X,XMDUZ,XMSUB,XMTEXT,Y
 K ^TMP("LRBL",$J)
 Q
ROU ;
 ;;,LRBLA,11361264
 ;;,LRBLA1,10106404
 ;;,LRBLA2,7315572
 ;;,LRBLAA,14549108
 ;;,LRBLAA1,3751384
 ;;,LRBLAB,1343081
 ;;,LRBLAGG,2364163
 ;;,LRBLAUD,4865269
 ;;,LRBLAUD1,9357692
 ;;,LRBLB,7956270
 ;;,LRBLBU,1843508
 ;;,LRBLC,1993068
 ;;,LRBLCAP,2878590
 ;;,LRBLCMV,4895767
 ;;,LRBLD,3295327
 ;;,LRBLDA,12115475
 ;;,LRBLDA1,3235853
 ;;,LRBLDAA,12463641
 ;;,LRBLDAL,9196362
 ;;,LRBLDC,14196802
 ;;,LRBLDC1,5278208
 ;;,LRBLDCR,10142238
 ;;,LRBLDCU,5556264
 ;;,LRBLDED,6231669
 ;;,LRBLDEL,9949435
 ;;,LRBLDELT,5309734
 ;;,LRBLDEX,2280729
 ;;,LRBLDEX1,13643755
 ;;,LRBLDEX2,14294791
 ;;,LRBLDK,4408781
 ;;,LRBLDL,8027727
 ;;,LRBLDL1,1162721
 ;;,LRBLDLG,19191632
 ;;,LRBLDMV,6414505
 ;;,LRBLDP,2025215
 ;;,LRBLDPA,3904141
 ;;,LRBLDPA1,13804264
 ;;,LRBLDPA2,13967384
 ;;,LRBLDPAW,3164806
 ;;,LRBLDPH,10358285
 ;;,LRBLDPL,3867229
 ;;,LRBLDR,15537243
 ;;,LRBLDR1,7668103
 ;;,LRBLDRR,17451778
 ;;,LRBLDRR1,25481475
 ;;,LRBLDRR2,3243087
 ;;,LRBLDRR3,1756464
 ;;,LRBLDSC,8397022
 ;;,LRBLDT,13410130
 ;;,LRBLDTA,2557404
 ;;,LRBLDUC,5267997
 ;;,LRBLDW,5874762
 ;;,LRBLDX,4935632
 ;;,LRBLJA,17223916
 ;;,LRBLJA1,6301480
 ;;,LRBLJB,9637545
 ;;,LRBLJC,9275638
 ;;,LRBLJCK,9867617
 ;;,LRBLJD,12678359
 ;;,LRBLJD1,14481597
 ;;,LRBLJDA,9462759
 ;;,LRBLJDM,15110824
 ;;,LRBLJDP,9494946
 ;;,LRBLJED,15092195
 ;;,LRBLJI,8199613
 ;;,LRBLJL,20113772
 ;;,LRBLJL1,8092253
 ;;,LRBLJLA,10021308
 ;;,LRBLJLG,22434197
 ;;,LRBLJLG1,12109485
 ;;,LRBLJM,5716689
 ;;,LRBLJM1,15670509
 ;;,LRBLJP,5290456
 ;;,LRBLJPA,5625331
 ;;,LRBLJPA1,15924799
 ;;,LRBLJPA2,11292074
 ;;,LRBLJPH,6292279
 ;;,LRBLJPP,12819280
 ;;,LRBLJPP1,6500529
 ;;,LRBLJR,8337634
 ;;,LRBLJRB,10178183
 ;;,LRBLJSH,10420740
 ;;,LRBLJT,9133136
 ;;,LRBLJTS,3989459
 ;;,LRBLJTS1,8904563
 ;;,LRBLJTS2,8577037
 ;;,LRBLJU,6005102
 ;;,LRBLJU1,7426924
 ;;,LRBLJUT,5844963
 ;;,LRBLJW,11281581
 ;;,LRBLJX,5755259
 ;;,LRBLP,17385811
 ;;,LRBLPA,3613885
 ;;,LRBLPAB,2266423
 ;;,LRBLPB,1128898
 ;;,LRBLPBR,6311255
 ;;,LRBLPBR1,15305150
 ;;,LRBLPC,15671667
 ;;,LRBLPC1,7192443
 ;;,LRBLPCS,10290637
 ;;,LRBLPCS1,13639750
 ;;,LRBLPCSS,9974761
 ;;,LRBLPD,11082733
 ;;,LRBLPD1,10207069
 ;;,LRBLPE,12068165
 ;;,LRBLPE1,9122655
 ;;,LRBLPED,17147926
 ;;,LRBLPED1,9800010
 ;;,LRBLPED2,3635113
 ;;,LRBLPEW,12743930
 ;;,LRBLPH,3897023
 ;;,LRBLPIT,7051859
 ;;,LRBLPP,1084680
 ;;,LRBLPQA,8810766
 ;;,LRBLPR,6345892
 ;;,LRBLPR1,13035995
 ;;,LRBLPRA,5693589
 ;;,LRBLPT,9150490
 ;;,LRBLPT1,11224251
 ;;,LRBLPTR,11758949
 ;;,LRBLPTR1,10126100
 ;;,LRBLPUS,13178966
 ;;,LRBLPUS1,10560710
 ;;,LRBLPUS2,6349114
 ;;,LRBLPX,16100078
 ;;,LRBLPX1,12892470
 ;;,LRBLQPR,5571366
 ;;,LRBLQST,7084452
 ;;,LRBLRCT,9631694
 ;;,LRBLS,11617800
 ;;,LRBLSCRN,1075474
 ;;,LRBLSET,2877561
 ;;,LRBLSRV,1681921
 ;;,LRBLSSN,3897804
 ;;,LRBLST,3535761
 ;;,LRBLSTR,3249276
 ;;,LRBLSUM,1987283
 ;;,LRBLTA,6903899
 ;;,LRBLTA1,4221860
 ;;,LRBLTX,13956029
 ;;,LRBLTXA,11752893
 ;;,LRBLU,14043423
 ;;,LRBLUL,1134633
 ;;,LRBLVAL,715004
 ;;,LRBLW,4545909
 ;;,LRBLWD,4923072
 ;;,LRBLWDS,8168141
 ;;,LRBLY,3089131
 ;;,LRUB,2794163
 ;;,LRUC,1474534
 ;;,LRUCN,5719131
 ;;,LRUD,8613944
 ;;,LRUDIT,1873186
 ;;,LRUL,6494173
 ;;,LRUMSG,549954
 ;;,LRUT,1482424
 ;***
 Q
