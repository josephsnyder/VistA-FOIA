YSNTEG0 ;ISC/XTSUMBLD KERNEL - Package checksum checker ;DEC 30, 1994@12:03:30
 ;;5.01;MENTAL HEALTH;;Dec 30, 1994
 ;;7.2;DEC 30, 1994@12:03:30
 S XT4="I 1",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
CONT F XT1=1:1 S XT2=$T(ROU+XT1) Q:XT2=""  S X=$P(XT2," ",1),XT3=$P(XT2,";",3) X XT4 I $T W !,X X ^%ZOSF("TEST") S:'$T XT3=0 X:XT3 ^%ZOSF("RSUM") W ?10,$S('XT3:"Routine not in UCI",XT3'=Y:"Calculated "_$C(7)_Y_", off by "_(Y-XT3),1:"ok")
 ;
 K %1,%2,%3,X,Y,XT1,XT2,XT3,XT4 Q
ONE S XT4="I $D(^UTILITY($J,X))",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
 W !,"Check a subset of routines:" K ^UTILITY($J) X ^%ZOSF("RSEL")
 W ! G CONT
ROU ;;
YTATQ ;;783989
YTAUDIT ;;6602350
YTAUIR ;;6595891
YTAUIRR ;;7352162
YTBECK ;;2239696
YTBI ;;10263261
YTBPRS ;;1984880
YTCHECK ;;13786690
YTCLERK ;;10496816
YTCLERK1 ;;5396070
YTCROSS ;;2730385
YTDEMO ;;925892
YTDESC ;;4447470
YTDP ;;12233757
YTDP1 ;;4433852
YTDRIV ;;6138159
YTEX ;;8262478
YTEXT ;;12642311
YTEXT1 ;;6625740
YTFEAR ;;2515560
YTFILE ;;10292787
YTFIRO ;;1838163
YTKIL ;;6560459
YTKILINC ;;4999571
YTLCTD ;;2849688
YTLIST ;;6963730
YTMATE ;;2398541
YTMCMI ;;6559924
YTMCMI2 ;;5805727
YTMCMI2A ;;6622615
YTMCMI2B ;;11536510
YTMCMI2C ;;15542391
YTMCMI2D ;;4032856
YTMILL ;;4357641
YTMMP1 ;;4734122
YTMMP2 ;;9566672
YTMMP3 ;;7456953
YTMMP4 ;;5122198
YTMMP5 ;;8096895
YTMMP6 ;;5119686
YTMMP7 ;;1105934
YTMMPI ;;4597251
YTMMPI2 ;;11681822
YTMMPI2A ;;8018650
YTMMPI2B ;;15269695
YTMMPI2C ;;3920472
YTMMPI2P ;;10613293
YTMMPP ;;6260198
YTMYER ;;3777254
YTPIT ;;2928525
YTPSI ;;2321930
YTREPT ;;8566612
YTS ;;9326856
YTSCII ;;10186838
YTSTAT ;;5058469
YTTB ;;13326259
YTTLS ;;2366172
