SOWKAR10 ;B'HAM ISC/SAB-Routine to compile RCS 10-0173 report ; 19 Apr 93 / 12:56 PM
 ;;3.0; Social Work ;;27 Apr 93
 K ^TMP($J,STA,"SWRCH") I '$D(ZTSK) W !!,"COMPILING RCH 10-0173 AMIS DATA",! D WAIT^DICD
 S DA=0 F B=0:0 S B=$O(^SOWK(652,B)) Q:'B  S (^TMP($J,STA,"SWD",B),^TMP($J,STA,"SWQ",B),^TMP($J,STA,"A",B))=0
 F B=0:0 S B=$O(^SOWK(655,B)) Q:'B  S F=^SOWK(655,B,0) I $P(F,"^",2) F HM=0:0 S HM=$O(^SOWK(655,$P(F,"^"),4,HM)) Q:'HM  S RCH=^SOWK(655,$P(F,"^"),4,HM,0) D SEA
 F B=0:0 S B=$O(^SOWK(652,B)) Q:'B  S F=^SOWK(652,B,0) D OUT W:'$D(ZTSK) "."
CLOS K SWMR,SWSN,SWNR,SWHC,HM,RCH,AD,^TMP($J,STA,"RCH")
 K DA,RCH,F,STAT,SWZ,SWL,SWBD,SWVO,SWA,SWQ,SWZP,B,X1,X2,X,R,J
 Q
OUT ;Format data
 S SWNR=$S($D(^SOWK(652,B,1))&($P(^(1),"^")]""):$P(^(1),"^"),1:$E($P(F,"^"),1,25)),SWHC=$E($P(F,"^",6),1,15),SWZP=$P(F,"^",8),SWSN=$P(F,"^",3)
 S STAT=$P(^DIC(5,$P(F,"^",7),0),"^",2),SWZ=$E($P(F,"^",13),4,5)_$E($P(F,"^",13),6,7)_$E($P(F,"^",13),2,3)
 S SWL=$P(F,"^",10),SWBD=$P(F,"^",11),SWVO=$P(F,"^",12),^TMP($J,STA,"SWD",B)=$S(^TMP($J,STA,"SWD",B):^TMP($J,STA,"SWD",B),1:0),SWA=$S(^TMP($J,STA,"SWQ",B):^TMP($J,STA,"A",B)\^TMP($J,STA,"SWQ",B),1:0)
 I $L(SWNR)<25 F J=0:0 Q:$L(SWNR)=25  S SWNR=SWNR_$C(32)
 I $L(SWHC)<15 F J=0:0 Q:$L(SWHC)=15  S SWHC=SWHC_$C(32)
 I $L(^TMP($J,STA,"SWQ",B))<3 F J=0:0 Q:$L(^TMP($J,STA,"SWQ",B))=3  S ^TMP($J,STA,"SWQ",B)=0_^TMP($J,STA,"SWQ",B)
 I $L(^TMP($J,STA,"SWD",B))<5 F J=0:0 Q:$L(^TMP($J,STA,"SWD",B))=5  S ^TMP($J,STA,"SWD",B)=0_^TMP($J,STA,"SWD",B)
 I $L(SWBD)<3 F J=0:0 Q:$L(SWBD)=3  S SWBD=0_SWBD
 I $L(SWA)<4 F J=0:0 Q:$L(SWA)=4  S SWA=0_SWA
 S AD=$S($D(^SOWK(652,B,1))&($P(^(1),"^",2)):"A",1:$C(32))
 S DA=DA+1,^TMP($J,STA,"RCH",B)=SWSN_AD_SWNR_SWHC_STAT_SWZ_SWL_^TMP($J,STA,"SWQ",B)_^TMP($J,STA,"SWD",B)_SWBD_SWVO_SWA I $L(^TMP($J,STA,"RCH",B))<80 F J=0:0 Q:$L(^TMP($J,STA,"RCH",B))=80  S ^TMP($J,STA,"RCH",B)=^TMP($J,STA,"RCH",B)_$C(32)
 S ^TMP($J,STA,"SWRCH",DA,0)=^TMP($J,STA,"RCH",B),$P(^SOWK(652,B,1),"^",2)=0
 Q
SEA ;calculate RCH AMIS data
 I $P(RCH,"^",2)<SWB,'$P(RCH,"^",4) S ^TMP($J,STA,"SWQ",$P(RCH,"^"))=^TMP($J,STA,"SWQ",$P(RCH,"^"))+1,X1=SWE,X2=SWB D COM
 I $P(RCH,"^",2)'<SWB,$P(RCH,"^",2)'>SWE,'$P(RCH,"^",4) S ^TMP($J,STA,"SWQ",$P(RCH,"^"))=^TMP($J,STA,"SWQ",$P(RCH,"^"))+1,X1=SWE,X2=$P(RCH,"^",2) D COM
 I $P(RCH,"^",2)'<SWB,$P(RCH,"^",4)'>SWE S X1=$P(RCH,"^",4),X2=$P(RCH,"^",2) D ^%DTC S ^TMP($J,STA,"SWD",$P(RCH,"^"))=X+^TMP($J,STA,"SWD",$P(RCH,"^"))
 I $P(RCH,"^",2)<SWB,$P(RCH,"^",4)>SWE S X1=SWE,X2=SWB,^TMP($J,STA,"SWQ",$P(RCH,"^"))=^TMP($J,STA,"SWQ",$P(RCH,"^"))+1 D COM
 I $P(RCH,"^",2)'<SWB,$P(RCH,"^",2)'>SWE,$P(RCH,"^",4)>SWE S X1=SWE,X2=$P(RCH,"^",2),^TMP($J,STA,"SWQ",$P(RCH,"^"))=^TMP($J,STA,"SWQ",$P(RCH,"^"))+1 D COM
 I $P(RCH,"^",2)<SWB,$P(RCH,"^",4)'<SWB,$P(RCH,"^",4)'>SWE S X1=$P(RCH,"^",4),X2=SWB D ^%DTC S ^TMP($J,STA,"SWD",$P(RCH,"^"))=X+^TMP($J,STA,"SWD",$P(RCH,"^"))
 Q
COM D ^%DTC S ^TMP($J,STA,"SWD",$P(RCH,"^"))=(X+^TMP($J,STA,"SWD",$P(RCH,"^")))+1
 F R=0:0 S R=$O(^SOWK(655,$P(F,"^"),4,HM,1,R)) Q:'R  S SWMR=$P(^SOWK(655,$P(F,"^"),4,HM,1,R,0),"^")
 S ^TMP($J,STA,"A",$P(RCH,"^"))=SWMR+^TMP($J,STA,"A",$P(RCH,"^"))
 Q
TRANS ;TRANSMIT RCH 10-0173 AMIS data
 S (R,REC,SOWKM)=0 F SWQ=0:0 S SWQ=$O(^TMP($J,STA,"SWRCH",SWQ)) Q:'SWQ  W "." S R=R+1,REC=REC+1,^TMP($J,STA,"TRANS",R,0)=^TMP($J,STA,"SWRCH",SWQ,0) I R=180 S SOWKM=SOWKM+1 D TRANS1 K ^TMP($J,STA,"TRANS") S R=0
 I 'SWQ,R S SOWKM=SOWKM+1 D TRANS1
 W !,"<"_REC_" RCH 10-0173 RECORDS TRANSMITTED TO AUSTIN>" K R,REC,SOWKM,XMDUZ,XMTEXT,XMY,XMDUZ
 Q
TRANS1 S XMSUB="RCH 10-0173 AMIS DATA "_$P(^DD("SITE"),"^")_" MESSAGE NO.:"_SOWKM,XMDUZ=DUZ,XMY(DUZ)="",XMY("XXX@Q-RHC.domain.ext")="",XMTEXT="^TMP($J,STA,""TRANS""," D ^XMD S R=0 K ^TMP($J,STA,"TRANS")
 Q
