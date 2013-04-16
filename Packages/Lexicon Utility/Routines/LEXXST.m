LEXXST ; ISL/KER - Lexicon Status (Main/Files) ; 02/22/2007
 ;;2.0;LEXICON UTILITY;**4,5,8,25,27,49**;Sep 23, 1996;Build 3
 ;            
 ; External References
 ;   DBIA 10096  ^%ZOSF("PROD"
 ;   DBIA 10096  ^%ZOSF("UCI"
 ;   DBIA 10060  ^VA(200 
 ;   DBIA 10000  NOW^%DTC
 ;   DBIA 10086  ^%ZIS   
 ;   DBIA 10086  HOME^%ZIS
 ;   DBIA 10089  ^%ZISC  
 ;   DBIA 10063  ^%ZTLOAD
 ;   DBIA  2052  FILE^DID
 ;   DBIA 10103  $$FMTE^XLFDT
 ;   DBIA 10104  $$UP^XLFSTR
 ;   DBIA 10070  ^XMD
 ;                         
DISP ; Display Status only
 K ^TMP($J,"LEXINFO"),LEXMAIL,LEXAO N X,Y,LEXM,LEXY
 D DATA,SHOW Q
SEND ; Send Status to G.LEXINS@ISC-SLC.domain.ext
 K ^TMP($J,"LEXINFO") N X,Y,LEXM,LEXY
 S:$L($G(LEXBUILD)) ZTSAVE("LEXBUILD")=""
 S:$D(LEXSHORT) ZTSAVE("LEXSHORT")=""
 S ZTRTN="SENDTO^LEXXST",ZTDESC="Lexicon Status Report Msg [LEXXST]",ZTIO="",ZTDTH=$H
 D ^%ZTLOAD
 D HOME^%ZIS K Y,ZTSK,ZTDESC,ZTDTH,ZTIO,ZTSAVE,ZTRTN
 Q
SENDTO ; Send Status (Tasked)
 N LEXMAIL,LEXAO S (LEXMAIL,LEXAO)="" S:$D(ZTQUEUED) ZTREQ="@"
 D:'$D(LEXSHORT) N0 D DATA,SHOW Q
DATA ; Get Data
 D TITLE Q:$D(LEXSHORT)
 D FILES,PT^LEXXST2,RTT^LEXXST2
 D KIDS^LEXXST3,RTN^LEXXST3
 Q
TITLE ; Title of display/message
 N LEXT,LEXA,LEXD,LEXU,LEXN,LEXP
 S LEXT="LEXICON UTILITY STATUS",LEXD=$$A,LEXA=$$U
 S LEXU=$$P,LEXN=$P(LEXU,"^",1),LEXP=$P(LEXU,"^",2)
 I $D(LEXAO) D  Q
 . D:$L(LEXT) TT(LEXT),BL S:$L(LEXD) LEXT="  AS OF:       "_LEXD D:$L(LEXD) TL(LEXT) S LEXT="" S:$L(LEXA) LEXT="  IN ACCOUNT:  "_$S($L($P(LEXA,"^",1)):"[",1:"")_$P(LEXA,"^",1)_$S($L($P(LEXA,"^",2)):"]",1:"")
 . S:$L(LEXT)&($L($P(LEXA,"^",2))) LEXT=LEXT_"  "_$P(LEXA,"^",2) D:$L(LEXA) TL(LEXT)
 . S LEXT="" S:$L(LEXU) LEXT="  MAINT BY:    " S:$L(LEXN) LEXT=LEXT_LEXN S:$L(LEXP)&($L(LEXN)) LEXT=LEXT_"   "_LEXP D:$L(LEXT) TL(LEXT)
 . S LEXT="" S:$L($G(LEXBUILD)) LEXT="  BUILD:       "_$G(LEXBUILD)
 . D:$L(LEXT) TL(LEXT) D BL
 I '$D(LEXAO) D  Q
 . D:$L(LEXT) TT(LEXT),BL S:$L(LEXD) LEXT="  AS OF:       "_LEXD D:$L(LEXD) TL(LEXT) S LEXT="" S:$L(LEXA) LEXT="  IN ACCOUNT:  "_$S($L($P(LEXA,"^",1)):"[",1:"")_$P(LEXA,"^",1)_$S($L($P(LEXA,"^",2)):"]",1:"")
 . S:$L(LEXT)&($L($P(LEXA,"^",2))) LEXT=LEXT_"  "_$P(LEXA,"^",2) D:$L(LEXA) TL(LEXT) D BL
 Q
U(X) ;   UCI where Lexicon is installed
 N LEXU,LEXP,LEXT,Y X ^%ZOSF("UCI") S LEXU=Y,LEXP=""
 S:LEXU=^%ZOSF("PROD")!($P(LEXU,",",1)=^%ZOSF("PROD")) LEXP=" (Production)"
 S:LEXU'=^%ZOSF("PROD")&($P(LEXU,",",1)'=^%ZOSF("PROD")) LEXP=" (Test)"
 S X="",$P(X,"^",1)=LEXU,$P(X,"^",2)=LEXP
 Q X
FILES ; File version/contents
 N LEXCT,LEXT,LEXSP,LEXFI,LEXX,LEXGL,LEXNM,LEXVR,LEXLR,LEXTR,LEXRLR
 N LEXDDA,LEXPRD,LEXRN,LEXRD,LEXFCT
 S LEXFI=756.999999,LEXCT=0,LEXSP="                           ",LEXFCT=$$FC
 S LEXT="     FILE     NAME                   VER  REV     DATE      LAST IEN    RECORDS"
 D BL,TT("FILE VERSIONS/REVISIONS"),BL,TL(LEXT),BK1
 D:+LEXFCT'>0 BL,TL("       NO FILES FOUND") Q:+LEXFCT'>0
 S LEXFI=756.999999 F  S LEXFI=$O(^LEX(LEXFI)) Q:+LEXFI=0!($E(LEXFI,1,3)'="757")  D FL
 S LEXFI=756.999999 F  S LEXFI=$O(^LEXT(LEXFI)) Q:+LEXFI=0!($E(LEXFI,1,3)'="757")  D FL
 S LEXFI=756.999999 F  S LEXFI=$O(^LEXC(LEXFI)) Q:+LEXFI=0!($E(LEXFI,1,3)'="757")  D FL
 F LEXFI=80,80.1,81,81.3 D FL
 Q
FL ; File List
 N LEXN K LEXDDA S LEXN=$$ATTR(LEXFI,"NAME") Q:'$L(LEXN)
 S LEXGL=$$ATTR(LEXFI,"GLOBAL NAME") Q:'$L(LEXGL)
 S LEXX=LEXGL_"0)",LEXX=$G(@LEXX),LEXTR=+($P(LEXX,"^",4)),LEXLR=+($P(LEXX,"^",3))
 S LEXRLR=$O(@(LEXGL_""" "")"),-1) S:'$L($G(LEXX)) (LEXTR,LEXLR)="??" S:LEXRLR'=LEXLR (LEXTR,LEXLR)="??"
 S LEXNM=$E(LEXN,1,21),LEXX=$$ATTR(LEXFI,"VERSION")
 S LEXVR=$P(LEXX,".",1),LEXX=$P(LEXX,".",2),LEXVR=$J(LEXVR,3)_$S($L(LEXVR):".",1:"")_LEXX
 S LEXPRD=$$ATTR(LEXFI,"PACKAGE REVISION DATA")
 S LEXRN=$P(LEXPRD,"^",1) S:LEXRN="" LEXRN="1"
 S LEXRD=$P(LEXPRD,"^",2) S:LEXRD'="" LEXRD=$$MDCY(LEXRD)
 S:LEXRD="" LEXRD="10/04/96"
 S LEXCT=LEXCT+1,LEXT=$J(LEXCT,3)_"  "_LEXFI_$E(LEXSP,1,(9-$L(LEXFI)))
 S LEXT=LEXT_LEXNM_$E(LEXSP,1,(21-$L(LEXNM))),LEXT=LEXT_LEXVR_$E(LEXSP,1,(8-$L(LEXVR)))
 S LEXT=LEXT_LEXRN_$E(LEXSP,1,(4-$L(LEXRN))),LEXT=LEXT_LEXRD_$E(LEXSP,1,(14-$L(LEXRD)))
 S LEXT=LEXT_$J(LEXLR,7)_"    "_$J(LEXTR,7) D TL(LEXT)
 Q
SHOW ; Show global array (display or mail)
 D:$D(LEXMAIL) MAIL,CLR D:'$D(LEXMAIL) DSP,CLR Q
SHOW2 ; Display global array
 N LEXI S LEXI=0 F  S LEXI=$O(^TMP($J,"LEXINFO",LEXI)) Q:+LEXI=0  W !,^TMP($J,"LEXINFO",LEXI)
 Q
MAIL ; Mail global array in message
 N DIFROM,LEXADR S U="^",XMSUB="LEXICON INFO"
 S:$L($G(LEXBUILD)) XMSUB=LEXBUILD_" Installation"
 S LEXADR=$$ADR^LEXU Q:'$L(LEXADR)
 S XMY(("G.LEXINS@"_LEXADR))="",XMTEXT="^TMP($J,""LEXINFO"",",XMDUZ=.5 D ^XMD
 K ^TMP($J,"LEXINFO"),%Z,XCNP,XMSCR,XMDUZ,XMY,XMZ,XMSUB,XMY,XMTEXT,XMDUZ Q
 Q
CLR ; Clean up
 K ^TMP($J,"LEXINFO") Q
BL ; Blank Line
 N LEXNX S LEXNX=+($$NX),^TMP($J,"LEXINFO",LEXNX)="" Q
TT(LEXX) ; Title Line
 Q:'$L($G(LEXX))  D TL(LEXX) N LEXBK S LEXBK="===============================================================================",LEXBK=$E(LEXBK,1,$L($G(LEXX))) D:$L(LEXBK) TL(LEXBK) Q
TL(LEXX) ; Text Line
 N LEXNX S LEXNX=+($$NX),^TMP($J,"LEXINFO",LEXNX)=$G(LEXX) Q
BK1 ; Break Line
 N LEXNX S LEXNX=+($$NX),^TMP($J,"LEXINFO",LEXNX)="-------------------------------------------------------------------------------" Q
NX(LEXX) ; Next Line #
 S (LEXX,^TMP($J,"LEXINFO",0))=+($G(^TMP($J,"LEXINFO",0)))+1 Q LEXX
DSP ; Display ^TMP($J,"LEXINFO")
 D DEV Q
DEV ; Select a device
 N %,%ZIS,IOP,ZTRTN,ZTSAVE,ZTDESC,ZTDTH,ZTIO,ZTSK
 S ZTRTN="DSPI^LEXXST",ZTDESC="printing Lexicon installation information"
 S ZTIO=ION,ZTDTH=$H,%ZIS="PQ",ZTSAVE("^TMP($J,""LEXINFO"",")=""
 D ^%ZIS Q:POP  S ZTIO=ION I $D(IO("Q")) D QUE,^%ZISC Q
 D NOQUE Q
NOQUE ; Do not que task
 W @IOF W:IOST["P-" !,"< Not queued, printing Lexicon Installations >",! H 2 U:IOST["P-" IO D @ZTRTN,^%ZISC Q
QUE ; Task queued to print user defaults
 K IO("Q") D ^%ZTLOAD W !,$S($D(ZTSK):"Request Queued",1:"Request Cancelled"),! H 2 Q
 Q
DSPI ; Display installations
 I '$D(ZTQUEUED),$G(IOST)'["P-" W:'$D(LEXDNC) # I '$D(^TMP($J,"LEXINFO")) W !,"Installations not found"
 I IOST["P-" U IO
 G:'$D(^TMP($J,"LEXINFO")) DSPQ
 N LEXCONT,LEXI,LEXLC,LEXEOP S LEXCONT="",(LEXLC,LEXI)=0,LEXEOP=+($G(IOSL)) S:LEXEOP=0 LEXEOP=24
 F  S LEXI=$O(^TMP($J,"LEXINFO",LEXI)) Q:+LEXI=0!(LEXCONT["^")  D
 . W !,^TMP($J,"LEXINFO",LEXI) D LF Q:LEXCONT["^"
 S:$D(ZTQUEUED) ZTREQ="@"
 W:$G(IOST)["P-" @IOF
DSPQ ; Quit Display
 Q
LF ; Line Feed
 S LEXLC=LEXLC+1 D:IOST["P-"&(LEXLC>(LEXEOP-7)) CONT D:IOST'["P-"&(LEXLC>(LEXEOP-4)) CONT
 Q
CONT ; Page/Form Feed
 S LEXLC=0 W:IOST["P-" @IOF Q:IOST["P-"  W !!,"Press <Return> to continue  " R LEXCONT:300 S:'$T LEXCONT="^" S:LEXCONT'["^" LEXCONT=""
 Q
A(LEX) ; As of date/time
 N %,X,%I,%H D NOW^%DTC S LEX=$$UP^XLFSTR($$FMTE^XLFDT(%,"1")) S:LEX["@" LEX=$P(LEX,"@",1)_"  "_$P(LEX,"@",2,299) Q LEX
P(LEX) ; Person
 S LEX=+($G(DUZ)) Q:'$L($P($G(^VA(200,+($G(LEX)),0)),"^",1)) "UNKNOWN^"
 N LEXDUZ,LEXPH S LEXDUZ=+($G(DUZ))
 S LEXPH=$P($G(^VA(200,LEXDUZ,.13)),"^",2) S:LEXPH="" LEXPH=$P($G(^VA(200,LEXDUZ,.13)),"^",1) S:LEXPH="" LEXPH=$P($G(^VA(200,LEXDUZ,.13)),"^",3) S:LEXPH="" LEXPH=$P($G(^VA(200,LEXDUZ,.13)),"^",4)
 S LEXDUZ=$P(^VA(200,LEXDUZ,0),"^",1),LEX=LEXDUZ_"^"_LEXPH Q LEX
N0 ; 0 Node
 N LEXFI,LEXCT,DIC S LEXCT=0
 S LEXFI=756.999999 F  S LEXFI=$O(^LEX(LEXFI)) Q:+LEXFI=0!($E(LEXFI,1,3)'="757")  S DIC=$$ATTR(LEXFI,"GLOBAL NAME") D N0C
 S LEXFI=756.999999 F  S LEXFI=$O(^LEXT(LEXFI)) Q:+LEXFI=0!($E(LEXFI,1,3)'="757")  S DIC=$$ATTR(LEXFI,"GLOBAL NAME") D N0C
 S LEXFI=756.999999 F  S LEXFI=$O(^LEXC(LEXFI)) Q:+LEXFI=0!($E(LEXFI,1,3)'="757")  S DIC=$$ATTR(LEXFI,"GLOBAL NAME") D N0C
 F LEXFI=80,80.1,81,81.3 S DIC=$$ATTR(LEXFI,"GLOBAL NAME") D N0C
 Q
N0C ; 0 Node Count
 N DA,LEXLR,LEXTR,LEXDDA,LEXNM
 K LEXDDA D FILE^DID(LEXFI,"N","NAME","LEXDDA","LEXDDA")
 S LEXNM=$G(LEXDDA("NAME")) Q:'$L(LEXNM)  S (DA,LEXLR,LEXTR)=0
 F  S DA=$O(@(DIC_DA_")")) Q:+DA=0  S LEXLR=DA,LEXTR=LEXTR+1
 S $P(@(DIC_"0)"),"^",3)=LEXLR,$P(@(DIC_"0)"),"^",4)=LEXTR
 W:'$D(ZTQUEUED) !,LEXFI,?10,$J(LEXLR,10),$J(LEXTR,10)
 Q
FC(X) ; File Count
 N LEXFI,LEXCT S LEXCT=0
 S LEXFI=756.999999 F  S LEXFI=$O(^LEX(LEXFI)) Q:+LEXFI=0!($E(LEXFI,1,3)'="757")  S LEXCT=LEXCT+1
 S LEXFI=756.999999 F  S LEXFI=$O(^LEXT(LEXFI)) Q:+LEXFI=0!($E(LEXFI,1,3)'="757")  S LEXCT=LEXCT+1
 S LEXFI=756.999999 F  S LEXFI=$O(^LEXC(LEXFI)) Q:+LEXFI=0!($E(LEXFI,1,3)'="757")  S LEXCT=LEXCT+1
 S X=LEXCT Q X
MDCY(X) ; Month/Day/Century-Year where X=FM Date
 N LEXCY S LEXCY=+($G(X)) Q:LEXCY=0 "" S LEXCY=$P($$FMTE^XLFDT(LEXCY,2),"/",1,2)_"/"_$P($P($$FMTE^XLFDT(LEXCY,1)," ",3),"@",1)
 S:$L($P(LEXCY,"/",1))<2 $P(LEXCY,"/",1)="0"_$P(LEXCY,"/",1) S:$L($P(LEXCY,"/",2))<2 $P(LEXCY,"/",2)="0"_$P(LEXCY,"/",2)
 S:$L($P(LEXCY,"/",1))<2 $P(LEXCY,"/",1)="0"_$P(LEXCY,"/",1) S:$L($P(LEXCY,"/",2))<2 $P(LEXCY,"/",2)="0"_$P(LEXCY,"/",2)
 S X=LEXCY Q X
ATTR(X,A) ; File Attributes
 N LEXFI,LEXATT,LEXDDA
 S LEXFI=+($G(X)) Q:+LEXFI'>0 ""  S LEXATT=$G(A) Q:'$L(LEXATT) ""
 D FILE^DID(LEXFI,"N",LEXATT,"LEXDDA","LEXDDA") S X=$G(LEXDDA(LEXATT))
 Q X
