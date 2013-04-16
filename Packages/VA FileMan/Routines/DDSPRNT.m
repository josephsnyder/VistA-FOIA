DDSPRNT ;SFISC/MKO-PRINT A FORM ;02:51 PM  18 Nov 1994
 ;;22.0;VA FileMan;;Mar 30, 1999;Build 1
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
 I '$D(DIFM) N DIFM S DIFM=1 D INIZE^DIEFU
 ;
 N DDSFORM,DDSPBRK
 D SELFORM(.DDSFORM) Q:DDSFORM=-1
 D PAGEBRK(.DDSPBRK) Q:$D(DDSPBRK)[0
 ;
 ;Device
 S %ZIS=$S($D(^%ZTSK):"Q",1:"")
 W ! D ^%ZIS K %ZIS I $G(POP) K POP Q
 K POP
 ;
 ;Queue report
 I $D(IO("Q")),$D(^%ZTSK) D  G END
 . S ZTRTN="PRINT^DDSPRNT"
 . S ZTDESC="Report of Form "_$P(DDSFORM,U,2)
 . N I F I="DDSFORM","DDSFORM(0)","DDSPBRK" S ZTSAVE(I)=""
 . D ^%ZTLOAD
 . I $D(ZTSK)#2 W !,"Report queued!",!,"Task number: "_$G(ZTSK),!
 . E  W !,"Report canceled!",!
 . K ZTSK
 . S IOP="HOME" D ^%ZIS
 ;
 U IO
 ;
PRINT ;Entry point for queued reports
 N DDSBK,DDSCOL1,DDSCOL2,DDSCOL3,DDSCRT,DDSFILE
 N DDSHLIN,DDSHBK,DDSPAGE,DDSQUE
 N DX,DY,X,Y
 ;
 I '$D(DIFM) N DIFM S DIFM=1 D INIZE^DIEFU
 D INIT
 D @("HDR"_(2-DDSCRT))
 D FORM,END
 Q
 ;
FORM ;Form data
 W !
 ;
 ;Description
 D WP($NA(^DIST(.403,+DDSFORM,15))) Q:$D(DIRUT)
 ;
 ;Other properties
 D W("PRIMARY FILE: "_$P(DDSFORM(0),U,8),9) Q:$D(DIRUT)
 W ?49,"READ ACCESS: "_$P(DDSFORM(0),U,2)
 D W("DATE CREATED: "_$$EXTERNAL^DILFD(.403,4,"",$P(DDSFORM(0),U,5)),9) Q:$D(DIRUT)
 W ?48,"WRITE ACCESS: "_$P(DDSFORM(0),U,3)
 D W("DATE LAST USED: "_$$EXTERNAL^DILFD(.403,5,"",$P(DDSFORM(0),U,6)),7) Q:$D(DIRUT)
 W ?53,"CREATOR: "_$P(DDSFORM(0),U,4)
 D W() Q:$D(DIRUT)
 ;
 I $P(DDSFORM(0),U,7)]"" D W("TITLE: "_$P(DDSFORM(0),U,7),16) Q:$D(DIRUT)
 I $P($G(^DIST(.403,+DDSFORM,21)),U)]"" D W("RECORD SELECTION PAGE: "_$P(^(21),U)) Q:$D(DIRUT)
 ;
 I $X D W() Q:$D(DIRUT)
 S X=$G(^DIST(.403,+DDSFORM,11))
 I X]"" D W("PRE ACTION:",11) Q:$D(DIRUT)  D PCOL(X,23)
 S X=$G(^DIST(.403,+DDSFORM,12))
 I X]"" D W("POST ACTION:",10) Q:$D(DIRUT)  D PCOL(X,23)
 S X=$G(^DIST(.403,+DDSFORM,14))
 I X]"" D W("POST SAVE:",12) Q:$D(DIRUT)  D PCOL(X,23)
 S X=$G(^DIST(.403,+DDSFORM,20))
 I X]"" D W("DATA VALIDATION:",6) Q:$D(DIRUT)  D PCOL(X,23)
 K DDSFORM(0)
 ;
 ;Loop through all pages
 I $X D W() Q:$D(DIRUT)
 Q:'$O(^DIST(.403,+DDSFORM,40,0))
 ;
 N DDSPG,DDSPGN
 S DDSPGN="",DDSPFRST=1
 F  S DDSPGN=$O(^DIST(.403,+DDSFORM,40,"B",DDSPGN)) Q:DDSPGN=""!$D(DIRUT)  S DDSPG=0 F  S DDSPG=$O(^DIST(.403,+DDSFORM,40,"B",DDSPGN,DDSPG)) Q:'DDSPG!$D(DIRUT)  D PAGE^DDSPRNT1
 K DDSPFRST Q:$D(DIRUT)
 ;
 D:$D(DDSHBK) HBLKS^DDSPRNT1
 Q
 ;
WR(DDSLAB,DDSVAL,DDSFLG) ;Write label and value
 I DDSVAL="",'$G(DDSFLG) Q
 ;
 D W() Q:$D(DIRUT)
 W ?DDSCOL2,DDSLAB
 ;
 I $X>DDSCOL3 N DDSCOL3 S DDSCOL3=$X+1
 D PCOL(DDSVAL,DDSCOL3)
 Q
 ;
PCOL(DDSVAL,DDSCOL) ;Print DDSVAL
 N DDSWIDTH,DDSIND
 S DDSWIDTH=IOM-DDSCOL-1
 F DDSIND=1:DDSWIDTH:$L(DDSVAL) D  Q:$D(DIRUT)
 . I DDSIND>1 D W() Q:$D(DIRUT)
 . W ?DDSCOL,$E(DDSVAL,DDSIND,DDSIND+DDSWIDTH-1)
 Q
 ;
WP(DDSWP,DIWL,DDSLF) ;Print text in array @DDSWP
 ;DDSLF [ A : LF after (def)
 ;        B : LF feed before
 ;
 Q:'$P($G(@DDSWP@(0)),U,3)
 N DIW,DIWF,DIWI,DIWR,DIWT,DIWTC,DIWX,DN
 N DDSI,DDSCNT,I,X,Z
 ;
 K ^UTILITY($J,"W")
 S:'$G(DIWL) DIWL=1
 S DIWR=IOM-1
 S:'$D(DDSLF) DDSLF="A"
 ;
 S DDSCNT=$P($G(@DDSWP@(0)),U,3)
 I DDSCNT D
 . F DDSI=1:1:DDSCNT I $D(@DDSWP@(DDSI,0))#2 S X=^(0) D ^DIWP
 . ;
 . I DDSLF'["B" D
 .. W ?DIWL-1,$G(^UTILITY($J,"W",DIWL,1,0))
 .. S DDSCNT=1
 . E  S DDSCNT=0
 . F  S DDSCNT=$O(^UTILITY($J,"W",DIWL,DDSCNT)) Q:'DDSCNT!$D(DIRUT)  D
 .. D W($G(^UTILITY($J,"W",DIWL,DDSCNT,0)),DIWL-1)
 ;
 K ^UTILITY($J,"W")
 D:DDSLF["A" W()
 Q
 ;
W(DDSSTR,DDSCOL) ;Write DDSSTR
 I $Y+3'<IOSL D HEADER Q:$D(DIRUT)
 W !?+$G(DDSCOL),$G(DDSSTR)
 Q
 ;
HEADER ;All headers except first
 I DDSCRT D  Q:$D(DIRUT)
 . N DIR,X,Y
 . S DIR(0)="E" W ! D ^DIR
 I DDSQUE,$$S^%ZTLOAD S (ZTSTOP,DIRUT)=1 Q
 ;
HDR1 ;First header for CRTs
 W @IOF
 ;
HDR2 ;First header for non-CRTs
 ;
 S DDSPAGE=$G(DDSPAGE)+1
 W "FORM LISTING - "_$P(DDSFORM,U,2)_" (#"_+DDSFORM_")"
 W !,"FILE: "_DDSFILE
 W ?(IOM-$L(DDSHLIN)-$L(DDSPAGE)-1),DDSHLIN_DDSPAGE
 W !,$TR($J("",IOM-1)," ","-")
 Q
 ;
SELFORM(DDSFORM) ;Select form
 N %,%W,%Y,C,I,Q,DDH,DIC,X,Y
 S DIC="^DIST(.403,",DIC(0)="QEAMZ"
 D ^DIC K DIC
 S DDSFORM=Y,DDSFORM(0)=$G(Y(0))
 Q
 ;
PAGEBRK(DDSPBRK) ;Prompt
 N DIR,DIRUT,DUOUT,DTOUT,DIROUT,X,Y
 S DIR(0)="YO"
 S DIR("A")="Start each page of the form on a new page"
 S DIR("B")="Yes"
 W ! D ^DIR Q:$D(DIRUT)
 S DDSPBRK=Y
 Q
 ;
INIT ;Setup
 N %,%H,X,Y
 S %H=$H D YX^%DTC
 S DDSHLIN=$P(Y,"@")_"  "_$P($P(Y,"@",2),":",1,2)_"    PAGE "
 S DDSFILE=$P(DDSFORM(0),U,8)
 I DDSFILE,$D(^DIC(DDSFILE,0))#2 S DDSFILE=$P(^(0),U)_" (#"_DDSFILE_")"
 E  S DDSFILE=""
 S DDSCRT=$E(IOST,1,2)="C-"
 S DDSQUE=$D(ZTQUEUED)
 Q
 ;
END ;Finish up
 I $D(ZTQUEUED) S ZTREQ="@"
 E  X $G(^%ZIS("C"))
 K DIRUT,DUOUT,DTOUT
 Q
