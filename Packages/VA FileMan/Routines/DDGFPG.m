DDGFPG ;SFISC/MKO-ADD A NEW PAGE ;2:26 PM  13 Sep 1995
 ;;22.0;VA FileMan;;Mar 30, 1999;Build 1
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
ADD ;Invoke forms to add a new page
 S DDGFDY=DY,DDGFDX=DX K DDGFPNUM
 ;
 ;Ask for new page number
 S DDSFILE=.403,DDSFILE(1)=.4031
 S DA(1)=+DDGFFM,DA="",DR="[DDGF PAGE ADD]",DDSPARM="KTW"
 D ^DDS K DDSFILE,DA,DR,DDSPARM
 ;
 G:$D(DDGFPNUM)[0 ADDQ
 ;
 ;Ask 'are you sure' page should be added
 K DDGFANS
 S DDSFILE=.403,DDSFILE(1)=.4031
 S DR="[DDGF PAGE ADD]",DA(1)=+DDGFFM,DA="",DDSPARM="KTW",DDSPAGE=11
 D ^DDS K DDSFILE,DA,DR,DDSPARM,DDSPAGE
 ;
 I '$G(DDGFANS) K DDGFANS G ADDQ
 K DDGFANS
 ;
 ;Add page to form
 S DIC="^DIST(.403,+DDGFFM,40,",DIC(0)="L",DA(1)=+DDGFFM
 S DIC("P")=$P(^DD(.403,40,0),U,2),X=DDGFPNUM
 K DD,DO D FILE^DICN K DIC,DA,X G:Y=-1 ADDQ
 S DDGFPG=+Y
 ;
 ;Stuff in values for coordinates and name
 S DIE="^DIST(.403,"_+DDGFFM_",40,",DA(1)=+DDGFFM,DA=DDGFPG
 S DR="2////1,1;7////Page "_DDGFPNUM
 D ^DIE K DIE,DA,DR
 ;
 K DDGFPNUM
 D LOADPG
 S DDGFNEW=1
 G EDIT
 ;
ADDQ D REFRESH^DDGF,RC(DDGFDY,DDGFDX)
 K DDGFPNUM,DDGFDY,DDGFDX
 Q
 ;
EDIT ;Invoke form to edit a page
 ;Input:  DDGFNEW (optional)
 ;  Set by ADD to indicate this is a brand new page.
 ;
 S DDGFDY=DY,DDGFDX=DX
 S DDGFND=@DDGFREF@("F",DDGFPG)
 S (DDGFTLC,DDGFTLC0)=$P(DDGFND,U)+1_","_($P(DDGFND,U,2)+1)
 S (DDGFLRC,DDGFLRC0)=$S($P(DDGFND,U,3)]"":$P(DDGFND,U,3)+1_","_($P(DDGFND,U,4)+1),1:"")
 S (DDGFPNM,DDGFPNM0)=$P(DDGFND,U,5)
 S DDGFPAR=$P($G(^DIST(.403,+DDGFFM,40,DDGFPG,1)),U,2)
 ;
 S DDSFILE=.403,DDSFILE(1)=.4031,DDSPARM="KTW"
 S DA(1)=+DDGFFM,DA=DDGFPG,DR="[DDGF PAGE EDIT]"
 D ^DDS K DDSFILE,DA,DR,DDSPARM
 ;
 S DDGFND=$G(^DIST(.403,+DDGFFM,40,DDGFPG,0))
 ;
 ;If page was deleted, destroy windows and set new page
 I DDGFND="" D  Q:DDGFE
 . I $D(DDGFWID)#2,$$EXIST^DDGLIBW(DDGFWID) D DESTROY^DDGLIBW(DDGFWID)
 . I $D(DDGFWIDB)#2,$$EXIST^DDGLIBW(DDGFWIDB) D DESTROY^DDGLIBW(DDGFWIDB)
 . K @DDGFREF@("F",DDGFPG),@DDGFREF@("RC",DDGFWID),@DDGFREF@("BKRC",DDGFWIDB)
 . I $D(@DDGFREF@("ASUB","B",DDGFPG)) D DEL^DDGFASUB(DDGFPG)
 . S DDGFPG=$O(^DIST(.403,+DDGFFM,40,"B",""))
 . S:DDGFPG]"" DDGFPG=$O(^DIST(.403,+DDGFFM,40,"B",DDGFPG,""))
 . D LOADPG,REFRESH^DDGF,RC(DDGFDY,DDGFDX)
 ;
 E  D
 . S:DDGFPNM'=DDGFPNM0 $P(@DDGFREF@("F",DDGFPG),U,5)=DDGFPNM,$P(^(DDGFPG),U,7)=1,DDGFCHG=1
 . D:DDGFPAR'=$P($G(^DIST(.403,+DDGFFM,40,DDGFPG,1)),U,2) EDIT^DDGFASUB(DDGFPG)
 . I DDGFTLC'=DDGFTLC0!(DDGFLRC'=DDGFLRC0) D
 .. D PAGE^DDGFUPDP($P(DDGFTLC,",")-1,$P(DDGFTLC,",",2)-1,$S(DDGFLRC]"":$P(DDGFLRC,",")-1,1:""),$S(DDGFLRC]"":$P(DDGFLRC,",",2)-1,1:""),$S(DDGFTLC=DDGFTLC0:"PBRC",1:"PTOP"))
 .. D STATUS^DDGF,RC($P(DDGFLIM,U),$P(DDGFLIM,U,2))
 . E  D REFRESH^DDGF,RC(DDGFDY,DDGFDX)
 ;
 K DDGFDX,DDGFDY,DDGFND,DDGFNEW
 K DDGFLRC,DDGFLRC0,DDGFPOP,DDGFPOP0,DDGFTLC,DDGFTLC0
 K DDGFPAR,DDGFPNM,DDGFPNM0
 Q
 ;
PGSEL ;Select a new page
 S DDGFDY=DY,DDGFDX=DX,DDGFPAGE=DDGFPG
 ;
 S DDSFILE=.403,DDSFILE(1)=.4031
 S DR="[DDGF PAGE SELECT]",DDSPARM="KTW"
 D ^DDS
 K DDSFILE,DA,DR,DDSPAGE,DDSPARM
 ;
 I DDGFPAGE]"",DDGFPAGE'=DDGFPG S DDGFPG=DDGFPAGE D LOADPG
 ;
 D REFRESH^DDGF,RC(DDGFDY,DDGFDX)
 K DDGFPAGE,DDGFDY,DDGFDX
 Q
 ;
NXTPRV(F) ;Go to page
 ;F=1:next page; -1:previous page
 S DDGFPAGE=$P($G(^DIST(.403,+DDGFFM,40,DDGFPG,0)),U,$S($G(F)=-1:5,1:4))
 G:DDGFPAGE="" NXTPRVQ
 S DDGFPAGE=$O(^DIST(.403,+DDGFFM,40,"B",DDGFPAGE,""))
 G:$D(^DIST(.403,+DDGFFM,40,+DDGFPAGE,0))[0!(DDGFPAGE=DDGFPG) NXTPRVQ
 ;
 S DDGFPG=DDGFPAGE
 D LOADPG,REFRESH^DDGF,RC(DDGFDY,DDGFDX)
NXTPRVQ K DDGFPAGE,DDGFDY,DDGFDX
 Q
 ;
CLSPG ;Close page
 Q:$G(DDGLSCR)'>1
 D CLOSE^DDGLIBW(DDGFWID)
 S DDGFPG=$E(DDGLSCR(DDGLSCR),2,999)
 D PG^DDGFLOAD(+DDGFFM,DDGFPG,1)
 D STATUS^DDGF,RC($P(DDGFLIM,U),$P(DDGFLIM,U,2))
 Q
 ;
SUBPG ;Go into subpage
 I $D(@DDGFREF@("ASUB",DDGFPG,B,F))#2 S DDGFSUBP=^(F)
 E  D
 . S DDGFSUBP=+$P($G(^DIST(.404,B,40,F,7)),U,2)
 . S DDGFSUBP=+$O(^DIST(.403,+DDGFFM,40,"B",DDGFSUBP,""))
 ;
 I $D(^DIST(.403,+DDGFFM,40,DDGFSUBP,0))[0 W $C(7) K DDGFSUBP Q
 I DDGFSUBP=DDGFPG K DDGFSUBP Q
 S DDGFE=1
 Q
 ;
SUBPG1 S DDGFPG=DDGFSUBP K DDGFSUBP
 D PG^DDGFLOAD(+DDGFFM,DDGFPG)
 D STATUS^DDGF,RC($P(DDGFLIM,U),$P(DDGFLIM,U,2))
 Q
 ;
LOADPG ;Load new page
 D PG^DDGFLOAD(+DDGFFM,DDGFPG,1)
 S DDGFDY=$P(DDGFLIM,U),DDGFDX=$P(DDGFLIM,U,2)
 Q
 ;
RC(DDGFY,DDGFX) ;Update status line, reset DX and DY, move cursor
 N S
 I DDGFR D
 . S DY=IOSL-6,DX=IOM-9,S="R"_(DDGFY+1)_",C"_(DDGFX+1)
 . X IOXY W S_$J("",7-$L(S))
 S DY=DDGFY,DX=DDGFX X IOXY
 Q
