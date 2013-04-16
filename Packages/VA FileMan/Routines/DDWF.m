DDWF ;SFISC/MKO-FIND, REPLACE ;8:14 AM  27 Mar 1996
 ;;22.0;VA FileMan;;Mar 30, 1999;Build 1
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
NEXT ;Find next occurrence of same text
 N DDWT
 G:$G(DDWFIND)="" FIND
 S DDWT=DDWFIND
 D FINDT(DDWT,$G(DDWFIND(1)))
 Q
 ;
FIND ;Prompt and find text
 N DDWCOD,DDWF,DDWT
 D ASK^DDWG(3,"Find What: ",30,$G(DDWFIND),"","",.DDWT,.DDWCOD)
 Q:DDWT=""
 D FINDT(DDWT,$P($G(DDWCOD),U)="U")
 Q
 ;
FINDT(DDWT,DDWBACK) ;Find DDWT
 D:$D(DDWMARK) UNMARK^DDW7
 S DDWFIND=DDWT,DDWT=$$UC(DDWT)
 I $G(DDWBACK) D
 . S DDWFIND(1)=1 D LOOKB
 E  K DDWFIND(1) D LOOK
 Q
 ;
LOOK ;Look in arrays
 N DDWF,DDWI,DDWX
 S DDWF=$F($$UC(DDWL(DDWRW)),DDWT,DDWC)
 I DDWF D REPOS(DDWRW+DDWA,DDWF,DDWT) Q
 ;
 F DDWI=DDWRW+1:1:DDWMR D  Q:DDWF
 . S DDWX=$F($$UC(DDWL(DDWI)),DDWT)
 . I DDWX D REPOS(DDWI+DDWA,DDWX,DDWT) S DDWF=1
 Q:DDWF
 ;
 D MSG^DDW("Searching ...")
 F DDWI=DDWSTB:-1:1 D  Q:DDWF
 . S DDWX=$F($$UC(^TMP("DDW1",$J,DDWI)),DDWT)
 . I DDWX D
 .. D MSG^DDW()
 .. D REPOS(DDWA+DDWMR+DDWSTB-DDWI+1,DDWX,DDWT)
 .. S DDWF=1
 Q:DDWF
 ;
 D MSG^DDW("Text not found.") H 2
 D MSG^DDW(),CUP(DDWRW,DDWC)
 F  R *DDWX:0 E  Q
 Q
 ;
LOOKB ;Look backward in arrays
 N DDWF,DDWI,DDWX
 S DDWF=$$RF($E($$UC(DDWL(DDWRW)),1,DDWC-1),DDWT)
 I DDWF=DDWC S DDWF=$$RF($E($$UC(DDWL(DDWRW)),1,DDWC-$L(DDWT)-1),DDWT)
 I DDWF D REPOS(DDWRW+DDWA,DDWF,DDWT) Q
 ;
 F DDWI=DDWRW-1:-1:1 D  Q:DDWF
 . S DDWX=$$RF($$UC(DDWL(DDWI)),DDWT)
 . I DDWX D REPOS(DDWI+DDWA,DDWX,DDWT) S DDWF=1
 Q:DDWF
 ;
 D MSG^DDW("Searching ...")
 F DDWI=DDWA:-1:1 D  Q:DDWF
 . S DDWX=$$RF($$UC(^TMP("DDW",$J,DDWI)),DDWT)
 . I DDWX D
 .. D MSG^DDW()
 .. D REPOS(DDWI,DDWX,DDWT)
 .. S DDWF=1
 Q:DDWF
 ;
 D MSG^DDW("Text not found.") H 2
 D MSG^DDW(),CUP(DDWRW,DDWC)
 F  R *DDWX:0 E  Q
 Q
 ;
REPOS(DDWY,DDWX,DDWT) ;Define DDWMARK, paint if on screen
 S DDWMARK=DDWY_U_(DDWX-$L(DDWT))_U_DDWY_U_(DDWX-1)
 I DDWY-DDWA>0,DDWY-DDWA'>DDWMR,DDWX-DDWOFS>0,DDWX-DDWOFS'>IOM D
 . D PAINT^DDW7(DDWMARK,1)
 . D POS(DDWY-DDWA,DDWX,"RN")
 E  D LINE^DDWG(DDWY,DDWX)
 D IND^DDW7(1)
 Q
 ;
UC(X) ;Return uppercase of X
 Q $TR(X,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")
 ;
RF(X,T) ;Find last occurrence of T in X
 N Y
 Q:X'[T 0
 S Y=1 F  S Y=$F(X,T,Y) Q:'$F(X,T,Y)
 Q Y
 ;
CUP(Y,X) ;Cursor positioning
 S DY=IOTM+Y-2,DX=X-1 X IOXY
 Q
 ;
POS(R,C,F) ;Pos cursor based on char pos C
 N DDWX
 S:$G(C)="E" C=$L($G(DDWL(R)))+1
 S:$G(F)["N" DDWN=$G(DDWL(R))
 S:$G(F)["R" DDWRW=R,DDWC=C
 ;
 S DDWX=C-DDWOFS
 I DDWX>IOM!(DDWX<1) D SHIFT^DDW3(C,.DDWOFS)
 S DY=IOTM+R-2,DX=C-DDWOFS-1 X IOXY
 Q
