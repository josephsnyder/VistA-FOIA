DIAXMS ;SFISC/DCM-MAP SUBFILES ;9/2/94  06:17
 ;;22.0;VA FileMan;;Mar 30, 1999;Build 1
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 S DIAXSB=1,DIAXTAB=DL+DL-2 S:DJ DIAXTAB=DIAXTAB+1 S $P(DIAXTABZ," ",DIAXTAB)=" "
 W !,$C(7),?DIAXTAB,DIAXDICA," is a multiple valued field",!,?DIAXTAB,"It MUST be mapped to a subfile."
 K DIC,DIAXUP N Y
 I $D(DC(DC)),$P(DC(1),U,3)]"" S DIAXDEF=$P(DC(1),U,3)
 S DIC="^DD(DIAXF,",DIC(0)="QEAZ",DIC("S")="I $P(^(0),U,2),'$F(DIAXLOC(DIAXFILE)_U,U_+Y_U),$P(^DD(+$P(^(0),U,2),.01,0),U,2)'[""P"",$P(^(0),U,2)'[""W"",$P(^(0),U,2)'[""V"""
 S DIC("A")=DIAXTABZ_"MAP "_DIAXDICA_" TO "_DIAXEF_" SUBFILE: " S:$D(DIAXDEF) DIC("B")=DIAXDEF
 D ^DIC I Y'>0 S DIAXUP=1 W:X=""&'$D(DTOUT) !,$C(7),DIAXDICA_" will not be extracted" S:$D(DTOUT) DIRUT=1 G QQ
 S DIAXLOC(DIAXFILE)=DIAXLOC(DIAXFILE)_U_+Y,DIAXEF=Y(0,0)
 S (DIAXFILE,DIAXF)=+$P(Y(0),U,2),DIAXLOC(DIAXFILE)="",DIAXNP(DL-1)=$P(Y(0),U,4)
QQ K DIAXDEF,DIAXDICA
 Q
IX Q:$P($G(^DD($$FNO^DILIBF(DIAXF),0,"DI")),U)'["Y"
 S (DIAXIX,DIAXFI,DIAXFD)=""
 F  S DIAXIX=$O(^DD(DIAXF,0,"IX",DIAXIX)) Q:DIAXIX=""  F  S DIAXFI=$O(^DD(DIAXF,0,"IX",DIAXIX,DIAXFI)) Q:DIAXFI'>0  F  S DIAXFD=$O(^DD(DIAXF,0,"IX",DIAXIX,DIAXFI,DIAXFD)) Q:DIAXFD'>0  D
 . I '$D(^DD(DIAXFI,DIAXFD,1)) S DIAXEM="Erroneous 'IX' node for "_DIAXIX D ERR^DIAXERR(DIAXEM) Q
 . S DIAXIXN=0 F  S DIAXIXN=$O(^DD(DIAXFI,DIAXFD,1,DIAXIXN)) Q:DIAXIXN'>0  S DIAXIX0=$P(^(DIAXIXN,0),U,2) Q:DIAXIX=DIAXIX0
 . Q:DIAXIXN'>0  S DIAXIX0=$P(^DD(DIAXFI,DIAXFD,1,DIAXIXN,0),U,3) D
 . . Q:DIAXIX0=""
 . . I DIAXIX0["MNE"!(DIAXIX0["REG")!(DIAXIX0["KWI")!(DIAXIX0["SOU") Q
 . . S DIAXEM="The """_DIAXIX_""" cross-reference in "_$P(^DD(DIAXFI,DIAXFD,0),U,1)_" is not allowed for an archive file." D ERR^DIAXERR(DIAXEM) Q:DIPG
 Q
 ;
Q K DIAXZ,DIAXFT,DIAXHI,DIAXLO,DIAXNO,DIAXLE,DIAXTABZ,DIC,DIAXDICA,DIAXS,DIAXDJ,DIAXC
 K DIAXDEF,DIAXA,DIAXX,DIAXFR,DIAXTO,DIAXS1,DIAXDT,DIAXZL,DIAXZLL,DIAXZY,DIAXZZ
 K DIAXIX,DIAXIX0,DIAXIXN,DIAXVFI,DIAXVFLD,DIAXVFR,DIAXDTY
 K DIAX41,DIAX42,DIAXFTY,DIAXEXT,DIAXE1,DIAXE2,DIAXPC I '$G(DIPG),'$G(DIAR)!($G(DIAR)=6) K DIAXMSG
 Q
Q1 K DIAXDK,DIAXDL,DIAXEF,DIAXF,DIAXFD,DIAXIX,DIAXIX0,DIAXIXN,DIAXTAB
 K DIAX1,DIAX2,DIAXFI,DIAXEM,DIAXLNK
 Q
F1 S (A1,B1,D1)=0 S:'$D(DIAR) DIAR=""
 F  S A1=$O(DIAXE01(A1)) Q:A1'>0  S B1=$G(DIAXE01(A1)),C="DIAXFR" S:+$P(B1,U,2) DIAXSB=1 D EN(B1,C) S C="DIAXTO",DIAXFR=0 D EN(A1,C) K DIAXSB
 K DIAXE01,A1,B1,D1 Q
EN(W,Z) S @Z=1
 S DIC="^DD("_+W_",",X=.01,DIC(0)="Z",DIAXEF=$O(^DD(+W,0,"NM","")) D ^DIC I Y'>0 Q
 D EN1^DIAXM
 Q
TYP(%) N W,W1,W2,X,Y
 S W="NPSVWCDFK",W1=%
 F X=1:1:$L(W) S W2=$F(W1,$E(W,X)) Q:W2
 S Y=$E(W1,W2-1)
 S:Y="" Y="F"
 Q Y
