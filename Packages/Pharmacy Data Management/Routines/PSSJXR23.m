PSSJXR23 ; COMPILED XREF FOR FILE #55.05 ; 09/12/12
 ; 
 S DA=0
A1 ;
 I $D(DISET) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^PS(55,DA(1),"NVA",DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^PS(55,DA(1),"NVA",DA,0))
 S X=$P($G(DIKZ(0)),U,1)
 I X'="" S ^PS(55,DA(1),"NVA","B",$E(X,1,30),DA)=""
 S X=$P($G(DIKZ(0)),U,6)
 I X'="" S:$G(PSODEATH) ^PS(55,DA(1),"NVA","APSOD",DA)=""
 S X=$P($G(DIKZ(0)),U,10)
 I X'="" S ^PS(55,"ADCDT",$E(X,1,30),DA(1),DA)=""
CR1 S DIXR=464
 K X
 S X(1)=$P(DIKZ(0),U,1)
 S X(2)=$P(DIKZ(0),U,10)
 S X(3)=$P(DIKZ(0),U,9)
 S X(4)=$P(DIKZ(0),U,7)
 S X=$G(X(1))
 I $G(X(1))]"",$G(X(2))]"" D
 . K X1,X2 M X1=X,X2=X
 . D SNVA^PSOPXRMU(.X,.DA)
CR2 K X
 G:'$D(DIKLM) A Q:$D(DISET)
END G ^PSSJXR24
