DVBHCE4 ; ;08/29/12
 S X=DE(33),DIC=DIE
 ;
 S X=DE(33),DIC=DIE
 ;
 S X=DE(33),DIC=DIE
 D AUTOUPD^DGENA2(DA)
 S X=DE(33),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".301;" D AVAFC^VAFCDD01(DA)
 S X=DE(33),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
 S X=DE(33),DIIX=2_U_DIFLD D AUDIT^DIET
