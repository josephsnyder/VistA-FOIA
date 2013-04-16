LRSRVR1 ;DALOI/JMC -LAB DATA SERVER, CONT'D - LOINC SECTION ; March 25, 2002
 ;;5.2;LAB SERVICE;**303**;Sep 27, 1994
 ;
 ;
LOINC ; Scan for LOINC Coding
 ;
 N LR60,LR61,LRLLINA,LRLLINB,LRLLINC,LRX
 K XMY
 ;S XMY("G.LOINCSERVER@ISC-DALLAS.domain.ext")=""
 S XMY(XQSND)=""
 S ^TMP($J,"LRDATA",1)="*"_$$NOW^XLFDT
 S ^TMP($J,"LRDATA",2)="No codes defined at "_LRSTN
 K ^TMP($J,"LRSERVER","LOINC")
 S LINE=2,LINR=1
 F LRSUB="AI","AH" D
 . S LRA=""
 . F  S LRA=$O(^LAM(LRSUB,LRA)) Q:'LRA  D
 . . S LRB=""
 . . F  S LRB=$O(^LAM(LRSUB,LRA,LRB)) Q:LRB=""  S ^TMP($J,"LRSERVER","LOINC",LRB)=""
 ;
 S LRA=""
 F  S LRA=$O(^TMP($J,"LRSERVER","LOINC",LRA)) Q:LRA=""  D
 . K LOINCDTA,LOINCDTB,LRERR
 . D GETS^DIQ(64,LRA,".01;1;25;25.5","IE","LOINCDTB","LRERR")
 . D GETS^DIQ(64,LRA,"20*","IE","LOINCDTA","LRERR")
 . S LRPNTB=$O(LOINCDTB(64,"")) Q:LRPNTB=""
 . I LINE>2 F  Q:'$D(^TMP($J,"LRDATA",LINE))  S LINE=LINE+1
 . S LRLLINA="~"_LRST_"^"_$G(LOINCDTB(64,LRPNTB,.01,"E"))
 . ;PROCEDURE (64,.01)
 . S LRLLINA=LRLLINA_"^"_$G(LOINCDTB(64,LRPNTB,1,"E"))
 . ;WKLD CODE (64,1)
 . S LRLLINA=LRLLINA_"^"_$G(LOINCDTB(64,LRPNTB,25,"E"))
 . ;DEFAULT LOINC CODE (64,25)
 . S LRLLINA=LRLLINA_"^"_$G(LOINCDTB(64,LRPNTB,25.5,"E"))
 . ;LOOK FOR 64.01 & 64.02 HERE
 . I '$O(LOINCDTA(64.01,"")) S ^TMP($J,"LRDATA",LINE)=LRLLINA S LINE=LINE+1
 . S LRAA1=""
 . F  S LRAA1=$O(LOINCDTA(64.01,LRAA1)) Q:LRAA1=""  D
 . . I '$D(LOINCDTA(64.01,LRAA1,.01,"I")) D  Q
 . . . S ^TMP($J,"LRDTERR",LINR)="Specimen sub-field error in file 64!!  "_LRAA1,LINR=LINR+1
 . . . S ^TMP($J,"LRDTERR",LINR)=$G(LRERR("DIERR",1,"TEXT",1)),LINR=LINR+1
 . . S LRPNTA=LOINCDTA(64.01,LRAA1,.01,"I")
 . . D GETS^DIQ(61,LRPNTA,.0961,,"LOINCTAS","LRERR")
 . . S LRLLINB="^"_$G(LOINCTAS(61,LRPNTA_",",.0961))
 . . ;TIME ASPECT (61,.0961)
 . . S LRLLINB=LRLLINB_"^"_LOINCDTA(64.01,LRAA1,.01,"E")
 . . ;SPECIMEN (64.01,.01)
 . . I '$O(LOINCDTA(64.02,"")) S ^TMP($J,"LRDATA",LINE)=LRLLINA_LRLLINB,LINE=LINE+1
 . . S LRAA=""
 . . F  S LRAA=$O(LOINCDTA(64.02,LRAA)) Q:LRAA=""  D
 . . . S LRLLINC="^"_$G(LOINCDTA(64.02,LRAA,2,"E"))
 . . . ;DATA LOCATION (64.02,2)
 . . . D TSTNAM
 . . . ;TEST (64.02,3)
 . . . S LRLLINC=LRLLINC_"^"_$G(LOINCDTA(64.02,LRAA,4,"E"))
 . . . S ^TMP($J,"LRDATA",LINE)=LRLLINA_LRLLINB_LRLLINC
 . . . D TSTTYP,TSTUNS
 . . . S LINE=LINE+1
 D EXIT^LRSRVR
 Q
 ;
 ;
LOINCL ; Build and send local LOINC report
 ;
 N LINE,LINR,LRA,LRXREF
 K ^TMP($J,"LRSERVER","LOINC")
 K XMY
 S XMY(XQSND)=""
 S ^TMP($J,"LRDATA",1)="Report Generated "_$$FMTE^XLFDT($$NOW^XLFDT)_" at "_LRSTN
 S ^TMP($J,"LRDATA",2)="No codes defined at "_LRSTN
 S LINE=2,LINR=1
 F LRXREF="AI","AH" D
 . S LRA=""
 . F  S LRA=$O(^LAM(LRXREF,LRA)) Q:'LRA  D
 . . S LRB=""
 . . F  S LRB=$O(^LAM(LRXREF,LRA,LRB)) Q:LRB=""  S ^TMP($J,"LRSERVER","LOINC",LRB)=""
 ;
 S LRA=""
 F  S LRA=$O(^TMP($J,"LRSERVER","LOINC",LRA)) Q:LRA=""  D LOINCLA
 D EXIT^LRSRVR
 Q
 ;
 ;
LOINCLA ;
 N LR60,LR61,LRERR,LOINCDTA,LOINCDTB,LRPNTB,LRX
 S:'$D(LINE) LINE=1 S:'$D(LINR) LINR=1
 D GETS^DIQ(64,LRA,".01;1;25;25.5","IE","LOINCDTB","LRERR")
 D GETS^DIQ(64,LRA,"20*","IE","LOINCDTA","LRERR")
 S LRPNTB=$O(LOINCDTB(64,"")) Q:LRPNTB=""
 S ^TMP($J,"LRDATA",LINE)="",LINE=LINE+1
 S ^TMP($J,"LRDATA",LINE)="NLT Procedure: "_$G(LOINCDTB(64,LRPNTB,.01,"E")),LINE=LINE+1
 ;
 ; Procedure (64,.01)
 S ^TMP($J,"LRDATA",LINE)="NLT Code: "_$G(LOINCDTB(64,LRPNTB,1,"E")),LINE=LINE+1
 ;
 ; WKLD CODE (64,1)
 S ^TMP($J,"LRDATA",LINE)="Default LOINC Code: "_$G(LOINCDTB(64,LRPNTB,25,"E"))_" : "_$G(^LAB(95.3,+$G(LOINCDTB(64,LRPNTB,25,"E")),80)),LINE=LINE+1
 ;
 ; Default LOINC code (64,25)
 S ^TMP($J,"LRDATA",LINE)="Default LOINC Code Test: "_$G(LOINCDTB(64,LRPNTB,25.5,"E")),LINE=LINE+1
 ;
 ; Look for 64.01 & 64.02 here
 S LRAA1=""
 F  S LRAA1=$O(LOINCDTA(64.01,LRAA1)) Q:LRAA1=""  D
 . I '$D(LOINCDTA(64.01,LRAA1,.01,"I")) D  Q
 . . S ^TMP($J,"LRDATA",LINE)="Specimen sub-field error in file 64!!  "_LRAA1,LINE=LINE+1
 . . S ^TMP($J,"LRDATA",LINE)=$G(LRERR("DIERR",1,"TEXT",1)),LINE=LINE+1
 . S LRPNTA=LOINCDTA(64.01,LRAA1,.01,"I")
 . D GETS^DIQ(61,LRPNTA,.0961,,"LOINCTAS","LRERR")
 . S ^TMP($J,"LRDATA",LINE)="Time Aspect: "_LOINCTAS(61,LRPNTA_",",.0961),LINE=LINE+1
 . ; TIME ASPECT (61,.0961)
 . S ^TMP($J,"LRDATA",LINE)="Specimen: "_LOINCDTA(64.01,LRAA1,.01,"E"),LINE=LINE+1
 . ; SPECIMEN (64.01,.01)
 . S LRAA=""
 . F  S LRAA=$O(LOINCDTA(64.02,LRAA)) Q:LRAA=""  I LRAA[LRAA1 D
 . . S ^TMP($J,"LRDATA",LINE)="Data Location: "_$G(LOINCDTA(64.02,LRAA,2,"E")),LINE=LINE+1
 . . ; DATA LOCATION (64.02,2)
 . . D TSTTYP,TSTNAM,TSTUNS
 . . S ^TMP($J,"LRDATA",LINE)="LOINC Code: "_$G(LOINCDTA(64.02,LRAA,4,"E"))_" : "_$G(^LAB(95.3,+$G(LOINCDTA(64.02,LRAA,4,"E")),80)),LINE=LINE+1
 . . ; LOINC CODE (64.02,4)
 Q
 ;
 ;
TSTTYP ; Determine test data type
 N LRX,LRTYPE,LRY
 I LOINCDTA(64.02,LRAA,2,"I")="" Q
 S LRX=$P(LOINCDTA(64.02,LRAA,2,"I"),"(",2)
 S LRTYPE=$$GET1^DID($P(LRX,","),$P(LRX,",",2,99),"","TYPE")
 I LRSUB="LOINC" S $P(^TMP($J,"LRDATA",LINE),"^",11)=LRTYPE
 I LRSUB="LOCAL REPORT" S ^TMP($J,"LRDATA",LINE)="Data Type: "_LRTYPE,LINE=LINE+1
 S LRY=$$GET1^DID($P(LRX,","),$P(LRX,",",2,99),"",$S(LRTYPE="SET":"POINTER",1:"INPUT TRANSFORM"))
 I LRSUB="LOINC" S LRY=$TR(LRY,"^","~"),$P(^TMP($J,"LRDATA",LINE),"^",12)=LRY
 I LRSUB="LOCAL REPORT" S ^TMP($J,"LRDATA",LINE)="Data Values: "_LRY,LINE=LINE+1
 S LRY=$$GET1^DID($P(LRX,","),$P(LRX,",",2,99),"","HELP-PROMPT")
 I LRSUB="LOINC" S $P(^TMP($J,"LRDATA",LINE),"^",13)=LRY
 I LRSUB="LOCAL REPORT" S ^TMP($J,"LRDATA",LINE)="Data Help: "_LRY,LINE=LINE+1
 Q
 ;
 ;
TSTNAM ; Test name and units
 N LRX,LRY
 S LRX=LOINCDTA(64.02,LRAA,3,"E")
 S LRY=""
 I LOINCDTA(64.02,LRAA,3,"I") S LRY=LOINCDTA(64.02,LRAA,3,"I")_"-"_LOINCDTA(64.01,$P(LRAA,",",2,4),.01,"I")
 I LRSUB="LOCAL REPORT" D
 . S ^TMP($J,"LRDATA",LINE)="Test: "_LRX,LINE=LINE+1
 . I LRY'="" S ^TMP($J,"LRDATA",LINE)="Test-Spec: "_LRY,LINE=LINE+1
 I LRSUB="LOINC" D
 . S LRLLINC=LRLLINC_"^"_LRX
 . S $P(^TMP($J,"LRDATA",LINE),"^",15)=LRY
 Q
 ;
 ;
TSTUNS ; Test units
 N LR60,LR61,LRY
 S LR60=+LOINCDTA(64.02,LRAA,3,"I"),LR61=+LOINCDTA(64.01,$P(LRAA,",",2,4),.01,"I")
 S LRY=$$GET1^DIQ(60.01,LR61_","_LR60_",",6)
 I LRSUB="LOINC" S $P(^TMP($J,"LRDATA",LINE),"^",14)=LRY
 I LRSUB="LOCAL REPORT" S ^TMP($J,"LRDATA",LINE)="Units: "_LRY,LINE=LINE+1
 Q
