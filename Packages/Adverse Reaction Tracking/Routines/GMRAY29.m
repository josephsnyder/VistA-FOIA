GMRAY29 ;SLC/DAN Installation Utilities ;3/1/07  14:34
 ;;4.0;Adverse Reaction Tracking;**29**;Mar 29, 1996;Build 15
 ;
 ;DBIA SECTION
 ;3744  - $$TESTPAT^VADPT
 ;10061 - VADPT
 ;10013 - DIK
 ;2056  - DIQ
 ;10018 - DIE
 ;10070 - XMD
 ;10103 - XLFDT
 ;2051  - DIC
 ;
PRETRAN ;Load conversion table into KIDS build
 M @XPDGREF@("GMRAFIX")=^XTMP("GMRAFIX")
 Q
 ;
POST ;Post installation processes
 K ^XTMP("GMRAFIX")
 M ^XTMP("GMRAFIX")=@XPDGREF@("GMRAFIX")
 I '$D(^XTMP("GMRAFIX")) W !,"Conversion table not loaded - INSTALLATION ABORTED" S XPDQUIT=2 Q
 D Q Q  ;Queue clean up to background
 ;
Q ;
 N ZTRTN,ZTDESC,ZTIO,ZTDTH,ZTSK
 S ZTRTN="DQ^GMRAY29",ZTDESC="GMRA*4*29 POST INSTALL ROUTINE",ZTIO="",ZTDTH=$H
 D ^%ZTLOAD I '$G(ZTSK) D BMES^XPDUTL("POST INSTALL NOT QUEUED - RUN DQ^GMRA29 AFTER INSTALL FINISHES") Q
 D BMES^XPDUTL("Post-install queued as task # "_$G(ZTSK))
 Q
 ;
DQ ;Process begins here
 N ERR,TU,TE,TF
 D FIXALG
 D MAIL
 S ^XTMP("GMRAFIX",0)=$$FMADD^XLFDT(DT,30)_"^"_DT_"^Patch GMRA*4*29 conversion table"
 K ^XTMP("GMRAFX","FREE") ;Kill free text list so it forces rebuild
 Q
 ;
FIXALG ;Loop through 120.8 update existing free text entries
 N GMRAI,FREE,REACTANT,ENTRY
 S FREE=$O(^GMRD(120.82,"B","OTHER ALLERGY/ADVERSE REACTION",0)) S:'+FREE ERR=1 S:FREE FREE=FREE_";GMRD(120.82," Q:$G(ERR)
 S GMRAI=0 F  S GMRAI=$O(^GMR(120.8,GMRAI)) Q:'+GMRAI  D
 .I '$D(^GMR(120.8,GMRAI,0))!($L($G(^GMR(120.8,GMRAI,0)),"^")=1) D DEL Q
 .Q:+$G(^GMR(120.8,GMRAI,"ER"))!($$TESTPAT^VADPT($P(^GMR(120.8,GMRAI,0),U)))!($$DECEASED^GMRAFX($P(^GMR(120.8,GMRAI,0),U)))  ;stop if entered in error, test patient or deceased patient
 .I $P(^GMR(120.8,GMRAI,0),U,3)=FREE D
 ..S REACTANT=$P(^GMR(120.8,GMRAI,0),U,2) Q:REACTANT[" ( FREE TEXT )"  ;Entry has already been updated
 ..S ENTRY=$G(^XTMP("GMRAFIX",$$UP^XLFSTR($E(REACTANT,1,30)))) ;For matching purposes, only use the 1st 30 characters
 ..I ENTRY="" D UPDATEF Q  ;Entry not found or designated free text
 ..I $P(ENTRY,U)="Entered In Error" D UPDATEE Q  ;Mark as entered in error
 ..D UPDATE ;Update free text entry from table
 Q
 ;
DEL ;No zero node, remove entry
 N DIK,DA,GMRADONT
 S GMRADONT=1 ;Stop HDR from receiving update as it's not needed
 S DIK="^GMR(120.8,",DA=GMRAI
 D ^DIK
 Q
 ;
UPDATEF ;Update reactant to say free text so users know it isn't a standardized entry
 N FDA
 S REACTANT=REACTANT_" ( FREE TEXT )"
 S FDA(120.8,(GMRAI_","),.02)=REACTANT
 D FILE^DIE(,"FDA")
 S TF=$G(TF)+1 ;Increment total free text updated counter
 Q
 ;
UPDATEE ;Mark as entered in error, check for NKA
 N DIE,DA,DR,DFN,USER,TIME
 S DFN=$P(^GMR(120.8,GMRAI,0),U) ;Patient's IEN
 S USER=$P(^GMR(120.8,GMRAI,0),U,5),TIME=$P(^(0),U,4)
 S DIE="^GMR(120.8,",DA=GMRAI,DR="22///1;23///NOW;24////"_$G(DUZ,.5)
 D ^DIE ;Entry is now entered in error
 D ADCOM^GMRAFX(GMRAI,"E","Marked entered in error by auto-update in patch GMRA*4*29") ;Adds comment to allergy record
 I $$NKASCR^GMRANKA(DFN) D
 .I $P(ENTRY,U,2)="NKDA" S DA=DFN,DIE="^GMR(120.86,",DR="1////0;2////"_$G(USER,DUZ)_";3////"_$G(TIME,$$NOW^XLFDT) D ^DIE Q  ;Set assessment to NKA
 .D CLN^GMRANKA ;Delete assessment
 S TE=$G(TE)+1 ;Increment total entered in error counter
 Q
 ;
UPDATE ;Update free text entry to data found in table
 N DFN,DIE,DA,DR,AIFN,COM,SIEN,FILE,NAME,IEN,GMRAAR,GMRAPA,GMRASCR,ERRCODE
 S DFN=$P(^GMR(120.8,GMRAI,0),U)
 S GMRAPA=GMRAI
 S FILE=$P(ENTRY,U),NAME=$P(ENTRY,U,2)
 S IEN=$$FIND1^DIC(FILE,"",$S(FILE=120.82:"X",1:"MX"),NAME,$S(FILE=120.82:"B",1:""),,"ERRCODE")
 I '+IEN S IEN=$$FIND1^DIC(FILE,"","MX",NAME_" ","",,"ERRCODE")
 I '+IEN,NAME="ANTIMUSCARINICS/ANTISPASMODICS" S IEN=$$FIND1^DIC(FILE,"","MX","GA801","",,"ERRCODE")
 I '+IEN,$L($T(SCREEN^XTID)) S GMRASCR="I '$$SCREEN^XTID(FILE,,Y_"","")" S IEN=$$FIND1^DIC(FILE,"","MX",NAME,"",$G(GMRASCR),"ERRCODE")
 I '+IEN S ERR(2,DFN,REACTANT)=ENTRY D UPDATEF Q
 S GMRAAR=IEN_";"_$S(FILE=50:"PSDRUG(",FILE=50.416:"PS(50.416,",FILE=50.605:"PS(50.605,",FILE=120.82:"GMRD(120.82,",1:"PSNDF(50.6,")
 S GMRAAR(0)=NAME
 S GMRAAR("O")=$S(FILE=120.82:$P(^GMRD(120.82,IEN,0),U,2),1:"D")
 I $$DUP^GMRAFX3 S ERR(3,DFN,REACTANT)=ENTRY D UPDATEF Q  ;Would create a duplicate if update occur
 ;Update reactant, allergy and signed off fields
 S DIE="^GMR(120.8,",DA=GMRAPA,DR=".02////"_GMRAAR(0)_";1////^S X=GMRAAR"_";3.1////"_GMRAAR("O")_";15///1" D ^DIE
 I $D(^GMR(120.85,"C",GMRAPA)) D  ;Observed reaction, need to update data
 .S AIFN=0
 .F  S AIFN=$O(^GMR(120.85,"C",GMRAPA,AIFN)) Q:'+AIFN  D
 ..S SIEN=$O(^GMR(120.85,AIFN,3,"B",REACTANT,0)) Q:'+SIEN  ;Was previous reactant stored as "suspected agent"
 ..S DA(1)=AIFN,DA=SIEN
 ..S DIE="^GMR(120.85,DA(1),3,",DR=".01////^S X=GMRAAR(0)" D ^DIE ;Update suspected agent to new value
 D DELMUL^GMRAFX3(2),DELMUL^GMRAFX3(3) ;Delete drug ingredient/drug classes multiples
 I GMRAAR("O")["D" D UPDATE^GMRAPES1 K LIST ;If reactant type is Drug then add appropriate ingredients and classes
 S COM="Updated using auto clean up process from GMRA*4*29.  Changed reactant from "_REACTANT_" (free text) "_"to "_GMRAAR(0)_"(file - "_$P(GMRAAR,";",2)_")"
 D ADCOM^GMRAFX(GMRAPA,"O",COM) ;Add a comment for this update
 S TU=$G(TU)+1 ;Increment total updated counter
 Q
 ;
MAIL ;Send message indicating post install is finished
 N XMSUB,XMTEXT,XMDUZ,XMY,XMZ,GMRATXT,CNT,VADM,DFN,REACTANT,LOOP,DIFROM,EXTRA
 S XMDUZ="PATCH GMRA*4*29 POST-INSTALL"
 S XMY("DAVID.NABER@domain.ext")="",XMY("CATHERINE.HOANG2@domain.ext")=""
 S XMY("HULET.LEE_ANN@FORUM.domain.ext")=""
 S EXTRA=($D(ERR(2))!($D(ERR(3))))
 I 'EXTRA S XMY(.5)="" S:$G(DUZ) XMY(DUZ)=""
 S CNT=1
 S GMRATXT(CNT)="The post-install routine for patch GMRA*4*29",CNT=CNT+1
 S GMRATXT(CNT)="finished on "_$$FMTE^XLFDT($$NOW^XLFDT)_".",CNT=CNT+1
 S GMRATXT(CNT)="",CNT=CNT+1
 I $G(ERR)=1 D
 .S GMRATXT(CNT)="**NOTE: There was a problem with the installation!",CNT=CNT+1
 .S GMRATXT(CNT)="Required entry missing from file 120.82 - CONVERSION ABORTED.",CNT=CNT+1
 .S GMRATXT(CNT)="Contact the National Help Desk for Immediate assistance.",CNT=CNT+1
 I $G(TU)!($G(TE))!($G(TF)) D
 .S GMRATXT(CNT)="Here are the results of the update:",CNT=CNT+1
 .S GMRATXT(CNT)="",CNT=CNT+1
 .F LOOP="TU","TF","TE" D
 ..S GMRATXT(CNT)=$S(+$G(@LOOP)=0:"No entries were",$G(@LOOP)=1:"One entry was",1:$G(@LOOP)_" entries were")_" "
 ..S GMRATXT(CNT)=GMRATXT(CNT)_$S(LOOP="TU":"updated to new terms",LOOP="TF":"updated to have (FREE TEXT) appended to the term",1:"marked entered in error")_".",CNT=CNT+1
 .S GMRATXT(CNT)="",CNT=CNT+1
 S XMTEXT="GMRATXT(",XMSUB="PATCH GMRA*4*29 Post Install COMPLETED"
 D ^XMD ;Send totals to OI reps, include local if no problems
 F LOOP=2,3 D
 .I $D(ERR(LOOP)) D
 ..S GMRATXT(CNT)="The following patients have allergies that couldn't be converted",CNT=CNT+1
 ..S GMRATXT(CNT)=$S(LOOP=2:"because the term to update them to couldn't be found in the local files.",1:"because it would create a duplicate entry."),CNT=CNT+1
 ..S GMRATXT(CNT)="",CNT=CNT+1
 ..S DFN=0 F  S DFN=$O(ERR(LOOP,DFN)) Q:'+DFN  D
 ...K VADM D DEM^VADPT
 ...S GMRATXT(CNT)="PATIENT: "_VADM(1)_" ("_$E(VADM(2),6,9)_")",CNT=CNT+1
 ...S REACTANT="" F  S REACTANT=$O(ERR(LOOP,DFN,REACTANT)) Q:REACTANT=""  D
 ....S GMRATXT(CNT)="Can't convert "_REACTANT_" to "_$P(ERR(LOOP,DFN,REACTANT),U,2)_" (file: "_$P(ERR(LOOP,DFN,REACTANT),U)_")",CNT=CNT+1
 ...S GMRATXT(CNT)="",CNT=CNT+1
 ..S $P(GMRATXT(CNT),"*",70)="*",CNT=CNT+1,GMRATXT(CNT)="",CNT=CNT+1
 I EXTRA D
 .K XMY S XMY(.5)="" S:$G(DUZ) XMY(DUZ)="" ;Send full report to local site only
 .S XMTEXT="GMRATXT(",XMSUB="PATCH GMRA*4*29 Post Install COMPLETED"
 .D ^XMD
 Q
