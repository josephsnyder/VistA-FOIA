ORY231 ;SLCOIFO - Post-init for patch OR*3*231 ; [4/19/05 7:21am]
 ;;3.0;ORDER ENTRY/RESULTS REPORTING;**231**;Dec 17, 1997
 ;
PRE ; initiate pre-init processes
 ;
 Q
 ;
POST ; initiate post-init processes
 ;
 I +$$PATCH^XPDUTL("TIU*1.0*112") D SURGREG
 I +$$PATCH^XPDUTL("OR*3.0*222") D GNRPCS
 I +$$PATCH^XPDUTL("MAG*3.0*7") D MAGRPC1
 I +$$PATCH^XPDUTL("MAG*3.0*37") D MAGRPC2
 D MAIL
 Q
 ;
MAIL ; send bulletin of installation time
 N COUNT,DIFROM,I,START,TEXT,XMDUZ,XMSUB,XMTEXT,XMY
 S COUNT=0,XMDUZ="CPRS PACKAGE",XMTEXT="TEXT("
 S XMSUB="Version "_$P($T(VERSION),";;",2)_" Installed"
 F I="G.CPRS GUI INSTALL@ISC-SLC.domain.ext",DUZ S XMY(I)=""
 ;
 S X=$P($T(VERSION),";;",2)
 D LINE("Version "_X_" has been installed.")
 D LINE(" ")
 D LINE("Install complete:  "_$$FMTE^XLFDT($$NOW^XLFDT()))
 ;
 D ^XMD
 Q
 ;
LINE(DATA) ; set text into array
 S COUNT=COUNT+1
 S TEXT(COUNT)=DATA
 Q
 ;
SURGREG ; Register TIU SURGERY RPCs if TIU*1.0*112 present
 N MENU,RPC
 S MENU="OR CPRS GUI CHART"
 F RPC="TIU IS THIS A SURGERY?","TIU IDENTIFY SURGERY CLASS","TIU LONG LIST SURGERY TITLES","TIU GET DOCUMENTS FOR REQUEST" D INSERT(MENU,RPC)
 Q
 ;
INSERT(OPTION,RPC) ; Call FM Updater with each RPC
 ; Input  -- OPTION   Option file (#19) Name field (#.01)
 ;           RPC      RPC sub-file (#19.05) RPC field (#.01)
 ; Output -- None
 N FDA,FDAIEN,ERR,DIERR
 S FDA(19,"?1,",.01)=OPTION
 S FDA(19.05,"?+2,?1,",.01)=RPC
 D UPDATE^DIE("E","FDA","FDAIEN","ERR")
 Q
 ;
GNRPCS ;
 N MENU,I
 S MENU="OR CPRS GUI CHART"
 F I="ORWGN GNLOC","ORWGN AUTHUSR" D INSERT(MENU,I)
 Q
 ;
MAGRPC1 ;  Register Imaging RPC if MAG*3.0*7 present (DBIA 4526)
 D INSERT("OR CPRS GUI CHART","MAG4 REMOTE IMPORT")
 Q
 ;
MAGRPC2 ; Register Imaging RPCS if MAG*3.0*37 installed (DBIA 4528/4530)
 D INSERT("OR CPRS GUI CHART","MAG IMPORT CHECK STATUS")
 D INSERT("OR CPRS GUI CHART","MAG IMPORT CLEAR STATUS")
 Q
 ;
VERSION ;;25.40
