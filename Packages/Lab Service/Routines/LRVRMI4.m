LRVRMI4 ;DALOI/STAFF - LAH/TMP TO FILE 63 ;09/06/11  11:11
 ;;5.2;LAB SERVICE;**350**;Sep 27, 1994;Build 230
 ;
 ; Extracts the information in the ^TMP("LRMI",$J) global and stores it into the Lab Data micro subfile.
 ;
 Q
 ;
EN ;
 N LRNODE,LRNOW,LRSTATUS,LR63539,X,I
 Q:'$D(^TMP("LRMI",$J,LRDFN,"MI",LRIDT))
 S LRNOW=$$NOW^XLFDT
 ; Get IEN of last Micro Audit on file
 S LR63539=0
 S X=$O(^LR(LRDFN,"MI",LRIDT,32,"B","A"),-1)
 I X S LR63539=$O(^LR(LRDFN,"MI",LRIDT,32,"B",X,0))
 ;
 ; If any of these nodes are defined then trigger the audit
 F I=1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,99 I $D(^LR(LRDFN,"MI",LRIDT,I)) D  Q
 . I $G(LRSB)'>0 N LRSB S LRSB=$S(I<5:1,I<8:5,I<11:8,I<14:11,I<19:16,I=99:.99,1:0)
 . I LRSB>0 D AUDTRG
 ;
 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,2,0)) D N2
 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,3,0)) D N3^LRVRMI4A
 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,4,0)) D N4
 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,6,0)) D N6^LRVRMI4A
 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,7,0)) D N7^LRVRMI2
 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,9,0)) D N9^LRVRMI2A
 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,10,0)) D N10^LRVRMI2
 I $D(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,11)) D N11^LRVRMI2A
 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,12,0)) D N12^LRVRMI2A
 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,13,0)) D N13^LRVRMI2
 ;I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,15,0)) D N15^LRVRMI2
 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,17,0)) D N17^LRVRMI2A
 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,18,0)) D N18^LRVRMI2
 ;
 F LRNODE=15,19:1:31 I $O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,LRNODE,0)) D NODE^LRVRMI3(LRNODE)
 ;
 I $D(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,99)) D N99
 ;
 D SETSTAT^LRVRMI4A(.LRSTATUS)
 I (LRSTATUS(0)="C")!(LRSTATUS(0)="F") D FIN  ; ccr_5439n - Added IF statement to only Do FIN if overall status is final or corrected. LMT 9/6/11
 ;
 ; Update MICRO AUDIT to reflect corrected status
 ;  If audit doesn't exist then create instead of updating.
 I LRSTATUS(0)="C" D
 . I LR63539<1 D AUDTRG Q
 . N LRFDA,LRIEN,LRMSG,DIERR
 . S LRIEN=LR63539_","_LRIDT_","_LRDFN_","
 . S LRFDA(1,63.539,LRIEN,3)=3 ; Edit Type
 . D FILE^DIE("","LRFDA(1)","LRMSG")
 ;
 Q
 ;
 ;
FIN ; Release report
 N LRFDA,LRIEN,LRMSG,DIERR
 S LRIEN=LRIDT_","_LRDFN_","
 S LRFDA(1,63.05,LRIEN,.04)=$S($G(LRDUZ):LRDUZ,1:$G(DUZ))
 S LRFDA(1,63.05,LRIEN,.03)=LRNOW
 ;S LRFDA(1,63.05,LRIEN,.2)=LRNOW ; ccr_5439n - Commented this line out as there is no field .2 in subfile #63.05. LMT 9/6/11
 D FILE^DIE("","LRFDA(1)","LRMSG")
 Q
 ;
 ;
N2 ; Process gram stain comments
 N DIERR,IEN,LRCMT,LRCSR,LRDATA,LRFDA,LRFDAIEN,LRIEN,LRIENS,LRMSG,LRPL,LRX,STAT
 ;
 M LRCMT=^LR(LRDFN,"MI",LRIDT,2)
 ;
 S IEN=0,STAT=""
 F  S IEN=$O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,2,IEN)) Q:IEN<1  D
 . S LRX=^TMP("LRMI",$J,LRDFN,"MI",LRIDT,2,IEN,0),LRX=$S(LRX'="":LRX,1:" ")
 . I LRX'=" ",$$DUPCHK^LRVRMI3(LRLL,LRPROF,.LRCMT,LRX) Q
 . S LRIEN="+"_IEN_","_LRIDT_","_LRDFN_","
 . S LRFDA(2,63.29,LRIEN,.01)=LRX
 . S LRX=^TMP("LRMI",$J,LRDFN,"MI",LRIDT,2,IEN,0,0)
 . I $P(LRX,"^") S LRPL(IEN)=$P(LRX,"^")
 . I $P(LRX,"^",3) S LRCSR(IEN,2,"LN")=$P(LRX,"^",3)
 . I $P(LRX,"^",4) S LRCSR(IEN,2,"NLT")=$P(LRX,"^",4)
 . I $P(LRX,"^",5)'="" D BLDSTAT^LRVRMI4A(63.05,11.5,$P(LRX,"^",5),.LRSTATUS)
 I '$D(LRFDA) Q
 ;
 D UPDATE^DIE("","LRFDA(2)","LRFDAIEN","LRMSG")
 ; Store performing lab
 S IEN=0
 F  S IEN=$O(LRPL(IEN)) Q:'IEN  D
 . I $G(LRPL(IEN)),$G(LRFDAIEN(IEN)) D SETPL^LRRPLUA(LRDFN_",MI,"_LRIDT_",2,"_LRFDAIEN(IEN),LRPL(IEN))
 ;
 ; Store code system references
 I $D(LRCSR) D CSR(.LRCSR,.LRFDAIEN,LRDFN_",MI,"_LRIDT_",2,")
 ;
 K LRFDA,LRIENS,LRMSG,DIERR
 S LRIEN=LRIDT_","_LRDFN_","
 S LRFDA(2,63.05,LRIEN,11)=LRNOW
 S LRFDA(2,63.05,LRIEN,11.55)=$S($G(LRDUZ):LRDUZ,1:$G(DUZ))
 D FILE^DIE("","LRFDA(2)","LRMSG")
 Q
 ;
 ;
N4 ; Bact report remarks
 N DIERR,IEN,LRCMT,LRCSR,LRFDA,LRFDAIEN,LRIEN,LRIENS,LRMSG,LRPL,LRX
 S LRX=$G(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,4,0))
 D BLDSTAT^LRVRMI4A(63.05,11.5,$P(LRX,"^",4),.LRSTATUS)
 S LRPL=$P(LRX,"^")
 M LRCMT=^LR(LRDFN,"MI",LRIDT,4)
 ;
 S IEN=0
 F  S IEN=$O(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,4,IEN)) Q:IEN<1  D  ;
 . S LRX=^TMP("LRMI",$J,LRDFN,"MI",LRIDT,4,IEN,0),LRX=$S(LRX'="":LRX,1:" ")
 . I LRX'=" ",$$DUPCHK^LRVRMI3(LRLL,LRPROF,.LRCMT,LRX) Q
 . S LRIEN="+"_IEN_","_LRIDT_","_LRDFN_","
 . S LRFDA(4,63.33,LRIEN,.01)=LRX
 . I $P(LRX,"^",3) S LRCSR(IEN,2,"LN")=$P(LRX,"^",3)
 . I $P(LRX,"^",4) S LRCSR(IEN,2,"NLT")=$P(LRX,"^",4)
 I '$D(LRFDA) Q
 ;
 D UPDATE^DIE("","LRFDA(4)","LRFDAIEN","LRMSG")
 ;
 ; Store performing lab
 S IEN=0
 F  S IEN=$O(LRFDAIEN(IEN)) Q:'IEN  I LRPL D SETPL^LRRPLUA(LRDFN_",MI,"_LRIDT_",4,"_LRFDAIEN(IEN),LRPL)
 ;
 ; Store code system references
 I $D(LRCSR) D CSR(.LRCSR,.LRFDAIEN,LRDFN_",MI,"_LRIDT_",4,")
 ;
 K LRFDA,LRIENS,LRMSG,DIERR
 S LRIEN=LRIDT_","_LRDFN_","
 S LRFDA(4,63.05,LRIEN,11)=LRNOW
 S LRFDA(4,63.05,LRIEN,11.5)=$P($G(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,4,0)),U,4)
 S LRFDA(4,63.05,LRIEN,11.55)=$S($G(LRDUZ):LRDUZ,1:$G(DUZ))
 D FILE^DIE("","LRFDA(4)","LRMSG")
 Q
 ;
 ;
N99 ; Comment on specimen
 N LRDATA,LRFDA,LRMSG,LRX
 S LRDATA=$G(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,99))
 Q:$TR(LRDATA," ","")=""  ; don't file empty comments
 ; Don't file same comment
 I LRDATA=$G(^LR(LRDFN,"MI",LRIDT,99)) Q
 S LRIEN=LRIDT_","_LRDFN_","
 S LRFDA(99,63.05,LRIEN,.99)=LRDATA
 D FILE^DIE("","LRFDA(99)","LRMSG")
 S LRX=$G(^TMP("LRMI",$J,LRDFN,"MI",LRIDT,99,0))
 I $P(LRX,"^") D SETPL^LRRPLUA(LRDFN_",MI,"_LRIDT_",99",$P(LRX,"^"))
 Q
 ;
 ;
AUDTRG ; Trigger the audit trail
 N LRDATA,LRMODE,LRBATCH
 S LRMODE="LDSI",LRBATCH=1
 S LRDATA(63.539,1)=LRNOW
 S LRDATA(63.539,3)=$S($G(LRSTATUS(0))'="C":1,1:3)
 I LRINTYPE=1 S LRDATA(63.539,4)="Update from lab automated instrument via HL7"
 I LRINTYPE=10 S LRDATA(63.539,4)="Update from performing lab via HL7"
 D LEDI^LRMIAUD(.LRDATA)
 Q
 ;
 ;
CSR(LRCSR,LRFDAIEN,LRREF) ; Store code system references
 ; Call with LRCSR = array of ien/codes to store as references (pass by value)
 ;        LRFDAIEN = FileMan array of entries added by DBA call (pass by value)
 ;           LRREF = root of reference to build full reference to data
 ;
 N IEN,LRDATA,LRDATAREF,LRDFN,LRROOT,ROLE,TYPE
 ;
 S LRROOT="LRCSR",LRDFN=$P(LRREF,",")
 F  S LRROOT=$Q(@LRROOT) Q:LRROOT=""  D
 . S IEN=$QS(LRROOT,1),ROLE=$QS(LRROOT,2),TYPE=$QS(LRROOT,3)
 . I '$G(LRFDAIEN(IEN)) Q
 . S LRDATAREF=LRREF_LRFDAIEN(IEN)
 . S LRDATA(.01)=LRDATAREF,LRDATA(.02)=ROLE,LRDATA(.03)=LRCSR(IEN,ROLE,TYPE),LRDATA(.04)=TYPE
 . D SETREF^LRUCSR(LRDFN,LRDATAREF,.LRDATA,1)
 Q
