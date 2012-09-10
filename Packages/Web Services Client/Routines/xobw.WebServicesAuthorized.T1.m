 ; xobw.WebServicesAuthorized.T1
 ; Filing Methods for table: xobw.WebServicesAuthorized (extent = xobw.WebServicesAuthorized) - Do Not Edit.  Compiled March 3, 2011 09:29:05
 ; Filing Methods for table: xobw.WebServicesAuthorized (extent = xobw.WebServicesAuthorized)
#classmethod xobw.WebServicesAuthorizedTableCode
#classcontext xobw.WebServicesAuthorized
#include %callout
#include %occExtent
#include %occInclude
#include %systemInclude
#import xobw
%delete(%rowid,%check,%tstart=1,%mv=0) n bva,%d,%e,%ele,%itm,%key,%l,%oper,%pos,%s,sn,sqlcode,subs s %oper="DELETE",sqlcode=0,%ROWID=%rowid,%d(1)=%rowid,%e(1)=%rowid,%d(0)=$p(%d(1),"||",1),%d(2)=$p(%d(1),"||",2),subs(7)=$p(%d(0),"||",1),%l=$c(0)
 k:'$TLEVEL %0CacheLock i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServicesAuthorized"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) s %ls=$s($t:2,1:0) } else { l +^XOB(18.12,subs(7),100,%d(2)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServicesAuthorized' on row with RowID = '"_$g(%d(1))_"'" q  } If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 i '$a(%check),'$zu(115,7) d  i sqlcode s SQLCODE=sqlcode g %EExit
 . n %fk,%k,%p,%st,%t,%z s %k="",%p("%1")="%d(1),",%p("ienIndex")="%d(0),%d(0),%d(2),,,"
 . f  q:sqlcode<0  s %k=$o(^oddEXTR("xobw.WebServicesAuthorized","n",%k)) q:%k=""  s %t="" f  s %t=$o(^oddEXTR("xobw.WebServicesAuthorized","n",%k,"f",%t)) q:%t=""  s %st=(%t="xobw.WebServicesAuthorized") s %fk="" f  s %fk=$o(^oddEXTR("xobw.WebServicesAuthorized","n",%k,"f",%t,%fk)) q:%fk=""  s %z=$g(^oddEXTR("xobw.WebServicesAuthorized","n",%k,"f",%t,%fk,61)) i %z'="",@("$$"_%z_"("_%p(%k)_",%k,%st)") s sqlcode=-124 q
 k ^XOB(18.12,subs(7),100,%d(2))
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q
%insert(%d,%check,%inssel,%vco,%tstart=1,%mv=0) n bva,%ele,%itm,%key,%l,%n,%oper,%pos,%s,sqlcode,sn,subs s %oper="INSERT",sqlcode=0,%l=$c(0,0,0) s:$d(%d(0)) subs(7)=$p(%d(0),"||",1) i '$a(%check),'$$FieldValidate() { s SQLCODE=sqlcode q "" } d Normalize
 k:'$TLEVEL %0CacheLock If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 i '$a(%check)  i $g(%d(0))="" { d missing("webServerRef") s SQLCODE=sqlcode g %EExit } i $g(%d(2))="" { d missing("ien") s SQLCODE=sqlcode g %EExit }
 s %d(1)=%d(0)_"||"_%d(2),subs(7)=$p(%d(0),"||",1)
 i '$a(%check) d  i sqlcode<0 s SQLCODE=sqlcode g %EExit
 . i $g(%vco)'="" d @%vco q:sqlcode<0
 . If '$a(%check,2) { l +^XOB("ienIndex",%d(0),%d(0),%d(2)):$zu(115,4) If $t { s $e(%l,2)=$c(1) } Else { d ulerror("ienIndex") q  } } If '$$fdU12(%d(0),%d(0),%d(2)) s sqlcode=-119,%msg="Table 'xobw.WebServicesAuthorized', Constraint 'ienIndex' (Fields 'webServerRef','webServerRef','ien') failed unique check" q
 . i '$$%1^xobw.WebServer.T1(%d(0)) s sqlcode=-104,%msg="Child table references non-existent row in parent table" q
 i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServicesAuthorized"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) s %ls=$s($t:2,1:0) } else { l +^XOB(18.12,subs(7),100,%d(2)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServicesAuthorized' on row with RowID = '"_$g(%d(1))_"'" g %EExit }
 s ^XOB(18.12,subs(7),100,%d(2))=""
 s:($g(%d(3))'="")||($g(%d(4))'="") ^XOB(18.12,subs(7),100,%d(2),0)=$g(%d(4))_"^^^^^"_$g(%d(3))
 d gunlock2
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q %d(1) 			// %insert-end
%update(%rowid,%check,%d,%vco,%tstart=1,%mv=0) n %e,bva,%ele,%itm,%key,%l,%n,%oper,%pos,%s,icol,s,sn,sqlcode,subs,t s %oper="UPDATE",sqlcode=0,%ROWID=%rowid,$e(%e,1)=$c(0),%l=$c(0,0,0) i '$a(%check),'$$FieldValidate() s SQLCODE=sqlcode q
 d Normalize i ($d(%d(1))&&($g(%d(1))'=%rowid))||($d(%d(0))&&($g(%d(0))'=$p(%rowid,"||",1)))||($d(%d(2))&&($g(%d(2))'=$p(%rowid,"||",2))) s SQLCODE=-107,%msg="Updating any of the RowID Fields ('ID', 'webServerRef', or 'ien') not allowed" q
 f icol=2:1:4 s $e(%e,icol)=$c($d(%d(icol)))
 s %d(1)=%rowid,%e(1)=%rowid,%d(0)=$p(%d(1),"||",1),%d(2)=$p(%d(1),"||",2),subs(7)=$p(%d(0),"||",1)
 k:'$TLEVEL %0CacheLock i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServicesAuthorized"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) s %ls=$s($t:2,1:0) } else { l +^XOB(18.12,subs(7),100,%d(2)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServicesAuthorized' on row with RowID = '"_$g(%d(1))_"'" q  } If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 i $g(%vco)'="" { d getoldall i sqlcode { s SQLCODE=-109 g %EExit } f icol=3,4 { s:'$d(%d(icol)) %d(icol)=%e(icol) s:%d(icol)=%e(icol) $e(%e,icol)=$c(0) }}
 d:'$a(%check)  i sqlcode s SQLCODE=sqlcode g %EExit
 . i $g(%vco)'="" d @%vco q:sqlcode<0
 s:$a(%e,3)||$a(%e,4) s=$g(^XOB(18.12,subs(7),100,%d(2),0)),^XOB(18.12,subs(7),100,%d(2),0)=$s($a(%e,4):%d(4),1:$p(s,"^"))_"^"_$p(s,"^",2)_"^"_$p(s,"^",3)_"^"_$p(s,"^",4)_"^"_$p(s,"^",5)_"^"_$s($a(%e,3):%d(3),1:$p(s,"^",6))_"^"_$p(s,"^",7,3641144)
 d gunlock2
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q
%1(%p1,lockonly=0,unlockref) // FKey validation entry point
 n id s id=%p1
 if '$$%getlock(id,1,.unlockref) { s sqlcode=-114,%msg="SQLCODE=-114:  Cannot acquire lock on referenced row for referenced key XOBW.WEBSERVICESAUTHORIZED:%1" q 0 }
 if 'lockonly { n qv s qv='$$fdU11(%p1) d:'$g(unlockref) %ReleaseLock(id,1) q qv } Else { d:'$g(unlockref) %ReleaseLock(id,1) q 1 }
ienIndex(%p1,%p2,%p3,lockonly=0,unlockref) // FKey validation entry point
 n id s id=$$%QuickFindRowIDByPKey($lb(%p1,%p2,%p3),0,1) q:SQLCODE $s('lockonly:0,1:1)
 i '$$%getlock(id,1,.unlockref) { s sqlcode=-114,%msg="SQLCODE=-114:  Cannot acquire lock on referenced row for referenced key XOBW.WEBSERVICESAUTHORIZED:IENINDEX" q 0 }
 if 'lockonly { n qv s qv='$$fdU12(%p1,%p1,%p3) d:'$g(unlockref) %ReleaseLock(id,1) q qv } else { d:'$g(unlockref) %ReleaseLock(id,1) q 1 }
%PurgeIndices(indices="") q $$BuildPurgeIndices(indices,0)
%BuildIndices(indices="") q $$BuildPurgeIndices(indices,1)
%CheckUniqueIndices(indices,ok) n d,g,n,o s d=0
 s ok=1 q
%AcquireLock(%rowid,s=0,unlockref) n %d,gotlock,subs s %d(1)=%rowid,%d(0)=$p(%d(1),"||",1),%d(2)=$p(%d(1),"||",2),subs(7)=$p(%d(0),"||",1) s s=$e("S",s) l +^XOB(18.12,subs(7),100,%d(2))#s:$zu(115,4) set gotlock=$t s:gotlock&&$g(unlockref) unlockref($i(unlockref))=$lb($name(^XOB(18.12,subs(7),100,%d(2))),"xobw.WebServicesAuthorized") q gotlock
%AcquireTableLock(s=0,SQLCODE=0) s s=$e("S",s) l +^XOB(18.12)#s:$zu(115,4) q:$t 1 s SQLCODE=-110,%msg="Unable to acquire "_$s(s="S":"shared ",1:"")_"table lock for table 'xobw.WebServicesAuthorized'" q 0
%ReleaseLock(%rowid,s=0,i=0) n %d s %d(1)=%rowid,%d(0)=$p(%d(1),"||",1),%d(2)=$p(%d(1),"||",2),subs(7)=$p(%d(0),"||",1) s s=$e("S",s)_$e("I",i) l -^XOB(18.12,subs(7),100,%d(2))#s s:i&&($g(%0CacheLock("xobw.WebServicesAuthorized"))) %0CacheLock("xobw.WebServicesAuthorized")=%0CacheLock("xobw.WebServicesAuthorized")-1 q
%ReleaseTableLock(s=0,i=0) s s=$e("S",s)_$e("I",i) l -^XOB(18.12)#s q 1
%getlock(%rowid,%s=0,unlockref) [] PUBLIC { k:'$TLEVEL %0CacheLock i $i(%0CacheLock("xobw.WebServicesAuthorized"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) q $s($t:2,1:0) } q $$%AcquireLock(%rowid,%s,.unlockref) }
gunlock l:$a(%l) -^XOB(18.12,subs(7),100,%d(2))
 q
gunlock2 l:$a(%l,2) -^XOB("ienIndex",%d(0),%d(0),%d(2))#"I" q
%nBuild s %n=$lb("ID","ID","ien","status","webServiceIen")
 q
FieldValidate() n %f ;Validate all fields
 i $g(%d(3))'="",'($$fdV3(%d(3))) { d invalid(3+1,%d(3)) }  q 'sqlcode
fdV3(%val="")	Quit $isvalidnum(%val,0,0,2)&&(+%val'=2)
invalid(ficol,val) [sqlcode] PUBLIC { s:$l($g(val))>40 val=$e(val,1,40)_"..." d:'$d(%n) %nBuild s %msg="Field 'WebServicesAuthorized."_$lg(%n,ficol)_"' "_$s($g(val)'="":" (value "_$s(val="":"<NULL>",val=$c(0):"<EMPTY STRING>",1:"'"_val_"'")_")",1:"")_" failed validation",sqlcode=$s(%oper="INSERT":-104,1:-105) q  }
Normalize n %f ;Normalize all fields
 f %f=0,2,4 { s:$g(%d(%f))'="" %d(%f)=$e(%d(%f),1,50) } s:$g(%d(3))'="" %d(3)=%d(3)\1  q
#sqlcompile SELECT=Logical
#import xobw
fdU11(%1,%id="") &sql(SELECT ID FROM xobw.WebServicesAuthorized WHERE  ID=:%1 AND (%ID <> :%id OR :%id IS NULL)) QUIT SQLCODE=100
#sqlcompile SELECT=Logical
#import xobw
fdU12(%1,%2,%3,%id="") &sql(SELECT webServerRef,webServerRef,ien FROM xobw.WebServicesAuthorized WHERE  webServerRef=:%1 AND webServerRef=:%2 AND ien=:%3 AND (%ID <> :%id OR :%id IS NULL)) QUIT SQLCODE=100
missing(fname) s sqlcode=-108,%msg="'"_fname_"' in table '"_"xobw"_"."_"WebServicesAuthorized"_"' is a required field" q
ulerror(cname) s sqlcode=-110,%msg="Unable to obtain lock to "_$s(%oper="DELETE":"maintain",1:"check")_" uniqueness of constraint '"_cname_"'" q
%ETrap s $zt="",SQLCODE=-415,%msg=$s($g(%msg)'="":%msg_" -- ",1:"")_"Error occuring during "_%oper_" in '"_"xobw"_"."_"WebServicesAuthorized"_"':  $ZE="_$ze i $ze["<FRAMESTACK>" { s %msg="Error '"_$ze_"' occurred during "_%oper_" in '"_"xobw"_"."_"WebServicesAuthorized"_" - Process HALTed" d ##Class(%SYS.System).WriteToConsoleLog(%msg) i ($zu(67,10,$j)=1)||($zu(67,10,$j)=3) { w !,%msg h 3 } HALT  } g %EExit
%EExit d:%oper'="DELETE" gunlock2 d gunlock If %tstart,$zu(115,1)=1,$TLEVEL { s %tstart=0 TROLLBACK 1 }  q:%oper="INSERT" "" q
getoldall ; Get all old data values
 #sqlcompile SELECT=Logical
#import xobw
 &sql(SELECT status,webServiceIen INTO :%e() FROM xobw.WebServicesAuthorized WHERE ID=:%rowid) s sqlcode=SQLCODE q
%QuickInsert(d,%nolock=0,pkey=0,parentpkey=0) // Insert new row with values d(icol)
 s:%nolock=2 %nolock=0
 i parentpkey { x "s d(0)=$$"_$p($g(^oddSQL("xobw","WebServer","Q1")),"(")_"(d(0),2)" i d(0)="" { s %qrc=0,%ROWCOUNT=0,%msg="Could not find parent row for the given parent Primary Key value" q  }}
 s %ROWID=$$%insert^xobw.WebServicesAuthorized.T1(.d,$c(0,%nolock=1,0,0,0)),%ROWCOUNT='SQLCODE,%qrc=SQLCODE
 i pkey { i %qrc { s %ROWID=$lb(-1) } else { s %ROWID=$lb(d(0),d(0),d(2)) } s d=$zobjexport(%ROWID,5) } k d q
%QuickBulkInsert(%inscall,%nolock=0) // Insert multiple new rows with values %qd(icol)
 n c,call,nc,nr,%qd,r,x s:%nolock=2 %nolock=0 s nr=$zobjexport(12) f r=1:1:nr { s nc=$zobjexport(12) k %qd f c=1:1:nc { s:$zobjexport(17) %qd(c+1)=$zobjexport(12) } d @%inscall s x=$zobjexport($s(%qrc:-1,1:%ROWID),18) } q  
%QuickUpdate(%rowid,d,%nolock=0,pkey=0) // Update row with SQLRowID=%rowid with values d(icol)
 s:pkey %rowid=$$%QuickFindRowIDByPKey(%rowid,2) i %rowid="" { s %qrc=0,%ROWCOUNT=0 q  }
 i '$$%1^xobw.WebServicesAuthorized.T1(%rowid) s %qrc=0,%ROWCOUNT=0 q
 s:%nolock=2 %nolock=0
 d %update^xobw.WebServicesAuthorized.T1(%rowid,$c(0,%nolock=1,0,0,0),.d) s %ROWCOUNT='SQLCODE s:SQLCODE=100 SQLCODE=0 s %qrc=SQLCODE k d q
%QuickBulkUpdate(%updcall,%nolock=0) // Update multiple new rows with values %qd(icol)
 n c,call,nc,nr,%qd,r,x s:%nolock=2 %nolock=0 s nr=$zobjexport(12) f r=1:1:nr { s %rowid=$zobjexport(12),nc=$zobjexport(12) k %qd f c=1:1:nc { s:$zobjexport(17) %qd(c+1)=$zobjexport(12) } d @%updcall s x=$zobjexport($s(%qrc:-1,1:%ROWID),18) q:%qrc  } q  
%QuickBulkSave(%inscall,%updcall,%nolock=0) // Insert and/or Update multiple [new] rows with values %qd(icol)
 n c,nc,nr,%pkey,%qd,r,%rowid,x s:%nolock=2 %nolock=0 s nr=$zobjexport(12) f r=1:1:nr { 
 	s %pkey=$zobjexport(12),nc=$zobjexport(12) k %qd
 	f c=1:1:nc { s:$zobjexport(17) %qd(c+1)=$zobjexport(12) }
 	s %rowid=$$%QuickFindRowIDByPKey(%pkey,2) if %rowid=""  { d @%inscall s x=$zobjexport($s(%qrc:-1,1:%ROWID),18) } // Insert new row
 	else { d @%updcall s x=$zobjexport($s(%qrc:-1,1:%ROWID),18) q:%qrc  } // Update existing Row
 } q  
%QuickDelete(%rowid,%nolock=0,pkey=0) // Delete row where SQLRowID=%rowid
 s:%nolock=2 %nolock=0
 s:pkey %rowid=$$%QuickFindRowIDByPKey(%rowid,2) i %rowid="" { s %qrc=0,%ROWCOUNT=0 q  }
 i '$$%1^xobw.WebServicesAuthorized.T1(%rowid) s %qrc=0,%ROWCOUNT=0 q
 d %delete^xobw.WebServicesAuthorized.T1(%rowid,$c(0,%nolock=1,0))
 If SQLCODE=-106 { s %qrc=0,%ROWCOUNT=0 } ElseIf SQLCODE<0 { s %qrc=-SQLCODE,%ROWCOUNT=0 } Else { s %ROWCOUNT=1,%qrc=SQLCODE } q
%QuickLoad(%rowid,%nolock=0,pkey=0,skipnewqout=0,qq=0) // Get fields from row where SQLRowID=%rowid
 n d,i,il,subs,t s:%nolock=2 %nolock=1
 s:pkey %rowid=$$%QuickFindRowIDByPKey(%rowid,2) i %rowid="" { s SQLCODE=100,%qrc=100,%ROWCOUNT=0 q  }
 i %nolock=0 { i '$$%AcquireLock^xobw.WebServicesAuthorized.T1(%rowid) { s %qrc=114,%msg="Unable to acquire exclusive lock on table xobw.WebServicesAuthorized for RowID value: "_%rowid,%ROWCOUNT=0 q  } s:$zu(115,2) il=$zu(115,2,0) }
#sqlcompile SELECT=ODBC
 &sql(SELECT %INTERNAL(webServerRef),%INTERNAL(ID),ien,status,webServiceIen INTO :d(0),:d(1),:d(2),:d(3),:d(4) FROM xobw.WebServicesAuthorized WHERE %ID = :%rowid)
 i SQLCODE { i %nolock=0 { d %ReleaseLock^xobw.WebServicesAuthorized.T1(%rowid,0,1) d:$g(il) $zu(115,2,il) } s %ROWCOUNT=0 s:SQLCODE<0 SQLCODE=-SQLCODE s %qrc=SQLCODE q  }
 If %nolock=0 { If $zu(115,1)=1 { TSTART  } ElseIf '$TLEVEL,$zu(115,1)=2 { TSTART  }}
 s:qq d=$zobjexport("xobw.WebServicesAuthorized",18),d=$zobjexport(5,18) s i=-1 f  { s i=$o(d(i)) q:i=""  s d=$zobjexport(d(i),18) } s %qrc=0,%ROWCOUNT=1 i %nolock=0 { d %ReleaseLock^xobw.WebServicesAuthorized.T1(%rowid,0,0) d:$g(il) $zu(115,2,il) } q
%QuickBulkLoad(%rowidlist,%nolock=0,pkey=0) // QuickLoad multiple rows
 n i,ql,rc,%rowid s:%nolock=2 %nolock=0 s rc=0,ql=$g(^oddSQL("xobw","WebServicesAuthorized","QL")) s $p(ql,",",4)="1)"
 f i=2:1:$lg(%rowidlist)+1 { s %rowid=$lg(%rowidlist,i) d @ql If SQLCODE=0 { s rc=rc+1 } Else { q  } } s %ROWCOUNT=rc q
%QuickLoadChildren(%parref,%nolock=0,pkey=0,returnpkey=0) // Get non-hidden fields from table where ParentReferenceField=%parref
 i pkey x "s %parref=$$"_$p($g(^oddSQL("xobw","WebServer","Q1")),"(")_"(%parref,2)" i %parref="" { s %qrc=0,%ROWCOUNT=0 q  }
 n d,i,il,row s:%nolock=2 %nolock=0 i %nolock=0 { s:$zu(115,2) il=$zu(115,2,0) }
#sqlcompile SELECT=ODBC
 If 'returnpkey { n id &sql(DECLARE qckchldid CURSOR FOR SELECT %ID INTO id FROM xobw.WebServicesAuthorized WHERE webServerRef = :%parref) s %ROWCOUNT=0,%qrc=0 &sql(OPEN qckchldid) i SQLCODE { s %qrc=-SQLCODE d:$g(il) $zu(115,2,il) q  } f  { &sql(FETCH qckchldid) i SQLCODE { s:SQLCODE<0 %qrc=-SQLCODE s:'%ROWCOUNT&&(SQLCODE=100) %qrc=100 d:$g(il) $zu(115,2,il) q  } s %qrc=0,d=$zobjexport($lb(id,id),18) } &sql(CLOSE qckchldid) s:SQLCODE %qrc=-SQLCODE d:$g(il) $zu(115,2,il) q  }
 Else { &sql(DECLARE qckchldpk CURSOR FOR SELECT %ID, %INTERNAL(webServerRef), %INTERNAL(webServerRef), ien INTO :d(1), :d(0), :d(0), :d(2) FROM xobw.WebServicesAuthorized WHERE webServerRef = :%parref) s %ROWCOUNT=0,%qrc=0 &sql(OPEN qckchldpk) i SQLCODE { s %qrc=-SQLCODE d:$g(il) $zu(115,2,il) q  } f  { &sql(FETCH qckchldpk) i SQLCODE { s:SQLCODE<0 %qrc=-SQLCODE s:'%ROWCOUNT&&(SQLCODE=100) %qrc=100 d:$g(il) $zu(115,2,il) q  } s row="",i=-1 f  { s i=$o(d(i)) q:i=""  s row=row_$lb(d(i)) } s %qrc=0,d=$zobjexport(row,18) } &sql(CLOSE qckchldpk) s:SQLCODE %qrc=-SQLCODE d:$g(il) $zu(115,2,il) q  }
%QuickFindRowIDByPKey(%pkey,output=1,internal=0) // Get ROWID value for PKEY value given in %pkey
 n %d,d,id,rtm s rtm=$zu(115,5,$s(internal:0,1:1)) s %d(0)=$lg(%pkey,1) s %d(0)=$lg(%pkey,2) s %d(2)=$lg(%pkey,3)
#sqlcompile SELECT=RUNTIME
 s %ROWCOUNT=0,%qrc=0 &sql(SELECT %ID INTO :id FROM xobw.WebServicesAuthorized WHERE webServerRef = :%d(0) AND webServerRef = :%d(0) AND ien = :%d(2))
 d $zu(115,5,rtm) q:internal $g(id) i SQLCODE { s:SQLCODE<0 %qrc=-SQLCODE s:'%ROWCOUNT&&(SQLCODE=100) id="",%qrc=0 } If output=1 { s d=$zobjexport(id,18) } ElseIf output=2 { QUIT id } q
%QuickFindPKeyByRowID(%rowid) // Get Primary Key fields from row where SQLRowID=%rowid
 n d,s,subs,ul
#sqlcompile SELECT=ODBC
 &sql(SELECT %INTERNAL(webServerRef),%INTERNAL(webServerRef),ien INTO :d(0),:d(0),:d(2) FROM xobw.WebServicesAuthorized WHERE %ID = :%rowid)
 i SQLCODE { s %ROWCOUNT=0 s:SQLCODE<0 SQLCODE=-SQLCODE s %qrc=SQLCODE q  }
 s d=$zobjexport($lb(d(0),d(0),d(2)),5) s %qrc=0,%ROWCOUNT=1 q
BuildPurgeIndices(indices="",build)  ; Create/Delete data from index global(s), return Status Code
 q $$$ERROR($$$MethodNotImplemented,"xobw.WebServicesAuthorized::%BuildIndices/%PurgeIndices")
SQLUPPER(v,l) PUBLIC { q $zu(28,v,7,$g(l,32767)) }
ALPHAUP(v,r) PUBLIC { q $zu(28,v,6) }
STRING(v,l) PUBLIC { q $zu(28,v,9,$g(l,32767)) }
SQLSTRING(v,l) PUBLIC { q $zu(28,v,8,$g(l,32767)) }
UPPER(v) PUBLIC { q $zu(28,v,5) }
MVR(v) PUBLIC { q $zu(28,v,2) }
#endclasscontext
#endclassmethod
