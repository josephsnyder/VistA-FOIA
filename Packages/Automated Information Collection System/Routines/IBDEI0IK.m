IBDEI0IK ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.6)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.6)
 ;;=^IBE(358.6,
 ;;^UTILITY(U,$J,358.6,0)
 ;;=IMP/EXP PACKAGE INTERFACE^358.6I^10^10
 ;;^UTILITY(U,$J,358.6,1,0)
 ;;=DPT PATIENT'S NAME^VADPT^IBDFN^REGISTRATION^1^2^1^1^1^^^1
 ;;^UTILITY(U,$J,358.6,1,1,0)
 ;;=^^2^2^2930212^^^^
 ;;^UTILITY(U,$J,358.6,1,1,1,0)
 ;;= 
 ;;^UTILITY(U,$J,358.6,1,1,2,0)
 ;;=Patient's Name
 ;;^UTILITY(U,$J,358.6,1,2)
 ;;=Patient's Name^30^^^^^^^^^^^^^^^1
 ;;^UTILITY(U,$J,358.6,1,3)
 ;;=PATIENT NAME
 ;;^UTILITY(U,$J,358.6,1,7,0)
 ;;=^358.67^1^1
 ;;^UTILITY(U,$J,358.6,1,7,1,0)
 ;;=DFN
 ;;^UTILITY(U,$J,358.6,2,0)
 ;;=DPT PATIENT'S PID^VADPT^IBDFN^REGISTRATION^1^2^1^1^1^^^1
 ;;^UTILITY(U,$J,358.6,2,1,0)
 ;;=^^1^1^2931015^^
 ;;^UTILITY(U,$J,358.6,2,1,1,0)
 ;;=Used to display the patient identifier.
 ;;^UTILITY(U,$J,358.6,2,2)
 ;;=PATIENT IDENTIFIER^15^^^^^^^^^^^^^^^1
 ;;^UTILITY(U,$J,358.6,2,3)
 ;;=PATIENT IDENTIFIER PID
 ;;^UTILITY(U,$J,358.6,2,7,0)
 ;;=^358.67^1^1
 ;;^UTILITY(U,$J,358.6,2,7,1,0)
 ;;=DFN
 ;;^UTILITY(U,$J,358.6,3,0)
 ;;=DPT PATIENT'S DOB/AGE^VADPT^IBDFN^REGISTRATION^1^2^2^^1^^^1
 ;;^UTILITY(U,$J,358.6,3,1,0)
 ;;=^^2^2^2951023^
 ;;^UTILITY(U,$J,358.6,3,1,1,0)
 ;;=Patient's DOB in MM DD, YYYY format
 ;;^UTILITY(U,$J,358.6,3,1,2,0)
 ;;=Patient's age in years.
 ;;^UTILITY(U,$J,358.6,3,2)
 ;;=Patient's DOB^12^Patient's Age^3^^^^^^^^^^^^^1
 ;;^UTILITY(U,$J,358.6,3,3)
 ;;=PATIENT DOB AGE PIMS
 ;;^UTILITY(U,$J,358.6,3,7,0)
 ;;=^358.67^1^1
 ;;^UTILITY(U,$J,358.6,3,7,1,0)
 ;;=DFN
 ;;^UTILITY(U,$J,358.6,3,15,0)
 ;;=^358.615I^1^1
 ;;^UTILITY(U,$J,358.6,3,15,1,0)
 ;;=Patient's Age^3^2
 ;;^UTILITY(U,$J,358.6,4,0)
 ;;=DG SELECT ICD-9 DIAGNOSIS CODES^ICD9^IBDFN4^SCHEDULING^^3^2^^1^^^1^5^^^^1^1
 ;;^UTILITY(U,$J,358.6,4,1,0)
 ;;=^^2^2^2970304^^^^
 ;;^UTILITY(U,$J,358.6,4,1,1,0)
 ;;=Allows the user to select ICD-9 diagnosis codes from the ICD Diagnosis
 ;;^UTILITY(U,$J,358.6,4,1,2,0)
 ;;=file. Allows only active codes to be selected.
 ;;^UTILITY(U,$J,358.6,4,2)
 ;;=CODE^7^DIAGNOSIS^30^DESCRIPTION^200^^^^^^^^^^^1^1
 ;;^UTILITY(U,$J,358.6,4,3)
 ;;=SELECT ICD9 ICD-9 CODES DIAGNOSIS
 ;;^UTILITY(U,$J,358.6,4,9)
 ;;=D INPUTICD^IBDFN8(.X)
 ;;^UTILITY(U,$J,358.6,4,11)
 ;;=D TESTICD^IBDFN7
 ;;^UTILITY(U,$J,358.6,4,13,0)
 ;;=^358.613V^2^2
 ;;^UTILITY(U,$J,358.6,4,13,1,0)
 ;;=1;IBD(358.98,
 ;;^UTILITY(U,$J,358.6,4,13,2,0)
 ;;=2;IBD(358.98,
 ;;^UTILITY(U,$J,358.6,4,15,0)
 ;;=^358.615I^2^2
 ;;^UTILITY(U,$J,358.6,4,15,1,0)
 ;;=DIAGNOSIS^30^2^^DIAGNOSIS
 ;;^UTILITY(U,$J,358.6,4,15,2,0)
 ;;=DESCRIPTION^200^3^^DIAGNOSIS
 ;;^UTILITY(U,$J,358.6,4,16)
 ;;=o^2^Diagnosis^^r^1^ICD-9 Code^^1
 ;;^UTILITY(U,$J,358.6,4,17)
 ;;=D SLCTDX^IBDFN12(.X)
 ;;^UTILITY(U,$J,358.6,4,19)
 ;;=D DX^IBDFN14(X)
 ;;^UTILITY(U,$J,358.6,5,0)
 ;;=INPUT DIAGNOSIS CODE (ICD9)^^^PATIENT CARE ENCOUNTER^^1^^^1^^^1^^^^SMP^^^1
 ;;^UTILITY(U,$J,358.6,5,1,0)
 ;;=^^1^1^2970304^^^^
 ;;^UTILITY(U,$J,358.6,5,1,1,0)
 ;;=Used for inputting ICD9 diagnosis codes.
 ;;^UTILITY(U,$J,358.6,5,2)
 ;;=^^^^^^^^^^^^^^^^^1
 ;;^UTILITY(U,$J,358.6,5,3)
 ;;=INPUT ICD9 ICD-9 DIAGNOSIS CODES
 ;;^UTILITY(U,$J,358.6,5,9)
 ;;=D INPUTICD^IBDFN8(.X)
 ;;^UTILITY(U,$J,358.6,5,10)
 ;;=Enter an active ICD9 diagnosis code.
 ;;^UTILITY(U,$J,358.6,5,11)
 ;;=D TESTICD^IBDFN7
 ;;^UTILITY(U,$J,358.6,5,12)
 ;;=DIAGNOSIS/PROBLEM^1^13^14^2
 ;;^UTILITY(U,$J,358.6,5,13,0)
 ;;=^358.613V^10^10
 ;;^UTILITY(U,$J,358.6,5,13,1,0)
 ;;=1;IBD(358.98,^^1^^^^^2
 ;;^UTILITY(U,$J,358.6,5,13,2,0)
 ;;=2;IBD(358.98,^^1^^^^^2
 ;;^UTILITY(U,$J,358.6,5,13,3,0)
 ;;=3;IBD(358.98,^^1^^^^^9
 ;;^UTILITY(U,$J,358.6,5,13,4,0)
 ;;=1;IBE(358.99,^^0
 ;;^UTILITY(U,$J,358.6,5,13,5,0)
 ;;=4;IBD(358.98,^^1^^^^^10
 ;;^UTILITY(U,$J,358.6,5,13,6,0)
 ;;=5;IBD(358.98,^^1^^^^^11
 ;;^UTILITY(U,$J,358.6,5,13,7,0)
 ;;=6;IBD(358.98,^^1^^^^^12
 ;;^UTILITY(U,$J,358.6,5,13,8,0)
 ;;=7;IBD(358.98,^^1^^^^^5
 ;;^UTILITY(U,$J,358.6,5,13,9,0)
 ;;=8;IBD(358.98,^^1^^^^^6
 ;;^UTILITY(U,$J,358.6,5,13,10,0)
 ;;=9;IBD(358.98,^^1^^^^^6
 ;;^UTILITY(U,$J,358.6,5,14)
 ;;=S Y=$$DSPLYICD^IBDFN9(Y)
 ;;^UTILITY(U,$J,358.6,5,17)
 ;;=D SLCTDX^IBDFN12(.X)
 ;;^UTILITY(U,$J,358.6,5,18)
 ;;=S IBDF("OTHER")="80^I '$P(^(0),U,9)" D LIST^IBDFDE2(.IBDSEL,.IBDF,"Diagnosis Code")
 ;;^UTILITY(U,$J,358.6,5,19)
 ;;=D DX^IBDFN14(X)
 ;;^UTILITY(U,$J,358.6,6,0)
 ;;=DG SELECT VISIT TYPE CPT PROCEDURES^VSIT^IBDFN4^SCHEDULING^^3^2^^1^^^1^7^^^^^^^^1
 ;;^UTILITY(U,$J,358.6,6,1,0)
 ;;=^^1^1^2941116^^^^
 ;;^UTILITY(U,$J,358.6,6,1,1,0)
 ;;=Allows for select of just Visit type CPT codes from the CPT file.
 ;;^UTILITY(U,$J,358.6,6,2)
 ;;=CODE^5^RECOMMENDED TEXT-SHORT NAME^40^RECOMMENDED HEADER^30^SHORT NAME FROM CPT FILE^28^^^^^^^^^1^1
 ;;^UTILITY(U,$J,358.6,6,3)
 ;;=SELECT TYPE OF VISIT CPT
 ;;^UTILITY(U,$J,358.6,6,11)
 ;;=D TESTVST^IBDFN7
 ;;^UTILITY(U,$J,358.6,6,15,0)
 ;;=^358.615I^4^3
 ;;^UTILITY(U,$J,358.6,6,15,2,0)
 ;;=RECOMMENDED TEXT-SHORT NAME^40^2
 ;;^UTILITY(U,$J,358.6,6,15,3,0)
 ;;=RECOMMENDED HEADER^30^3
 ;;^UTILITY(U,$J,358.6,6,15,4,0)
 ;;=SHORT NAME FROM CPT FILE^28^4
 ;;^UTILITY(U,$J,358.6,7,0)
 ;;=INPUT VISIT TYPE^^^PATIENT CARE ENCOUNTER^^1^^^1^^^1^^^^SMP
 ;;^UTILITY(U,$J,358.6,7,1,0)
 ;;=^^1^1^2951023^
 ;;^UTILITY(U,$J,358.6,7,1,1,0)
 ;;=Used for inputting the visit type that applies to the visit.
 ;;^UTILITY(U,$J,358.6,7,2)
 ;;=^^^^^^^^^^^^^^^^^1
 ;;^UTILITY(U,$J,358.6,7,3)
 ;;=VISIT TYPE OF VISIT
 ;;^UTILITY(U,$J,358.6,7,9)
 ;;=D INPUTCPT^IBDFN8(.X)
 ;;^UTILITY(U,$J,358.6,7,10)
 ;;=Enter an active Visit Type code.
 ;;^UTILITY(U,$J,358.6,7,11)
 ;;=D TESTVST^IBDFN7
 ;;^UTILITY(U,$J,358.6,7,12)
 ;;=ENCOUNTER^5
 ;;^UTILITY(U,$J,358.6,7,14)
 ;;=S Y=$$DSPLYCPT^IBDFN9(Y)
 ;;^UTILITY(U,$J,358.6,7,17)
 ;;=D SLCTVST^IBDFN12(.X)
 ;;^UTILITY(U,$J,358.6,7,18)
 ;;=S IBDF("OTHER")="357.69^I '$P(^(0),U,4)" D LIST^IBDFDE2(.IBDSEL,.IBDF,"Visit Type (EM) Code")
 ;;^UTILITY(U,$J,358.6,7,19)
 ;;=D VST^IBDFN14(X)
 ;;^UTILITY(U,$J,358.6,8,0)
 ;;=DG SELECT CPT PROCEDURE CODES^CPT^IBDFN4^SCHEDULING^^3^2^^1^^^1^9^^^^^^^^1
 ;;^UTILITY(U,$J,358.6,8,1,0)
 ;;=^^2^2^2961007^^^^
 ;;^UTILITY(U,$J,358.6,8,1,1,0)
 ;;=Allows for the selection of CPT codes from the CPT file. Only active codes
 ;;^UTILITY(U,$J,358.6,8,1,2,0)
 ;;=are allowed.
 ;;^UTILITY(U,$J,358.6,8,2)
 ;;=CODE^5^SHORT NAME^28^DESCRIPTION^161^^^^^^^^^^CODE^1^1
 ;;^UTILITY(U,$J,358.6,8,3)
 ;;=SELECT CPT PROCEDURE CODES
 ;;^UTILITY(U,$J,358.6,8,9)
 ;;=S X=$$CPT^IBDFN12(X)
 ;;^UTILITY(U,$J,358.6,8,11)
 ;;=D TESTCPT^IBDFN7
 ;;^UTILITY(U,$J,358.6,8,15,0)
 ;;=^357.615I^2^2
 ;;^UTILITY(U,$J,358.6,8,15,1,0)
 ;;=SHORT NAME^28^2^^PROCEDURE
 ;;^UTILITY(U,$J,358.6,8,15,2,0)
 ;;=DESCRIPTION^161^3^^PROCEDURE
 ;;^UTILITY(U,$J,358.6,8,16)
 ;;=o^2^Procedure Narrative^^r^3^CPT CODE^1
 ;;^UTILITY(U,$J,358.6,9,0)
 ;;=INPUT PROCEDURE CODE (CPT4)^^^PATIENT CARE ENCOUNTER^^1^4^^1^0^^1^^^^SMP
 ;;^UTILITY(U,$J,358.6,9,1,0)
 ;;=^^1^1^2960205^^^^
 ;;^UTILITY(U,$J,358.6,9,1,1,0)
 ;;=Used for inputting CPT coded procedures performed on the patient.
 ;;^UTILITY(U,$J,358.6,9,2)
 ;;=^^^^^^^^^^^^^^^^^1
 ;;^UTILITY(U,$J,358.6,9,3)
 ;;=CPT4 PROCEDURE CODES
 ;;^UTILITY(U,$J,358.6,9,9)
 ;;=D INPUTCPT^IBDFN8(.X)
 ;;^UTILITY(U,$J,358.6,9,10)
 ;;=Enter an active CPT procedure code.
 ;;^UTILITY(U,$J,358.6,9,11)
 ;;=D TESTCPT^IBDFN7
 ;;^UTILITY(U,$J,358.6,9,12)
 ;;=PROCEDURE^1^6^7^3^2
 ;;^UTILITY(U,$J,358.6,9,13,0)
 ;;=^358.613V^2^2
 ;;^UTILITY(U,$J,358.6,9,13,1,0)
 ;;=1;IBD(358.98,^^0
 ;;^UTILITY(U,$J,358.6,9,13,2,0)
 ;;=2;IBD(358.98,^^0
 ;;^UTILITY(U,$J,358.6,9,14)
 ;;=S Y=$$DSPLYCPT^IBDFN9(Y)
 ;;^UTILITY(U,$J,358.6,9,15,0)
 ;;=^357.615I^0^0
 ;;^UTILITY(U,$J,358.6,9,17)
 ;;=D SLCTCPT^IBDFN12(.X)
 ;;^UTILITY(U,$J,358.6,9,18)
 ;;=S IBDF("OTHER")="81^I '$P(^(0),U,4)" D LIST^IBDFDE2(.IBDSEL,.IBDF,"CPT Procedure Code")
 ;;^UTILITY(U,$J,358.6,9,19)
 ;;=D CPT^IBDFN14(X)
 ;;^UTILITY(U,$J,358.6,10,0)
 ;;=IBDF UTILITY FOR LABELS ONLY^LABELS^IBDFN^AUTOMATED INFO COLLECTION SYS^0^2^2^^1^^^1
 ;;^UTILITY(U,$J,358.6,10,1,0)
 ;;=^^2^2^2970319^^^
 ;;^UTILITY(U,$J,358.6,10,1,1,0)
 ;;=This interface returns no data. Its purpose is to print labels without
 ;;^UTILITY(U,$J,358.6,10,1,2,0)
 ;;=data to the form.
 ;;^UTILITY(U,$J,358.6,10,2)
 ;;=Underscore Only^0^^^^^^^^^^^^^^^1
 ;;^UTILITY(U,$J,358.6,10,3)
 ;;=UTILITY BLANKS LABELS
 ;;^UTILITY(U,$J,358.6,10,15,0)
 ;;=^357.615I^0^0
