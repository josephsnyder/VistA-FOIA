IBDEI00E ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.2)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.2,101,2,1,0)
 ;;=1^ ^^2^^1^^1
 ;;^UTILITY(U,$J,358.2,101,2,2,0)
 ;;=2^ ^5^1^1^^0
 ;;^UTILITY(U,$J,358.2,101,2,3,0)
 ;;=3^ ^50^1^2^^1
 ;;^UTILITY(U,$J,358.2,102,0)
 ;;=VISIT TYPE^168^^^^^1^0^C^^6^0^^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,102,1,0)
 ;;=^358.21I^1^1
 ;;^UTILITY(U,$J,358.2,102,1,1,0)
 ;;=1^1
 ;;^UTILITY(U,$J,358.2,102,2,0)
 ;;=^358.22I^4^3
 ;;^UTILITY(U,$J,358.2,102,2,1,0)
 ;;=1^^29^1^2^^1
 ;;^UTILITY(U,$J,358.2,102,2,3,0)
 ;;=3^ ^^2^^1^^1^^1
 ;;^UTILITY(U,$J,358.2,102,2,4,0)
 ;;=2^ ^5^1^1
 ;;^UTILITY(U,$J,358.2,103,0)
 ;;=ICD 9^169^^^^^1^0^BC^^4^0^2^0^^0
 ;;^UTILITY(U,$J,358.2,103,1,0)
 ;;=^358.21I^2^2
 ;;^UTILITY(U,$J,358.2,103,1,1,0)
 ;;=1
 ;;^UTILITY(U,$J,358.2,103,1,2,0)
 ;;=2^3^54
 ;;^UTILITY(U,$J,358.2,103,2,0)
 ;;=^358.22I^4^4
 ;;^UTILITY(U,$J,358.2,103,2,1,0)
 ;;=1^P^^2^^1^^1^1^1
 ;;^UTILITY(U,$J,358.2,103,2,2,0)
 ;;=3^ ^7^1^1^^0
 ;;^UTILITY(U,$J,358.2,103,2,3,0)
 ;;=4^ ^50^1^2^^1
 ;;^UTILITY(U,$J,358.2,103,2,4,0)
 ;;=2^S^^2^^1^^1^2^0
 ;;^UTILITY(U,$J,358.2,104,0)
 ;;=CPT CODES^174^^^^^1^0^CSU^^8^0^2^0^^0^2^2
 ;;^UTILITY(U,$J,358.2,104,1,0)
 ;;=^358.21I^3^3
 ;;^UTILITY(U,$J,358.2,104,1,1,0)
 ;;=1^3^2
 ;;^UTILITY(U,$J,358.2,104,1,2,0)
 ;;=3^^91
 ;;^UTILITY(U,$J,358.2,104,1,3,0)
 ;;=2^3^54
 ;;^UTILITY(U,$J,358.2,104,2,0)
 ;;=^358.22I^3^3
 ;;^UTILITY(U,$J,358.2,104,2,1,0)
 ;;=2^ ^5^1^1^^0
 ;;^UTILITY(U,$J,358.2,104,2,2,0)
 ;;=3^ ^50^1^2^^1
 ;;^UTILITY(U,$J,358.2,104,2,3,0)
 ;;=1^ ^^2^^1^^1^^0
 ;;^UTILITY(U,$J,358.2,105,0)
 ;;=DIAGNOSES^175^^^^^1^0^BC^^4^0^^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,105,1,0)
 ;;=^358.21I^1^1
 ;;^UTILITY(U,$J,358.2,105,1,1,0)
 ;;=1^2
 ;;^UTILITY(U,$J,358.2,105,2,0)
 ;;=^358.22I^5^5
 ;;^UTILITY(U,$J,358.2,105,2,1,0)
 ;;=4^ ^7^1^1^^0
 ;;^UTILITY(U,$J,358.2,105,2,2,0)
 ;;=5^ ^40^1^2^^1
 ;;^UTILITY(U,$J,358.2,105,2,3,0)
 ;;=1^P^^2^^1^^1^1^1
 ;;^UTILITY(U,$J,358.2,105,2,4,0)
 ;;=2^S^^2^^1^^1^2^0
 ;;^UTILITY(U,$J,358.2,105,2,5,0)
 ;;=3^A^^2^^1^^1^7^0
 ;;^UTILITY(U,$J,358.2,106,0)
 ;;=VISIT TYPE^176^^^^^2^0^C^^6^0^^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,106,1,0)
 ;;=^358.21I^1^1
 ;;^UTILITY(U,$J,358.2,106,1,1,0)
 ;;=1^2
 ;;^UTILITY(U,$J,358.2,106,2,0)
 ;;=^358.22I^4^3
 ;;^UTILITY(U,$J,358.2,106,2,1,0)
 ;;=1^ ^28^1^2^^1
 ;;^UTILITY(U,$J,358.2,106,2,3,0)
 ;;=3^^^2^^1^^1^^1
 ;;^UTILITY(U,$J,358.2,106,2,4,0)
 ;;=2^ ^5^1^1
 ;;^UTILITY(U,$J,358.2,107,0)
 ;;=DIAGNOSES^179^^^^^1^0^BC^^4^0^2^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,107,1,0)
 ;;=^358.21I^2^2
 ;;^UTILITY(U,$J,358.2,107,1,1,0)
 ;;=1^2^1
 ;;^UTILITY(U,$J,358.2,107,1,2,0)
 ;;=2^2^64
 ;;^UTILITY(U,$J,358.2,107,2,0)
 ;;=^358.22I^5^5
 ;;^UTILITY(U,$J,358.2,107,2,1,0)
 ;;=4^ ^7^1^1^^0
 ;;^UTILITY(U,$J,358.2,107,2,2,0)
 ;;=5^ ^35^1^2^^1
 ;;^UTILITY(U,$J,358.2,107,2,3,0)
 ;;=1^P^^2^^1^^1^1^1
 ;;^UTILITY(U,$J,358.2,107,2,4,0)
 ;;=2^S^^2^^1^^1^2^0
 ;;^UTILITY(U,$J,358.2,107,2,5,0)
 ;;=3^A^^2^^1^^1^7^0
 ;;^UTILITY(U,$J,358.2,108,0)
 ;;=CPT CODES^180^^^^^1^0^CSU^^8^0^2^0^^0^2^2
 ;;^UTILITY(U,$J,358.2,108,1,0)
 ;;=^358.21I^3^3
 ;;^UTILITY(U,$J,358.2,108,1,1,0)
 ;;=1^2^2
 ;;^UTILITY(U,$J,358.2,108,1,2,0)
 ;;=3^^91
 ;;^UTILITY(U,$J,358.2,108,1,3,0)
 ;;=2^3^64
 ;;^UTILITY(U,$J,358.2,108,2,0)
 ;;=^358.22I^3^3
 ;;^UTILITY(U,$J,358.2,108,2,1,0)
 ;;=2^ ^5^1^1^^0
 ;;^UTILITY(U,$J,358.2,108,2,2,0)
 ;;=3^ ^50^1^2^^1
 ;;^UTILITY(U,$J,358.2,108,2,3,0)
 ;;=1^ ^^2^^1^^1^^0
 ;;^UTILITY(U,$J,358.2,109,0)
 ;;=DIAGNOSIS CODES^181^^^^^1^0^UBC^^4^0^2^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,109,1,0)
 ;;=^358.21I^2^2
 ;;^UTILITY(U,$J,358.2,109,1,1,0)
 ;;=1^3^1
 ;;^UTILITY(U,$J,358.2,109,1,2,0)
 ;;=2^3^59
 ;;^UTILITY(U,$J,358.2,109,2,0)
 ;;=^358.22I^4^4
 ;;^UTILITY(U,$J,358.2,109,2,1,0)
 ;;=2^CODE^6^1^1^^0
 ;;^UTILITY(U,$J,358.2,109,2,2,0)
 ;;=5^ ^35^1^2^^1
 ;;^UTILITY(U,$J,358.2,109,2,3,0)
 ;;=4^S^^2^^1^^1^2^0
 ;;^UTILITY(U,$J,358.2,109,2,4,0)
 ;;=3^P^^2^^1^^1^1^1
 ;;^UTILITY(U,$J,358.2,110,0)
 ;;=CPT CODES^182^^^^^1^0^UBC^^8^0^^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,110,1,0)
 ;;=^358.21I^3^3
 ;;^UTILITY(U,$J,358.2,110,1,1,0)
 ;;=1^2^4
 ;;^UTILITY(U,$J,358.2,110,1,2,0)
 ;;=2^2
 ;;^UTILITY(U,$J,358.2,110,1,3,0)
 ;;=3^2
 ;;^UTILITY(U,$J,358.2,110,2,0)
 ;;=^358.22I^3^3
 ;;^UTILITY(U,$J,358.2,110,2,1,0)
 ;;=1^ ^^2^^1^^1
 ;;^UTILITY(U,$J,358.2,110,2,2,0)
 ;;=2^ ^5^1^1^^1
 ;;^UTILITY(U,$J,358.2,110,2,3,0)
 ;;=3^ ^35^1^2^^1
 ;;^UTILITY(U,$J,358.2,111,0)
 ;;=TYPE OF VISIT^185^^R^^^3^0^UBSC^^6^1^^0
 ;;^UTILITY(U,$J,358.2,111,2,0)
 ;;=^358.22I^3^3
 ;;^UTILITY(U,$J,358.2,111,2,1,0)
 ;;=1^VISIT (mark one only)^31^1^2^^1
 ;;^UTILITY(U,$J,358.2,111,2,2,0)
 ;;=2^CODE^5^1^1^^0
 ;;^UTILITY(U,$J,358.2,111,2,3,0)
 ;;=3^x^^2^^1^^1
 ;;^UTILITY(U,$J,358.2,112,0)
 ;;=DIAGNOSIS CODES^186^^^^^1^0^UBC^^4^0^2^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,112,1,0)
 ;;=^358.21I^2^2
 ;;^UTILITY(U,$J,358.2,112,1,1,0)
 ;;=1^3^1
 ;;^UTILITY(U,$J,358.2,112,1,2,0)
 ;;=2^3^59
 ;;^UTILITY(U,$J,358.2,112,2,0)
 ;;=^358.22I^4^4
 ;;^UTILITY(U,$J,358.2,112,2,1,0)
 ;;=2^CODE^6^1^1^^0
 ;;^UTILITY(U,$J,358.2,112,2,2,0)
 ;;=5^ ^35^1^2^^1
 ;;^UTILITY(U,$J,358.2,112,2,3,0)
 ;;=4^S^^2^^1^^1^2^0
 ;;^UTILITY(U,$J,358.2,112,2,4,0)
 ;;=3^P^^2^^1^^1^1^1
 ;;^UTILITY(U,$J,358.2,113,0)
 ;;=CPT CODES^187^^^^^1^0^UBC^^8^0^^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,113,1,0)
 ;;=^358.21I^3^3
 ;;^UTILITY(U,$J,358.2,113,1,1,0)
 ;;=1^2^4
 ;;^UTILITY(U,$J,358.2,113,1,2,0)
 ;;=2^2
 ;;^UTILITY(U,$J,358.2,113,1,3,0)
 ;;=3^2
 ;;^UTILITY(U,$J,358.2,113,2,0)
 ;;=^358.22I^3^3
 ;;^UTILITY(U,$J,358.2,113,2,1,0)
 ;;=1^ ^^2^^1^^1
 ;;^UTILITY(U,$J,358.2,113,2,2,0)
 ;;=2^ ^5^1^1^^1
 ;;^UTILITY(U,$J,358.2,113,2,3,0)
 ;;=3^ ^35^1^2^^1
 ;;^UTILITY(U,$J,358.2,114,0)
 ;;=CPT CODES^190^^^^^1^0^SC^^8^0^2^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,114,1,0)
 ;;=^358.21I^2^2
 ;;^UTILITY(U,$J,358.2,114,1,1,0)
 ;;=1^2^1
 ;;^UTILITY(U,$J,358.2,114,1,2,0)
 ;;=2^2^65
 ;;^UTILITY(U,$J,358.2,114,2,0)
 ;;=^358.22I^3^3
 ;;^UTILITY(U,$J,358.2,114,2,1,0)
 ;;=3^ ^5^1^1^^0
 ;;^UTILITY(U,$J,358.2,114,2,2,0)
 ;;=2^ ^45^1^2^^1
 ;;^UTILITY(U,$J,358.2,114,2,3,0)
 ;;=1^ ^^2^^1^^1^^0
 ;;^UTILITY(U,$J,358.2,115,0)
 ;;=VISIT TYPE^191^^^^^1^0^C^^6^0^^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,115,1,0)
 ;;=^358.21I^1^1
 ;;^UTILITY(U,$J,358.2,115,1,1,0)
 ;;=1^2
 ;;^UTILITY(U,$J,358.2,115,2,0)
 ;;=^358.22I^4^3
 ;;^UTILITY(U,$J,358.2,115,2,1,0)
 ;;=1^ ^28^1^2^^1
 ;;^UTILITY(U,$J,358.2,115,2,3,0)
 ;;=3^^^2^^1^^1^^1
 ;;^UTILITY(U,$J,358.2,115,2,4,0)
 ;;=2^ ^5^1^1
 ;;^UTILITY(U,$J,358.2,116,0)
 ;;=DIAGNOSES^192^^R^^^1^0^SC^^4^1^^0^^^3^2
 ;;^UTILITY(U,$J,358.2,116,2,0)
 ;;=^358.22I^5^4
 ;;^UTILITY(U,$J,358.2,116,2,1,0)
 ;;=3^CODE^7^1^1^^0
 ;;^UTILITY(U,$J,358.2,116,2,2,0)
 ;;=5^DIAGNOSIS^37^1^2^^1
 ;;^UTILITY(U,$J,358.2,116,2,4,0)
 ;;=1^P^^2^^1^^1^1^1
 ;;^UTILITY(U,$J,358.2,116,2,5,0)
 ;;=2^S^^2^^1^^1^2^0
 ;;^UTILITY(U,$J,358.2,117,0)
 ;;=CPT CODES^195^^^^^1^0^SC^^8^0^2^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,117,1,0)
 ;;=^358.21I^2^2
 ;;^UTILITY(U,$J,358.2,117,1,1,0)
 ;;=1^2^1
 ;;^UTILITY(U,$J,358.2,117,1,2,0)
 ;;=2^2^65
 ;;^UTILITY(U,$J,358.2,117,2,0)
 ;;=^358.22I^3^3
 ;;^UTILITY(U,$J,358.2,117,2,1,0)
 ;;=3^ ^5^1^1^^0
 ;;^UTILITY(U,$J,358.2,117,2,2,0)
 ;;=2^ ^45^1^2^^1
 ;;^UTILITY(U,$J,358.2,117,2,3,0)
 ;;=1^ ^^2^^1^^1^^0
 ;;^UTILITY(U,$J,358.2,118,0)
 ;;=VISIT TYPE^196^^^^^1^0^C^^6^0^^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,118,1,0)
 ;;=^358.21I^1^1
 ;;^UTILITY(U,$J,358.2,118,1,1,0)
 ;;=1^2
 ;;^UTILITY(U,$J,358.2,118,2,0)
 ;;=^358.22I^4^3
 ;;^UTILITY(U,$J,358.2,118,2,1,0)
 ;;=1^ ^28^1^2^^1
 ;;^UTILITY(U,$J,358.2,118,2,3,0)
 ;;=3^^^2^^1^^1^^1
 ;;^UTILITY(U,$J,358.2,118,2,4,0)
 ;;=2^ ^5^1^1
 ;;^UTILITY(U,$J,358.2,119,0)
 ;;=DIAGNOSES^199^^R^^^1^0^SC^^4^1^^0^^^3^2
 ;;^UTILITY(U,$J,358.2,119,2,0)
 ;;=^358.22I^5^4
 ;;^UTILITY(U,$J,358.2,119,2,1,0)
 ;;=3^CODE^7^1^1^^0
 ;;^UTILITY(U,$J,358.2,119,2,2,0)
 ;;=5^DIAGNOSIS^37^1^2^^1
 ;;^UTILITY(U,$J,358.2,119,2,4,0)
 ;;=1^P^^2^^1^^1^1^1
 ;;^UTILITY(U,$J,358.2,119,2,5,0)
 ;;=2^S^^2^^1^^1^2^0
 ;;^UTILITY(U,$J,358.2,120,0)
 ;;=VISIT TYPE^200^^^^^2^0^C^^6^0^^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,120,1,0)
 ;;=^358.21I^1^1
 ;;^UTILITY(U,$J,358.2,120,1,1,0)
 ;;=1^2
 ;;^UTILITY(U,$J,358.2,120,2,0)
 ;;=^358.22I^4^3
 ;;^UTILITY(U,$J,358.2,120,2,1,0)
 ;;=1^ ^28^1^2^^1
 ;;^UTILITY(U,$J,358.2,120,2,3,0)
 ;;=3^^^2^^1^^1^^1
 ;;^UTILITY(U,$J,358.2,120,2,4,0)
 ;;=2^ ^5^1^1
 ;;^UTILITY(U,$J,358.2,121,0)
 ;;=DIAGNOSES^203^^^^^1^0^BC^^4^0^^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,121,1,0)
 ;;=^358.21I^1^1
 ;;^UTILITY(U,$J,358.2,121,1,1,0)
 ;;=1^2
 ;;^UTILITY(U,$J,358.2,121,2,0)
 ;;=^358.22I^5^5
 ;;^UTILITY(U,$J,358.2,121,2,1,0)
 ;;=4^ ^7^1^1^^0
 ;;^UTILITY(U,$J,358.2,121,2,2,0)
 ;;=5^ ^40^1^2^^1
 ;;^UTILITY(U,$J,358.2,121,2,3,0)
 ;;=1^P^^2^^1^^1^1^1
 ;;^UTILITY(U,$J,358.2,121,2,4,0)
 ;;=2^S^^2^^1^^1^2^0
 ;;^UTILITY(U,$J,358.2,121,2,5,0)
 ;;=3^A^^2^^1^^1^7^0
 ;;^UTILITY(U,$J,358.2,122,0)
 ;;=CPT CODES^204^^^^^1^0^CSU^^8^0^2^0^^0^2^2
 ;;^UTILITY(U,$J,358.2,122,1,0)
 ;;=^358.21I^3^3
 ;;^UTILITY(U,$J,358.2,122,1,1,0)
 ;;=1^3^2
 ;;^UTILITY(U,$J,358.2,122,1,2,0)
 ;;=3^^91
 ;;^UTILITY(U,$J,358.2,122,1,3,0)
 ;;=2^3^54
 ;;^UTILITY(U,$J,358.2,122,2,0)
 ;;=^358.22I^3^3
 ;;^UTILITY(U,$J,358.2,122,2,1,0)
 ;;=2^ ^5^1^1^^0
 ;;^UTILITY(U,$J,358.2,122,2,2,0)
 ;;=3^ ^50^1^2^^1
 ;;^UTILITY(U,$J,358.2,122,2,3,0)
 ;;=1^ ^^2^^1^^1^^0
 ;;^UTILITY(U,$J,358.2,123,0)
 ;;=VISIT TYPE^205^^^^^2^0^C^^6^0^^0^^0^3^2
 ;;^UTILITY(U,$J,358.2,123,1,0)
 ;;=^358.21I^1^1
 ;;^UTILITY(U,$J,358.2,123,1,1,0)
 ;;=1^2
 ;;^UTILITY(U,$J,358.2,123,2,0)
 ;;=^358.22I^4^3
 ;;^UTILITY(U,$J,358.2,123,2,1,0)
 ;;=1^ ^28^1^2^^1
 ;;^UTILITY(U,$J,358.2,123,2,3,0)
 ;;=3^^^2^^1^^1^^1
 ;;^UTILITY(U,$J,358.2,123,2,4,0)
 ;;=2^ ^5^1^1
 ;;^UTILITY(U,$J,358.2,124,0)
 ;;=CPT CODES^208^^^^^1^0^CSU^^8^0^2^0^^0^2^2
 ;;^UTILITY(U,$J,358.2,124,1,0)
 ;;=^358.21I^3^3
 ;;^UTILITY(U,$J,358.2,124,1,1,0)
 ;;=1^3^2
 ;;^UTILITY(U,$J,358.2,124,1,2,0)
 ;;=3^^91
 ;;^UTILITY(U,$J,358.2,124,1,3,0)
 ;;=2^3^54
 ;;^UTILITY(U,$J,358.2,124,2,0)
 ;;=^358.22I^3^3
 ;;^UTILITY(U,$J,358.2,124,2,1,0)
 ;;=2^ ^5^1^1^^0
 ;;^UTILITY(U,$J,358.2,124,2,2,0)
 ;;=3^ ^50^1^2^^1
