IBDEI0I1 ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,24353,1,2,0)
 ;;=2^99215
 ;;^UTILITY(U,$J,358.3,24354,0)
 ;;=99241^^195^1663^1
 ;;^UTILITY(U,$J,358.3,24354,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,24354,1,1,0)
 ;;=1^Problem Focused
 ;;^UTILITY(U,$J,358.3,24354,1,2,0)
 ;;=2^99241
 ;;^UTILITY(U,$J,358.3,24355,0)
 ;;=99242^^195^1663^2
 ;;^UTILITY(U,$J,358.3,24355,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,24355,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,24355,1,2,0)
 ;;=2^99242
 ;;^UTILITY(U,$J,358.3,24356,0)
 ;;=99243^^195^1663^3
 ;;^UTILITY(U,$J,358.3,24356,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,24356,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,24356,1,2,0)
 ;;=2^99243
 ;;^UTILITY(U,$J,358.3,24357,0)
 ;;=99244^^195^1663^4
 ;;^UTILITY(U,$J,358.3,24357,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,24357,1,1,0)
 ;;=1^Comprehensive, Moderate
 ;;^UTILITY(U,$J,358.3,24357,1,2,0)
 ;;=2^99244
 ;;^UTILITY(U,$J,358.3,24358,0)
 ;;=99245^^195^1663^5
 ;;^UTILITY(U,$J,358.3,24358,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,24358,1,1,0)
 ;;=1^Comprehensive, High
 ;;^UTILITY(U,$J,358.3,24358,1,2,0)
 ;;=2^99245
