IBDEI0HZ ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,24266,1,3,0)
 ;;=3^Chemo,IV push,Init
 ;;^UTILITY(U,$J,358.3,24267,0)
 ;;=96411^^194^1658^15^^^^1
 ;;^UTILITY(U,$J,358.3,24267,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24267,1,1,0)
 ;;=1^96411
 ;;^UTILITY(U,$J,358.3,24267,1,3,0)
 ;;=3^Chemo,IV push, addl drug
 ;;^UTILITY(U,$J,358.3,24268,0)
 ;;=96413^^194^1658^14^^^^1
 ;;^UTILITY(U,$J,358.3,24268,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24268,1,1,0)
 ;;=1^96413
 ;;^UTILITY(U,$J,358.3,24268,1,3,0)
 ;;=3^Chemo,IV Infusn,Init Hr
 ;;^UTILITY(U,$J,358.3,24269,0)
 ;;=96417^^194^1658^17^^^^1
 ;;^UTILITY(U,$J,358.3,24269,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24269,1,1,0)
 ;;=1^96417
 ;;^UTILITY(U,$J,358.3,24269,1,3,0)
 ;;=3^Chemo,Infusn,ea add seql drug
 ;;^UTILITY(U,$J,358.3,24270,0)
 ;;=96415^^194^1658^11^^^^1
 ;;^UTILITY(U,$J,358.3,24270,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24270,1,1,0)
 ;;=1^96415
 ;;^UTILITY(U,$J,358.3,24270,1,3,0)
 ;;=3^Chemo, IV Infusn,Ea add hr
 ;;^UTILITY(U,$J,358.3,24271,0)
 ;;=96416^^194^1658^9^^^^1
 ;;^UTILITY(U,$J,358.3,24271,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24271,1,1,0)
 ;;=1^96416
 ;;^UTILITY(U,$J,358.3,24271,1,3,0)
 ;;=3^Chemo Infusn Pump,Init >8hr
 ;;^UTILITY(U,$J,358.3,24272,0)
 ;;=96423^^194^1658^7^^^^1
 ;;^UTILITY(U,$J,358.3,24272,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24272,1,1,0)
 ;;=1^96423
 ;;^UTILITY(U,$J,358.3,24272,1,3,0)
 ;;=3^Chemo, IA Infusn,Ea Addl Hr
 ;;^UTILITY(U,$J,358.3,24273,0)
 ;;=96425^^194^1658^12^^^^1
 ;;^UTILITY(U,$J,358.3,24273,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24273,1,1,0)
 ;;=1^96425
 ;;^UTILITY(U,$J,358.3,24273,1,3,0)
 ;;=3^Chemo,IA,Init pump >8hr
 ;;^UTILITY(U,$J,358.3,24274,0)
 ;;=96360^^194^1658^20^^^^1
 ;;^UTILITY(U,$J,358.3,24274,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24274,1,1,0)
 ;;=1^96360
 ;;^UTILITY(U,$J,358.3,24274,1,3,0)
 ;;=3^Hydration IV Infusn,Init hr
 ;;^UTILITY(U,$J,358.3,24275,0)
 ;;=96361^^194^1658^21^^^^1
 ;;^UTILITY(U,$J,358.3,24275,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24275,1,1,0)
 ;;=1^96361
 ;;^UTILITY(U,$J,358.3,24275,1,3,0)
 ;;=3^Hydration IV Infusn Ea Addl Hr
 ;;^UTILITY(U,$J,358.3,24276,0)
 ;;=96365^^194^1658^27^^^^1
 ;;^UTILITY(U,$J,358.3,24276,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24276,1,1,0)
 ;;=1^96365
 ;;^UTILITY(U,$J,358.3,24276,1,3,0)
 ;;=3^Ther/Diag/Proph,IV Infusn,Init hr
 ;;^UTILITY(U,$J,358.3,24277,0)
 ;;=96366^^194^1658^28^^^^1
 ;;^UTILITY(U,$J,358.3,24277,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24277,1,1,0)
 ;;=1^96366
 ;;^UTILITY(U,$J,358.3,24277,1,3,0)
 ;;=3^Ther/Diag/Proph, IV Infusn,Ea Add Hr
 ;;^UTILITY(U,$J,358.3,24278,0)
 ;;=96372^^194^1658^23^^^^1
 ;;^UTILITY(U,$J,358.3,24278,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24278,1,1,0)
 ;;=1^96372
 ;;^UTILITY(U,$J,358.3,24278,1,3,0)
 ;;=3^Ther/Diag/Proph IM/SQ injection
 ;;^UTILITY(U,$J,358.3,24279,0)
 ;;=96374^^194^1658^26^^^^1
 ;;^UTILITY(U,$J,358.3,24279,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24279,1,1,0)
 ;;=1^96374
 ;;^UTILITY(U,$J,358.3,24279,1,3,0)
 ;;=3^Ther/Diag/Proph IV push, Init
 ;;^UTILITY(U,$J,358.3,24280,0)
 ;;=96375^^194^1658^25^^^^1
 ;;^UTILITY(U,$J,358.3,24280,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24280,1,1,0)
 ;;=1^96375
 ;;^UTILITY(U,$J,358.3,24280,1,3,0)
 ;;=3^Ther/Diag/Proph IV push ea add seql,new drug
 ;;^UTILITY(U,$J,358.3,24281,0)
 ;;=96367^^194^1658^29^^^^1
 ;;^UTILITY(U,$J,358.3,24281,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24281,1,1,0)
 ;;=1^96367
 ;;^UTILITY(U,$J,358.3,24281,1,3,0)
 ;;=3^Ther/Diag/Proph,IV Infusn,Ea add hr seql
 ;;^UTILITY(U,$J,358.3,24282,0)
 ;;=96368^^194^1658^24^^^^1
 ;;^UTILITY(U,$J,358.3,24282,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24282,1,1,0)
 ;;=1^96368
 ;;^UTILITY(U,$J,358.3,24282,1,3,0)
 ;;=3^Ther/Diag/Proph IV Infusn, Concurrent
 ;;^UTILITY(U,$J,358.3,24283,0)
 ;;=96446^^194^1658^5^^^^1
 ;;^UTILITY(U,$J,358.3,24283,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24283,1,1,0)
 ;;=1^96446
 ;;^UTILITY(U,$J,358.3,24283,1,3,0)
 ;;=3^Chemo Admn, Peritoneal Cavity
 ;;^UTILITY(U,$J,358.3,24284,0)
 ;;=96406^^194^1658^2^^^^1
 ;;^UTILITY(U,$J,358.3,24284,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24284,1,1,0)
 ;;=1^96406
 ;;^UTILITY(U,$J,358.3,24284,1,3,0)
 ;;=3^Chemo Admin Intralesional,> 7
 ;;^UTILITY(U,$J,358.3,24285,0)
 ;;=J9000^^194^1659^9^^^^1
 ;;^UTILITY(U,$J,358.3,24285,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24285,1,1,0)
 ;;=1^J9000
 ;;^UTILITY(U,$J,358.3,24285,1,3,0)
 ;;=3^Doxorubicin 10mg
 ;;^UTILITY(U,$J,358.3,24286,0)
 ;;=J9031^^194^1659^1^^^^1
 ;;^UTILITY(U,$J,358.3,24286,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24286,1,1,0)
 ;;=1^J9031
 ;;^UTILITY(U,$J,358.3,24286,1,3,0)
 ;;=3^B C G (Intravesical)
 ;;^UTILITY(U,$J,358.3,24287,0)
 ;;=J9040^^194^1659^2^^^^1
 ;;^UTILITY(U,$J,358.3,24287,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24287,1,1,0)
 ;;=1^J9040
 ;;^UTILITY(U,$J,358.3,24287,1,3,0)
 ;;=3^Bleomycin Sulfate Inj 15U
 ;;^UTILITY(U,$J,358.3,24288,0)
 ;;=J9045^^194^1659^4^^^^1
 ;;^UTILITY(U,$J,358.3,24288,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24288,1,1,0)
 ;;=1^J9045
 ;;^UTILITY(U,$J,358.3,24288,1,3,0)
 ;;=3^Carboplatin 50mg
 ;;^UTILITY(U,$J,358.3,24289,0)
 ;;=J9060^^194^1659^5^^^^1
 ;;^UTILITY(U,$J,358.3,24289,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24289,1,1,0)
 ;;=1^J9060
 ;;^UTILITY(U,$J,358.3,24289,1,3,0)
 ;;=3^Cisplatin 10 MG injection
 ;;^UTILITY(U,$J,358.3,24290,0)
 ;;=J9100^^194^1659^7^^^^1
 ;;^UTILITY(U,$J,358.3,24290,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24290,1,1,0)
 ;;=1^J9100
 ;;^UTILITY(U,$J,358.3,24290,1,3,0)
 ;;=3^Cytarabine (arac) 100mg
 ;;^UTILITY(U,$J,358.3,24291,0)
 ;;=J9181^^194^1659^11^^^^1
 ;;^UTILITY(U,$J,358.3,24291,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24291,1,1,0)
 ;;=1^J9181
 ;;^UTILITY(U,$J,358.3,24291,1,3,0)
 ;;=3^Etoposide (VP 16) 10mg
 ;;^UTILITY(U,$J,358.3,24292,0)
 ;;=J9185^^194^1659^12^^^^1
 ;;^UTILITY(U,$J,358.3,24292,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24292,1,1,0)
 ;;=1^J9185
 ;;^UTILITY(U,$J,358.3,24292,1,3,0)
 ;;=3^Fludarabine 50mg
 ;;^UTILITY(U,$J,358.3,24293,0)
 ;;=J9190^^194^1659^13^^^^1
 ;;^UTILITY(U,$J,358.3,24293,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24293,1,1,0)
 ;;=1^J9190
 ;;^UTILITY(U,$J,358.3,24293,1,3,0)
 ;;=3^Fluorouracil 500mg
 ;;^UTILITY(U,$J,358.3,24294,0)
 ;;=J9201^^194^1659^14^^^^1
 ;;^UTILITY(U,$J,358.3,24294,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24294,1,1,0)
 ;;=1^J9201
 ;;^UTILITY(U,$J,358.3,24294,1,3,0)
 ;;=3^Gemcitabine 200mg
 ;;^UTILITY(U,$J,358.3,24295,0)
 ;;=J9202^^194^1659^15^^^^1
 ;;^UTILITY(U,$J,358.3,24295,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24295,1,1,0)
 ;;=1^J9202
 ;;^UTILITY(U,$J,358.3,24295,1,3,0)
 ;;=3^Goserelin, per 3.6mg
 ;;^UTILITY(U,$J,358.3,24296,0)
 ;;=J9206^^194^1659^3^^^^1
 ;;^UTILITY(U,$J,358.3,24296,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24296,1,1,0)
 ;;=1^J9206
 ;;^UTILITY(U,$J,358.3,24296,1,3,0)
 ;;=3^Camptosar 20mg
 ;;^UTILITY(U,$J,358.3,24297,0)
 ;;=J9208^^194^1659^16^^^^1
 ;;^UTILITY(U,$J,358.3,24297,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24297,1,1,0)
 ;;=1^J9208
 ;;^UTILITY(U,$J,358.3,24297,1,3,0)
 ;;=3^Ifosfamide (IFEX) 1gr
 ;;^UTILITY(U,$J,358.3,24298,0)
 ;;=J9209^^194^1659^22^^^^1
 ;;^UTILITY(U,$J,358.3,24298,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24298,1,1,0)
 ;;=1^J9209
 ;;^UTILITY(U,$J,358.3,24298,1,3,0)
 ;;=3^Mesna 200mgs
 ;;^UTILITY(U,$J,358.3,24299,0)
 ;;=J9213^^194^1659^18^^^^1
 ;;^UTILITY(U,$J,358.3,24299,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24299,1,1,0)
 ;;=1^J9213
 ;;^UTILITY(U,$J,358.3,24299,1,3,0)
 ;;=3^Interferon alfa, 3 mil u
 ;;^UTILITY(U,$J,358.3,24300,0)
 ;;=J9214^^194^1659^17^^^^1
 ;;^UTILITY(U,$J,358.3,24300,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24300,1,1,0)
 ;;=1^J9214
 ;;^UTILITY(U,$J,358.3,24300,1,3,0)
 ;;=3^Interferon alfa, 1 mil u
 ;;^UTILITY(U,$J,358.3,24301,0)
 ;;=J9217^^194^1659^20^^^^1
 ;;^UTILITY(U,$J,358.3,24301,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24301,1,1,0)
 ;;=1^J9217
 ;;^UTILITY(U,$J,358.3,24301,1,3,0)
 ;;=3^Lupron Depot 7.5mg
 ;;^UTILITY(U,$J,358.3,24302,0)
 ;;=J9218^^194^1659^21^^^^1
 ;;^UTILITY(U,$J,358.3,24302,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24302,1,1,0)
 ;;=1^J9218
 ;;^UTILITY(U,$J,358.3,24302,1,3,0)
 ;;=3^Lupron, per 1mg
 ;;^UTILITY(U,$J,358.3,24303,0)
 ;;=J9250^^194^1659^24^^^^1
 ;;^UTILITY(U,$J,358.3,24303,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24303,1,1,0)
 ;;=1^J9250
 ;;^UTILITY(U,$J,358.3,24303,1,3,0)
 ;;=3^Methotrexate Sodium 5mgs
 ;;^UTILITY(U,$J,358.3,24304,0)
 ;;=J9260^^194^1659^23^^^^1
 ;;^UTILITY(U,$J,358.3,24304,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24304,1,1,0)
 ;;=1^J9260
 ;;^UTILITY(U,$J,358.3,24304,1,3,0)
 ;;=3^Methotrexate Sodium 50mgs
 ;;^UTILITY(U,$J,358.3,24305,0)
 ;;=J9265^^194^1659^27^^^^1
 ;;^UTILITY(U,$J,358.3,24305,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24305,1,1,0)
 ;;=1^J9265
 ;;^UTILITY(U,$J,358.3,24305,1,3,0)
 ;;=3^Paclitaxel 30mgs
 ;;^UTILITY(U,$J,358.3,24306,0)
 ;;=J9293^^194^1659^25^^^^1
 ;;^UTILITY(U,$J,358.3,24306,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24306,1,1,0)
 ;;=1^J9293
 ;;^UTILITY(U,$J,358.3,24306,1,3,0)
 ;;=3^Mitoxantrone HCl(Novan)5mgs
 ;;^UTILITY(U,$J,358.3,24307,0)
 ;;=J9310^^194^1659^28^^^^1
 ;;^UTILITY(U,$J,358.3,24307,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24307,1,1,0)
 ;;=1^J9310
 ;;^UTILITY(U,$J,358.3,24307,1,3,0)
 ;;=3^Rituxan 100mg
 ;;^UTILITY(U,$J,358.3,24308,0)
 ;;=J9360^^194^1659^30^^^^1
 ;;^UTILITY(U,$J,358.3,24308,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24308,1,1,0)
 ;;=1^J9360
 ;;^UTILITY(U,$J,358.3,24308,1,3,0)
 ;;=3^Vinblastine Sulfate (Velban) 1mg
 ;;^UTILITY(U,$J,358.3,24309,0)
 ;;=J9370^^194^1659^31^^^^1
 ;;^UTILITY(U,$J,358.3,24309,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24309,1,1,0)
 ;;=1^J9370
 ;;^UTILITY(U,$J,358.3,24309,1,3,0)
 ;;=3^Vincristine Sulfate(VCR) 1mg
