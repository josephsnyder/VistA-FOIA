IBDEI0DS ; ; 20-FEB-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;FEB 20, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,18389,2)
 ;;=^64512
 ;;^UTILITY(U,$J,358.3,18390,0)
 ;;=296.36^^133^1164^28
 ;;^UTILITY(U,$J,358.3,18390,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18390,1,3,0)
 ;;=3^Maj Depr, Recurr, Full Rem
 ;;^UTILITY(U,$J,358.3,18390,1,4,0)
 ;;=4^296.36
 ;;^UTILITY(U,$J,358.3,18390,2)
 ;;=^303605
 ;;^UTILITY(U,$J,358.3,18391,0)
 ;;=296.30^^133^1164^29
 ;;^UTILITY(U,$J,358.3,18391,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18391,1,3,0)
 ;;=3^Maj Depr, Recurr, Unspec
 ;;^UTILITY(U,$J,358.3,18391,1,4,0)
 ;;=4^296.30
 ;;^UTILITY(U,$J,358.3,18391,2)
 ;;=^303614
 ;;^UTILITY(U,$J,358.3,18392,0)
 ;;=V71.09^^133^1164^30
 ;;^UTILITY(U,$J,358.3,18392,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18392,1,3,0)
 ;;=3^Mental Cond, Other Suspec
 ;;^UTILITY(U,$J,358.3,18392,1,4,0)
 ;;=4^V71.09
 ;;^UTILITY(U,$J,358.3,18392,2)
 ;;=^295604
 ;;^UTILITY(U,$J,358.3,18393,0)
 ;;=293.83^^133^1164^31
 ;;^UTILITY(U,$J,358.3,18393,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18393,1,3,0)
 ;;=3^Mood Disor d/t Gen Med Cond
 ;;^UTILITY(U,$J,358.3,18393,1,4,0)
 ;;=4^293.83
 ;;^UTILITY(U,$J,358.3,18393,2)
 ;;=^331838
 ;;^UTILITY(U,$J,358.3,18394,0)
 ;;=291.89^^133^1164^32
 ;;^UTILITY(U,$J,358.3,18394,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18394,1,3,0)
 ;;=3^Mood Disor, Alco-Ind
 ;;^UTILITY(U,$J,358.3,18394,1,4,0)
 ;;=4^291.89
 ;;^UTILITY(U,$J,358.3,18394,2)
 ;;=^303498
 ;;^UTILITY(U,$J,358.3,18395,0)
 ;;=300.3^^133^1164^33
 ;;^UTILITY(U,$J,358.3,18395,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18395,1,3,0)
 ;;=3^Obsessive-Compulsive Dis
 ;;^UTILITY(U,$J,358.3,18395,1,4,0)
 ;;=4^300.3
 ;;^UTILITY(U,$J,358.3,18395,2)
 ;;=^84904
 ;;^UTILITY(U,$J,358.3,18396,0)
 ;;=305.50^^133^1164^34
 ;;^UTILITY(U,$J,358.3,18396,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18396,1,3,0)
 ;;=3^Opioid Abuse, Unspec
 ;;^UTILITY(U,$J,358.3,18396,1,4,0)
 ;;=4^305.50
 ;;^UTILITY(U,$J,358.3,18396,2)
 ;;=^85868
 ;;^UTILITY(U,$J,358.3,18397,0)
 ;;=304.00^^133^1164^35
 ;;^UTILITY(U,$J,358.3,18397,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18397,1,3,0)
 ;;=3^Opioid Dependence, Unspec
 ;;^UTILITY(U,$J,358.3,18397,1,4,0)
 ;;=4^304.00
 ;;^UTILITY(U,$J,358.3,18397,2)
 ;;=^81364
 ;;^UTILITY(U,$J,358.3,18398,0)
 ;;=307.80^^133^1164^37
 ;;^UTILITY(U,$J,358.3,18398,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18398,1,3,0)
 ;;=3^Pain Disorder
 ;;^UTILITY(U,$J,358.3,18398,1,4,0)
 ;;=4^307.80
 ;;^UTILITY(U,$J,358.3,18398,2)
 ;;=^265317
 ;;^UTILITY(U,$J,358.3,18399,0)
 ;;=300.01^^133^1164^38
 ;;^UTILITY(U,$J,358.3,18399,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18399,1,3,0)
 ;;=3^Panic Disor w/o Agora
 ;;^UTILITY(U,$J,358.3,18399,1,4,0)
 ;;=4^300.01
 ;;^UTILITY(U,$J,358.3,18399,2)
 ;;=^331906
 ;;^UTILITY(U,$J,358.3,18400,0)
 ;;=300.20^^133^1164^39
 ;;^UTILITY(U,$J,358.3,18400,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18400,1,3,0)
 ;;=3^Panic Disorder w/ Agora
 ;;^UTILITY(U,$J,358.3,18400,1,4,0)
 ;;=4^300.20
 ;;^UTILITY(U,$J,358.3,18400,2)
 ;;=^93428
 ;;^UTILITY(U,$J,358.3,18401,0)
 ;;=310.1^^133^1164^40
 ;;^UTILITY(U,$J,358.3,18401,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18401,1,3,0)
 ;;=3^Personality Chg d/t Med Cond
 ;;^UTILITY(U,$J,358.3,18401,1,4,0)
 ;;=4^310.1
 ;;^UTILITY(U,$J,358.3,18401,2)
 ;;=^303672
 ;;^UTILITY(U,$J,358.3,18402,0)
 ;;=301.7^^133^1164^41
 ;;^UTILITY(U,$J,358.3,18402,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18402,1,3,0)
 ;;=3^Personality Disor, Antisocial
 ;;^UTILITY(U,$J,358.3,18402,1,4,0)
 ;;=4^301.7
 ;;^UTILITY(U,$J,358.3,18402,2)
 ;;=^9066
 ;;^UTILITY(U,$J,358.3,18403,0)
 ;;=301.83^^133^1164^43
 ;;^UTILITY(U,$J,358.3,18403,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18403,1,3,0)
 ;;=3^Personality Disor, Borderline
 ;;^UTILITY(U,$J,358.3,18403,1,4,0)
 ;;=4^301.83
 ;;^UTILITY(U,$J,358.3,18403,2)
 ;;=^331921
 ;;^UTILITY(U,$J,358.3,18404,0)
 ;;=301.6^^133^1164^44
 ;;^UTILITY(U,$J,358.3,18404,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18404,1,3,0)
 ;;=3^Personality Disor, Dependence
 ;;^UTILITY(U,$J,358.3,18404,1,4,0)
 ;;=4^301.6
 ;;^UTILITY(U,$J,358.3,18404,2)
 ;;=^32860
 ;;^UTILITY(U,$J,358.3,18405,0)
 ;;=301.4^^133^1164^45
 ;;^UTILITY(U,$J,358.3,18405,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18405,1,3,0)
 ;;=3^Personality Disor, Obs-Comp
 ;;^UTILITY(U,$J,358.3,18405,1,4,0)
 ;;=4^301.4
 ;;^UTILITY(U,$J,358.3,18405,2)
 ;;=^331918
 ;;^UTILITY(U,$J,358.3,18406,0)
 ;;=301.0^^133^1164^46
 ;;^UTILITY(U,$J,358.3,18406,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18406,1,3,0)
 ;;=3^Personality Disor, Paranoid
 ;;^UTILITY(U,$J,358.3,18406,1,4,0)
 ;;=4^301.0
 ;;^UTILITY(U,$J,358.3,18406,2)
 ;;=^89982
 ;;^UTILITY(U,$J,358.3,18407,0)
 ;;=301.22^^133^1164^47
 ;;^UTILITY(U,$J,358.3,18407,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18407,1,3,0)
 ;;=3^Personality Disor, Schizotypal
 ;;^UTILITY(U,$J,358.3,18407,1,4,0)
 ;;=4^301.22
 ;;^UTILITY(U,$J,358.3,18407,2)
 ;;=^331917
 ;;^UTILITY(U,$J,358.3,18408,0)
 ;;=301.82^^133^1164^42
 ;;^UTILITY(U,$J,358.3,18408,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18408,1,3,0)
 ;;=3^Personality Disor, Avoidant
 ;;^UTILITY(U,$J,358.3,18408,1,4,0)
 ;;=4^301.82
 ;;^UTILITY(U,$J,358.3,18408,2)
 ;;=^331920
 ;;^UTILITY(U,$J,358.3,18409,0)
 ;;=295.20^^133^1164^48
 ;;^UTILITY(U,$J,358.3,18409,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18409,1,3,0)
 ;;=3^Schizo, Canatonic, NOS
 ;;^UTILITY(U,$J,358.3,18409,1,4,0)
 ;;=4^295.20
 ;;^UTILITY(U,$J,358.3,18409,2)
 ;;=^108310
 ;;^UTILITY(U,$J,358.3,18410,0)
 ;;=295.10^^133^1164^49
 ;;^UTILITY(U,$J,358.3,18410,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18410,1,3,0)
 ;;=3^Schizo, Disorganized NOS
 ;;^UTILITY(U,$J,358.3,18410,1,4,0)
 ;;=4^295.10
 ;;^UTILITY(U,$J,358.3,18410,2)
 ;;=^108319
 ;;^UTILITY(U,$J,358.3,18411,0)
 ;;=295.30^^133^1164^50
 ;;^UTILITY(U,$J,358.3,18411,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18411,1,3,0)
 ;;=3^Schizo, Paranoid
 ;;^UTILITY(U,$J,358.3,18411,1,4,0)
 ;;=4^295.30
 ;;^UTILITY(U,$J,358.3,18411,2)
 ;;=^108330
 ;;^UTILITY(U,$J,358.3,18412,0)
 ;;=295.62^^133^1164^51
 ;;^UTILITY(U,$J,358.3,18412,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18412,1,3,0)
 ;;=3^Schizo, Residual, Chr
 ;;^UTILITY(U,$J,358.3,18412,1,4,0)
 ;;=4^295.62
 ;;^UTILITY(U,$J,358.3,18412,2)
 ;;=^331853
 ;;^UTILITY(U,$J,358.3,18413,0)
 ;;=295.92^^133^1164^52
 ;;^UTILITY(U,$J,358.3,18413,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18413,1,3,0)
 ;;=3^Schizo, Undiff, Chr
 ;;^UTILITY(U,$J,358.3,18413,1,4,0)
 ;;=4^295.92
 ;;^UTILITY(U,$J,358.3,18413,2)
 ;;=^268093
 ;;^UTILITY(U,$J,358.3,18414,0)
 ;;=295.70^^133^1164^53
 ;;^UTILITY(U,$J,358.3,18414,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18414,1,3,0)
 ;;=3^Schizoaffective Dis
 ;;^UTILITY(U,$J,358.3,18414,1,4,0)
 ;;=4^295.70
 ;;^UTILITY(U,$J,358.3,18414,2)
 ;;=^331857
 ;;^UTILITY(U,$J,358.3,18415,0)
 ;;=295.90^^133^1164^54
 ;;^UTILITY(U,$J,358.3,18415,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18415,1,3,0)
 ;;=3^Schizophrenia NOS
 ;;^UTILITY(U,$J,358.3,18415,1,4,0)
 ;;=4^295.90
 ;;^UTILITY(U,$J,358.3,18415,2)
 ;;=^108287
 ;;^UTILITY(U,$J,358.3,18416,0)
 ;;=304.10^^133^1164^56
 ;;^UTILITY(U,$J,358.3,18416,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18416,1,3,0)
 ;;=3^Sedative, Hypnotic, Anxio Depend
 ;;^UTILITY(U,$J,358.3,18416,1,4,0)
 ;;=4^304.10
 ;;^UTILITY(U,$J,358.3,18416,2)
 ;;=^331931
 ;;^UTILITY(U,$J,358.3,18417,0)
 ;;=305.40^^133^1164^55
 ;;^UTILITY(U,$J,358.3,18417,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18417,1,3,0)
 ;;=3^Sedative, Hypnotic, Anxio Abuse
 ;;^UTILITY(U,$J,358.3,18417,1,4,0)
 ;;=4^305.40
 ;;^UTILITY(U,$J,358.3,18417,2)
 ;;=^331935
 ;;^UTILITY(U,$J,358.3,18418,0)
 ;;=780.99^^133^1164^36
 ;;^UTILITY(U,$J,358.3,18418,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18418,1,3,0)
 ;;=3^Other General Symptoms
 ;;^UTILITY(U,$J,358.3,18418,1,4,0)
 ;;=4^780.99
 ;;^UTILITY(U,$J,358.3,18418,2)
 ;;=^328568
 ;;^UTILITY(U,$J,358.3,18419,0)
 ;;=780.79^^133^1164^21
 ;;^UTILITY(U,$J,358.3,18419,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18419,1,3,0)
 ;;=3^General Weakness
 ;;^UTILITY(U,$J,358.3,18419,1,4,0)
 ;;=4^780.79
 ;;^UTILITY(U,$J,358.3,18419,2)
 ;;=^73344
 ;;^UTILITY(U,$J,358.3,18420,0)
 ;;=369.4^^133^1164^27
 ;;^UTILITY(U,$J,358.3,18420,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18420,1,3,0)
 ;;=3^Legal Blindness (USA Definition)
 ;;^UTILITY(U,$J,358.3,18420,1,4,0)
 ;;=4^369.4
 ;;^UTILITY(U,$J,358.3,18420,2)
 ;;=^268887
 ;;^UTILITY(U,$J,358.3,18421,0)
 ;;=V57.89^^134^1165^1
 ;;^UTILITY(U,$J,358.3,18421,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,18421,1,3,0)
 ;;=3^Rehabilitation Procedure NEC
 ;;^UTILITY(U,$J,358.3,18421,1,4,0)
 ;;=4^V57.89
 ;;^UTILITY(U,$J,358.3,18421,2)
 ;;=^177367
 ;;^UTILITY(U,$J,358.3,18422,0)
 ;;=97110^^135^1166^14^^^^1
 ;;^UTILITY(U,$J,358.3,18422,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18422,1,2,0)
 ;;=2^Therapeutic Exerc,ea 15min
 ;;^UTILITY(U,$J,358.3,18422,1,3,0)
 ;;=3^97110
 ;;^UTILITY(U,$J,358.3,18423,0)
 ;;=97112^^135^1166^9^^^^1
 ;;^UTILITY(U,$J,358.3,18423,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18423,1,2,0)
 ;;=2^Neuromuscular Re-Educ,ea 15min
 ;;^UTILITY(U,$J,358.3,18423,1,3,0)
 ;;=3^97112
 ;;^UTILITY(U,$J,358.3,18424,0)
 ;;=97113^^135^1166^2^^^^1
 ;;^UTILITY(U,$J,358.3,18424,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18424,1,2,0)
 ;;=2^Aquatic Exercise,ea 15min
 ;;^UTILITY(U,$J,358.3,18424,1,3,0)
 ;;=3^97113
 ;;^UTILITY(U,$J,358.3,18425,0)
 ;;=97116^^135^1166^6^^^^1
 ;;^UTILITY(U,$J,358.3,18425,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18425,1,2,0)
 ;;=2^Gait Training,ea 15 Min
 ;;^UTILITY(U,$J,358.3,18425,1,3,0)
 ;;=3^97116
 ;;^UTILITY(U,$J,358.3,18426,0)
 ;;=97530^^135^1166^13^^^^1
 ;;^UTILITY(U,$J,358.3,18426,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18426,1,2,0)
 ;;=2^Therapeutic Activ,ea 15min
 ;;^UTILITY(U,$J,358.3,18426,1,3,0)
 ;;=3^97530
 ;;^UTILITY(U,$J,358.3,18427,0)
 ;;=97537^^135^1166^4^^^^1
