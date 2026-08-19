*     *PPAB291JQGM85  PB12BBA7916T  2020/09/1016:00:18XGUA837 099
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.  JQGM85.                                             JQGM85
000300*AUTHOR.         MAJ HISTORIQUE DES COMMANDES.                    JQGM85
000400*DATE-COMPILED.   10/09/20.                                       JQGM85
000500 ENVIRONMENT DIVISION.                                            JQGM85
000600 CONFIGURATION SECTION.                                           JQGM85
000700 SOURCE-COMPUTER. IBM-370.                                        JQGM85
000800 OBJECT-COMPUTER. IBM-370.                                        JQGM85
000900 INPUT-OUTPUT SECTION.                                            JQGM85
001000 FILE-CONTROL.                                                    JQGM85
001100      SELECT     KA-FICHIER    ASSIGN    UT-S-VA.                 JQGM85
001200      SELECT          QH-FICHIER  ASSIGN  TO  QH                  JQGM85
001300      ORGANIZATION INDEXED                                        JQGM85
001400      ACCESS IS RANDOM                                            JQGM85
001500      FILE STATUS IS                   1-QH01-STATUS              JQGM85
001600                                       VSAM-STATUS                JQGM85
001700      RECORD KEY  IS                     QH00-KQH01.              JQGM85
001800 DATA DIVISION.                                                   JQGM85
001900 FILE SECTION.                                                    JQGM85
002000 FD                 KA-FICHIER                                    JQGM85
002100      BLOCK              00000 RECORDS                            JQGM85
002200      RECORDING  F.                                               JQGM85
002300 01                 KA00.                                         JQGM85
002400      10            KA00-NCNCP  PICTURE  X(11).                   JQGM85
002500      10            KA00-DCD1A  PICTURE  9(8).                    JQGM85
002600      10            KA00-CPD1A  PICTURE  X(2).                    JQGM85
002700      10            KA00-CCD1D  PICTURE  X.                       JQGM85
002800      10            KA00-ZF058  PICTURE  X(58).                   JQGM85
002900 FD                 QH-FICHIER.                                   JQGM85
003000 01                 QH00.                                         JQGM85
003100      10            QH00-KQH01.                                   JQGM85
003200      11            QH00-NCNCP.                                   JQGM85
003300      12            QH00-NCC1A  PICTURE  9(7).                    JQGM85
003400      12            QH00-ZNCLC  PICTURE  X.                       JQGM85
003500      12            QH00-ZG003.                                   JQGM85
003600      13            QH00-Z9001  PICTURE  9.                       JQGM85
003700      13            QH00-CUGCP  PICTURE  XX.                      JQGM85
003800      11            QH00-DCD1A  PICTURE  9(8).                    JQGM85
003900      11            QH00-CPD1A  PICTURE  X(2).                    JQGM85
004000      10            QH00-CCD1A  PICTURE  X.                       JQGM85
004100      10            QH00-QEN1A  PICTURE  999.                     JQGM85
004200      10            QH00-CCD1D  PICTURE  X.                       JQGM85
004300      10            QH00-QCD1B  PICTURE  9(4).                    JQGM85
004400      10            QH00-CCA1A  PICTURE  X.                       JQGM85
004500      10            QH00-CEN1A  PICTURE  X.                       JQGM85
004600      10            QH00-NEN1A  PICTURE  9(8).                    JQGM85
004700      10            QH00-ZEN1A  PICTURE  9(1).                    JQGM85
004800      10            QH00-CCP1A  PICTURE  9(2).                    JQGM85
004900      10            QH00-NCHSU  PICTURE  X(20).                   JQGM85
005000      10            QH00-ZF017  PICTURE  X(17).                   JQGM85
005100 WORKING-STORAGE SECTION.                                         JQGM85
005200 77          VERSION   PIC       X(24)                            7AB010
005300                       VALUE     'V01*28*05*20*JQGM85  ***'.      7AB020
005400 01               W-WH00-CFR    PIC X(1)                          7WH010
005500                  VALUE ZERO.                                     7WH011
005600 01                 WH00.                                         JQGM85
005700    05              WH00-SUITE.                                   JQGM85
005800      15       FILLER         PICTURE  X(00080).                  JQGM85
005900 01                 WH01  REDEFINES      WH00.                    JQGM85
006000      10            WH01-KQH01.                                   JQGM85
006100      11            WH01-NCNCP.                                   JQGM85
006200      12            WH01-NCC1A  PICTURE  9(7).                    JQGM85
006300      12            WH01-ZNCLC  PICTURE  X.                       JQGM85
006400      12            WH01-ZG003.                                   JQGM85
006500      13            WH01-Z9001  PICTURE  9.                       JQGM85
006600      13            WH01-CUGCP  PICTURE  XX.                      JQGM85
006700      11            WH01-DCD1A  PICTURE  9(8).                    JQGM85
006800      11            WH01-CPD1A  PICTURE  X(2).                    JQGM85
006900      10            WH01-CCD1A  PICTURE  X.                       JQGM85
007000      10            WH01-QEN1A  PICTURE  999.                     JQGM85
007100      10            WH01-CCD1D  PICTURE  X.                       JQGM85
007200      10            WH01-QCD1B  PICTURE  9(4).                    JQGM85
007300      10            WH01-CCA1A  PICTURE  X.                       JQGM85
007400      10            WH01-CEN1A  PICTURE  X.                       JQGM85
007500      10            WH01-NEN1A  PICTURE  9(8).                    JQGM85
007600      10            WH01-ZEN1A  PICTURE  9(1).                    JQGM85
007700      10            WH01-CCP1A  PICTURE  9(2).                    JQGM85
007800      10            WH01-NCHSU  PICTURE  X(20).                   JQGM85
007900      10            WH01-ZF017  PICTURE  X(17).                   JQGM85
008000 01                 WW00.                                         JQGM85
008100      10            WW00-ZCSRT1 PICTURE  9(2)                     JQGM85
008200                    VALUE                ZERO.                    JQGM85
008300      10            WW00-ZCACTE PICTURE  X                        JQGM85
008400                    VALUE                SPACE.                   JQGM85
008500      10            WW00-NCNCP  PICTURE  X(11)                    JQGM85
008600                    VALUE                SPACE.                   JQGM85
008700      10            WW00-ZBOOL  PICTURE  X                        JQGM85
008800                    VALUE                SPACE.                   JQGM85
008900      10            WW00-QCD1B  PICTURE  9(4)                     JQGM85
009000                    VALUE                ZERO.                    JQGM85
009100 01               N-1        PIC 9(01) VALUE 1.                   7WW220
009200 01   DEBUT-WSS.                                                  JQGM85
009300      05   FILLER PICTURE X(7) VALUE 'WORKING'.                   JQGM85
009400      05   IK     PICTURE X.                                      JQGM85
009500 01  CONSTANTES-PAC.                                              JQGM85
009600     05  FILLER  PICTURE X(87)   VALUE                            JQGM85
009700               '7916TBBA10/09/20PAB291XGUA837 16:00:18JQGM85  PB12JQGM85
009800-    '10/09/20203.5 V0426/04/200716/01/2007'.                     JQGM85
009900 01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     JQGM85
010000     05  NUGNA   PICTURE X(5).                                    JQGM85
010100     05  APPLI   PICTURE X(3).                                    JQGM85
010200     05  DATGN   PICTURE X(8).                                    JQGM85
010300     05  PROGR   PICTURE X(6).                                    JQGM85
010400     05  CODUTI  PICTURE X(8).                                    JQGM85
010500     05  TIMGN   PICTURE X(8).                                    JQGM85
010600     05  PROGE   PICTURE X(8).                                    JQGM85
010700     05  COBASE  PICTURE X(4).                                    JQGM85
010800     05  DATGNC  PICTURE X(10).                                   JQGM85
010900     05  RELEAS  PICTURE X(7).                                    JQGM85
011000     05  DATGE   PICTURE X(10).                                   JQGM85
011100     05  DATSQ   PICTURE X(10).                                   JQGM85
011200 01  DATCE.                                                       JQGM85
011300   05  CENTUR   PICTURE XX   VALUE '20'.                          JQGM85
011400   05  DATOR.                                                     JQGM85
011500     10  DATOA  PICTURE XX.                                       JQGM85
011600     10  DATOM  PICTURE XX.                                       JQGM85
011700     10  DATOJ  PICTURE XX.                                       JQGM85
011800 01  DAT6.                                                        JQGM85
011900      10 DAT61.                                                   JQGM85
012000      15 DAT619  PICTURE 99.                                      JQGM85
012100      10 DAT62.                                                   JQGM85
012200      15 DAT629  PICTURE 99.                                      JQGM85
012300      10 DAT63   PICTURE XX.                                      JQGM85
012400 01  DAT8.                                                        JQGM85
012500      10 DAT81   PICTURE XX.                                      JQGM85
012600      10 DAT8S1  PICTURE X.                                       JQGM85
012700      10 DAT82   PICTURE XX.                                      JQGM85
012800      10 DAT8S2  PICTURE X.                                       JQGM85
012900      10 DAT83   PICTURE XX.                                      JQGM85
013000 01  TIMCO.                                                       JQGM85
013100   05  TIMCOH   PICTURE XX.                                       JQGM85
013200   05  TIMCOM   PICTURE XX.                                       JQGM85
013300   05  TIMCOS   PICTURE XX.                                       JQGM85
013400   05  TIMOC.                                                     JQGM85
013500    10 TIMCOC   PICTURE XX.                                       JQGM85
013600 01  TIMDAY.                                                      JQGM85
013700   05  TIMHOU   PICTURE XX.                                       JQGM85
013800   05  TIMS1    PICTURE X  VALUE ':'.                             JQGM85
013900   05  TIMMIN   PICTURE XX.                                       JQGM85
014000   05  TIMS2    PICTURE X  VALUE ':'.                             JQGM85
014100   05  TIMSEC   PICTURE XX.                                       JQGM85
014200 01  DATSEP     PICTURE X VALUE '/'.                              JQGM85
014300 01  DATSEW     PICTURE X.                                        JQGM85
014400 01  DAT-TRANS.                                                   JQGM85
014500   05  DAT-CTYD   PICTURE XX VALUE '61'.                          JQGM85
014600 01   VARIABLES-CONDITIONNELLES.                                  JQGM85
014700      05               FT.                                        JQGM85
014800        10          KA-FT      PICTURE X VALUE '0'.               JQGM85
014900 01   INDICES  COMPUTATIONAL  SYNC.                               JQGM85
015000      05          TALLI   PICTURE S9(4) VALUE  ZERO.              JQGM85
015100 01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   JQGM85
015200      05       5-KA00-CPTENR PICTURE S9(9) VALUE ZERO.            JQGM85
015300      05       5-QH00-CPTENR PICTURE S9(9) VALUE ZERO.            JQGM85
015400      05       5-WH00-CPTENR PICTURE S9(9) VALUE ZERO.            JQGM85
015500 01  ZONES-STATUS.                                                JQGM85
015600      05   VSAM-STATUS.                                           JQGM85
015700        10 VSAM-RCODE   PICTURE 9(2) COMP VALUE ZERO.             JQGM85
015800        10 VSAM-FCODE   PICTURE 9(1) COMP VALUE ZERO.             JQGM85
015900        10 VSAM-FBCODE  PICTURE 9(3) COMP VALUE ZERO.             JQGM85
016000      05          1-QH01-STATUS PICTURE XX VALUE ZERO.            JQGM85
016100 01   ZONES-UTILISATEUR PICTURE X.                                JQGM85
016200 01                 SW44.                                         JQGM85
016300      10            SW44-ZCSRT6 PICTURE  9(4)                     JQGM85
016400                    VALUE                ZERO                     JQGM85
016500                    BINARY.                                       JQGM85
016600      10            SW44-ZCSCA3 PICTURE  99                       JQGM85
016700                    VALUE                ZERO                     JQGM85
016800                    BINARY.                                       JQGM85
016900 PROCEDURE DIVISION.                                              JQGM85
017000 DECLARATIVES.                                                    JQGM85
017100 SECQH SECTION.                                                   JQGM85
017200     USE AFTER ERROR PROCEDURE ON   QH-FICHIER.                   JQGM85
017300 F0AQH. DISPLAY  'STATUS : QH     = '  1-QH01-STATUS.             JQGM85
017400 F0AQH-A. GO TO  F0A90.                                           JQGM85
017500 F0AQH-FN. EXIT.                                                  JQGM85
017600 F0A90.                                                           P000
017700     PERFORM     F99BA THRU F99BA-FN.                             P820
017800 F0A90-FN. EXIT.                                                  P820
017900 END DECLARATIVES.                                                JQGM85
018000 SEC00 SECTION.                                                   JQGM85
018100 F0B.                                                             P000
018200     ACCEPT DATOR FROM DATE                                       P120
018300     IF DATOA < DAT-CTYD MOVE '20' TO CENTUR END-IF               P120
018400     ACCEPT TIMCO FROM TIME.                                      P220
018500 F0B-FN.   EXIT.                                                  P220
018600 F0DBA.                                                           P000
018700     ACCEPT DATOR FROM DATE                                       P100
018800     IF DATOA < DAT-CTYD MOVE '20' TO CENTUR END-IF               P100
018900     DISPLAY     '--> LE ' DATOJ '/' DATOM '/'                    P200
019000     DATOA                                                        P210
019100     DISPLAY     '--> DEBUT DU PROGRAMME ' PROGR                  P300
019200     ' GENERE LE ' DATGN                                          P310
019300     ' PAR ' CODUTI                                               P320
019400     DISPLAY     '--> A PARTIR DE LA '                            P330
019500     'BIBLIOTHEQUE ' APPLI                                        P335
019600     ' ET SESSION ' NUGNA                                         P340
019700     DISPLAY     '--> VERSION ' VERSION.                          P350
019800 F0DBA-FN. EXIT.                                                  P350
019900 F01.      EXIT.                                                  JQGM85
020000 F01KA.    OPEN INPUT                    KA-FICHIER.              JQGM85
020100 F01KA-FN. EXIT.                                                  JQGM85
020200 F01QH.    OPEN I-O                      QH-FICHIER.              JQGM85
020300        IF        1-QH01-STATUS  NOT  = ZERO                      JQGM85
020400             AND  1-QH01-STATUS  NOT  = '97'                      JQGM85
020500        PERFORM    F0AQH                                          JQGM85
020600        PERFORM    F0A90         THRU F0A90-FN.                   JQGM85
020700 F01QH-FN. EXIT.                                                  JQGM85
020800 F01-FN.   EXIT.                                                  JQGM85
020900 F03.           EXIT.                                             P000
021000 F03-FN.   EXIT.                                                  P000
021100*          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            JQGM85
021200 F05.                                                             JQGM85
021300 F05KA.    IF       KA-FT       = '0'                             JQGM85
021400           NEXT SENTENCE ELSE GO TO     F05KA-FN.                 JQGM85
021500 F05KA-10. READ     KA-FICHIER       AT END                       JQGM85
021600           MOVE  1 TO                    KA-FT                    JQGM85
021700           GO TO   F05KA-FN.                                      JQGM85
021800           ADD 1 TO 5-KA00-CPTENR.                                JQGM85
021900 F05KA-FN. EXIT.                                                  JQGM85
022000 F05-FN.   EXIT.                                                  JQGM85
022100 F20.      IF FT =            ALL '1'                             JQGM85
022200           NEXT SENTENCE ELSE GO TO     F20-FN.                   JQGM85
022300 F20KA.                                                           P000
022400     CLOSE          KA-FICHIER                                    P100
022500     DISPLAY                      'FICHIER KA: NOMBRE ENREGISTREMEP500
022600-                'NTS TRAITES = ' 5-KA00-CPTENR.                  P510
022700 F20KA-FN. EXIT.                                                  P510
022800 F20QH.                                                           P000
022900     CLOSE          QH-FICHIER                                    P100
023000     DISPLAY                      'FICHIER QH: NOMBRE ENREGISTREMEP500
023100-                'NTS TRAITES = ' 5-QH00-CPTENR.                  P510
023200 F20QH-FN. EXIT.                                                  P510
023300 F2098.                                                           P000
023400     DISPLAY     '--> FIN   DU PROGRAMME ' PROGR.                 P300
023500 F2098-FN. EXIT.                                                  P300
023600 F2099.     GOBACK.                                               JQGM85
023700 F2099-FN. EXIT.                                                  JQGM85
023800 F20-FN.   EXIT.                                                  JQGM85
023900 F60.           EXIT.                                             P000
024000 F60BB.                                                           P000
024100     MOVE        LOW-VALUE TO WH01-KQH01                          P100
024200     MOVE        KA00-NCNCP TO WH01-NCNCP                         P120
024300     MOVE        KA00-DCD1A TO WH01-DCD1A                         P140
024400     MOVE        KA00-CPD1A TO WH01-CPD1A                         P160
024500     PERFORM     F95WH THRU F95WH-FN.                             P180
024600 F60CB.    IF    W-WH00-CFR = N-1                                 P000
024700           NEXT SENTENCE ELSE GO TO     F60CB-FN.                 P000
024800     PERFORM     F91BB THRU F91BB-FN.                             P100
024900 F60CB-FN. EXIT.                                                  P100
025000 F60BB-FN. EXIT.                                                  P100
025100 F60-FN.   EXIT.                                                  P100
025200 F9099-ITER-FN.  GO TO F05.                                       JQGM85
025300 F91.           EXIT.                                             P000
025400 F91BB.                                                           P000
025500     MOVE        KA00-CCD1D TO WH01-CCD1D                         P100
025600     MOVE        ZEROES TO WH01-QCD1B                             P120
025700     PERFORM     F94WH THRU F94WH-FN.                             P140
025800 F91BB-FN. EXIT.                                                  P140
025900 F91-FN.   EXIT.                                                  P140
026000 F94.           EXIT.                                             P000
026100 F94QH.                                                           P000
026200     MOVE        WH01 TO QH00                                     P100
026300     MOVE 0 TO IK                                                 P120
026400     WRITE       QH00                                             P120
026500     INVALID KEY MOVE 1 TO IK.                                    P120
026600           IF    IK = ZERO                                        P200
026700     ADD         1 TO 5-WH00-CPTENR.                              P220
026800           IF    IK NOT = ZERO                                    P300
026900     PERFORM     F0AQH THRU F0AQH-FN                              P320
027000     PERFORM     F0A90 THRU F0A90-FN.                             P340
027100 F94QH-FN. EXIT.                                                  P340
027200 F94WH.                                                           P000
027300     MOVE        WH01 TO QH00                                     P100
027400     MOVE 0 TO IK                                                 P120
027500     REWRITE     QH00                                             P120
027600     INVALID KEY MOVE 1 TO IK.                                    P120
027700           IF    IK NOT = ZERO                                    P300
027800     PERFORM     F0AQH THRU F0AQH-FN                              P320
027900     PERFORM     F0A90 THRU F0A90-FN.                             P340
028000 F94WH-FN. EXIT.                                                  P340
028100 F94-FN.   EXIT.                                                  P340
028200 F95.           EXIT.                                             P000
028300 F95WH.                                                           P000
028400     MOVE        ZERO TO W-WH00-CFR                               P100
028500     MOVE        WH01 TO QH00                                     P120
028600     MOVE 0 TO IK                                                 P140
028700     READ           QH-FICHIER                                    P140
028800     INVALID KEY MOVE 1 TO IK.                                    P140
028900           IF    IK = ZERO                                        P200
029000     MOVE        QH00 TO WH01                                     P220
029100     MOVE        '1' TO W-WH00-CFR                                P240
029200     ADD         1 TO 5-WH00-CPTENR                               P260
029300     ADD         1 TO 5-QH00-CPTENR.                              P280
029400 F95WH-FN. EXIT.                                                  P280
029500 F95-FN.   EXIT.                                                  P280
029600 F99BA.                                                           P000
029700     DISPLAY     'ARRET ANORMAL DU PROGRAMME '                    P100
029800     PROGR.                                                       P120
029900 F99BB.                                                           P000
030000     MOVE        WW00-ZCSRT1 TO SW44-ZCSRT6                       P800
030100     CALL        'ILBOABN0' USING SW44-ZCSRT6.                    P820
030200 F99BB-FN. EXIT.                                                  P820
030300 F99BA-FN. EXIT.                                                  P820