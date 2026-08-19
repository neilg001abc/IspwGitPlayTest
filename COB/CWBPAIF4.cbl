       IDENTIFICATION DIVISION.
       PROGRAM-ID. CWBPAIF4.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT ENTREE ASSIGN TO ENTREE.
           SELECT CLES ASSIGN TO CLES.

       DATA DIVISION.
       FILE SECTION.
       FD  ENTREE.
       01  INPUT-RECORD.
           05  FIELD-1       PIC X(10).
           05  FIELD-2       PIC 9(5).
           05  FIELD-3       PIC X(20).

       FD  CLES.
       01  CLES-RECORD.
           05  CLES-VALEUR   PIC X(10).


       WORKING-STORAGE SECTION.
       01  WS-EOF            PIC X VALUE 'N'.
           88  END-OF-FILE   VALUE 'Y'.

       01  WS-TABLE.
           05  WS-TABLE-ENTRY OCCURS 3000 TIMES.
               10  WS-FIELD-1 PIC X(10)  VALUE SPACES.
               10  WS-FIELD-2 PIC 9(5)    VALUE ZEROES.
               10  WS-FIELD-3 PIC X(20) VALUE SPACES.

       01  WS-CHAISE.
           05  WS-CHAISE-ENTRY OCCURS 3000 TIMES.
               10  WS-CHAISE-1 PIC X(10)  VALUE SPACES.
               10  WS-CHAISE-2 PIC 9(5)    VALUE ZEROES.
               10  WS-CHAISE-3 PIC X(20) VALUE SPACES.

mauv   01  WS-INDEX          PIC 9(4) VALUE 1.
mauv   01  WS-INDEX-MAX      PIC 9(4) VALUE 0.
bon   *01  WS-INDEX          PIC 9(4) comp VALUE 1.
bon   *01  WS-INDEX-MAX      PIC 9(4) comp VALUE 0.

       01  WS-SEARCH-KEY     PIC X(10).
       01  WS-FIELD-3-DECOUPE.
           10 WS-FIELD-3-AGE PIC 9(3).
           10 WS-FIELD-3-TAUX PIC 9(2).
           10 WS-FIELD-3-MONTANT PIC 9(10).
           10 WS-FIELD-3-TAILLE PIC 9(5).
       01  WS-CALCUL-1       PIC 9(9) COMP.
       01  WS-CALCUL-2       PIC 9(9) COMP.
       01  WS-CALCUL-3       PIC 9(9) COMP.
       01  WS-SCORE          PIC 9(10).
       01  WS-SCORE-TEXT     PIC X(15).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM OPEN-FILES.
           INITIALIZE WS-TABLE.
           PERFORM UNTIL END-OF-FILE
               PERFORM READ-INPUT
           END-PERFORM.
           move WS-INDEX to WS-INDEX-MAX.
           CLOSE ENTREE.

           MOVE 'N' to WS-EOF.
           PERFORM UNTIL END-OF-FILE
               PERFORM READ-CLES
           END-PERFORM.
           PERFORM CLOSE-FILES.
           STOP RUN.

       OPEN-FILES.
           OPEN INPUT ENTREE CLES.

       READ-INPUT.
           READ ENTREE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM MOVE-DATA
           END-READ.

       READ-CLES.
           READ CLES
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM SEARCH-TABLE
           END-READ.

       MOVE-DATA.
           MOVE INPUT-RECORD TO WS-TABLE-ENTRY(WS-INDEX)
           ADD 1 TO WS-INDEX.


       SEARCH-TABLE.
           MOVE CLES-VALEUR TO WS-SEARCH-KEY.
mauv       OPEN INPUT ENTREE.

mauv       INITIALIZE WS-CHAISE.
bon   *    PERFORM VARYING WS-INDEX FROM 1 BY 1
bon   *              UNTIL WS-INDEX = 3000
bon   *       Move ' ' to WS-CHAISE-1 ( WS-INDEX )
bon   *       Move 0   to WS-CHAISE-2 ( WS-INDEX )
bon   *       Move ' ' to WS-CHAISE-3 ( WS-INDEX )
bon   *    end-perform.

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                     UNTIL WS-INDEX > WS-INDEX-MAX



            MOVE WS-FIELD-3 (WS-INDEX) to WS-FIELD-3-DECOUPE
            if WS-FIELD-3-AGE = ZERO or WS-FIELD-3-TAUX = ZERO
              MOVE 'Trés mauvais' to WS-SCORE-TEXT
            else
mauv           COMPUTE WS-SCORE =
mauv             (WS-FIELD-3-MONTANT / WS-FIELD-3-AGE )
mauv           * (WS-FIELD-3-TAILLE / WS-FIELD-3-TAUX )

bon   *        DIVIDE WS-FIELD-3-MONTANT by WS-FIELD-3-AGE
bon   *           GIVING WS-CALCUL-1
bon   *        DIVIDE WS-FIELD-3-TAILLE by WS-FIELD-3-TAUX
bon   *           GIVING WS-CALCUL-2
bon   *        MULTIPLY WS-CALCUL-1 BY WS-CALCUL-2
bon   *           GIVING WS-CALCUL-3
bon   *        MOVE WS-CALCUL-3 TO  WS-SCORE

             MOVE WS-SCORE to WS-SCORE-TEXT
            END-IF
            IF WS-SEARCH-KEY = WS-FIELD-1(WS-INDEX)
                   DISPLAY 'Trouvé : ' WS-FIELD-1 (WS-INDEX)
                           'Score  : ' WS-SCORE-TEXT
            END-IF

           END-PERFORM.
mauv           CLOSE ENTREE.

       CLOSE-FILES.
           CLOSE CLES.
