       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAUVAIS1.

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

       01  WS-INDEX          PIC 9(4) VALUE 1.
       01  WS-INDEX-MAX      PIC 9(4) VALUE 0.
       01  WS-SEARCH-KEY     PIC X(10).
       01  WS-FIELD-3-DECOUPE.
           10 WS-FIELD-3-AGE PIC 9(3).
           10 WS-FIELD-3-TAUX PIC 9(2).
           10 WS-FIELD-3-MONTANT PIC 9(10).
           10 WS-FIELD-3-TAILLE PIC 9(5).
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
           OPEN INPUT ENTREE
           INITIALIZE WS-CHAISE.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                     UNTIL WS-INDEX > WS-INDEX-MAX



            MOVE WS-FIELD-3 (WS-INDEX) to WS-FIELD-3-DECOUPE
            if WS-FIELD-3-AGE = ZERO or WS-FIELD-3-TAUX = ZERO
              MOVE 'Trés mauvais' to WS-SCORE-TEXT
            else
             COMPUTE WS-SCORE =
              (WS-FIELD-3-MONTANT / WS-FIELD-3-AGE )
             * (WS-FIELD-3-TAILLE / WS-FIELD-3-TAUX )

             MOVE WS-SCORE to WS-SCORE-TEXT
            END-IF
            IF WS-SEARCH-KEY = WS-FIELD-1(WS-INDEX)
                   DISPLAY 'Trouvé : ' WS-FIELD-1 (WS-INDEX)
                           'Score  : ' WS-SCORE-TEXT
            END-IF

           END-PERFORM.
           CLOSE ENTREE.

       CLOSE-FILES.
           CLOSE CLES.
