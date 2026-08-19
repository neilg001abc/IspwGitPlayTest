       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TRIMAIN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE  ASSIGN TO UT-S-INFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  IN-REC                  PIC X(80).
       WORKING-STORAGE SECTION.
       01  NAME-N-CNTR-TABLE.
           05  FILLER  PIC X(21)   VALUE 'EQUILATERAL TRIANGLES'.
           05  FILLER  PIC 9(04).
           05  FILLER  PIC X(21)   VALUE 'ISOSCELES TRIANGLES'.
           05  FILLER  PIC 9(04).
           05  FILLER  PIC X(21)   VALUE 'SCALENE TRIANGLES'.
           05  FILLER  PIC 9(04).
           05  FILLER  PIC X(21)   VALUE 'INVALID TRIANGLES'.
           05  FILLER  PIC 9(04).
       01  N-N-C-TABLE             REDEFINES  NAME-N-CNTR-TABLE.
           05  N-N-C               OCCURS 4 TIMES
                                   INDEXED BY TX.
               10  N-NAME          PIC X(21).
               10  N-CNTR          PIC 9(04).
       01  OUT-OF-RECS             PIC X.
       01  TRIANGLE-TYPE           PIC 9.
       01  WORK-REC.
           05  SIDE-A              PIC 9(01).
           05  SIDE-B              PIC 9(01).
           05  SIDE-C              PIC 9(01).
       PROCEDURE DIVISION.
        MAIN-PARA.
           PERFORM INIT-PARA.
           PERFORM ANALYZE-NEXT-REC
               UNTIL OUT-OF-RECS = 'Y'.
           PERFORM ENDING-PARA.
           GOBACK.
       INIT-PARA.
           MOVE ZERO TO N-CNTR (1) N-CNTR (2) N-CNTR (3) N-CNTR (4).
           OPEN INPUT INFILE.
           MOVE 'N' TO OUT-OF-RECS.
       ANALYZE-NEXT-REC.
           READ INFILE INTO WORK-REC
              AT END
              MOVE 'Y' TO OUT-OF-RECS.
           IF OUT-OF-RECS = 'N'
              MOVE ZERO TO TRIANGLE-TYPE
              CALL 'TRITST' USING WORK-REC TRIANGLE-TYPE
              SET TX TO TRIANGLE-TYPE
              ADD 1 TO N-CNTR (TX).
       ENDING-PARA.
           CLOSE INFILE.
           CALL 'TRIRPT' USING NAME-N-CNTR-TABLE.