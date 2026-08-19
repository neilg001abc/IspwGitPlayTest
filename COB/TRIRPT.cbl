       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TRIRPT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE  ASSIGN TO UT-S-OUTFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  OUTFILE
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  OUT-REC                 PIC X(80).
       WORKING-STORAGE SECTION.
       01  HDR-LINE                PIC X(80)  VALUE
                                        '*** TRIANGLE REPORT ***'.
       01  DTL-LINE.
           05 FILLER               PIC X(10)  VALUE 'NUMBER OF '.
           05 DTL-TITLE            PIC X(21).
           05 FILLER               PIC X(05)  VALUE SPACE.
           05 DTL-CNTR             PIC Z(05).
           05 FILLER               PIC X(39)  VALUE SPACE.

       01  BLANK-LINE              PIC X(80)  VALUE SPACE.

       01  FIELD-A                 PIC 9(01)  VALUE 0.
       01  FIELD-B                 PIC 9(01)  VALUE 0.
       01  FIELD-C                 PIC 9(01)  VALUE 0.
       01  FIELD-D                 PIC 9(01)  VALUE 0.
       01  FIELD-E                 PIC 9(01)  VALUE 0.
       01  FIELD-1                 PIC 9(01)  VALUE 0.
       01  FIELD-2                 PIC 9(01)  VALUE 0.
       01  FIELD-3                 PIC 9(01)  VALUE 0.
       01  FIELD-4                 PIC 9(01)  VALUE 0.
       01  FIELD-5                 PIC 9(01)  VALUE 0.

       LINKAGE SECTION.
       01  TABLE-OF-NAMES-N-CNTRS.
           05  N-N-C               OCCURS 4 TIMES
                                   INDEXED BY TX.
              10  T-NAME           PIC X(21).
              10  T-CNTR           PIC 9(04).
       PROCEDURE DIVISION USING TABLE-OF-NAMES-N-CNTRS.
           OPEN OUTPUT OUTFILE.
           WRITE OUT-REC FROM HDR-LINE.
           WRITE OUT-REC FROM BLANK-LINE.
           PERFORM MOVE-FIELDS.
           PERFORM WRITE-DTLS
                   VARYING TX FROM 1 BY 1
                   UNTIL TX > 4.
           WRITE OUT-REC FROM BLANK-LINE.
           ADD T-CNTR (1) T-CNTR (2) T-CNTR (3) T-CNTR (4)
               GIVING DTL-CNTR.
           MOVE 'INPUT RECORDS' TO DTL-TITLE.
           WRITE OUT-REC FROM DTL-LINE.
           CLOSE OUTFILE.
           GOBACK.
       WRITE-DTLS.
           MOVE T-NAME (TX) TO DTL-TITLE.
           MOVE T-CNTR (TX) TO DTL-CNTR.
           WRITE OUT-REC FROM DTL-LINE.
       MOVE-FIELDS.
           MOVE FIELD-E TO FIELD-D.
           MOVE FIELD-D TO FIELD-C.
           MOVE FIELD-C TO FIELD-B.
           MOVE FIELD-B TO FIELD-A.
           MOVE FIELD-A TO FIELD-1.
           MOVE FIELD-1 TO FIELD-2.
           MOVE FIELD-2 TO FIELD-3.
           MOVE FIELD-3 TO FIELD-4.