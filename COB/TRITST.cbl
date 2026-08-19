       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TRITST.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A-N-B                   PIC 999.
       01  B-N-C                   PIC 999.
       01  A-N-C                   PIC 999.
       LINKAGE SECTION.
       01  TST-REC.
           05  A                   PIC 9.
           05  B                   PIC 9.
           05  C                   PIC 9.
       01  TYPE-OF-TRIANGLE        PIC 9.
       PROCEDURE DIVISION   USING  TST-REC
                                   TYPE-OF-TRIANGLE.
       VALIDATE-TRIANGLE.
           ADD A B GIVING A-N-B.
           ADD A C GIVING A-N-C.
           ADD B C GIVING B-N-C.
           IF (B-N-C NOT > A) OR (A-N-C NOT > B) OR (A-N-B NOT > C)
              MOVE 4 TO TYPE-OF-TRIANGLE.
       DETERMINE-TYPE.
           IF TYPE-OF-TRIANGLE = 4
               NEXT SENTENCE
           ELSE
               IF (A = B) AND (B = C)
                  MOVE 1 TO TYPE-OF-TRIANGLE
               ELSE
                  IF (A = B) OR (B = C) OR (A = C)
                     MOVE 2 TO TYPE-OF-TRIANGLE
                  ELSE
                     MOVE 3 TO TYPE-OF-TRIANGLE.
       RETURN-TO-CALLER.
           GOBACK.