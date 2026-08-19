       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CWXTCDB2.
      ******************************************************************
      *                                                                *
BB23  * MODIF AVEC NATIXIS
BB23  * remodif
BB23  * encore une modif 2
      ******      C O M P U W A R E   C O R P O R A T I O N       ******
      *                                                                *
      *  TAKES EMPLOYEE NUMBER AND FETCHES ALL RECORDS FROM            *
      *  TABLE RVIIEMP AND RETURN EMP-TOTAL-WAGE                       *
      *                                                                *
      *  IF THE EMPLOYEE IS NOT FOUND, 0 IS RETURNED AS EMP-TOTAL-WAGE *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.

           EXEC SQL INCLUDE RVIIEMP END-EXEC.

       01  WS-FIELDS.
           05 WS-EMP-NUMBER        PIC X(05).
           05 WS-EMP-TOTAL-WAGE    PIC S9(9)V99   COMP-3.


           EXEC SQL DECLARE C CURSOR FOR
                SELECT EMPNO, YTDSAL
                FROM HDDRXM0.RVIIEMP
           END-EXEC.


*********
*********
*********

       LINKAGE SECTION.
       01  EMP-FIELDS.
           05  EMP-NUMBER           PIC 9(05).
           05  EMP-TOTAL-WAGE       PIC S9(9)V99   COMP-3.

       PROCEDURE DIVISION USING EMP-FIELDS.
       0000-MAINLINE.
           MOVE 0 TO EMP-TOTAL-WAGE.
           EXEC SQL OPEN C END-EXEC.
           IF SQLCODE = 0
              PERFORM 1000-FETCH
              UNTIL SQLCODE IS NOT = 0
           END-IF
           EXEC SQL CLOSE C END-EXEC.
           GOBACK.

       1000-FETCH.
           EXEC SQL FETCH C INTO
                :WS-EMP-NUMBER, :EMP-TOTAL-WAGE
           END-EXEC
           IF SQLCODE = 0
               IF WS-EMP-NUMBER = EMP-NUMBER
                    MOVE WS-EMP-TOTAL-WAGE to EMP-TOTAL-WAGE
               END-IF
           END-IF
           GOBACK.