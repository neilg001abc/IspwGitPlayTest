      ******************************************************************
      * DCLGEN TABLE(HDDRXM0.RVIIEMP)                                  *
      *        LIBRARY(HDDRXM0.DEMO.COPY(RVIIEMP))                     *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE HDDRXM0.RVIIEMP TABLE
           ( EMPNO                          CHAR(5) NOT NULL,
             LASTNAME                       CHAR(15) NOT NULL,
             FIRSTNME                       CHAR(10) NOT NULL,
             MIDINIT                        CHAR(1),
             YTDSAL                         DECIMAL(10, 2),
             MAXNATL                        DECIMAL(10, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE HDDRXM0.RVIIEMP                    *
      ******************************************************************
       01  DCLRVIIEMP.
           10 EMPNO                PIC X(5).
           10 LASTNAME             PIC X(15).
           10 FIRSTNME             PIC X(10).
           10 MIDINIT              PIC X(1).
           10 YTDSAL               PIC S9(8)V9(2) USAGE COMP-3.
           10 MAXNATL              PIC S9(8)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************