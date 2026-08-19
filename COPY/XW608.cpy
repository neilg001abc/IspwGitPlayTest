      *=================================================================
      *                 Pseudo working-storage AIDA
      *                 ---------------------------
      *
      *   COPYBOOK : XW608
      *
      *=================================================================
      * MODIFICATIONS
      *=================================================================
      *    DATE    I    AUTEUR     I DESCRIPTION
      *------------I---------------I------------------------------------
SP0209* 19/02/2009 I P. Savelon    I Ajout de la zone QAU30G2
      *            I               I
SP0999* 09/09/1999 I P. Savelon    I Ajout de la zone QFCMTG1
      *            I               I
1097  * 03/01/1991 I JT            I ADD NEW ERROR RETURN SWITCH
      *=================================================================

       01  FILLER.

           05  W-W01-CONSTANTS.
               10  W-W01-MESSAGE-ONLY-LIT    PIC X     VALUE 'M'.
               10  W-W01-SEND-MAP-LIT        PIC X     VALUE 'S'.
               10  W-W01-XFER-FWD-NOSTAK-LIT PIC X     VALUE 'T'.
               10  W-W01-XFER-FORWARD-LIT    PIC X     VALUE 'W'.
               10  W-W01-XFER-BACKWARD-LIT   PIC X     VALUE 'R'.
               10  W-W01-ERROR-MODULE-LIT    PIC X(8)  VALUE 'PW606U0'.
               10  W-W01-BATCH-SUBMISSION    PIC X(8)  VALUE 'PB600I0'.
               10  W-W01-PAU30G1             PIC X(8)  VALUE 'PAU30G1'.
SP0209         10  W-W01-PAU30G2             PIC X(8)  VALUE 'PAU30G2'.

               10  W-W01-LCASE               PIC X(26)
                   VALUE 'abcdefghijklmnopqrstuvwxyz'.
               10  W-W01-UCASE               PIC X(26)
                   VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
               10  W-W01-LOW-TIMESTAMP       PIC X(26)
                                    VALUE '0001-01-01-00.00.00.000000'.
               10  W-W01-LOW-DATE            PIC X(10)
                                             VALUE '0001-01-01'.
               10  W-W01-HIGH-TIMESTAMP      PIC X(26)
                                    VALUE '9999-12-31-23.59.59.999999'.
               10  W-W01-HIGH-DATE           PIC X(10)
                                             VALUE '9999-12-31'.
               10  W-W01-RETURN-A-LEVEL      PIC X(4)  VALUE 'A9RR'.
               10  W-W01-PROG-SYNCHRO-POINT  PIC X(8)  VALUE 'PFCMTG1'.

           05  W-W01-GENERAL-VARIABLES.
               10  W-W01-CURRENT-TIMESTAMP.
                 15  W-W01-CURRENT-DATE      PIC X(10).
                 15  FILLER                  PIC X     VALUE '-'.
                 15  W-W01-CURRENT-TIME      PIC X(8).
                 15  FILLER                  PIC X(7)  VALUE '.000000'.
               10  W-W01-SQL-ERROR-CD        PIC Z(9)-.
               10  FILLER REDEFINES W-W01-SQL-ERROR-CD.
                   15  FILLER                PIC X(5).
                   15  W-W01-SQL-ERROR       PIC X(5).