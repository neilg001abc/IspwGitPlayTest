       01  HOLD-TABLE.
           05  HOLD-AREA        OCCURS 4 TIMES
                                INDEXED BY REG-IX.
               10  HOLD-LINE    OCCURS 20 TIMES
                                INDEXED BY HOLD-IX.
                   15  HOLD-NAME               PIC X(15).
                   15  HOLD-REGION             PIC X(5).
                   15  HOLD-TYPE               PIC X.
                   15  HOLD-YEARS              PIC 9(2).
                   15  HOLD-HIRE-DATE.
                       20  HOLD-HIRE-YY        PIC 9(2).
                       20  HOLD-HIRE-MM        PIC 9(2).
                       20  HOLD-HIRE-DD        PIC 9(2).
                   15  HOLD-WAGES              PIC 9(5)V99.
                   15  HOLD-OT                 PIC 9(5)V99.
                   15  HOLD-COMM               PIC 9(5)V99.
                   15  HOLD-TOTAL              PIC 9(5)V99.