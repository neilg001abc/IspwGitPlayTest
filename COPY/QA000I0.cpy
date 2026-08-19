      *----------------------------------------------------------------
      * QA000I0     COMMAREA.
      *----------------------------------------------------------------
      *  Répercuter les changements de ce COPYBOOK dans les
      *  copybooks suivants AA000I0 and BLCABS.
      *----------------------------------------------------------------
      *  05/2004 QPF00X0 CONTIENT LES INDICATIVES TECHNIQUES
      *          QEV00X0 CONTIENT LES INDICATIVES FONCTIONNELLES
      *          PF702B0 FAIT LES INCLUDE DE CES COPYS
      *----------------------------------------------------------------
      *  05/2008 OL2405 AJOUT ZONE UTILISER PAR STC CONSOLE
      *          OL0711 AJOUT ZONE PR LE PF300B8
      *----------------------------------------------------------------
      * Longueur 11702 car. soit 3 blocs de 4K
      *----------------------------------------------------------------
      *  Commarea details. (30 car.)
      *----------------------------------------------------------------
        03  C-COMMAREA.
         05  C-COMMAREA-DETAILS.
           10  C-COMMAREA-NAME               PIC X(8).
           10  C-COMMAREA-VERSION            PIC X(8).
           10  C-COMMAREA-LENGTH             PIC S9(4) COMP.
           10  C-COMMAREA-ORIGINATOR         PIC X(8).
                88  C-TSM                    VALUE 'PA000I0'.
                88  C-BEM                    VALUE 'PA300B0' 'PF300B0'.
                88  C-BDM                    VALUE 'PA300B0' 'PF300B0'.
SP0513          88  C-TQM                    VALUE 'PA200P0' 'PF200Q0'.
CCB        10  C-COMMAREA-COBOL              PIC X(4).

      *----------------------------------------------------------------
      *  Called module details. (20 car.)
      *----------------------------------------------------------------
         05  C-CALLED-MODULE-DATA-LIT        PIC X(4).
         05  C-CALLED-MODULE-DATA.
           10  C-CALL-HANDLER-ADDRESS        PIC X(4).
           10  C-CALLED-MODULE-NAME          PIC X(8).
SP0297          88  C-CALLED-ABEND-MODULE    VALUE 'PAABEND' 'PW606U0'.
           10  C-TASK-CONTROL-PARMS.
             15  C-TASK-CONTROL-PARMS-PTR    USAGE IS POINTER .

      *----------------------------------------------------------------
      *  System profile. (60 car.)
      *----------------------------------------------------------------
         05  C-SYSTEM-PROFILE-LIT            PIC X(4).
         05  C-SYSTEM-PROFILE.
           10  C-DEFAULT-CENTURY             PIC 9(2).
           10  C-DEFAULT-LANGUAGE            PIC X.
           10  C-REFERENCE-DELAY             PIC 9(7).
           10  C-IF-MSG-HEADER-REQD          PIC X.
                88  C-MSG-HEADER-REQD        VALUE 'Y'.
           10  C-INVOKE-DELAY                PIC 9(7).