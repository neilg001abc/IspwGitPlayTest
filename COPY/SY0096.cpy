*******SY0096   VFHX 301013 085524 JALLET P                             00000010
*******SY0096   XXXX 221008 140743 HOURIGAT                             00000020
      ******************************************************************00000030
      *  DESCRIPTION DE LA CLAUSE COPY DE GET DIAGNOSTIC               *00000040
      *  NOM                  : SY0096                                 *00000050
      *  LONGUEUR             :                                        *00000060
      *  AUTEUR               : SDE                                    *00000070
      *  DATE DE CREATION     : 12 JUIN 2008                           *00000080
      *  DATE DE MODIFICATION :                                        *00000090
      *  REMARQUES            : CETTE CLAUSE COPY EST UTILISEE PAR     *00000100
      *                         TOUS LES PROGRAMMES BATCH OU CICS      *00000110
      *                         POUR RECUPERER LES MESSAGES DB2        *00000120
      ******************************************************************00000130
                                                                        00000140
         02 SY0096.                                                     00000150
           05 DASQLCODE      PIC S9(9) USAGE COMP.                      00000160
           05 DATOKENCNT     PIC S9(9) USAGE COMP.                      00000170
           05 DATOKEN1       PIC X(515).                                00000180
           05 DASQLERRD1B    PIC X(10).                                 00000190
           05 DASQLERRP      PIC X(8).                                  00000200
           05 DASQLSTATE     PIC X(5).                                  00000210
           05 DAMSGTEXT      PIC X(260).                                00000220
           05 WSQLCODE       PIC +(7)9.                                 00000230
           05 NBROWS         PIC S9(3) USAGE COMP-3.                    00000240
           05 NUMROW         PIC S9(10) USAGE COMP-3.                   00000250
           05 NBERR          PIC S9(9)  USAGE COMP.                     00000260
           05 NUMERR         PIC S9(9)  USAGE COMP.                     00000270
           05 NUM-ERR        PIC S9(4)  COMP VALUE +720.                00000280
           05 I-ERR          PIC 9(2).                                  00000290