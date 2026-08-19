      *TYPVERS=EX01
      ******************************************************************
      *
      *      P e r s o n a l   F i n a n c e   T e c h n o l o g y
      *
      ******************************************************************
      *        C a r t o g r a p h i e    A p p l i c a t i v e
      ******************************************************************
      *CàRT0A : -------------            (Code application)
      *CàRT0F : ---------                (Code fonctionnalité)
      *
      ******************************************************************
      *                    F i c h e     C o m p o s a n t
      ******************************************************************
      * Composant     : ESA49751    Copy échange applicative
      *
      * Nature / Type : COPY       (PGM - S/PGM - COPY - MAPSET - MAP)
      *                 BATCH/TP   (BATCH - TP - BATCH/TP)
      *                 Applicatif (Métier - Applicatif - Transverse)
      *
      * Version       : 1          (1-9 - A-Z)
      *
      * Fonction      : Zone échange pour le service applicatif SA49751
      *                 -----------------------------------------------
      *                 -----------------------------------------------
      *                 -----------------------------------------------
      *                 -----------------------------------------------
      *
      ******************************************************************
      * Créé    le 13/06/2025 par L572097
      *
      ******************************************************************
      * Modifié le JJ/MM/SSAA par [userid] (signature : xxxxxxxx)
      *
      * > ------------------------------------------------------------ -
      * > ------------------------------------------------------------ -
      * > ------------------------------------------------------------ -
      * > ------------------------------------------------------------ -
      *
      ******************************************************************
      *
      *....Niveau 01 neutralisé
      *01  ESA49751.
      *
      *....Constantes
           05  ESA4975-CST.
      *
      *........Nom du Service applicatif
               10  ESA4975-CST-PGM                  PIC  X(08)
                                                    VALUE 'SA49751'.
      *
      *....Zone entrée
           05  ESA4975-IN.
               10  ESA4975-IN-TDARNFOY              PIC S9(11)
                                                    COMP-3.
               10  ESA4975-IN-TDARNCONT             PIC S9(01)
                                                    COMP-3.
               10  ESA4975-IN-TDARNCARTE            PIC S9(01)
                                                    COMP-3.
               10  ESA4975-IN-TDARNDT               PIC S9(03)
                                                    COMP-3.
      *
      *....Zone sortie
           05  ESA4975-OUT.
      *
      *........Code raison
               10  ESA4975-OUT-CODE-RSN             PIC  9(02).
                   88  ESA4975-OUT-RSN-OK           VALUE  00.
                   88  ESA4975-OUT-RSN-INDISPO      VALUE  01.
                   88  ESA4975-OUT-RSN-TIMEOUT      VALUE  02.
                   88  ESA4975-OUT-RSN-ERREUR       VALUE  99.
      *
      *........Données restituées
               10  ESA4975-OUT-TDARMASSREPORT       PIC S9(09)V9(02)
                                                    COMP-3.
               10  ESA4975-OUT-TDARMASSREPORTFP     PIC S9(09)V9(02)
                                                    COMP-3.
               10  ESA4975-OUT-TDARMASSREPORTRD     PIC S9(09)V9(02)
                                                    COMP-3.
               10  ESA4975-OUT-TDARMFASSTOT         PIC S9(09)V9(02)
                                                    COMP-3.
      *