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
      * Composant     : ESA49771    Copy échange applicative
      *
      * Nature / Type : COPY       (PGM - S/PGM - COPY - MAPSET - MAP)
      *                 BATCH/TP   (BATCH - TP - BATCH/TP)
      *                 Applicatif (Métier - Applicatif - Transverse)
      *
      * Version       : 1          (1-9 - A-Z)
      *
      * Fonction      : Zone échange pour le service applicatif SA49771
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
      *01  ESA49771.
      *
      *....Constantes
           05  ESA4977-CST.
      *
      *........Nom du Service applicatif
               10  ESA4977-CST-PGM                  PIC  X(08)
                                                    VALUE 'SA49771'.
      *
      *....Zone entrée
           05  ESA4977-IN.
               10  ESA4977-IN-TDARNFOY              PIC S9(11)
                                                    COMP-3.
               10  ESA4977-IN-TDARNCONT             PIC S9(01)
                                                    COMP-3.
               10  ESA4977-IN-TDARNCARTE            PIC S9(01)
                                                    COMP-3.
               10  ESA4977-IN-TDARNDT               PIC S9(03)
                                                    COMP-3.
               10  ESA4977-IN-TDARMASSREPORT        PIC S9(09)V9(02)
                                                    COMP-3.
               10  ESA4977-IN-TDARMASSREPORTFP      PIC S9(09)V9(02)
                                                    COMP-3.
               10  ESA4977-IN-TDARMASSREPORTRD      PIC S9(09)V9(02)
                                                    COMP-3.
               10  ESA4977-IN-TDARMFASSTOT          PIC S9(09)V9(02)
                                                    COMP-3.
      *
      *....Zone sortie
           05  ESA4977-OUT.
      *
      *........Code raison
               10  ESA4977-OUT-CODE-RSN             PIC  9(02).
                   88  ESA4977-OUT-RSN-OK           VALUE  00.
                   88  ESA4977-OUT-RSN-INDISPO      VALUE  01.
                   88  ESA4977-OUT-RSN-TIMEOUT      VALUE  02.
                   88  ESA4977-OUT-RSN-ERREUR       VALUE  99.
      *
      *........Nb de lignes traitées
               10  ESA4977-OUT-NBALIG               PIC S9(09)
                                                    COMP.
      *