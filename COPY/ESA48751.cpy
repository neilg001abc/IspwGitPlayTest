      *TYPVERS=EC01
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
      * Composant     : ESA48751    Copy échange applicative
      *
      * Nature / Type : COPY       (PGM - S/PGM - COPY - MAPSET - MAP)
      *                 BATCH/TP   (BATCH - TP - BATCH/TP)
      *                 Applicatif (Métier - Applicatif - Transverse)
      *
      * Version       : 1          (1-9 - A-Z)
      *
      * Fonction      : Zone échange pour le service applicatif SA48751
      *                 -----------------------------------------------
      *                 -----------------------------------------------
      *                 -----------------------------------------------
      *                 -----------------------------------------------
      *
      ******************************************************************
      * Créé    le 20/02/2025 par LB95880
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
      *01  ESA48751.
      *
      *....Constantes
           05  ESA4875-CST.
      *
      *........Nom du Service applicatif
               10  ESA4875-CST-PGM                  PIC  X(08)
                                                    VALUE 'SA48751'.
      *
      *....Zone entrée
           05  ESA4875-IN.
      *
      *........Indicateur d'accès
               10  ESA4875-IN-IND-ACC               PIC  X(01).
                   88  ESA4875-IN-LEC               VALUE 'L'.
                   88  ESA4875-IN-MAJ               VALUE 'M'.
                   88  ESA4875-IN-DEL               VALUE 'D'.
      *
      *........Prédicat
               10  ESA4875-IN-PREDICAT.
      *
                   15  ESA4875-IN-TDOPNFOYCOP       PIC S9(11)
                                                    COMP-3.
                   15  ESA4875-IN-TDOPNCONTCOP      PIC S9(01)
                                                    COMP-3.
                   15  ESA4875-IN-TDOPNCARTECOP     PIC S9(01)
                                                    COMP-3.
                   15  ESA4875-IN-TDOPNDTCOP        PIC S9(03)
                                                    COMP-3.
      *
      *....Zone update
           05  ESA4875-UPD.
      *
      *........Données mises à jour
               10  ESA4875-UPD-TDOPDRACH            PIC  X(10).
               10  ESA4875-UPD-TDOPCETATDEM         PIC  X(03).
      *
      *....Zone sortie
           05  ESA4875-OUT.
      *
      *........Code raison
               10  ESA4875-OUT-CODE-RSN             PIC  9(02).
                   88  ESA4875-OUT-RSN-OK           VALUE  00.
                   88  ESA4875-OUT-RSN-INDISPO      VALUE  01.
                   88  ESA4875-OUT-RSN-TIMEOUT      VALUE  02.
                   88  ESA4875-OUT-RSN-ERREUR       VALUE  99.
      *
      *........Indicateur fin curseur
               10  ESA4875-OUT-CURSEUR              PIC  X(01).
                   88  ESA4875-OUT-CURS-FIN         VALUE HIGH-VALUE.
      *
      *........Données restituées
               10  ESA4875-OUT-TDOPNFOY             PIC S9(11)
                                                    COMP-3.
               10  ESA4875-OUT-TDOPNCONT            PIC S9(01)
                                                    COMP-3.
               10  ESA4875-OUT-TDOPNCARTE           PIC S9(01)
                                                    COMP-3.
               10  ESA4875-OUT-TDOPNDT              PIC S9(03)
                                                    COMP-3.
               10  ESA4875-OUT-TDOPNAFFEXT          PIC  X(15).
               10  ESA4875-OUT-TDOPNFOYCOP          PIC S9(11)
                                                    COMP-3.
               10  ESA4875-OUT-TDOPNCONTCOP         PIC S9(01)
                                                    COMP-3.
               10  ESA4875-OUT-TDOPNCARTECOP        PIC S9(01)
                                                    COMP-3.
               10  ESA4875-OUT-TDOPNDTCOP           PIC S9(03)
                                                    COMP-3.
               10  ESA4875-OUT-TDOPDRACH            PIC  X(10).
               10  ESA4875-OUT-TDOPCETATDEM         PIC  X(03).
               10  ESA4875-OUT-TDOPNAUTOCOPRO       PIC S9(09)
                                                    COMP-3.
               10  ESA4875-OUT-TDOPNAUTOLIE         PIC S9(09)
                                                    COMP-3.
      *