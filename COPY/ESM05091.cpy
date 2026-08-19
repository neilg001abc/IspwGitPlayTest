      *TYPVERS=ES01
      ******************************************************************
      *
      *      P e r s o n a l   F i n a n c e   T e c h n o l o g y
      *
      ******************************************************************
      *        C a r t o g r a p h i e    A p p l i c a t i v e
      ******************************************************************
      *CàRT0A :                          (Code application)
      *CàRT0F : U-DOSTRT54R              (Code fonctionnalité)
      *
      ******************************************************************
      *                    F i c h e     C o m p o s a n t
      ******************************************************************
      * Composant     : ESM05091    Copy échange service SM05091
      *
      * Nature / Type : COPY        (PGM - S/PGM - COPY - MAPSET - MAP)
      *                 BATCH/TP    (BATCH - TP - BATCH/TP)
      *                 Métier      (Cinématique - Métier - Transverse)
      *
      * Version       : 1           (1-9 - A-Z)
      *
      * Fonction      : Zone échange SM05091
      *                 -----------------------------------------------
      *                 -----------------------------------------------
      *                 -----------------------------------------------
      *                 -----------------------------------------------
      *
      ******************************************************************
      * Créé    le 02/02/2015 par CDS Steria
      *
      ******************************************************************
      * Modifié le JJ/MM/SSAA par [userid] (signature : xxxxxxxx)
      *
      * > -------------------------------------------------------------
      * > -------------------------------------------------------------
      * > -------------------------------------------------------------
      * > -------------------------------------------------------------
      *
      ******************************************************************
      *
      *....Niveau 01 neutralisé
      *01  ESM05091.
      *
      *....Constantes
           05  ESM0509-CST.
      *
      *........Nom du programme associé
               10  ESM0509-CST-PGM                  PIC  X(08)
                                                    VALUE 'SM05091 '.
      *
      *....Zone entrée
           05  ESM0509-IN.
      *
      *........
               10  ESM0509-IN-POLEACT               PIC X(1).
      *
      *........
               10  ESM0509-IN-SOCCOM                PIC 9(3).
      *
      *
      *........
               10  ESM0509-IN-CPRODCOM              PIC X(04).
      *
      *........
               10  ESM0509-IN-CSOUSGEST             PIC 9(3).
      *
      *....Zone sortie
           05  ESM0509-OUT.
      *
      *........Zone sortie data
               10  ESM0509-OUT-DATA.
      *
      *.............durée
                    15  ESM0509-OUT-DUREE           PIC 9(3).
      *.............
                    15  ESM0509-OUT-PREAUTO         PIC X(01).
      *.............
                    15  ESM0509-OUT-EDITION-PRA     PIC X(01).
      *
      *
      *........Zone code raison
      *........n.b: doit être alimenté en cas de ZCONTGN-NOT-FOUND et
      *........     ZCONTGN-ERREUR
               10  ESM0509-OUT-RSNCOD               PIC  9(03).
      *.... code retour OK
                   88  ESM0509-OUT-RSNCOD-OK        VALUE 000.
      *.... donnée pôle activité incorrecte
                   88  ESM0509-OUT-RSNCOD-001       VALUE 001.
      *.... donnée société commerciale incorrecte
                   88  ESM0509-OUT-RSNCOD-002       VALUE 002.
      *.... donnée code produit commercial ncorrecte
                   88  ESM0509-OUT-RSNCOD-003       VALUE 003.
      *.... donnée sous code gestionincorrecte
                   88  ESM0509-OUT-RSNCOD-004       VALUE 004.
      *.... appel au service non effectué
                   88  ESM0509-OUT-PAS-APPEL        VALUE 998.
      *.... Erreur non référencée
                   88  ESM0509-OUT-RSNCOD-999       VALUE 999.
      *