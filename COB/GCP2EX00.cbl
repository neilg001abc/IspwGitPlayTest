      ******************************************************************
      *    NE PAS OUBLIER DE METTRE A JOUR LE MODULE GCP2EY10  !!!!!!! *
      ******************************************************************
      *                                                                *
      *    NOM DU MODULE       GCP2EX00                                *
      *    TYPE DU MODULE      PGM                                     *
      *    LANGAGE DU MODULE   COBOL                                   *
      *                                                                *
      ******************************************************************
      *                                                                *
      *    FONCTION MODULE     HABILLAGE GCP                           *
      *    AUTEUR DU MODULE    D.VALEAMA                               *
      *    DATE DE CREATION    29/10/99                                *
      *                                                                *
      ******************************************************************
      *                                                                *
      *    MODULES             VIPS054  : MATRICULE / CAC              *
      *                                   DES RESEAUX AF               *
      *                        SFPS011  : MATRICULE / CAC              *
      *                         DES RESEAUX GA                         *
      *                        YPPDICD  : STRUCTURES COMMERCIALES      *
      *                                   DES RESEAUX SALARIES (AF,GA) *
      *                        GCP2EC54 : CODES MATRICULE ET           *
      *                                   INTERMEDIAIRE                *
      *                        GCP2EY50 : DONNEES AGENCE / INTERMED    *
      *                                   ET HIERARCHIE DES RES AGC    *
      *                        NNPHA11  : DONNEES AGENCE / INTERMED    *
      *                                   DU RESEAU AD                 *
      *                        NNPHA12  : DONNEES HIERARCHIE ET CODE   *
      *                                   AGENCE DES RESEAUX SALARIES  *
      *                        NNPHA13  : COORDONNEES DES CENTRES      *
      *                                   ADMINISTRATIFS ET LES RIB    *
      *                                                                *
      ******************************************************************
      *                                                                *
      *    DATE DERNIERE MISE A JOUR  11/07/00                         *
      *         AJOUT DES DONNEES DU VIPS055 CONCERNANT LES RESEAUX    *
      *         SALARIES ( AF ET GA )                                  *
      *    DATE DERNIERE MISE A JOUR  21/08/00                         *
      *         MODIF POUR RECUPERER LA HIERARCHIE DES RESEAUX SALARIES*
      *         A PARTIR DU MATRICULE ( HORS PROPO )                   *
      *    DATE DERNIERE MISE A JOUR  28/08/00                         *
      *         RECUPERATION DU TYPE D'AFFECTATION POUR LES RESEAUX    *
      *         SALARIES ( AGENDA )                                    *
      *    DATE DERNIERE MISE A JOUR  08/09/00                         *
      *         PB D'HABILLAGE DES PRESCRIPTEURS                       *
      *    DATE DERNIERE MISE A JOUR  17/10/00                         *
      *         CAS DES RESEAUX SALARIES ( AF ET GA ) :                *
      *         - APPEL VIPS054 POUR AF ET SFPS011 POUR GA             *
      *         - APPEL YPPXDIC ( LECTURE TABLE ANNA YPTPIND )         *
      *    DATE DERNIERE MISE A JOUR  13/11/00                         *
      *         PAS DE PLANTAGE SI LE CODE INTERMEDIAIRE N'EST PAS     *
      *         RENSEIGNE                                              *
      *    DATE DERNIERE MISE A JOUR  28/11/00                         *
      *         AJOUT NOUVELLE DONNEE DANS LA MACRO : EX00-CA-NU-POLE  *
      *    DATE DERNIERE MISE A JOUR  08/12/00                         *
      *         AJOUT 2 NOUVELLES DONNEES POUR LA PROPO DANS LA MACRO  *
      *    DATE DERNIERE MISE A JOUR  12/01/01                         *
      *         - SI LE CODE CENTRE N'EST PAS RENSEIGNE, ON N'APPELLE  *
      *           PAS LE MODULE D'ACCES A GCA                          *
      *         - RESTITUTION DU NUMERO DE TELEPHONE PORTABLE POUR     *
      *           LES RESEAUX SALARIES                                 *
      *    DATE DERNIERE MISE A JOUR  09/05/01                         *
      *         MODIF POUR FD : RECUPERATION DE L'ADRESSE POSTALE POUR *
      *         LETTRES CHEQUES                                        *
      *    DATE DERNIERE MISE A JOUR  28/05/01                         *
      *         HABILLAGE DES RESEAUX SALARIES PAR CODE MATRICULE :    *
      *         APPEL YPPXDIC                                          *
      *    DATE DERNIERE MISE A JOUR  21/01/02                         *
      *         LEVEE DU CONTROLE DU CODE CENTRE ADM POUR LES HIER.    *
      *         A TROUS, LE NNPHA12 DOIT TOUJOURS RENDRE UN CODE CENTRE*
      *         OU GERER L'ANOMALIE                                    *
      *                                                                *
      *    DATE DERNIERE MISE A JOUR  05/09/03                         *
      *         AJOUT CODE ACTION K POUR RECUP INFOS CA + CODE SGI     *
      *    DATE DERNIERE MISE A JOUR  01/03/04                         *
      *         CE N'EST PLUS LE CENTRE ADM QUI DETERMINE LA COMPAGNIE *
      *         D'ORIGINE DES COURTIERS                                *
      *                                                                *
P9033 *    DATE DERNIERE MISE A JOUR    /12/2017                       *
P9033 *    CODE PROJET : P9033 - RESTRUCTURATION D'AEC                 *
P9033 *         CORRECTION D'ALIMENTATION DU CODE SECTEUR POUR LES     *
P9033 *         RESEAU APS.                                            *
      *                                                                *
P9041 *    DATE DERNIERE MISE A JOUR    /02/2018, VERSION GCP : VR97   *
P9041 *    CODE PROJET : P9041 & P7622 - AJOUT D'INDICATEUR REMUNERATION
P9041 *         DE L'INTERMEDIAIRE.                                    *
      *                                                                *
P9033R*    DATE DERNIERE MISE A JOUR   20/12/2017
P9033R*    CODE PROJET : RETOUR ARRIERE P9033 - RESTRUCTURATION D'AEC
P9033R*    MODIF       : ATOS
      *
ANA001*    DATE DERNIERE MISE A JOUR   18/02/2019                      *
ANA001*    CODE ITSM   : INCIDENT NÂ° 1721237                           *
ANA001*    MODIF       : ATOS - A.NAHRI                                *
AAI001*                                                                *
AAI001*    DATE DERNIERE MISE A JOUR   06/11/2020                      *
AAI001*    CODE ITSM   : RELIVRAISON DE LA VERSION 0116 ECRASEE        *
      ******************************************************************
       ID DIVISION.
       PROGRAM-ID. GCP2EX00.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *COPY GCMDEBUG.
      *SOURCE-COMPUTER. IBM-390 WITH DEBUGGING MODE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *
       77  WSTOP                             PIC  9.
       77  WEC54                             PIC  9.
      *
       01  W-NU-RCC.
           05  W-NU-CNT-EXT                  PIC  X(10).
           05  FILLER                        PIC  X(06) VALUE SPACES.
      *
       01  SAUVE-CD-ACTION                   PIC  X(01).
       01  SAUVE-CD-RES                      PIC  S9(04) COMP.
       01  W-POP                             PIC  X(02).
       01  W-ANA-CODE                        PIC  X(01).
       01  W-TOP-CDA                         PIC  X(01).
       01  W-DOMLOG                          PIC  X(06).
       01  WS-ADMAIL                         PIC  X(103).

       01  W-DATE-JOUR.
           05  W-SSAA                        PIC  9(04).
           05  W-MM                          PIC  9(02).
           05  W-JJ                          PIC  9(02).
      *
       01  W-DATEJOUR.
           05  W-SSAADJ                      PIC  9(04).
           05  W-MMDJ                        PIC  9(02).
           05  W-JJDJ                        PIC  9(02).
      *
       01  W-DATE-STRUCT.
        05 W-DATE-STRUCT-AAAAMM              PIC  X(6) VALUE '201501'.
        05 W-DATE-STRUCT-JJ                  PIC  X(2) VALUE '12'.
      * W-DATE-STRUCT-JJ : JOUR OU LE SIC EST TRANSCODIFIE
       01  W-DATE.
           05  W-SSAA                        PIC  9(04).
           05  FILLER                        PIC  X(01) VALUE '-'.
           05  W-MM                          PIC  9(02).
           05  FILLER                        PIC  X(01) VALUE '-'.
           05  W-JJ                          PIC  9(02).
      *
       01  W-DATE-DEAG.
           05  DEAG-ANNEE                    PIC  9(04).
           05  FILLER                        PIC  X(01) VALUE '-'.
           05  DEAG-MOIS                     PIC  9(02).
           05  FILLER                        PIC  X(01) VALUE '-'.
           05  DEAG-JOUR                     PIC  9(02).
      *
       01  W-DATE-ANAB.
           05  W-SSAA                        PIC  9(04).
           05  FILLER                        PIC  X(01) VALUE '-'.
           05  W-MM                          PIC  9(02).
           05  FILLER                        PIC  X(01) VALUE '-'.
           05  W-JJ                          PIC  9(02).
      *
       01  W-LIBELLE.
           05  W-LIB1                        PIC  X(50).
           05  W-LIB2                        PIC  X(10).
           05  W-LIBX REDEFINES W-LIB2.
               10  W-LIB3                    PIC  9(02).
               10  W-LIB4                    PIC  X(08).
      *
       01  W-CPTAGE.
           05  DEB-CAR-CPTAGE                PIC  X(01).
           05  FIN-CAR-CPTAGE                PIC  X(05).
      *
       01  W-MATRIC.
           05  W-MATR                        PIC  X(05).
           05  W-DERCAR                      PIC  X(01).
      *
       01  W-MATRIC2.
           05  FILLER                        PIC  X(01) VALUE '0'.
           05  W-MATR2                       PIC  X(05).
      *
       01  W-MATRIC3.
           05  W-PRCAR                       PIC  X(01).
           05  W-MATR3                       PIC  X(05).
      *
       01  W-CD-RATTACH.
           05  FILLER                        PIC  X(07).
           05  W-MATRICULE                   PIC  X(06).
      *
      *    PARAMETRES DE LA TABLE ANNA ( YPPXDIC )
      *
       01  PARAM-ANA.
           05  W-ANA-POP                     PIC  X(02).
           05  W-ANA-MAT1                    PIC  X(05).
           05  W-ANA-POPM                    PIC  X(01).
           05  W-ANA-MAT2                    PIC  X(01).
           05  W-ANA-MATRIC                  PIC  X(06).
           05  W-ANA-MEMOID                  PIC  X(07).
           05  W-ANA-ETATCIV                 PIC  X(03).
           05  W-ANA-NOM                     PIC  X(32).
           05  W-ANA-PRENOM                  PIC  X(32).
           05  W-ANA-SIT                     PIC  X(01).
           05  W-ANA-IMM                     PIC  X(05).
           05  W-ANA-SECTIONI                PIC  X(04).
           05  W-ANA-DIR                     PIC  X(06).
           05  W-ANA-SERV                    PIC  X(06).
           05  W-ANA-FONCT                   PIC  X(06).
           05  W-ANA-NOTEL                   PIC  X(11).
           05  W-ANA-POSTE                   PIC  X(04).
           05  W-ANA-BUREAU                  PIC  X(04).
           05  W-ANA-TYPIDENT                PIC  X(01).
           05  W-ANA-DATEENT                 PIC  X(08).
           05  W-ANA-DATEPOP                 PIC  X(08).
           05  W-ANA-DATEMOD                 PIC  X(08).
           05  W-ANA-DATESOR                 PIC  X(08).
           05  W-ANA-DATESUS                 PIC  X(08).
           05  W-ANA-NOIMP                   PIC  X(08).
           05  W-ANA-NOVOIE                  PIC  X(05).
           05  W-ANA-TYPVOIE                 PIC  X(05).
           05  W-ANA-NOMVOIE1                PIC  X(30).
           05  W-ANA-NOMVOIE2                PIC  X(30).
           05  W-ANA-CODEPOST                PIC  X(05).
           05  W-ANA-VILLE                   PIC  X(30).
           05  W-ANA-FONCTION                PIC  X(30).
           05  W-ANA-DEPT                    PIC  X(06).
           05  W-ANA-PARRAIN                 PIC  X(07).
           05  W-ANA-BAL                     PIC  X(07).
           05  W-ANA-CEDEX                   PIC  X(05).
           05  W-ANA-FAX                     PIC  X(11).
           05  W-ANA-TOPTEL                  PIC  X(01).
           05  W-ANA-TOPBUR                  PIC  X(01).
           05  W-ANA-TOPIMP                  PIC  X(01).
           05  W-ANA-INDICTEL                PIC  X(08).
           05  W-ANA-POPR                    PIC  X(02).
           05  W-ANA-MATR                    PIC  X(06).
           05  W-ANA-PROFILM                 PIC  X(06).
           05  W-ANA-DMNELOG                 PIC  X(06).
           05  W-ANA-TOPBAL                  PIC  X(01).
           05  W-ANA-PAYS                    PIC  X(03).
           05  W-ANA-LOCMEMO                 PIC  X(08).
           05  W-ANA-NOTEL20                 PIC  X(20).
           05  W-ANA-FAX20                   PIC  X(20).
           05  W-ANA-CASEPOST                PIC  X(10).
           05  W-ANA-CDSTIND                 PIC  X(02).
           05  W-ANA-TELPORTABLE             PIC  X(20).
           05  W-ANA-IDENTPFA                PIC  X(08).
           05  FILLER                        PIC  X(146).
      *
       01  W-ANA-RETOUR                      PIC S9(04) COMP.
      *
      *    PARAMETRES D'APPEL DES MODULES
      *
       01  GCP2EY50                          PIC  X(8) VALUE 'GCP2EY50'.
       01  NNPHA11                           PIC  X(8) VALUE 'NNPHA11 '.
       01  NNPHA12                           PIC  X(8) VALUE 'NNPHA12 '.
       01  NNPHA13                           PIC  X(8) VALUE 'NNPHA13 '.
       01  YPPXDIC                           PIC  X(8) VALUE 'YPPXDIC '.
       01  NNPDICAN                          PIC  X(8) VALUE 'NNPDICAN'.
      *
      *    PARAMETRES D'APPEL DU VIPS054
      *
       01  VIPS054                           PIC  X(8) VALUE 'VIPS054 '.
       01  VIPS-ENSOR.
           COPY VIM050 REPLACING ==: PX :== BY VIPS.
       01  VIPS-RETOUR                       PIC S9(4).
      *
      *    PARAMETRES D'APPEL DU IAPSV001
      *
       01  IAPSV001                          PIC  X(8) VALUE 'IAPSV001'.
           COPY IAMSV001.
      *
      *    PARAMETRES D'APPEL DU SFPS015
      *
       01  SFPS015                           PIC  X(8) VALUE 'SFPS015 '.
       01  SF15-ENSOR.
           COPY SFM038 REPLACING ==: PX :== BY SF15.
       01  SF15-RETOUR                       PIC S9(4).
      *
      *
      *    PARAMETRES D'APPEL DU GCP2EC54
      *
       01  GCP2EC54                          PIC  X(8) VALUE 'GCP2EC54'.
           COPY GCM2EC54.
      *
      *
      *    PARAMETRES D'APPEL DU GCP2EC10
      *
       01  GCP2EC10                          PIC  X(8) VALUE 'GCP2EC10'.
           COPY GCM2EC10.
      *
      *    PARAMETRES D'APPEL DU GCP2EX24 POUR ACQUISITION DE LA MEMOIRE
      *    ( STOCKAGE DES DONNEES )
      *
       01  GCP2EX24                          PIC  X(8) VALUE 'GCP2EX24'.
           COPY GCM2EX24.
      *
      *    PARAMETRES POUR GESTION DE LA MEMOIRE
      *
           COPY   NAMSTODM.
       01  PARAM.
           COPY   NAMSTOZN REPLACING ==:ZONE:== BY ==MEM==.
      *
       01  GCP2DEAG                          PIC X(8) VALUE 'GCP2DEAG'.
           COPY   GCM2DEAG.
      *
      *
       01  GCP2DERI                          PIC X(8) VALUE 'GCP2DERI'.
           COPY   GCM2DERI.
      *
       01  NNPAPDOM                          PIC X(8) VALUE 'NNPAPDOM'.
           COPY NNMAPDOM.
      *
       01  NNPIOLOG                          PIC X(8) VALUE 'NNPIOLOG'.
           COPY NNMIOLOG.
      *
      *--> YPPDIC3B RESTITUE L'ADRESSE EMAIL
       01 YPPDIC3B                         PIC X(08) VALUE 'YPPDIC3B'.
           COPY YPMDIC3B.

       01 ZONE-SORTIE-YPMDIC3B.
           COPY YPMDDIC3.

      *
      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *
           COPY GCM2EX00.
      *
      *    PARAMETRES D'APPEL DU MODULE YPPXDIC
      *
       01  ANA-ENSOR.
           05  ANA-RETOUR                    PIC S9 COMP.
           05  ANA-POP                       PIC  X(02).
           05  ANA-MATRIC                    PIC  X(06).
           05  ANA-MEMOID                    PIC  X(07).
           05  ANA-ACT                       PIC  X(01).
           05  ANA-SORTIE                    PIC  X(700).
           05  ANA-TEST                      PIC  X(01).
      *    PARAMETRES D'APPEL DU MODULE NNPDICAN
      *
       01  ANAB-ENSOR.
           05  ANAB-RETOUR                    PIC S9 COMP.
           05  ANAB-POP                      PIC  X(02).
           05  ANAB-MATRIC                   PIC  X(06).
           05  ANAB-MEMOID                   PIC  X(07).
           05  ANAB-DATE                     PIC  X(10).
           05  ANAB-SORTIE                   PIC  X(700).
      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING EX00-ENSOR.
      *----------------------------------------------------------------*
      *
      *    INITIALISATION
      *
      D    DISPLAY '2EX EX00-CD-RES      <' EX00-CD-RES      '>'
      D    DISPLAY '2EX EX00-CD-MATRIC   <' EX00-CD-MATRIC   '>'
      D    DISPLAY '2EX EX00-CD-INTERMED <' EX00-CD-INTERMED '>'
      D    DISPLAY '2EX EX00-NU-RCC      <' EX00-NU-RCC      '>'
           MOVE EX00-CD-RES TO SAUVE-CD-RES
           IF EX00-CD-RES = 3
            MOVE 1 TO EX00-CD-RES
           END-IF
           PERFORM INITIALISATION
      *
      *
      *    CHARGEMENT DES DONNEES EN MEMOIRE POUR LES TRAITEMENTS
      *    GROS BATCH
      *
           IF  EX00-CD-ACTION = '1' AND EX00-TOP-BATCH = 'O'
               PERFORM TRAIT-INIT
           END-IF
      *
      *    CONTROLE DES PARAMATRES D'ENTREE
      *
           IF  EX00-CD-ACTION NOT = '1'
               PERFORM CONTROLE-PARAM
           END-IF
      *
      *    TRAITEMENT PRINCIPAL
      *
           IF  WSTOP = ZERO AND EX00-CD-ACTION NOT = '1'
               PERFORM TRAITEMENT
           END-IF
      *
           IF SAUVE-CD-RES  NOT = 0
             MOVE SAUVE-CD-RES TO EX00-CD-RES
           END-IF
      D    DISPLAY '2EX EX00-RETCODE       <' EX00-RETCODE       '>'
      D    DISPLAY '2EX EX00-SYSTEME       <' EX00-SYSTEME       '>'
      D    DISPLAY '2EX EX00-ERRCODE       <' EX00-ERRCODE       '>'
      D    DISPLAY '2EX EX00-ERRCOM1       <' EX00-ERRCOM1       '>'
      D    DISPLAY '2EX EX00-ERRCOM2       <' EX00-ERRCOM2       '>'
      D    DISPLAY '2EX EX00-ERRCOM3       <' EX00-ERRCOM3       '>'
      D    DISPLAY '2EX EX00-ERRCOM4       <' EX00-ERRCOM4       '>'
      D    DISPLAY '2EX EX00-INT-AG-CDA    <' EX00-INT-AG-CDA    '>'
      D    DISPLAY '2EX EX00-INT-CD-MATRIC <' EX00-INT-CD-MATRIC '>'
      D    DISPLAY '2EX EX00-INT-MAT1-ALZ  <' EX00-INT-MAT1-ALZ  '>'
      D    DISPLAY '2EX EX00-INT-MAT2-ALZ  <' EX00-INT-MAT2-ALZ  '>'
      D    DISPLAY '2EX EX00-INT-MAT1-PFA  <' EX00-INT-MAT1-PFA  '>'
      D    DISPLAY '2EX EX00-INT-MAT2-PFA  <' EX00-INT-MAT2-PFA  '>'
      D    DISPLAY '2EX EX00-INT-N0-MATRIC <' EX00-INT-N0-MATRIC '>'
      D    DISPLAY '2EX EX00-SAL-MATRICULE <' EX00-SAL-MATRICULE '>'
      D    DISPLAY '2EX EX00-SAL-N1-MATRIC <' EX00-SAL-N1-MATRIC '>'
      D    DISPLAY '2EX EX00-CD-MATRIC     <' EX00-CD-MATRIC     '>'
      D    DISPLAY '2EX EX00-CD-INTERMED   <' EX00-CD-INTERMED   '>'
           GOBACK.
      *
      *----------------------------------------------------------------*
      *   TRAITEMENT PRINCIPAL
      *----------------------------------------------------------------*
       TRAITEMENT.
      *
           IF  EX00-TOP-BATCH = 'O'
               MOVE EX00-PTC                  TO EX24E-PTC
           END-IF
      *
      *    INITIALISATION DES DONNEES INTERMEDIAIRE ET HIERARCHIE
      *    ( ZONES DE REDEFINES )
      *
           IF  EX00-CD-RES = 1 OR 3 OR 4  OR 0
               INITIALIZE EX00-SAL
           ELSE
               INITIALIZE EX00-INT
           END-IF
      *
           EVALUATE EX00-CD-ACTION
              WHEN 'A'
              WHEN 'B'
                      IF  EX00-CD-SITAG = SPACES AND EX00-CD-CA = SPACES
                          PERFORM TRAIT-HAB
                      END-IF
                      PERFORM TRAIT-GCA
      *
              WHEN 'C'
              WHEN 'D'
              WHEN 'E'
              WHEN 'F'
              WHEN 'K'
              WHEN 'L'
                      PERFORM TRAIT-HAB
                      PERFORM TRAIT-GCA
      *
              WHEN 'G'
              WHEN 'I'
              WHEN 'J'
                      PERFORM TRAIT-HAB
              WHEN 'H'
                      PERFORM TRANSFO-INTERMED
                      PERFORM TRAIT-AGC
           END-EVALUATE

P9041 * .. AJOUT L'INDICATEUR REMUNERATION
P9041      PERFORM ALIMENTER-IND-REMUN
           .
      *
P9041 *----------------------------------------------------------------*
P9041 * IL PREND LES VALEURS SUIVANTES : NON ALIMENTE, 'S' (SALARIE),
P9041 * 'A' (AGENT), 'M' (MANDATAIRE)
P9041 *----------------------------------------------------------------*
P9041  ALIMENTER-IND-REMUN.
P9041
P9041      EVALUATE TRUE
P9041               WHEN EX00-CD-RES = 1 OR 4
P9041                    MOVE 'S'           TO EX00-SAL-IND-REMUN
P9041
P9041               WHEN EX00-CD-RES = 9
P9041                    MOVE 'S'           TO EX00-INT-IND-REMUN
P9041
P9041               WHEN EX00-CD-RES = 2
P9041                AND EX00-INT-NATURE = '18'
P9041                AND (EX00-INT-SOUS-NATURE = '03' OR '04')
P9041                    MOVE 'S'           TO EX00-INT-IND-REMUN
P9041
P9041               WHEN EX00-CD-RES = 7
P9041                AND EX00-INT-CD-MATRIC (8:6) = '851019'
P9041                    MOVE 'S'           TO EX00-INT-IND-REMUN
P9041
P9041               WHEN EX00-INT-NATURE = '10'
P9041                AND (EX00-INT-SOUS-NATURE = '00' OR ' ')
P9041                    MOVE 'A'           TO EX00-INT-IND-REMUN
P9041
P9041               WHEN EX00-INT-NATURE = '18'
P9041                AND (EX00-INT-SOUS-NATURE = '00' OR ' ')
P9041                    MOVE 'M'           TO EX00-INT-IND-REMUN
P9041      END-EVALUATE
P9041
P9041 D    DISPLAY 'EX00-SAL-IND-REMUN : ' EX00-SAL-IND-REMUN
P9041 D    DISPLAY 'EX00-INT-IND-REMUN : ' EX00-INT-IND-REMUN
           .
      *
      *----------------------------------------------------------------*
      *   TRAITEMENT HABILLAGE DES RESEAUX SALARIES ET AGC
      *----------------------------------------------------------------*
       TRAIT-HAB.
      *
      *    RECHERCHE DE L'INTERMEDIAIRE
      *
           IF  EX00-CD-MATRIC   NOT > SPACES
           AND EX00-CD-INTERMED  NOT > SPACES
           AND EX00-CD-CPT-AGE NOT > SPACES
      *
      *        SI NI LE CODE AGENDA, NI LE CODE CENTRE,
      *        NI LE CODE MATRICULE NE SONT RENSEIGNES,
      *        IL FAUT AVOIR AU MOINS LE NUMERO DE RCC DE RENSEIGNE
      *
               IF  EX00-NU-RCC NOT > SPACES
                   MOVE 'IL FAUT RENSEIGNER LE NUMERO DE RCC'
                                              TO EX00-ERRCOM2
                   PERFORM ANOMALIE-4008
               ELSE
                 IF EX00-CD-RES = 0
                        PERFORM  RECH-AGC
                        MOVE 1 TO  WEC54
                 END-IF
                 IF WSTOP = ZERO
                      EVALUATE EX00-CD-RES
                      WHEN 1
                      WHEN 3
                             IF EX00-CD-BRANCHE = 'F' THEN
                                PERFORM RECH-AF-FINAN
                             ELSE
                                PERFORM RECH-AF-VIE
                             END-IF
                      WHEN 4
                                PERFORM RECH-MA-VIE
                      WHEN 2
                      WHEN 6
                      WHEN 7
                      WHEN 9
                      WHEN 12
                      WHEN 13
                          IF WEC54 = 0
                             PERFORM RECH-AGC
                          END-IF
                      END-EVALUATE
                 END-IF
               END-IF
           END-IF
      *
      *    REPOSITIONNEMENT DU CODE INTERMEDIAIRE
      *
           IF  WSTOP = ZERO
               PERFORM TRANSFO-INTERMED
           END-IF
      *
      *    RECHERCHE DES DONNEES INTERMEDIAIRE ET DES DONNES HIERARCHIES
      *
           IF  WSTOP = ZERO  AND  EX00-CD-ACTION NOT = 'J'
               EVALUATE EX00-CD-RES
                  WHEN 1
                  WHEN 3
                  WHEN 4
                         PERFORM TRAIT-AF
                  WHEN 2
      *           WHEN 4
                  WHEN 6
                  WHEN 7
                  WHEN 12
                  WHEN 13
                         PERFORM TRAIT-AGC
                  WHEN 9
                         PERFORM TRAIT-AD
               END-EVALUATE
           END-IF
      *
      *!!! MODIF Ã  AMELIORER POUR FD : LETTRES CHEQUES !!!!!!!
      *    RECUPERATION DE LA BONNE ADRESSE POSTALE
      *
           IF  WSTOP = ZERO  AND  EX00-CD-ACTION = 'J'
               PERFORM TRAIT-AGC
           END-IF
      *
      *    RECUP ADRESSE MAIL DU SUIVEUR
      *
           IF  WSTOP = ZERO
               AND  (EX00-CD-ACTION NOT = 'A' AND 'B' )
               AND  (EX00-CD-RES = 1 OR 2 OR 4)
                   PERFORM TRAIT-ADMAIL
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   RECHERCHE DU CODE CAC POUR LE RESEAU AF ( VIPS054 )
      *   ET BRANCHE VIE
      *----------------------------------------------------------------*
       RECH-AF-VIE.
      *
           INITIALIZE                            VIPS-ENSOR
           MOVE W-NU-CNT-EXT                  TO VIPS-SAFNO0-CTRT
           IF EX00-TYP-RCC = SPACES
           THEN
               PERFORM RECH-EC10
           END-IF
           IF WSTOP = 0
           THEN
            MOVE EX00-TYP-RCC                  TO VIPS-SAFCE0
            MOVE '1'                           TO VIPS-SAFCN0
      **
            MOVE '0'                           TO VIPS-SAFNO0-DBLE1
            IF  EX00-CD-DBL NOT NUMERIC
                MOVE '0'                       TO VIPS-SAFNO0-DBLE2
            ELSE
                MOVE EX00-CD-DBL               TO VIPS-SAFNO0-DBLE2
            END-IF
      *
            IF  EX00-CD-BRANCHE = 'V'
                EVALUATE EX00-TYP-RCC
                    WHEN '1'  MOVE '1'         TO VIPS-SAFCE0
                                                  VIPS-SAFCN0
                    WHEN '2'  MOVE '2'         TO VIPS-SAFCE0
                              MOVE '1'         TO VIPS-SAFCN0
                    WHEN '3'  MOVE '1'         TO VIPS-SAFCE0
                              MOVE '2'         TO VIPS-SAFCN0
                END-EVALUATE
            END-IF
      *
      *    IF  EX00-CD-BRANCHE = 'F'
      *        MOVE '1'                       TO VIPS-SAFCE0
      *        MOVE '3'                       TO VIPS-SAFCN0
      *        IF  EX00-TYP-RCC = '3'
      *            MOVE '2'                   TO VIPS-SAFCN0
      *        END-IF
      *    END-IF
      *
            CALL VIPS054                    USING VIPS-ENSOR
                                                 VIPS-RETOUR
      *
            IF  VIPS-RETOUR NOT = ZERO
                MOVE 'VIPS054'                 TO EX00-ERRCOM1
                MOVE VIPS-RETOUR               TO EX00-ERRCOM3
                MOVE 'PB DE RECHERCHE CODE CAC / MATRICULE SUIVEUR'
                                               TO EX00-ERRCOM4
                PERFORM ANOMALIE
            ELSE
      **
      **        SI TYPE D'AFFAIRE EST EGAL A 1, ON RECUPERE LE MATRICULE
      **        ET ACCES A LA TABLE ANNA POUR RECUPERER LE CODE CAC
      **
      **        IF  VIPS-SVVCT0 NOT = 1
                IF  VIPS-SVVCT0 = 1
                OR  (VIPS-SVVCT0 = 4 AND VIPS-SVVCD0(5:1) NOT = ' ')
                THEN
                    MOVE VIPS-SVVCD0           TO W-CD-RATTACH
                    MOVE W-MATRICULE           TO EX00-CD-MATRIC
                    MOVE 'BI'                  TO W-POP
      D             DISPLAY '2EX APPEL01 YPPXDIC'
                    PERFORM APPEL-YPPXDIC
                    IF WSTOP NOT = ZERO
                    THEN
                    MOVE ZERO TO WSTOP
                    MOVE 'OK'          TO EX00-RETCODE
                    MOVE ZERO       TO EX00-ERRCODE
                    MOVE SPACES     TO EX00-ERRCOM1
                                       EX00-ERRCOM2
                                       EX00-ERRCOM3
                                       EX00-ERRCOM4
                                       EX00-SYSTEME
                    MOVE 'M '                  TO W-POP
      D             DISPLAY '2EX APPEL02 YPPXDIC'
                    PERFORM APPEL-YPPXDIC
                    PERFORM TRANSCO-CPS
                    MOVE 'BI'                  TO W-POP
                    END-IF
                ELSE
                    MOVE VIPS-SVVCD0           TO EX00-CD-CAC
                                                  EX00-SAL-CD-CAC
                END-IF
      **
                MOVE VIPS-SVVCT0               TO EX00-SAL-TYP-AFF
                MOVE VIPS-POOADM               TO EX00-SAL-CLI-SUIV
            END-IF
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   RECHERCHE DU SUIVEUR  POUR LE RESEAU MA ( IAPSV001)
      *   ET BRANCHE VIE
      *----------------------------------------------------------------*
       RECH-MA-VIE.
P9033 D    DISPLAY 'GCP2EX00 : RECH-MA-VIE'
      *
           INITIALIZE                          IAMSV001
            MOVE '0'                           TO IASV001-SAFNO0(1:1)
            IF  EX00-CD-DBL NOT NUMERIC
                MOVE '0'                       TO IASV001-SAFNO0(2:1)
            ELSE
                MOVE EX00-CD-DBL               TO IASV001-SAFNO0(2:1)
            END-IF
           MOVE W-NU-CNT-EXT                  TO IASV001-SAFNO0(3:10)
           IF EX00-TYP-RCC = SPACES
           THEN
               PERFORM RECH-EC10
           END-IF
           IF WSTOP = 0
           THEN
      *
      *
             MOVE '3'         TO IASV001-SAFCE0
             MOVE '1'         TO IASV001-SAFCN0
             CALL IAPSV001                   USING IAMSV001
      *
           IF  IASV001-CODE-SQL NOT = ZERO
               MOVE 'IAPSV001'                TO EX00-ERRCOM1
               STRING  'SQL-CODE: ' DELIMITED BY SPACE
                        IASV001-CODE-SQL DELIMITED BY SIZE
                                            INTO EX00-ERRCOM3
               MOVE IASV001-ERR-MSG
                                              TO EX00-ERRCOM4
               PERFORM ANOMALIE
           ELSE
      *
      *        SI TYPE D'AFFAIRE EST EGAL A 1, ON RECUPERE LE MATRICULE
      *        ET ACCES A LA TABLE ANNA POUR RECUPERER LE CODE CAC

P9033R*        EVALUATE IASV001-SVVCT0
P9033R*            WHEN '1'
P9033R*                 MOVE IASV001-SVVCD0      TO W-CD-RATTACH
P9033R*                 MOVE W-MATRICULE         TO EX00-CD-MATRIC
P9033R*                 MOVE 'M '                TO W-POP
P9033R*                 PERFORM APPEL-YPPXDIC
P9033R*
P9033R*            WHEN '2'
P9033R*            WHEN '0'
P9033R*                 IF IASV001-SVVCD1 NOT = SPACES
P9033R*                    MOVE IASV001-SVVCD1   TO EX00-CD-CAC
P9033R*                                             EX00-SAL-CD-CAC
P9033R*                    MOVE EX00-CD-CAC      TO EX00-SAL-N4-CODE
P9033R*                 ELSE
P9033R*                    MOVE IASV001-SVVCD0   TO W-DOMLOG
P9033R*                    PERFORM RECH-DOMLOG
P9033R*                 END-IF
P9033R*
P9033R*        END-EVALUATE
P9033
      D        DISPLAY 'PB EDITION W-MATRICULE    <' W-MATRICULE   '>'
      D        DISPLAY 'PB EDITION IASV001-SVVCD0 <' IASV001-SVVCD0'>'
P9033R         IF  IASV001-SVVCT0 = 1
P9033R         THEN
P9033R             MOVE IASV001-SVVCD0        TO W-CD-RATTACH
P9033R             MOVE W-MATRICULE           TO EX00-CD-MATRIC
P9033R             MOVE 'M '                  TO W-POP
      D             DISPLAY '2EX APPEL03 YPPXDIC'
P9033R             PERFORM APPEL-YPPXDIC
P9033R         ELSE
P9033R             MOVE IASV001-SVVCD0        TO W-DOMLOG
P9033R             PERFORM RECH-DOMLOG
P9033R         END-IF


               MOVE IASV001-SVVCT0           TO EX00-SAL-TYP-AFF
               MOVE IASV001-POOADM           TO EX00-SAL-CLI-SUIV
           END-IF
           .
      *----------------------------------------------------------------*
      *   RECHERCHE DU CODE DOMAINE LOGIQUE  POUR SANTE
      *----------------------------------------------------------------*
      *
       RECH-DOMLOG.

           INITIALIZE     IOLOG
           MOVE W-DOMLOG   TO IOLOG-DOMLOG
           CALL NNPIOLOG   USING IOLOG
      *
           IF  IOLOG-CODESQL NOT = 0000
               MOVE 'NNPIOLOG'             TO EX00-ERRCOM1
               STRING  'SQL-CODE: ' DELIMITED BY SPACE
                        IOLOG-CODESQL DELIMITED BY SIZE
                        ' '           DELIMITED BY SIZE
                        W-DOMLOG      DELIMITED BY SIZE
                                            INTO EX00-ERRCOM3
               MOVE IOLOG-MESSERR
                                           TO EX00-ERRCOM4
               PERFORM ANOMALIE
           ELSE
               MOVE IOLOG-POSTEIO TO EX00-CD-CAC
                                     EX00-SAL-CD-CAC
               MOVE EX00-CD-CAC   TO EX00-SAL-N4-CODE
      D        DISPLAY 'RECH-DOMLOG - PB EDITION <' EX00-CD-CAC '>'
           END-IF
            .
      *----------------------------------------------------------------*
      *   RECHERCHE DU CODE CAC OU DU MATRICULE
      *   POUR LE RESEAU AF ( SFPS015 ) ET BRANCHE FINANCIÃ¨RE !!!
      *       !!!! ATTENTION LE NUM DE RCC DOIT ÃªTRE SUR 16 !!!!
      *----------------------------------------------------------------*
       RECH-AF-FINAN.
      *
           INITIALIZE                            SF15-ENSOR
MACRO      MOVE EX00-CD-CIE-PART              TO SF15-CD-PART-AFF
           MOVE EX00-NU-RCC                   TO SF15-NU-CNT-16-AFF
           MOVE '16'                          TO SF15-CD-TYP-APPEL
           MOVE '1'                           TO SF15-CD-TYP-ENREG-AFF
           MOVE '3'                           TO SF15-CD-TYP-CNT-AFF
      *
           CALL SFPS015                    USING SF15-ENSOR
                                                 SF15-RETOUR
      *
           IF  SF15-RETOUR NOT = ZERO
               MOVE 'SFPS015'                 TO EX00-ERRCOM1
               MOVE SF15-RETOUR               TO EX00-ERRCOM3
               MOVE 'PB DE RECHERCHE CODE CAC / MATRICULE SUIVEUR'
                                              TO EX00-ERRCOM4
               PERFORM ANOMALIE
           ELSE
      *
SV1505*        SI TYPE D'AFFAIRE EST EGAL A 1 (LE SUIVEUR EST UN MATR)
 "    *     OU SI TYPE D'AFFAIRE EST EGAL A 4 (HORS SIC)
 "    *     MAIS QUE LE L'AIGUILLAGE RENSEIGNE LE MATRICULE, ALORS
 "    *        LE SUIVEUR EST UN MATRICULE
      *        ET ACCES A LA TABLE ANNA POUR RECUPERER LE CODE CAC
      *
 "             IF  SF15-CD-TYP-SVR-AFF = 1
 "             OR (SF15-CD-TYP-SVR-AFF = 4
 "             AND SF15-CD-MATR-CAC-SVR (5:1) > SPACE)
 "    *            LE SUIVEUR EST UN MATRICULE
                   MOVE SF15-CD-MATR-CAC-SVR  TO W-CD-RATTACH
                   MOVE W-MATRICULE           TO EX00-CD-MATRIC
                   MOVE 'BI'                  TO W-POP
      D             DISPLAY '2EX APPEL04 YPPXDIC'
                   PERFORM APPEL-YPPXDIC
 "             ELSE
 "    *            LE SUIVEUR EST UN CAC
 "                 MOVE SF15-CD-MATR-CAC-SVR  TO EX00-CD-CAC
 "                                               EX00-SAL-CD-CAC
               END-IF
      *
               MOVE SF15-CD-TYP-SVR-AFF       TO EX00-SAL-TYP-AFF
               MOVE SF15-NU-CLI-AGF-AFF       TO EX00-SAL-CLI-SUIV
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   RECHERCHE STRUCTURE COMMERCIAL POUR LES RESEAUX SALARIES
      *----------------------------------------------------------------*
       APPEL-YPPXDIC.
      *
      *    CREATION D'UN HEAP POUR ACQUERIR
      *    DE LA MEMOIRE ADRESSE EN DESSOUS DES 16 MGBYTES
      *
             MOVE SPACE  TO W-ANA-CODE
             MOVE SPACE  TO EX00-CD-TRANSCO
             IF EX00-DATE <=   W-DATEJOUR
             OR W-POP = 'M '
             THEN
                MOVE 'A' TO W-ANA-CODE
             ELSE
                MOVE 'B' TO W-ANA-CODE
             END-IF
           PERFORM TRT-GET-MEMOIRE
      *
           IF  WSTOP = ZERO
             IF W-ANA-CODE = 'A'
             THEN
               INITIALIZE  ANA-ENSOR  PARAM-ANA
               MOVE W-POP                     TO ANA-POP
               MOVE W-MATRICULE               TO ANA-MATRIC
               MOVE SPACES                    TO ANA-MEMOID
               MOVE 'X'                       TO ANA-TEST
      D        DISPLAY 'PB EDITION W-POP       <' W-POP       '>'
      D        DISPLAY 'PB EDITION W-MATRICULE <' W-MATRICULE '>'
      *
               CALL YPPXDIC                USING ANA-ENSOR
      *
      D        DISPLAY '2EX Y ANA-RETOUR <' ANA-RETOUR '>'
      D        DISPLAY '2EX Y W-ANA-RETOUR <' W-ANA-RETOUR '>'
               EVALUATE ANA-RETOUR
                  WHEN 0
                    MOVE ANA-SORTIE           TO PARAM-ANA
                    PERFORM STRUCTURE-COMMERCIALE
      *
                  WHEN 100
                    MOVE 'YPPXDIC'            TO EX00-ERRCOM1
                    MOVE ANA-RETOUR           TO W-ANA-RETOUR
                    MOVE W-ANA-RETOUR         TO EX00-ERRCOM3
                    MOVE 'MATRICULE INEXISTANT DANS LA TABLE ANNA'
                                              TO EX00-ERRCOM4
                    PERFORM ANOMALIE
      *
                  WHEN OTHER
                    MOVE 'YPPXDIC'            TO EX00-ERRCOM1
                    MOVE ANA-RETOUR           TO W-ANA-RETOUR
                    MOVE W-ANA-RETOUR         TO EX00-ERRCOM3
                    MOVE 'PB APPEL YPPXDIC'   TO EX00-ERRCOM4
                    PERFORM ANOMALIE
               END-EVALUATE
             ELSE
               INITIALIZE  ANAB-ENSOR  PARAM-ANA
               MOVE W-POP                     TO ANAB-POP
               MOVE W-MATRICULE               TO ANAB-MATRIC
               MOVE SPACES                    TO ANAB-MEMOID
               MOVE CORR W-DATE-JOUR   TO  W-DATE-ANAB
               MOVE W-DATE-ANAB               TO ANAB-DATE
      *
      D        DISPLAY '2EX APPEL NNPDICAN'
               CALL NNPDICAN               USING ANAB-ENSOR
      *
      D        DISPLAY '2EX D ANAB-RETOUR <' ANAB-RETOUR '>'
      D        DISPLAY '2EX D W-ANA-RETOUR <' W-ANA-RETOUR '>'
               EVALUATE ANAB-RETOUR
                  WHEN 0
                    MOVE ANAB-SORTIE          TO PARAM-ANA
                    PERFORM STRUCTURE-COMMERCIALE
      *
                  WHEN 100
                    MOVE 'NNPDICAN'           TO EX00-ERRCOM1
                    MOVE ANAB-RETOUR          TO W-ANA-RETOUR
                    MOVE W-ANA-RETOUR        TO EX00-ERRCOM3
                    MOVE 'MATRICULE INEXISTANT DANS LA TABLE ANNA'
                                              TO EX00-ERRCOM4
                    PERFORM ANOMALIE
      *
                  WHEN OTHER
                    MOVE 'NNPDICAN'           TO EX00-ERRCOM1
                    MOVE ANAB-RETOUR          TO W-ANA-RETOUR
                    MOVE W-ANA-RETOUR        TO EX00-ERRCOM3
                    MOVE 'PB APPEL NNPDICAN'  TO EX00-ERRCOM4
                    PERFORM ANOMALIE
               END-EVALUATE
           END-IF
      *
      *       LIBERATION DE LA MEMOIRE RESERVEE
      *
               PERFORM TRT-FREE-MEMOIRE
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   RECUPERATION DES DONNEES DU YPPXDIC CONCERNANT LES RESEAUX
      *   SALARIES ( AF ET GA )
      *----------------------------------------------------------------*
       STRUCTURE-COMMERCIALE.
      *
           MOVE W-ANA-DMNELOG                TO EX00-SAL-CD-IC
           MOVE W-ANA-SERV                    TO EX00-CD-CAC
                                                 EX00-SAL-CD-CAC
SV0303     MOVE W-MATRICULE                   TO EX00-SAL-MATRIC
      *
           MOVE W-ANA-MEMOID                  TO EX00-SAL-MEMOID
           MOVE W-ANA-ETATCIV                 TO EX00-SAL-QUAL
           MOVE W-ANA-NOM                     TO EX00-SAL-NOM
           MOVE W-ANA-PRENOM                  TO EX00-SAL-PRENOM
           MOVE W-ANA-NOVOIE                  TO EX00-SAL-NO-VOIE
           MOVE W-ANA-TYPVOIE                 TO EX00-SAL-TYP-VOIE
           MOVE W-ANA-NOMVOIE1                TO EX00-SAL-NOM-VOIE1
           MOVE W-ANA-NOMVOIE2                TO EX00-SAL-NOM-VOIE2
           MOVE W-ANA-CODEPOST                TO EX00-SAL-CD-POST
           MOVE W-ANA-VILLE                   TO EX00-SAL-VILLE
           MOVE W-ANA-NOTEL20                 TO EX00-SAL-TEL
           MOVE W-ANA-FAX20                   TO EX00-SAL-FAX
           MOVE W-ANA-TELPORTABLE             TO EX00-SAL-NU-PORTABLE
           .
      *
      *----------------------------------------------------------------*
      *   REPOSITIONNEMENT DU CODE INTERMEDIAIRE :
      *     - SI IL EST SUR 6 CAR. ON NE LE TOUCHE PAS
      *     - SI IL EST SUR 5 CAR. ON AJOUTE UN 0 DEVANT.
      *----------------------------------------------------------------*
       TRANSFO-INTERMED.
      *
           IF   EX00-CD-RES  NOT = 1 AND 3  AND 4
           AND  EX00-CD-INTERMED > SPACES
                MOVE EX00-CD-INTERMED         TO W-MATRIC W-MATRIC3
                IF  W-DERCAR = ' '
                    MOVE W-MATR               TO W-MATR2
                    MOVE W-MATRIC2            TO EX00-CD-INTERMED
                END-IF
      *
                IF  W-PRCAR = ' '
                    MOVE '0'                  TO W-PRCAR
                    MOVE W-MATRIC3            TO EX00-CD-INTERMED
                END-IF
           END-IF
           .
       TRANSCO-CPS.
      *
           IF   EX00-CD-CAC  = '3009'
                MOVE '30X9' TO EX00-CD-CAC
                               EX00-SAL-CD-CAC
           END-IF
           IF   EX00-CD-CAC  = '3016'
                MOVE '30X6' TO EX00-CD-CAC
                               EX00-SAL-CD-CAC
           END-IF
           IF   EX00-CD-CAC  = '9301'
                MOVE '93X1' TO EX00-CD-CAC
                               EX00-SAL-CD-CAC
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   RECHERCHE DES CODES INTERMEDIARE ET MATRICULE POUR LES
      *   RESEAUX AGC ( GCP2EC54 )
      *----------------------------------------------------------------*
       RECH-AGC.
      *
           INITIALIZE                            EC54-ENSOR
           MOVE W-NU-CNT-EXT                  TO EC54E-NU-CNT-EXT
           MOVE CORR W-DATE-JOUR              TO W-DATE
           MOVE W-DATE                        TO EC54E-DATE
      *
           CALL GCP2EC54                   USING EC54-ENSOR
      *
           IF  EC54S-RETCODE NOT = 'OK'
               MOVE 1                         TO WSTOP
               MOVE EC54S-RETCODE             TO EX00-RETCODE
               MOVE EC54S-SYSTEME             TO EX00-SYSTEME
               MOVE EC54S-ERRCODE             TO EX00-ERRCODE
               MOVE EC54S-ERRCOM1             TO EX00-ERRCOM1
               MOVE EC54S-ERRCOM2             TO EX00-ERRCOM2
               MOVE EC54S-ERRCOM3             TO EX00-ERRCOM3
               MOVE EC54S-ERRCOM4             TO EX00-ERRCOM4
           ELSE
               IF  EX00-CD-RES = 0
               THEN
                  MOVE EC54S-CD-RES TO  EX00-CD-RES
                  IF EX00-CD-RES = 1 OR 4
                  THEN
                      IF EX00-DATE < W-DATEJOUR
                      THEN
                      MOVE FUNCTION CURRENT-DATE (1:8) TO EX00-DATE
                                                    W-DATE-JOUR
                     END-IF
                  END-IF
               END-IF
               IF  EX00-CD-RES = 7 OR 6 OR 12 OR 13
                   MOVE EC54S-CD-INTERMED     TO EX00-CD-MATRIC
               ELSE
                  IF  EX00-CD-RES = 2 OR 9
                   MOVE EC54S-CD-MATRIC       TO EX00-CD-MATRIC
                   MOVE EC54S-CD-INTERMED     TO EX00-CD-INTERMED
                   MOVE EC54S-TOP-CDA-CONTRAT TO W-TOP-CDA
                  END-IF
               END-IF
           END-IF
           .
      *----------------------------------------------------------------*
      *   RECHERCHE DU TYPE DE RCC
      *----------------------------------------------------------------*
      *
       RECH-EC10.
           INITIALIZE                         EC10-ENSOR
           MOVE W-NU-CNT-EXT                 TO EC10-CNTEXT
      *
      D    DISPLAY 'EC10 NUMERO DE CONTRAT  '  EC10-CNTEXT
      D    DISPLAY 'EC10 ENRSOR             '  EC10-ENSOR
           CALL GCP2EC10                   USING EC10-ENSOR
      *
           IF  EC10-RETCODE NOT = 'OK'
               MOVE 1                         TO WSTOP
               MOVE EC10-RETCODE             TO EX00-RETCODE
               MOVE EC10-ERRCODE             TO EX00-ERRCODE
               MOVE EC10-ERRCOM1             TO EX00-ERRCOM1
               MOVE EC10-ERRCOM2             TO EX00-ERRCOM2
               MOVE EC10-ERRCOM3             TO EX00-ERRCOM3
               MOVE EC10-ERRCOM4             TO EX00-ERRCOM4
               PERFORM ANOMALIE
      D            DISPLAY 'GCP2EC10   KO'
      D            DISPLAY EC10-RETCODE
      D            DISPLAY EC10-ERRCODE
      D            DISPLAY EC10-ERRCOM1
      D            DISPLAY EC10-ERRCOM2
      D            DISPLAY EC10-ERRCOM3
      D            DISPLAY EC10-ERRCOM4
            ELSE
      D            DISPLAY 'GCP2EC10   OK'
      D            DISPLAY 'EC10-CD-FAM' EC10-CD-FAM-GEST-PRD
                   MOVE '1' TO EX00-TYP-RCC
                IF EC10-CD-FAM-GEST-PRD = 4
                MOVE '3'  TO EX00-TYP-RCC
                END-IF
            END-IF
           .
      *
      *
      *----------------------------------------------------------------*
      *   RECHERCHE DES COORDONNEES ET HIERARCHIE POUR RESEAUX SALARIES
      *   APPEL MODULE NNPHA12 POUR ACCES A NNTSTVIE
      *----------------------------------------------------------------*
       TRAIT-AF.
      *
      *    RECHERCHE INFORMATIONS COMMERCIAL ET HIERARCHIE
      *    POUR LES RESEAUX SALARIES (AF ET GA) PAR LE CODE MATRICULE
      *
      D    DISPLAY 'PB EDITION EX00-CD-CAC    <' EX00-CD-CAC    '>'
      D    DISPLAY 'PB EDITION EX00-CD-MATRIC <' EX00-CD-MATRIC '>'
           IF   EX00-CD-CAC    NOT > SPACES
           AND  EX00-CD-MATRIC     > SPACES
                MOVE SPACES                   TO W-CD-RATTACH
                MOVE EX00-CD-MATRIC           TO W-MATRICULE
      *
                IF  EX00-CD-RES = 1
                    MOVE 'BI'                 TO W-POP
                ELSE
                IF  EX00-CD-RES = 4
                    MOVE 'M '                 TO W-POP
                ELSE
                    MOVE 'XP'                 TO W-POP
                END-IF
                END-IF
      *
      *         ACCES DIRECT PAR LA TABLE ANNA
      *
      D             DISPLAY '2EX APPEL05 YPPXDIC'
                PERFORM APPEL-YPPXDIC
                IF   WSTOP NOT = ZERO
                THEN
                   IF W-POP = 'BI'
                   THEN
                   MOVE ZERO TO WSTOP
                   MOVE 'OK'          TO EX00-RETCODE
                   MOVE ZERO       TO EX00-ERRCODE
                   MOVE SPACES     TO EX00-ERRCOM1
                                      EX00-ERRCOM2
                                      EX00-ERRCOM3
                                      EX00-ERRCOM4
                                      EX00-SYSTEME
                   MOVE 'M '                  TO W-POP
      D             DISPLAY '2EX APPEL06 YPPXDIC'
                   PERFORM APPEL-YPPXDIC
                   PERFORM TRANSCO-CPS
                   MOVE 'BI'                  TO W-POP
                   END-IF
                END-IF
                MOVE W-CD-RATTACH             TO EX00-SAL-MATRICULE
      *
           END-IF
      *
           IF   WSTOP  =  ZERO
      D         DISPLAY '2EX APPEL NNPHA12'
                CALL NNPHA12               USING EX00-ENSOR EX24-ENSOR
      D         DISPLAY '2EX NNPHA12 EX00-RETCODE <' EX00-RETCODE '>'
                IF  EX00-RETCODE  NOT = 'OK'
                    MOVE 1                    TO WSTOP
                END-IF
           END-IF
           .
      *
      *   BIDOUILLE PROVISOIRE POUR AFFICHER LE NÂ° DE LA PFO AU LIEU DE
      *   CELUI DES IC POUR LES EDITIONS POUR LE RESEAU AF
      *
           IF  EX00-CD-RES = 1
           OR              = 4
               MOVE '0978 978 008  '          TO EX00-SAL-N3-TEL
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   RECHERCHE DES DONNEES AGENCE/INTERMEDIAIRE ET DES DONNEES
      *   HIERARCHIES POUR LES RESEAUX AGC AUTRES QUE AD
      *   APPEL GCP2EY50 POUR ACCES AUX DB TAMPON
      *----------------------------------------------------------------*
       TRAIT-AGC.
      *
           CALL GCP2EY50                   USING EX00-ENSOR
      *
           IF  EX00-RETCODE  NOT = 'OK'
               MOVE 1                         TO WSTOP
           ELSE
           IF EX00-CD-RES = 2
           THEN
 TM01 *    APPEL DU MODULE DE DETERMINATION DU CODE RI3A

           MOVE EX00-CD-INTERMED              TO DERI-ENTREE-CD-AGENCE
      *    ON ALIMENTE LE TOP VERSION AVEC 'R' ==> RETOURNE LE CD RI3A
           MOVE 'R'                           TO DERI-TOP-VERSION

           CALL GCP2DERI USING DERI-ZCOM

      *    ALIMENTATION DE LA MACRO DE SORTIE AVEC LE CODE CRÃ©Ã©
      *    MOVE DERI-SORTIE-CD-AGENCE         TO EX00-INT-CD-RI3A

      *    APPEL DU MODULE DE DETERMINATION AGENT CIBLE AVEC
      *    LE CODE RI3A VENANT DU GCP2DERI


               MOVE EX00-DATE-AAAA            TO DEAG-ANNEE
               MOVE EX00-DATE-MM              TO DEAG-MOIS
               MOVE EX00-DATE-JJ              TO DEAG-JOUR
               MOVE W-DATE-DEAG               TO DE01-DT-FONCT
               MOVE EX00-INT-CD-RI3A          TO DE01-CD-AGENCE
      *
      *
               CALL GCP2DEAG               USING DE01-ZCOM
      *
      *    RECUP DU TOP CIBLE - PREND LES VALEURS :
      *    '1' = AGENT CIBLE
      *    '0' = AGENT NON CIBLE

               MOVE DE01-TOP-CIBLE           TO EX00-INT-TOP-CIBLE
 TM01
 TM01 *
      *  GESTION DU NUMERO DE GESTION FORCE A 0 EN VR59
      *
      *        MOVE '0'                      TO EX00-INT-CD-GEST

      *
      *  GESTION DU MATRICULE DU CDA ASSOCIE  L AGENCE
      *  ON ECRASE LE MATRICULE CDA TROUVE SI LE CONTRAT EST HORS
      *  CDA
      *
             IF W-TOP-CDA NOT = '1'
                MOVE SPACE TO EX00-INT-AG-CDA
             END-IF
           END-IF
      *
      * RECHERCHE CODE ES2 DOM-TOM - TOUS RÃ©SEAUX AGC
      *
           MOVE EX00-CD-INTERMED TO APDOM-E-APPORTEUR
      *
      *    DISPLAY 'NNPAPDOM'

           CALL NNPAPDOM USING PARAM-APDOM

           IF APDOM-RETCODE = 'OK'
              MOVE APDOM-S-TOP-DOM-TOM TO EX00-INT-AG-ES2-DOMTOM
           ELSE
              MOVE '9' TO EX00-INT-AG-ES2-DOMTOM
           END-IF
           END-IF
           .
      *----------------------------------------------------------------*
      *   RECHERCHE DES DONNEES INTERMEDIAIRE ET DES DONNEES
      *   HIERARCHIES POUR LE RESEAU AD
      *----------------------------------------------------------------*
       TRAIT-AD.
      *
           CALL NNPHA11                    USING EX00-ENSOR
      *
           IF  EX00-RETCODE  NOT = 'OK'
               MOVE 1                         TO WSTOP
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   RECHERCHE DES COORDONNEES DES CENTRES ET DE SES RIB
      *----------------------------------------------------------------*
       TRAIT-GCA.
      *
      *    SI LE CODE CENTRE EN ENTREE N'EST PAS RENSEIGNE,
      *    ON CONTROLE SI LE CODE CENTRE RENDU PAR LE GCP2EY50
      *    EST VALIDE AVANT D'APPELER LE NNPHA13
      *
           IF  WSTOP = ZERO
               IF  EX00-CD-CA  NOT > SPACES
                   IF EX00-CA-CODE = HIGH-VALUE OR LOW-VALUE OR SPACE
                       MOVE 'AUCUN CENTRE ADM ASSOCIE '
                                              TO EX00-ERRCOM2 (1:25)
                       MOVE 'DANS LES DB TAMPONS '
                                              TO EX00-ERRCOM2 (26:20)
                       PERFORM ANOMALIE-4008
                   ELSE
                      MOVE EX00-CA-CODE       TO EX00-CD-CA
                   END-IF
               END-IF
           END-IF
      *
      *   BIDOUILLE INFAME ET PROVISOIRE POUR AFFECTER TOUS LES CONTRATS
      *   AG A LEUR CENTRE ADM POLE 1 (14 ET 15 AU LIEU DE 6 ET 7)
      *
           IF  EX00-CD-RES = 2
               IF EX00-CD-CA = '06'
               OR EX00-CA-CODE = 6
                  MOVE '14'                   TO EX00-CD-CA
                  MOVE  14                    TO EX00-CA-CODE
               ELSE
                  IF EX00-CD-CA = '07'
                  OR EX00-CA-CODE = 7
                     MOVE '15'                   TO EX00-CD-CA
                     MOVE  15                    TO EX00-CA-CODE
                  END-IF
               END-IF
           END-IF
      *
      *   NOUVELLE BIDOUILLE POUR LE RUN-OFF COURTAGE, ON RE-AIGUILLE
      *   LES COURTIERS VERS LEURS NOUVEAUX CENTRES POLE 2
      *   SAUF LES COURTIERS ARCALIS ET DOM-TOM QUI RESTENT POLE 1
      *
           IF  EX00-CD-RES = 7
               IF (EX00-INT-CD-RES   = 'AR'
               AND EX00-INT-QUA-APP  = 'CT')
               OR (EX00-CD-MATRIC    = '133970'
                                    OR '146130'
                                    OR '146410'
                                    OR '146540'
                                    OR '146760')
                   CONTINUE
               ELSE
                   IF EX00-CD-CA = '12'
                   OR EX00-CA-CODE = 12
                      MOVE '16'                   TO EX00-CD-CA
                      MOVE  16                    TO EX00-CA-CODE
                   ELSE
                      IF EX00-CD-CA = '13'
                      OR EX00-CA-CODE = 13
                         MOVE '15'                   TO EX00-CD-CA
                         MOVE  15                    TO EX00-CA-CODE
                      END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF  EX00-CD-RES = 6
               IF EX00-CD-CA = '06'
               OR EX00-CA-CODE = 6
                  MOVE '07'                   TO EX00-CD-CA
                  MOVE  7                     TO EX00-CA-CODE
               END-IF
           END-IF
      *
           IF  WSTOP = ZERO
                   CALL NNPHA13                USING EX00-ENSOR
                                                     EX24-ENSOR
      *
      *        SI OK, POUR LE RESEAU COURTAGE, LA COMPAGNIE D'ORIGINE
      *        EST ALZ POUR LE POLE 1 ET AGF POUR LE POLE 2
      *
                   IF  EX00-RETCODE NOT = 'OK'
                       MOVE 1                     TO WSTOP
                   ELSE
      *   BIDOUILLE PAS TRES BELLE POUR FORCER LA LOC POUR LES
      *   AGENTS CIBLES. HORCADA - VR59
                       IF EX00-CD-RES = 2
                       AND EX00-INT-TOP-CIBLE = '1'
                          MOVE '316' TO EX00-CA-LOC-ADM
                       END-IF
                   END-IF
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   INITIALISATION DES ZONES EN SORTIE
      *   CONTROLE DE LA DATE : SI ELLE N'EST PAS RENSEIGNE,
      *   L'HABILLAGE SERA A LA DATE DU JOUR
      *----------------------------------------------------------------*
       INITIALISATION.
      *
           MOVE ZERO                             TO WSTOP
           MOVE ZERO                             TO WEC54
           MOVE SPACES                           TO EX00-SORTIE
           MOVE ZERO                             TO EX00-ERRCODE
                                                    EX00-CA-CODE
                                                    EX00-RIB-NB
                                                    EX00-RIB-TYPE (01)
                                                    EX00-RIB-TYPE (02)
                                                    EX00-RIB-TYPE (03)
                                                    EX00-RIB-TYPE (04)
                                                    EX00-RIB-TYPE (05)
                                                    EX00-RIB-TYPE (06)
                                                    EX00-RIB-TYPE (07)
                                                    EX00-RIB-TYPE (08)
                                                    EX00-RIB-TYPE (09)
                                                    EX00-RIB-TYPE (10)
                                                    EX00-RIB-TYPE (11)
                                                    EX00-RIB-TYPE (12)
                                                    EX00-RIB-TYPE (13)
                                                    EX00-RIB-TYPE (14)
                                                    EX00-RIB-TYPE (15)
                                                    EX00-RIB-TYPE (16)
                                                    EX00-RIB-TYPE (17)
                                                    EX00-RIB-TYPE (18)
                                                    EX00-RIB-TYPE (19)
                                                    EX00-RIB-TYPE (20)
                                                    EX00-RIB-TYPE (21)
                                                    EX00-RIB-TYPE (22)
                                                    EX00-RIB-TYPE (23)
                                                    EX00-RIB-TYPE (24)
                                                    EX00-RIB-TYPE (25)
                                                    EX00-RIB-TYPE (26)
                                                    EX00-RIB-TYPE (27)
                                                    EX00-RIB-TYPE (28)
                                                    EX00-RIB-TYPE (29)
                                                    EX00-RIB-TYPE (30)
                                                    EX00-INT1ENR
                                                    EX00-INT1SOL
                                                    EX00-INT1DIF
                                                    EX00-INT1DFG
                                                    EX00-INT2TAU
           MOVE 'OK'                             TO EX00-RETCODE
           MOVE SPACES                           TO SAUVE-CD-ACTION
           MOVE EX00-NU-RCC                      TO W-NU-RCC
           MOVE '1'                              TO W-TOP-CDA
           MOVE FUNCTION CURRENT-DATE (1:8) TO W-DATEJOUR
      *
           IF   EX00-DATE = SPACES OR '00010101'
                MOVE FUNCTION CURRENT-DATE (1:8) TO EX00-DATE
                                                    W-DATE-JOUR
           ELSE
                MOVE EX00-DATE                   TO W-DATE-JOUR
           END-IF
           IF EX00-CD-RES = 1 OR 4
           THEN
              IF EX00-DATE < W-DATEJOUR
              THEN
                MOVE FUNCTION CURRENT-DATE (1:8) TO EX00-DATE
                                                    W-DATE-JOUR
              END-IF
           END-IF
           MOVE SPACE  TO EX00-CD-TRANSCO
      *    SI  LA DATE D'HABILLAGE ( EX00-DATE )
      *    EST EGALE A  DATE DES NOUVELLLES
      *    STRUCTURES (W-DATE-STRUCT) AAAAMM
      *    ET DATE DU TRAITEMENT (W-DATEJOUR)
      *    INFERIEURE A DATE STRUCTURE
           IF EX00-DATE (1:6) = W-DATE-STRUCT-AAAAMM
           AND W-DATEJOUR <= W-DATE-STRUCT
           THEN
                MOVE 'O' TO EX00-CD-TRANSCO
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   CHARGEMENT EN MEMOIRE DES INFOS POUR LES TRAITEMENTS
      *   GROS BATCH
      *----------------------------------------------------------------*
       TRAIT-INIT.
      *
      *    APPEL AU MODULE GCP2EX24 POUR ACQUISITION DE L'ESPACE MEMOIRE
      *
           PERFORM ACQUIS-MEM
      *
      *    MISE EN MEMOIRE DES INFOS DU CENTRE ADMINISTRATIF
      *
           PERFORM TRAIT-GCA
      *
      *    MISE EN MEMOIRE DES NIVEAUX HIERARCHIQUES POUR LES
      *    RESEAUX SALARIES (AF ET GA)
      *
           IF  WSTOP = ZERO
               PERFORM TRAIT-AF
           END-IF
      *
      *    MISE EN MEMOIRE DES NIVEAUX HIERARCHIQUES POUR LES
      *    RESEAUX AGC (AG, MA, GP ET CT)
      *
           IF  WSTOP = ZERO
               PERFORM TRAIT-AGC
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   CONTROLE DE PRESENCE, DE COHERENCE ET DE VALIDITE
      *   DES PARAMETRES D' ENTREE
      *----------------------------------------------------------------*
       CONTROLE-PARAM.
      *
           INITIALIZE W-LIBELLE  W-LIBX
      *
      *    CODE ACTION OBLIGATOIRE EN ENTREE
      *
           IF EX00-CD-MATRIC = LOW-VALUE
           THEN
               MOVE SPACES TO EX00-CD-MATRIC
           END-IF
           IF EX00-CD-INTERMED = LOW-VALUE
           THEN
               MOVE SPACES TO EX00-CD-INTERMED
           END-IF
           IF EX00-CD-ACTION NOT > SPACES
              MOVE 'CODE ACTION NON RENSEIGNE'   TO W-LIB1
              MOVE EX00-CD-ACTION                TO W-LIB2
              MOVE W-LIBELLE                     TO EX00-ERRCOM2
              PERFORM ANOMALIE-4008
           END-IF
      *
      *    TEST DE VALIDITE SUR LE MODE D'APPEL
      *
           IF EX00-CD-ACTION NOT = 'A' AND 'B' AND 'C' AND 'D' AND 'E'
                               AND 'F' AND 'G' AND 'H' AND 'I' AND 'J'
                               AND 'K' AND 'L'
              MOVE 'CODE ACTION NON VALIDE'      TO W-LIB1
              MOVE EX00-CD-ACTION                TO W-LIB2
              MOVE W-LIBELLE                     TO EX00-ERRCOM2
              PERFORM ANOMALIE-4008
           END-IF
      *
      *    POUR AF SI LE NUMERO DE RCC EST RENSEIGNE, IL FAUT PRECISER
      *    LE TYPE DE RCC
      *
      *    IF ( EX00-NU-RCC > SPACES ) AND ( EX00-CD-RES = 1 OR 3 )
      *       IF EX00-TYP-RCC NOT = '1' AND '2' AND '3'
      *          MOVE 'TYPE RCC NON VALIDE :'    TO W-LIB1
      *          MOVE EX00-TYP-RCC               TO W-LIB2
      *          MOVE W-LIBELLE                  TO EX00-ERRCOM2
      *          PERFORM ANOMALIE-4008
      *       END-IF
      *    END-IF
      *
      *    CODE RESEAU OBLIGATOIRE SI CODE SITE AGENDA NON ALIMENTE
      *
           IF  EX00-CD-RES = ZERO
             IF  EX00-CD-SITAG = SPACE AND EX00-NU-RCC = SPACE
               MOVE 'CODE RESEAU OBLIGATOIRE :'  TO W-LIB1
               MOVE EX00-CD-RES                  TO W-LIB3
               MOVE W-LIBELLE                    TO EX00-ERRCOM2
               PERFORM ANOMALIE-4008
             END-IF
           END-IF
      *
      *    TEST DE VALIDITE DU CODE RESEAU
      *
           IF  EX00-CD-RES > ZERO
           AND EX00-CD-RES NOT = 1 AND 2 AND 3 AND 4 AND 6 AND 7 AND 9
                                   AND 12 AND 13
               MOVE 'CODE RESEAU INCONNU :'      TO W-LIB1
               MOVE EX00-CD-RES                  TO W-LIB3
               MOVE W-LIBELLE                    TO EX00-ERRCOM2
               PERFORM ANOMALIE-4008
           END-IF
      *
      *    POUR LE CODE ACTION H ( INTERMEDIAIRE POUR UNE PROPO )
      *    LE CODE MATRICULE EST OBLIGATOIRE
      *
           IF    EX00-CD-ACTION = 'H'
           AND ( EX00-CD-MATRIC   NOT > SPACES  AND
                 EX00-CD-INTERMED NOT > SPACES )
                 MOVE 'MATRICULE OU INTERMEDIAIRE OBLIGATOIRE '
                                                 TO EX00-ERRCOM2 (1:39)
                 MOVE 'POUR CE CODE ACTION'      TO EX00-ERRCOM2 (40:21)
                 PERFORM ANOMALIE-4008
           END-IF
      *
      *    POUR LE CODE COMPTABLE RESEAU 2 SEULEMENT            )
      *
      *
           IF EX00-CD-CPT-AGE  > SPACES
           THEN
              IF  EX00-CD-RES NOT = 2
              THEN
              MOVE 'CODE RESEAU DOIT ETRE 2  '   TO W-LIB1
              MOVE EX00-CD-RES                   TO W-LIB2
              MOVE W-LIBELLE                     TO EX00-ERRCOM2
              PERFORM ANOMALIE-4008
              END-IF
              IF  EX00-CD-INTERMED NOT = SPACES
              OR  EX00-CD-MATRIC   NOT = SPACES
              OR  EX00-NU-RCC      NOT = SPACES
              THEN
              MOVE 'NE SAISIR QUE LE CODE COMPTABLE'   TO W-LIB1
              MOVE ' '                           TO W-LIB2
              MOVE W-LIBELLE                     TO EX00-ERRCOM2
              PERFORM ANOMALIE-4008
              END-IF
              MOVE EX00-CD-CPT-AGE TO W-CPTAGE
              IF  DEB-CAR-CPTAGE = "0"
              THEN
              MOVE SPACES            TO EX00-CD-CPT-AGE
              MOVE FIN-CAR-CPTAGE   TO EX00-CD-CPT-AGE
              END-IF
           END-IF
      *VR79 APS DANS GCP ; MODIFICATION CONTROLE
      * 1) SI CODE RESEAU = 7 OU 6 ET SI INTERMEDIAIRE RENSEIGNE ON LE
      *  MET DANS MATRICULE
           IF EX00-CD-RES = 7 OR 6
           THEN
              IF EX00-CD-MATRIC = SPACES
              AND EX00-CD-INTERMED NOT = SPACES
              THEN
                MOVE EX00-CD-INTERMED TO EX00-CD-MATRIC
                MOVE SPACES TO EX00-CD-INTERMED
              END-IF
             END-IF
           .
      *
      *----------------------------------------------------------------*
      *   APPEL DU MODULE GCP2EX24 POUR ACQUISITION DE LA MEMOIRE      *
      *   POUR LE STOCKAGE DES DONNEES DANS LES TRAITEMENTS GROS BATCH *
      *----------------------------------------------------------------*
       ACQUIS-MEM.
      *
           INITIALIZE                            EX24-ENTREE
           MOVE 'ACQ'                         TO EX24E-FCT
      *
           CALL GCP2EX24                   USING EX24-ENSOR
      *
           IF EX24S-RETCODE NOT = 'OK'
              MOVE 1                          TO WSTOP
              MOVE EX24S-RETCODE              TO EX00-RETCODE
              MOVE EX24S-SYSTEME              TO EX00-SYSTEME
              MOVE EX24S-ERRCODE              TO EX00-ERRCODE
              MOVE EX24S-ERRCOM1              TO EX00-ERRCOM1
              MOVE EX24S-ERRCOM2              TO EX00-ERRCOM2
              MOVE EX24S-ERRCOM3              TO EX00-ERRCOM3
              MOVE EX24S-ERRCOM4              TO EX00-ERRCOM4
           ELSE
              MOVE EX24E-PTC                  TO EX00-PTC
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   CREATION D'UN HEAP POUR ACQUERIR
      *   DE LA MEMOIRE ADRESSE EN DESSOUS DES 16 MGBYTES
      *----------------------------------------------------------------*
       TRT-GET-MEMOIRE.
      *
           IF W-ANA-CODE = 'A'
           THEN
           MOVE LENGTH OF ANA-ENSOR           TO L-MEM
           ELSE
           MOVE LENGTH OF ANAB-ENSOR           TO L-MEM
           END-IF
           MOVE 'A'                           TO ACTION
           MOVE '1'                           TO NB
           MOVE '<'                           TO LOCALI

           CALL 'NAPSTORG'                 USING DEMANDE  PARAM

           IF RETURN-CODE NOT = ZERO
              MOVE 'NAPSTORG'                 TO EX00-ERRCOM1
              MOVE 'PB ACQUISITION MEM'       TO EX00-ERRCOM3
              MOVE RETURN-CODE                TO EX00-ERRCOM4
              PERFORM ANOMALIE
           ELSE
           IF W-ANA-CODE = 'A'
           THEN
              SET ADDRESS OF ANA-ENSOR        TO P-MEM
           ELSE
              SET ADDRESS OF ANAB-ENSOR        TO P-MEM
           END-IF
           END-IF
           .
      *
      *-------------------------------------------------------------*
      *  LIBERATION DE LA MEMOIRE RESERVEE
      *-------------------------------------------------------------*
       TRT-FREE-MEMOIRE.
      *
           IF W-ANA-CODE = 'A'
           THEN
           MOVE LENGTH OF ANA-ENSOR           TO L-MEM
           ELSE
           MOVE LENGTH OF ANAB-ENSOR           TO L-MEM
           END-IF

           MOVE 'L'                           TO ACTION
           MOVE '1'                           TO NB
           MOVE '<'                           TO LOCALI

           CALL 'NAPSTORG'                 USING DEMANDE  PARAM

           IF RETURN-CODE NOT = ZERO
              MOVE 'NAPSTORG'                 TO EX00-ERRCOM1
              MOVE 'PB LIBERATION MEM'        TO EX00-ERRCOM3
              MOVE RETURN-CODE                TO EX00-ERRCOM4
              PERFORM ANOMALIE
           END-IF
           .
      *
      *----------------------------------------------------------------*
      *   INSERTION D'UNE ANOMALIE SUR LES PARAMETRES D'ENTREE
      *----------------------------------------------------------------*
       ANOMALIE-4008.
      *
           MOVE 1                             TO WSTOP
           MOVE 'KO'                          TO EX00-RETCODE
           MOVE 'GC'                          TO EX00-SYSTEME
           MOVE 4008                          TO EX00-ERRCODE
           MOVE 'GCP2EX00'                    TO EX00-ERRCOM1
           .
      *
      *----------------------------------------------------------------*
      *   INSERTION D'UNE ANOMALIE
      *----------------------------------------------------------------*
       ANOMALIE.
      *
           MOVE 1                             TO WSTOP
           MOVE 'KO'                          TO EX00-RETCODE
           MOVE 'GC'                          TO EX00-SYSTEME
           MOVE 4003                          TO EX00-ERRCODE
           MOVE 'GCP2EX00'                    TO EX00-ERRCOM2
           .
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *--> RECHERCHE ADRESSE EMAIL                                     *
      *----------------------------------------------------------------*
       TRAIT-ADMAIL.
      ****************

      *--> VALORISATION DES PARAMETRES EN ENTREE DU SERVICE
           MOVE SPACES                             TO WS-ADMAIL
           MOVE SPACES                             TO YPMDIC3B-CRIT
           MOVE SPACES                             TO YPMDIC3B-ENT
           MOVE  2                                 TO YPMDIC3B-ACT
           MOVE 'P'                                TO YPMDIC3B-TYP

           EVALUATE EX00-CD-RES
              WHEN 1
              WHEN 4
      *--> RESEAU AF ET MA
                   MOVE  EX00-SAL-MEMOID           TO YPMDIC3B-ENT
              WHEN 2
      *--> RESEAU AG
                      IF EX00-INT-AG-CODE(1:1) = '0'
                          MOVE 'H'                 TO YPMDIC3B-ENT(1:1)
                          MOVE EX00-INT-AG-CODE(2:5)
                                                   TO YPMDIC3B-ENT(2:5)
                          MOVE '1'                 TO YPMDIC3B-ENT(7:1)
                      ELSE
                       MOVE EX00-INT-AG-CODE       TO YPMDIC3B-ENT(1:6)
                       MOVE '1'                    TO YPMDIC3B-ENT(7:1)
                      END-IF
           END-EVALUATE

           IF YPMDIC3B-ENT NOT = SPACES
      *--> APPEL DU SERVICE YPPDIC3B

           CALL YPPDIC3B                USING YPMDIC3B-CRIT

           MOVE YPMDIC3B-SORT              TO ZONE-SORTIE-YPMDIC3B

           IF YPMDIC3B-CR  = '+000'
                 MOVE ADRDEF                  TO WS-ADMAIL
           END-IF

           EVALUATE EX00-CD-RES
              WHEN 1
              WHEN 4
      *--> RESEAU AF ET MA
ANA001             IF EX00-CD-CAC(1:1) = '4'
ANA001               MOVE 'afs2-conseil@allianz.fr' TO EX00-SAL-ADMAIL
ANA001             ELSE
                      MOVE WS-ADMAIL TO EX00-SAL-ADMAIL
ANA001             END-IF
              WHEN 2
                   MOVE  WS-ADMAIL  TO EX00-INT-ADMAIL
      *--> RESEAU AG
           END-EVALUATE
           END-IF
           .