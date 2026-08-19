      ******************************************************************
      *                                                                *
      *     PROGRAMME : GBDJCH02      DATE DE CREATION : 22/04/2026    *
      *-----------------------------------------------------------------
      * DATE       ! INTERVENANT ! MODIFICATIONS EFFECTUEES
      *-----------------------------------------------------------------
      * 04/03/2026 ! JC HEIN     ! Tests COPY
      *-----------------------------------------------------------------
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GBDJCH02.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
        COPY FVINSBAP.
        COPY ESA48751.
        COPY ESA49751.
        COPY ESA49771.
        COPY ESM05091.
        COPY FVREGLEP.
        COPY HOLDTB.
      *
      * ---------------------------------------------------------------*
      *                      PROCEDURE DIVISION
      * ---------------------------------------------------------------*
       PROCEDURE DIVISION.
      *    Initialisation
           PERFORM INITIALISATIONS
      *    Traitement principal
           PERFORM TRAITEMENT
      *    Fin du programme
           PERFORM FIN-TRAITEMENT
      *
           STOP RUN
      *
           .
      *
      *----------------------------------------------------------------*
      * Initialisation des variables                                   *
      *----------------------------------------------------------------*
       INITIALISATIONS.
      *---------------*
           DISPLAY 'Initialisation'
           DISPLAY 'Test VSCODE JCH'
      *
           .
      *
      *----------------------------------------------------------------*
      * Traitement principal                                           *
      *----------------------------------------------------------------*
       TRAITEMENT.
      *----------*
      *
           DISPLAY 'Traitement principal'
           DISPLAY 'Test COPY'
      *
      *     MOVE 'VAR1'                      TO VARS-VAR1
      *     MOVE 'VAR2'                      TO VARS-VAR2
           .
      *
      *----------------------------------------------------------------*
      * Fin du traitment                                               *
      *----------------------------------------------------------------*
       FIN-TRAITEMENT.
      *--------------*
           DISPLAY '*====================================*'
           DISPLAY '*       FIN TRAITEMENT GBDJCH02      *'
           DISPLAY '*====================================*'
           .