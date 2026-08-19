COND  *-------------------------------------------------------------
COND   >>IF TYPEPROG = 'CICS'
COND  * c'est un programme CICS
COND    COPY CONDCICS.
COND   >>END-IF
COND   >>IF TYPEPROG = 'BATCH'
COND  * c'est un programme BATCH
COND    COPY CONDBAT.
COND   >>END-IF
      * S'il n'y a pas de commentaire batch ou CICS au dessus alors bug
      *-------------------------------------------------------------