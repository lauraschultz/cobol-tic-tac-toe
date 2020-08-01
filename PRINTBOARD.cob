       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. PRINTBOARD.
DATA DIVISION.
WORKING-STORAGE SECTION.
       01 Temp PIC 9.
       01 Idx PIC 9.
LINKAGE SECTION.
       01 BoardTable.
           02 BoardValue PIC 9 OCCURS 9 TIMES INDEXED BY I.

PROCEDURE DIVISION USING BoardTable.
       PERFORM PrintBox VARYING I FROM 1 BY 1 UNTIL I=9.
    *>    EXIT PROGRAM.
       
       PrintBox.
        *>    DISPLAY 'HELLO, I IS ' I
           MOVE I TO Idx
           COMPUTE Temp = FUNCTION MOD(I 3).
           IF BoardValue(I) = 0
            *>    nothing in square
            DISPLAY " [" Idx "] " WITH NO ADVANCING
           END-IF
           IF BoardValue(I) = 1
            *>    X in square
            DISPLAY "  X  " WITH NO ADVANCING
           END-IF
           IF BoardValue(I) = 2
            *>    O in square
            DISPLAY "  O  " WITH NO ADVANCING
           END-IF
           IF Temp NOT = 0
               DISPLAY "|" WITH NO ADVANCING
           END-IF
           IF Temp = 0 AND I<9
               DISPLAY " "
               DISPLAY "----------------"
           END-IF.
        *>    DISPLAY "END OF PRINTBOX".
       

