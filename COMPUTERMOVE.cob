       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. COMPUTERMOVE.
DATA DIVISION.
WORKING-STORAGE SECTION.
       01 CVal PIC 9.
           88 IsZero VALUE 0.
       01 WS-TIME PIC 9(8).
       01 Depth PIC 9 VALUE 0.
       01 NodeValue PIC S9 VALUE 0.
       01 CPlayer PIC 9 VALUE 2.
       01 Pos PIC 9.

LINKAGE SECTION.
       01 BoardTable.
           02 BoardValue PIC 9 OCCURS 9 TIMES INDEXED BY I.

PROCEDURE DIVISION USING BoardTable.
    *>    --- FIND FIRST AVAILABLE SPOT: ---
    *>    PERFORM WITH TEST AFTER VARYING I FROM 1 BY 1 UNTIL IsZero
    *>        MOVE BoardValue(I) TO CVal
    *>    END-PERFORM

    *>    --- FIND RANDOM POSITION, CHECK IF SPOT AVAILABLE: ---
    *>    ACCEPT WS-TIME FROM TIME
    *>    COMPUTE I = FUNCTION RANDOM(WS-TIME)
    *>    PERFORM WITH TEST AFTER UNTIL IsZero
    *>        COMPUTE I = FUNCTION RANDOM * 9 + 1
    *>        MOVE BoardValue(I) TO CVal
    *>    END-PERFORM

    *>     --- USE RECURSIVE LOOK AHEAD ALG: ---
           CALL 'GETNODE' USING
               BoardTable, Depth, NodeValue, CPlayer, Pos.
           DISPLAY "FINISHED, POS IS " Pos
           
       MOVE 2 TO BoardValue(Pos).        
