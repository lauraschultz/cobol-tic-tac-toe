       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. GETNODE RECURSIVE.
ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
       01 Winner PIC 9 VALUE 0.
           88 IsWinner VALUE 1 OR 2.
       01 MaxDepth PIC 9 VALUE 2.
       01 CurrentMax PIC S9 VALUE -2.
       01 CNodeVal PIC 9.
       01 NewDepth PIC 9.

LINKAGE SECTION.
       01 BoardTable.
           02 BoardValue PIC 9 OCCURS 9 TIMES INDEXED BY I.
       01 Depth PIC 9.
       01 NodeValue PIC S9.
       01 CPlayer PIC 9.
       01 Pos PIC 9.

PROCEDURE DIVISION USING BoardTable, Depth, NodeValue, CPlayer, Pos.
       DISPLAY "STARTING FN, DEPTH IS " Depth " CPLAYER IS " CPlayer
       CALL 'COMPUTEWINNER' USING BoardTable, Winner
       IF IsWinner OR Depth = MaxDepth *> base cases
           DISPLAY "depth is " Depth
           IF Winner = 2
               MOVE 1 TO NodeValue
           END-IF
           IF Winner = 1
               MOVE -1 TO NodeValue
           END-IF
           IF NOT IsWinner
               MOVE 0 TO NodeValue
           END-IF
           GOBACK
       ELSE
    *>    DISPLAY "HERE"
           PERFORM WITH TEST AFTER VARYING I FROM 1 BY 1 UNTIL I>9
               DISPLAY "Starting loop, I is " I
               IF BoardValue(I) = 0 *> space is empty
                   DISPLAY "EMPTY SPACE, I IS " I
                   MOVE CPlayer TO BoardValue(I)
                   COMPUTE NewDepth = Depth + 1
                   *>    switches CPlayer 1 <=> 2
                   COMPUTE CPlayer = FUNCTION MOD(CPlayer 2) + 1
                   CALL 'GETNODE' USING
                       BoardTable, NewDepth, CNodeVal, CPlayer, I
                   IF CNodeVal > CurrentMax
                       MOVE CNodeVal TO CurrentMax
                       MOVE I TO Pos
                       DISPLAY "found new max node: " CNodeVal
                   END-IF
                   MOVE 0 TO BoardValue(I)
               END-IF   
           END-PERFORM
           MOVE CurrentMax TO NodeValue
           DISPLAY "HERE"
       END-IF.
