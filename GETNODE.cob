       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. GETNODE RECURSIVE.
ENVIRONMENT DIVISION.
DATA DIVISION.
LOCAL-STORAGE SECTION.
       01 MinMax PIC 9.
       01 CNodeVal PIC 9.
       01 NewDepth PIC 9.
       01 NP PIC 9.
       01 Winner PIC 9 VALUE 0.
           88 IsWinner VALUE 1 OR 2.
       01 MaxDepth PIC 9 VALUE 3.
       01 NCPlayer PIC 9.

LINKAGE SECTION.
       01 BoardTable.
           02 BoardValue PIC 9 OCCURS 9 TIMES INDEXED BY I.
       01 Depth PIC 9.
       01 NodeValue PIC S9.
       01 CPlayer PIC 9.
       01 Pos PIC 9.

PROCEDURE DIVISION USING BoardTable, Depth, NodeValue, CPlayer, Pos.
       DISPLAY "STARTING FN: " BoardTable
    *>    DISPLAY "BOARD IS " BoardTable
       CALL 'COMPUTEWINNER' USING BoardTable, Winner
       IF IsWinner OR Depth = MaxDepth THEN *> base cases
        *>    DISPLAY "depth is " Depth
           IF Winner = 2
               MOVE 1 TO NodeValue
           END-IF
           IF Winner = 1
               MOVE -1 TO NodeValue
           END-IF
           IF NOT IsWinner
               MOVE 0 TO NodeValue
           END-IF
        *>    DISPLAY "BASE CASE, NODEVAL IS " NodeValue 
           GOBACK.
                COMPUTE NewDepth = Depth + 1
                   IF CPlayer=1 THEN
                       MOVE -2 TO MinMax
                   ELSE 
                       MOVE 2 TO MinMax
                   END-IF
                  *>    switches CPlayer 1 <=> 2
           COMPUTE NCPlayer = FUNCTION MOD(CPlayer 2) + 1

           PERFORM WITH TEST BEFORE VARYING I FROM 1 BY 1 UNTIL I>9
            *>    DISPLAY "Starting loop, I is " I " depth is " Depth
               IF BoardValue(I) = 0 *> space is empty
                   MOVE CPlayer TO BoardValue(I)
                *>    DISPLAY "the board is " BoardTable
                   CALL 'GETNODE' USING
                       BoardTable, NewDepth, CNodeVal, NCPlayer, NP
                   IF (CPlayer=1 AND CNodeVal > MinMax) OR
                       (CPlayer=2 AND CNodeVal < MinMax) THEN
                       MOVE CNodeVal TO MinMax
                       MOVE I TO Pos 
                    *>    DISPLAY "found new minmax: " MinMax ", " I
                   END-IF
                *>    DISPLAY "I is " I " boardvalue is " BoardValue(I)
                   MOVE ZERO TO BoardValue(I)
                *>    DISPLAY "the board is " BoardTable
               END-IF  
           END-PERFORM
           MOVE MinMax TO NodeValue
           DISPLAY "NODE DONE: " BoardTable " VALUE: " NodeValue
           GOBACK.
