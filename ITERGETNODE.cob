       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. ITERGETNODE.
DATA DIVISION.
WORKING-STORAGE SECTION.
       01 Winner PIC 9 VALUE 0.
           88 IsWinner VALUE 1 OR 2.
LOCAL-STORAGE SECTION.
       01 Stack.
           02 StackE OCCURS 70 TIMES INDEXED BY StackP.
               03 BoardTableE.
                   04 BoardValueE PIC 9 OCCURS 9 TIMES INDEXED BY J.
               03 NodeVal PIC S9.
               03 ParentP PIC 99.
               03 Depth PIC 9.
               03 Pos PIC 9.
               03 ChildPos PIC 9.
               03 Visited PIC 9 VALUE 0.
                   88 NotVisited VALUE 0.
       01 MaxDepth PIC 9 VALUE 2.
       01 ChDepth PIC 9.
       01 ParentRef PIC 99.

LINKAGE SECTION.
       01 BoardTable.
           02 BoardValue PIC 9 OCCURS 9 TIMES.
       01 FPos PIC 9.

PROCEDURE DIVISION USING BoardTable, FPos.
       MOVE 1 TO StackP
       MOVE BoardTable TO BoardTableE(StackP)
       MOVE 0 TO Depth(StackP)
       MOVE -2 TO NodeVal(StackP)
       PERFORM WITH TEST AFTER UNTIL StackP = 1
           CALL 'COMPUTEWINNER' USING BoardTableE(StackP), Winner
        *>    DISPLAY "compute winner with " BoardTableE(StackP)": " Winner
           MOVE ParentP(StackP) TO ParentRef
           IF IsWinner OR Depth(StackP) = MaxDepth *> base cases
               PERFORM UpdateSelf
               PERFORM UpdateParent 
               COMPUTE StackP = StackP - 1
           ELSE
               IF NotVisited(StackP)
                   PERFORM AddChildren
               ELSE
                   PERFORM UpdateParent
                   COMPUTE StackP = StackP - 1
               END-IF
           END-IF
       END-PERFORM
       MOVE ChildPos(StackP) TO FPos
       EXIT PROGRAM.

       UpdateSelf.
           IF Winner = 2
               MOVE 1 TO NodeVal(StackP)
           END-IF
           IF Winner = 1
               MOVE -1 TO NodeVal(StackP)
           END-IF
           IF NOT IsWinner
               MOVE 0 TO NodeVal(StackP)
           END-IF.

       UpdateParent. 
            IF (FUNCTION MOD(Depth(ParentRef) 2) = 0 AND
                NodeVal(ParentRef) < NodeVal(StackP))
                OR
                (FUNCTION MOD(Depth(ParentRef) 2) = 1 AND
                NodeVal(ParentRef) > NodeVal(StackP))
                MOVE NodeVal(StackP) TO NodeVal(ParentRef)
                MOVE Pos(StackP) TO ChildPos(ParentRef)
                COMPUTE Visited(ParentRef) = 1
             END-IF.
             
       AddChildren.
    *>    adds all children of current node to stack
           MOVE StackP TO ParentRef
           COMPUTE ChDepth = Depth(StackP) + 1
           PERFORM WITH TEST BEFORE VARYING J FROM 1 BY 1 UNTIL J>9
               IF BoardValueE(StackP, J) = 0
                   COMPUTE StackP = StackP + 1
                   MOVE BoardTableE(ParentRef) TO BoardTableE(StackP)
                   IF FUNCTION MOD(ChDepth 2) = 0 *> HUMAN PLAYING
                       MOVE -2 TO NodeVal(StackP)
                       MOVE 1 TO BoardValueE(StackP, J)
                   ELSE *> COMPUTER PLAYING
                       MOVE 2 TO NodeVal(StackP)
                       MOVE 2 TO BoardValueE(StackP, J)
                   END-IF
                   MOVE ChDepth TO Depth(StackP)
                   MOVE ParentRef TO ParentP(StackP)
                   MOVE J TO Pos(StackP)
               END-IF
           END-PERFORM.
