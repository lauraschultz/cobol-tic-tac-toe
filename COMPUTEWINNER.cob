       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. COMPUTEWINNER.
DATA DIVISION.
LOCAL-STORAGE SECTION.
       01 LWinner PIC 9 VALUE 0.
LINKAGE SECTION.
       01 BoardTable.
           02 BoardValue PIC 9 OCCURS 9 TIMES INDEXED BY I.
       01 Winner PIC 9.

PROCEDURE DIVISION USING BoardTable, Winner.
    *>    check horizontal
       IF BoardValue(1) = BoardValue(2) AND BoardValue(2) = BoardValue(3)
           AND BoardValue(1) NOT = 0
           MOVE BoardValue(1) TO LWinner
       END-IF
       IF BoardValue(4) = BoardValue(5) AND BoardValue(5) = BoardValue(6)
           AND BoardValue(4) NOT = 0
           MOVE BoardValue(4) TO LWinner
       END-IF
       IF BoardValue(7) = BoardValue(8) AND BoardValue(8) = BoardValue(9)
           AND BoardValue(7) NOT = 0
           MOVE BoardValue(7) TO LWinner
       END-IF
    *>    check vertical
       IF BoardValue(1) = BoardValue(4) AND BoardValue(4) = BoardValue(7)
           AND BoardValue(1) NOT = 0
           MOVE BoardValue(1) TO LWinner
       END-IF
       IF BoardValue(2) = BoardValue(5) AND BoardValue(5) = BoardValue(8)
           AND BoardValue(2) NOT = 0
           MOVE BoardValue(2) TO LWinner
       END-IF
       IF BoardValue(3) = BoardValue(6) AND BoardValue(6) = BoardValue(9)
           AND BoardValue(3) NOT = 0
           MOVE BoardValue(3) TO LWinner
       END-IF
    *>    check diagonal
       IF BoardValue(1) = BoardValue(5) AND BoardValue(5) = BoardValue(9)
           AND BoardValue(1) NOT = 0
           MOVE BoardValue(1) TO LWinner
       END-IF
       IF BoardValue(3) = BoardValue(5) AND BoardValue(5) = BoardValue(7)
           AND BoardValue(3) NOT = 0
           MOVE BoardValue(3) TO LWinner
       END-IF
       MOVE LWinner TO Winner.
