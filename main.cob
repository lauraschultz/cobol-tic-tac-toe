       >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. MAIN.
    ENVIRONMENT DIVISION.
    DATA DIVISION.
    WORKING-STORAGE SECTION.
       01 BoardTable.
           02 BoardValue PIC 9 OCCURS 9 TIMES INDEXED BY I.
       01 UserInput PIC 9 VALUE 1.
           88 IsValid VALUE 1 THRU 9.
       01 Winner PIC 9 VALUE 0.
           88 IsWinner VALUE 1 OR 2.
       01 NumMoves PIC 9 VALUE 0.

    PROCEDURE DIVISION.
       MOVE 000000000 TO BoardTable
       PERFORM UNTIL NOT IsValid OR IsWinner
               CALL 'PRINTBOARD' USING BoardTable
               DISPLAY " "
               DISPLAY " "
               DISPLAY "Your move: " WITH NO ADVANCING
               ACCEPT UserInput
               MOVE UserInput TO I
               MOVE 1 TO BoardValue(I)
               COMPUTE NumMoves = NumMoves + 1
               PERFORM CheckForWinner
               CALL 'COMPUTERMOVE' USING BoardTable
               COMPUTE NumMoves = NumMoves + 1
               PERFORM CheckForWinner
        END-PERFORM.

        CheckForWinner.
           CALL 'COMPUTEWINNER' USING BoardTable, Winner
           IF IsWinner OR NumMoves = 9
               CALL 'PRINTBOARD' USING BoardTable
               DISPLAY " "
               IF Winner = 0
                    DISPLAY "You tied!"
                END-IF
               IF Winner = 1
                   DISPLAY "Congrats, you won!"
                END-IF
                IF Winner = 2
                    DISPLAY "You lost, better luck next time!"
                END-IF
                STOP RUN
           END-IF.
