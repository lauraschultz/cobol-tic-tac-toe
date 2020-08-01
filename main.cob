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

    PROCEDURE DIVISION.
       MOVE 000000000 TO BoardTable
       PERFORM UNTIL NOT IsValid
               CALL 'PRINTBOARD' USING BoardTable
               DISPLAY " "
               DISPLAY " "
               DISPLAY "Your move: " WITH NO ADVANCING
               ACCEPT UserInput
               MOVE UserInput TO I
               MOVE 1 TO BoardValue(I)
               CALL 'COMPUTERMOVE' USING BoardTable
        END-PERFORM

    STOP RUN.
