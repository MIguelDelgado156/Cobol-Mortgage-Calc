       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INCOME ASSIGN TO 'input.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT OUTFILE ASSIGN TO 'output.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INCOME.
       01 INCOME-FILE.
         05 MID PIC 9(3).
         05 FILLER PIC X(1).
         05 NAME PIC A(14).
         05 FILLER PIC X(1).
         05 P PIC 9(6).
         05 FILLER PIC X(1).
         05 APR PIC S9(1)V9(2).
         05 FILLER PIC X(1).
         05 MONTH PIC 9(2).

       WORKING-STORAGE SECTION.
       01 WS-INCOME.
         05 WS-MID PIC 9(3).
         05 FILLER PIC X(1).
         05 WS-NAME PIC A(14).
         05 FILLER PIC X(1).
         05 WS-P PIC 9(6).
         05 FILLER PIC X(1).
         05 WS-APR PIC S9(1)V9(2).
         05 FILLER PIC X(1).
         05 WS-MONTH PIC 9(2).
       01 WS-MORT PIC 9(5).
       01 WS-OUTPUT PIC ZZZZ.ZZ.
       01 WS-EOF PIC A(1).

       PROCEDURE DIVISION.
           OPEN INPUT INCOME. 
           PERFORM UNTIL WS-EOF='Y'
               MULTIPLY WS-P BY WS-APR GIVING WS-MORT
               DIVIDE WS-MONTH INTO WS-MORT GIVING WS-OUTPUT
               IF WS-OUTPUT = SPACE THEN
                 CONTINUE
               ELSE
                 DISPLAY "Mortage for: " WS-NAME " is " WS-OUTPUT
               END-IF
             READ INCOME INTO WS-INCOME 
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END DISPLAY WS-INCOME
             END-READ
           END-PERFORM.
           CLOSE INCOME.
           STOP RUN.

