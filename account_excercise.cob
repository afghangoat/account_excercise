      * 
      * 
      *


       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
    
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *  SELECT PRINT-LINE ASSIGN TO PRTLINE.
      *  SELECT ACCT-REC ASSIGN TO ACCTREC.
      
      * Read file from ACCTREC.DAT file and return the formatted output to the PRINT.DAT file.
      
      * Example acctrec.dat file:
      
      *ACCT-NO	12345678	Account number (8 characters).
      *ACCT-LIMIT	+10000.00	Credit limit (packed decimal).
      *ACCT-BALANCE	-1234.56	Account balance (packed decimal, negative).
      *LAST-NAME	Doe	Last name, padded with spaces.
      *FIRST-NAME	John	First name, padded with spaces.
      *RESERVED		Reserved, blank (7 spaces).
      *COUNTRY	USA	Country, padded with spaces.
      *COMMENTS	Overdue account.	Comments about the account.
      
         SELECT ACCT-REC ASSIGN TO "ACCTREC.DAT"
          ORGANIZATION IS LINE SEQUENTIAL.
         SELECT PRINT-LINE ASSIGN TO "PRINT.DAT"
          ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
        FD PRINT-LINE RECORDING MODE F.
         01 PRINT-REC.
          05 ACCT-NO-O PIC X(8).
          05 ACCT-LIMIT-O PIC $$,$$$,$$9.99.
          05 ACCT-BALANCE-O PIC $$,$$$,$$9.99.
          05 LAST-NAME-O PIC X(15).
          05 FIRST-NAME-O PIC X(15).
          05 COMMENTS-O PIC X(30).
        
        FD ACCT-REC RECORDING MODE F.
         01 ACCT-FIELDS.
          05 ACCT-NO PIC X(8).
          05 ACCT-LIMIT PIC S9(7)V99 COMP-3.
          05 ACCT-BALANCE PIC S9(7)V99 COMP-3.
          05 LAST-NAME PIC X(15).
          05 FIRST-NAME PIC X(15).
          05 RESERVED PIC X(7).
          05 COUNTRY PIC X(20).
          05 COMMENTS PIC X(30).
       
       WORKING-STORAGE SECTION.
      *   01 WS-NAME PIC A(6) VALUE IS "World".
      *   01 MEAL-COST PIC A(10)
        77 WHO PIC X(15).
        77 WHERE PIC X(20).
        77 WHY PIC X(30).
        77 RATE PIC 9(3).
        77 HOURS PIC 9(3).
        77 GROSS-PAY PIC 9(3).
        
        77 LASTREC PIC X(100).
        
       PROCEDURE DIVISION.
        
         
        MOVE "Captain COBOL" TO WHO.
        MOVE "ZedLand" TO WHERE.
        MOVE "To suffer." TO WHY.
        MOVE 19 TO HOURS.
        MOVE 23 TO RATE.
        
        COMPUTE GROSS-PAY = HOURS * RATE.
        
        DISPLAY "Name: " WHO.
        DISPLAY "Location: " WHERE.
        DISPLAY "Reason: " WHY.
        DISPLAY "Worked: " HOURS.
        DISPLAY "Hourly rate: " RATE.
        DISPLAY "Salary: " GROSS-PAY.
        
      *  WRITE-RECORD.
      *   MOVE ACCT-NO TO ACCT-NO-O.
      *   MOVE ACCT-LIMIT TO ACCT-LIMIT-O.
      *   MOVE ACCT-BALANCE TO ACCT-BALANCE-O.
      *   MOVE FIRST-NAME TO FIRST-NAME-O.
      *   MOVE LAST-NAME TO LAST-NAME-O.
      *   MOVE COMMENTS TO COMMENTS-O.
      *   WRITE PRINT-REC.
      *   
      *  READ-RECORD.
      *   READ ACCT-REC.
      *   AT END MOVE 'Y' TO LASTREC.
      *   END-READ.
      *   
      *  READ-NEXT-RECORD.
      *   PERFORM READ-RECORD
      *    PERFORM UNTIL LASTREC = 'Y'
      *    PERFORM WRITE-RECORD
      *    PERFORM READ-RECORD
      *   END PERFORM
      
        BEGIN.
         OPEN INPUT ACCT-REC
         OPEN OUTPUT PRINT-LINE
         PERFORM PROCESS-RECORDS
         PERFORM CLOSE-STOP
         STOP RUN.

        PROCESS-RECORDS.
         PERFORM UNTIL LASTREC = 'Y'
          READ ACCT-REC
           AT END MOVE 'Y' TO LASTREC
          NOT AT END
           MOVE ACCT-NO TO ACCT-NO-O
           MOVE ACCT-LIMIT TO ACCT-LIMIT-O
           MOVE ACCT-BALANCE TO ACCT-BALANCE-O
           MOVE FIRST-NAME TO FIRST-NAME-O
           MOVE LAST-NAME TO LAST-NAME-O
           MOVE COMMENTS TO COMMENTS-O
           WRITE PRINT-REC
          END-READ
         END-PERFORM.
        
      *  OPEN-FILES.
      *   OPEN INPUT ACCT-REC.
      *   OPEN OUTPUT PRINT-LINE.
        
        CLOSE-STOP.
         CLOSE ACCT-REC.
         CLOSE PRINT-LINE.
         STOP RUN.
