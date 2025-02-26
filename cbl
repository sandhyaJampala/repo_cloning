       IDENTIFICATION DIVISION.                                         06000000
       PROGRAM-ID. ACCT00.                                              12000000
       REMARKS. THIS PROGRAM IS THE FIRST INVOKED BY THE 'ACCT'         18000000
                TRANSACTION.  IT DISPLAYS A MENU SCREEN FOR THE ON-LINE 24000000
                ACCOUNT FILE APPLICATION, WHICH PROMPTS THE USER FOR    30000000
                INPUT.  TRANSACTION 'AC01' IS INVOKED WHEN THAT INPUT   36000000
                IS RECEIVED.                                            42000000
       ENVIRONMENT DIVISION.                                            48000000
       DATA DIVISION.                                                   54000000
       PROCEDURE DIVISION.                                              60000000
       INITIAL-MAP.                                                     66000000
           EXEC CICS SEND MAP('ACCTMNU') MAPSET('ACCTSET') FREEKB       72000000
                ERASE MAPONLY END-EXEC.                                 79000000
           EXEC CICS RETURN TRANSID('AC01') END-EXEC.                   86000000
           GOBACK.                                                      93000000
