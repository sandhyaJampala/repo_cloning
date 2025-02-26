       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCT02.
       REMARKS. THIS PROGRAM IS THE FIRST INVOKED BY THE 'AC02'
                TRANSACTION.  IT COMPLETES REQUESTS FOR ACCOUNT FILE
                UPDATES (ADDS, MODIFIES, AND DELETES), AFTER THE USER
                ENTERED THE UPDATE INFORMATION.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MISC.
           02  MENU-MSGNO             PIC S9(4) COMP VALUE +1.
           02  DTL-MSGNO              PIC S9(4) COMP VALUE +0.
           02  ACCT-LNG               PIC S9(4) COMP VALUE +383.
           02  DTL-LNG                PIC S9(4) COMP VALUE +751.
           02  DUMMY                  PIC S9(4) COMP VALUE +128.
           02  FILLER REDEFINES DUMMY.
               04  FILLER             PIC X.
               04  HEX80              PIC X.
           02  ACIX-KEY               PIC X(12) VALUE SPACES.
           02  STARS                  PIC X(12) VALUE '************'.
           02  USE-QID.
               04  USE-QID1           PIC X(3) VALUE 'AC0'.
               04  USE-QID2           PIC X(5).
           02  USE-REC.
               04  USE-TERM           PIC X(4).
               04  USE-TIME           PIC S9(7) COMP-3.
               04  USE-DATE           PIC S9(7) COMP-3.
           02  USE-LNG                PIC S9(4) COMP VALUE +12.
           02  OLD-IXKEY.
               04  IXOLD-SNAME        PIC X(12).
           02  IXOLD-ACCT         PIC X(5).
           02  COMMAREA-FOR-ACCT04.
               04  ERR-PGRMID         PIC X(8) VALUE 'ACCT02'.
               04  ERR-FN             PIC X.
               04  ERR-RCODE          PIC X.
           02  PAY-INIT               PIC X(36) VALUE
                   '    0.00000000    0.00000000    0.00'.
           02  MENU-MSG-LIST.
               04  FILLER             PIC X(60) VALUE
                   'PREVIOUS REQUEST CANCELED AS REQUESTED'.
               04  FILLER             PIC X(60) VALUE
                   'REQUESTED ADDITION COMPLETED'.
               04  FILLER             PIC X(60) VALUE
                   'REQUESTED MODIFICATION COMPLETED'.
               04  FILLER             PIC X(60) VALUE
                   'REQUESTED DELETION COMPLETED'.
           02  MENU-MSG REDEFINES MENU-MSG-LIST PIC X(60) OCCURS 4.
           02  DTL-MSG-LIST.
               04  FILLER             PIC X(60) VALUE
                   'EITHER ENTER "Y" TO CONFIRM OR "CLEAR" TO CANCEL'.
               04  FILLER             PIC X(60) VALUE
               'YOUR REQUEST WAS INTERRUPTED; PLEASE CANCEL AND RETRY'.
               04  FILLER             PIC X(60) VALUE
               'CORRECT HIGHLIGHTED ITEMS (STARS MEAN ITEM REQUIRED)'.
               04  FILLER             PIC X(60) VALUE
               'USE ONLY "ENTER" (TO PROCEED) OR "CLEAR" (TO CANCEL)'.
               04  FILLER             PIC X(60) VALUE
               'MAKE SOME ENTRIES AND "ENTER" OR "CLEAR" TO CANCEL'.
           02  DTL-MSG REDEFINES DTL-MSG-LIST PIC X(60) OCCURS 5.
           02  MOD-LINE.
               04  FILLER             PIC X(25) VALUE
                   '==========> CHANGES TO:  '.
               04  MOD-NAME           PIC X(6) VALUE SPACES.
               04  MOD-TELE           PIC X(5) VALUE SPACES.
               04  MOD-ADDR           PIC X(6) VALUE SPACES.
               04  MOD-AUTH           PIC X(6) VALUE SPACES.
               04  MOD-CARD           PIC X(6) VALUE SPACES.
               04  MOD-CODE           PIC X(5) VALUE SPACES.
           02  UPDT-LINE.
               04  FILLER             PIC X(30) VALUE
                   '==========> UPDATED AT TERM:  '.
               04  UPDT-TERM          PIC X(4).
               04  FILLER             PIC X(6) VALUE '  AT  '.
               04  UPDT-TIME          PIC 9(7).
               04  FILLER             PIC X(6) VALUE '  ON  '.
               04  UPDT-DATE          PIC 9(7).
       01  NEW-ACCTREC. COPY ACCTREC.
       01  OLD-ACCTREC. COPY ACCTREC.
           COPY ACCTSET.
           COPY DFHAID.
           COPY DFHBMSCA.
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           02  REQC                   PIC X.
           02  ACCTC                  PIC X(5).
       PROCEDURE DIVISION.
           MOVE LOW-VALUES TO ACCTDTLI.
           MOVE SPACES TO OLD-ACCTREC, NEW-ACCTREC.
           EXEC CICS HANDLE AID CLEAR(CK-OWN) PA1(PA-KEY)
               PA2(PA-KEY) PA3(PA-KEY) END-EXEC.
           EXEC CICS HANDLE CONDITION QIDERR(NO-OWN)
               MAPFAIL(NO-MAP) ERROR(NO-GOOD) END-EXEC.
           EXEC CICS IGNORE CONDITION DUPKEY END-EXEC.
           EXEC CICS RECEIVE MAP('ACCTDTL') MAPSET('ACCTSET') END-EXEC.
           IF REQC NOT = 'A',
               EXEC CICS READ DATASET('ACCTFIL') INTO(OLD-ACCTREC)
                   RIDFLD(ACCTC) UPDATE LENGTH(ACCT-LNG) END-EXEC
               MOVE OLD-ACCTREC TO NEW-ACCTREC,
               MOVE SNAMEDO IN OLD-ACCTREC TO IXOLD-SNAME,
               MOVE ACCTC TO IXOLD-ACCT.
           IF REQC = 'X',
               IF VFYDI = 'Y', GO TO CK-OWN,
               ELSE MOVE -1 TO VFYDL, MOVE DFHUNIMD TO VFYDA,
                   MOVE 1 TO DTL-MSGNO,
                   GO TO INPUT-REDISPLAY.
           IF SNAMEDL > 0 MOVE SNAMEDI TO SNAMEDO IN NEW-ACCTREC.
           IF FNAMEDL > 0 MOVE FNAMEDI TO FNAMEDO IN NEW-ACCTREC.
           IF MIDL > 0 MOVE MIDI TO MIDO IN NEW-ACCTREC.
           IF TTLDL > 0 MOVE TTLDI TO TTLDO IN NEW-ACCTREC.
           IF TELDL > 0 MOVE TELDI TO TELDO IN NEW-ACCTREC.
           IF ADDR1DL > 0 MOVE ADDR1DI TO ADDR1DO IN NEW-ACCTREC.
           IF ADDR2DL > 0 MOVE ADDR2DI TO ADDR2DO IN NEW-ACCTREC.
           IF ADDR3DL > 0 MOVE ADDR3DI TO ADDR3DO IN NEW-ACCTREC.
           IF AUTH1DL > 0 MOVE AUTH1DI TO AUTH1DO IN NEW-ACCTREC.
           IF AUTH2DL > 0 MOVE AUTH2DI TO AUTH2DO IN NEW-ACCTREC.
           IF AUTH3DL > 0 MOVE AUTH3DI TO AUTH3DO IN NEW-ACCTREC.
           IF AUTH4DL > 0 MOVE AUTH4DI TO AUTH4DO IN NEW-ACCTREC.
           IF CARDSDL > 0 MOVE CARDSDI TO CARDSDO IN NEW-ACCTREC.
           IF IMODL > 0 MOVE IMODI TO IMODO IN NEW-ACCTREC.
           IF IDAYDL > 0 MOVE IDAYDI TO IDAYDO IN NEW-ACCTREC.
           IF IYRDL > 0 MOVE IYRDI TO IYRDO IN NEW-ACCTREC.
           IF RSNDL > 0 MOVE RSNDI TO RSNDO IN NEW-ACCTREC.
           IF CCODEDL > 0 MOVE CCODEDI TO CCODEDO IN NEW-ACCTREC.
           IF APPRDL > 0 MOVE APPRDI TO APPRDO IN NEW-ACCTREC.
           IF SCODE1DL > 0 MOVE SCODE1DI TO SCODE1DO IN NEW-ACCTREC.
           IF SCODE2DL > 0 MOVE SCODE2DI TO SCODE2DO IN NEW-ACCTREC.
           IF SCODE3DL > 0 MOVE SCODE3DI TO SCODE3DO IN NEW-ACCTREC.
           IF REQC = 'A' GO TO EDIT-0.
           IF SNAMEDF = HEX80 MOVE SPACES TO SNAMEDO IN NEW-ACCTREC.
           IF FNAMEDF = HEX80 MOVE SPACES TO FNAMEDO IN NEW-ACCTREC.
           IF MIDF = HEX80 MOVE SPACES TO MIDO IN NEW-ACCTREC.
           IF TTLDF = HEX80 MOVE SPACES TO TTLDO IN NEW-ACCTREC.
           IF TELDF = HEX80 MOVE SPACES TO TELDO IN NEW-ACCTREC.
           IF ADDR1DF = HEX80 MOVE SPACES TO ADDR1DO IN NEW-ACCTREC.
           IF ADDR2DF = HEX80 MOVE SPACES TO ADDR2DO IN NEW-ACCTREC.
           IF ADDR3DF = HEX80 MOVE SPACES TO ADDR3DO IN NEW-ACCTREC.
           IF AUTH1DF = HEX80 MOVE SPACES TO AUTH1DO IN NEW-ACCTREC.
           IF AUTH2DF = HEX80 MOVE SPACES TO AUTH2DO IN NEW-ACCTREC.
           IF AUTH3DF = HEX80 MOVE SPACES TO AUTH3DO IN NEW-ACCTREC.
           IF AUTH4DF = HEX80 MOVE SPACES TO AUTH4DO IN NEW-ACCTREC.
           IF CARDSDF = HEX80 MOVE SPACE TO CARDSDO IN NEW-ACCTREC.
           IF IMODF = HEX80 MOVE ZERO TO IMODO IN NEW-ACCTREC.
           IF IDAYDF = HEX80 MOVE ZERO TO IDAYDO IN NEW-ACCTREC.
           IF IYRDF = HEX80 MOVE ZERO TO IYRDO IN NEW-ACCTREC.
           IF RSNDF = HEX80 MOVE SPACE TO RSNDO IN NEW-ACCTREC.
           IF CCODEDF = HEX80 MOVE SPACES TO CCODEDO IN NEW-ACCTREC.
           IF APPRDF = HEX80 MOVE SPACES TO APPRDO IN NEW-ACCTREC.
           IF SCODE1DF = HEX80 MOVE SPACES TO SCODE1DO IN NEW-ACCTREC.
           IF SCODE2DF = HEX80 MOVE SPACES TO SCODE2DO IN NEW-ACCTREC.
           IF SCODE3DF = HEX80 MOVE SPACES TO SCODE3DO IN NEW-ACCTREC.
           IF OLD-ACCTREC = NEW-ACCTREC,
               MOVE 5 TO DTL-MSGNO,
               GO TO INPUT-REDISPLAY.
       EDIT-0.
           MOVE LOW-VALUES TO ACCTDTLI.
           IF SNAMEDO IN NEW-ACCTREC = SPACES,
               MOVE STARS TO SNAMEDI,
           ELSE IF SNAMEDO IN NEW-ACCTREC ALPHABETIC GO TO EDIT-1.
           MOVE DFHUNIMD TO SNAMEDA, MOVE -1 TO SNAMEDL.
       EDIT-1.
           IF FNAMEDO IN NEW-ACCTREC = SPACES,
               MOVE STARS TO FNAMEDI,
           ELSE IF FNAMEDO IN NEW-ACCTREC ALPHABETIC, GO TO EDIT-2.
           MOVE DFHUNIMD TO FNAMEDA, MOVE -1 TO FNAMEDL.
       EDIT-2.
           IF MIDO IN NEW-ACCTREC NOT ALPHABETIC,
               MOVE DFHUNIMD TO MIDA, MOVE -1 TO MIDL.
           IF TTLDO IN NEW-ACCTREC NOT ALPHABETIC,
               MOVE DFHUNIMD TO TTLDA, MOVE -1 TO TTLDL.
           IF ADDR1DO IN NEW-ACCTREC = SPACES,
               MOVE STARS TO ADDR1DI,
               MOVE DFHBMBRY TO ADDR1DA, MOVE -1 TO ADDR1DL.
           IF ADDR2DO IN NEW-ACCTREC = SPACES,
               MOVE STARS TO ADDR2DI,
               MOVE DFHBMBRY TO ADDR2DA, MOVE -1 TO ADDR2DL.
           IF CARDSDO IN NEW-ACCTREC = SPACES,
               MOVE STARS TO CARDSDI,
           ELSE IF (CARDSDO IN NEW-ACCTREC > '0' AND
                   CARDSDO IN NEW-ACCTREC NOT > '9'), GO TO EDIT-3.
           MOVE DFHUNIMD TO CARDSDA, MOVE -1 TO CARDSDL.
       EDIT-3.
           IF IMODO IN NEW-ACCTREC = SPACES,
               MOVE STARS TO IMODI,
           ELSE IF IMODO IN NEW-ACCTREC NUMERIC AND
               IMODO IN NEW-ACCTREC > '00' AND
               IMODO IN NEW-ACCTREC < '13', GO TO EDIT-4.
           MOVE DFHUNIMD TO IMODA, MOVE -1 TO IMODL.
       EDIT-4.
           IF IDAYDO IN NEW-ACCTREC = SPACES,
               MOVE STARS TO IDAYDI,
           ELSE IF IDAYDO IN NEW-ACCTREC NUMERIC AND
                   IDAYDO IN NEW-ACCTREC > '00' AND
                   IDAYDO IN NEW-ACCTREC < '32',
                   GO TO EDIT-5.
           MOVE DFHUNIMD TO IDAYDA, MOVE -1 TO IDAYDL.
       EDIT-5.
           IF IYRDO IN NEW-ACCTREC = SPACES,
               MOVE STARS TO IYRDI,
P30976*    ELSE IF IYRDO IN NEW-ACCTREC NUMERIC AND
P30976*        IYRDO IN NEW-ACCTREC > '75', GO TO EDIT-6.
P30976     ELSE IF IYRDO IN NEW-ACCTREC NUMERIC GO TO EDIT-6.
           MOVE DFHUNIMD TO IYRDA, MOVE -1 TO IYRDL.
       EDIT-6.
           IF RSNDO IN NEW-ACCTREC = SPACES,
               MOVE STARS TO RSNDI,
           ELSE IF (RSNDO IN NEW-ACCTREC = 'N' OR
                   RSNDO IN NEW-ACCTREC = 'L' OR
                   RSNDO IN NEW-ACCTREC = 'S' OR
                   RSNDO IN NEW-ACCTREC = 'R'), GO TO EDIT-7.
           MOVE DFHUNIMD TO RSNDA, MOVE -1 TO RSNDL.
       EDIT-7.
           IF CCODEDO IN NEW-ACCTREC = SPACES,
               MOVE STARS TO CCODEDI,
               MOVE -1 TO CCODEDL, MOVE DFHBMBRY TO CCODEDA.
           IF APPRDO IN NEW-ACCTREC = SPACES,
               MOVE STARS TO APPRDI,
               MOVE -1 TO APPRDL, MOVE DFHBMBRY TO APPRDA.
           IF ACCTDTLI NOT = LOW-VALUES,
               MOVE 3 TO DTL-MSGNO, GO TO INPUT-REDISPLAY.
           IF REQC = 'A' MOVE ACCTC TO ACCTDO IN NEW-ACCTREC,
               MOVE 'N ' TO STATDO IN NEW-ACCTREC,
               MOVE ' 1000.00' TO LIMITDO IN NEW-ACCTREC,
               MOVE PAY-INIT TO PAY-HIST IN NEW-ACCTREC (1),
                   PAY-HIST IN NEW-ACCTREC (2),
                   PAY-HIST IN NEW-ACCTREC (3).
       CK-OWN.
           MOVE ACCTC TO USE-QID2.
           EXEC CICS HANDLE CONDITION LENGERR(NO-OWN) END-EXEC.
           EXEC CICS READQ TS QUEUE(USE-QID) INTO(USE-REC)
               LENGTH(USE-LNG) ITEM(1) END-EXEC.
           EXEC CICS HANDLE CONDITION LENGERR (NO-GOOD) END-EXEC.
           IF EIBAID = DFHCLEAR GO TO RELEASE-ACCT.
           MOVE LOW-VALUES TO ACCTDTLO.
           MOVE DFHBMDAR TO HISTTLDA, STATTLDA, STATDA, LIMTTLDA,
               LIMITDA.
           IF REQC = 'A' MOVE 'NEW RECORD' TO TITLEDO, GO TO LOG-1.
           MOVE CORRESPONDING OLD-ACCTREC TO ACCTDTLO.
           IF REQC = 'X' MOVE 'DELETION' TO TITLEDO, GO TO LOG-2.
           MOVE 'BEFORE CHANGE' TO TITLEDO.
           IF SNAMEDO IN OLD-ACCTREC NOT = SNAMEDO IN NEW-ACCTREC OR
               FNAMEDO IN OLD-ACCTREC NOT = FNAMEDO IN NEW-ACCTREC
               OR MIDO IN OLD-ACCTREC NOT = MIDO IN NEW-ACCTREC OR
               TTLDO IN OLD-ACCTREC NOT = TTLDO IN NEW-ACCTREC
               MOVE 'NAME' TO MOD-NAME.
           IF TELDO IN OLD-ACCTREC NOT = TELDO IN NEW-ACCTREC
               MOVE 'TEL' TO MOD-TELE.
           IF ADDR1DO IN OLD-ACCTREC NOT = ADDR1DO IN NEW-ACCTREC OR
               ADDR2DO IN OLD-ACCTREC NOT = ADDR2DO IN NEW-ACCTREC OR
               ADDR3DO IN OLD-ACCTREC NOT = ADDR3DO IN NEW-ACCTREC
               MOVE 'ADDR' TO MOD-ADDR.
           IF AUTH1DO IN OLD-ACCTREC NOT = AUTH1DO IN NEW-ACCTREC OR
               AUTH2DO IN OLD-ACCTREC NOT = AUTH2DO IN NEW-ACCTREC OR
               AUTH3DO IN OLD-ACCTREC NOT = AUTH3DO IN NEW-ACCTREC OR
               AUTH4DO IN OLD-ACCTREC NOT = AUTH4DO IN NEW-ACCTREC
               MOVE 'AUTH' TO MOD-AUTH.
           IF CARDSDO IN OLD-ACCTREC NOT = CARDSDO IN NEW-ACCTREC OR
               IMODO IN OLD-ACCTREC NOT = IMODO IN NEW-ACCTREC OR
               IDAYDO IN OLD-ACCTREC NOT = IDAYDO IN NEW-ACCTREC OR
               IYRDO IN OLD-ACCTREC NOT = IYRDO IN NEW-ACCTREC OR
               RSNDO IN OLD-ACCTREC NOT = RSNDO IN NEW-ACCTREC OR
               CCODEDO IN OLD-ACCTREC NOT = CCODEDO IN NEW-ACCTREC OR
               APPRDO IN OLD-ACCTREC NOT = APPRDO IN NEW-ACCTREC
               MOVE 'CARD' TO MOD-CARD.
           IF SCODE1DO IN OLD-ACCTREC NOT = SCODE1DO IN NEW-ACCTREC OR
               SCODE2DO IN OLD-ACCTREC NOT = SCODE2DO IN NEW-ACCTREC OR
               SCODE3DO IN OLD-ACCTREC NOT = SCODE3DO IN NEW-ACCTREC
               MOVE 'CODES' TO MOD-CODE.
           MOVE MOD-LINE TO MSGDO.
           EXEC CICS WRITEQ TS QUEUE('ACCTLOG') FROM(ACCTDTLO)
               LENGTH(DTL-LNG) END-EXEC.
           MOVE 'AFTER CHANGE' TO TITLEDO.
       LOG-1.
           MOVE CORRESPONDING NEW-ACCTREC TO ACCTDTLO.
       LOG-2.
           MOVE EIBTRMID TO UPDT-TERM, MOVE EIBTIME TO UPDT-TIME,
           MOVE EIBDATE TO UPDT-DATE, MOVE UPDT-LINE TO MSGDO.
           EXEC CICS WRITEQ TS QUEUE('ACCTLOG') FROM(ACCTDTLO)
               LENGTH(DTL-LNG) END-EXEC.
           IF REQC = 'X' GO TO UPDT-DELETE.
           IF REQC = 'M' GO TO UPDT-MODIFY.
       UPDT-ADD.
           MOVE 2 TO MENU-MSGNO.
           MOVE SNAMEDO IN NEW-ACCTREC TO ACIX-KEY.
           EXEC CICS WRITE DATASET('ACCTFIL') FROM(NEW-ACCTREC)
               RIDFLD(ACCTC) LENGTH(ACCT-LNG) END-EXEC.
           GO TO RELEASE-ACCT.
       UPDT-MODIFY.
           MOVE 3 TO MENU-MSGNO.
           EXEC CICS REWRITE DATASET('ACCTFIL') FROM(NEW-ACCTREC)
               LENGTH (ACCT-LNG) END-EXEC.
           GO TO RELEASE-ACCT.
       UPDT-DELETE.
           MOVE 4 TO MENU-MSGNO.
           EXEC CICS DELETE DATASET('ACCTFIL') END-EXEC.
       RELEASE-ACCT.
           EXEC CICS DELETEQ TS QUEUE(USE-QID) END-EXEC.
           GO TO MENU-REFRESH.
      *    IF EIBAID = DFHCLEAR GO TO MENU-REFRESH.
       CONFIRM-SEND.
           MOVE LOW-VALUES TO CONFIRMO.
           MOVE 'C' TO REQC.
           EXEC CICS SEND MAP('CONFIRM')
                          MAPSET('ACCTSET')
                          ERASE
                          FREEKB
                     END-EXEC.
           EXEC CICS RECEIVE MAP('CONFIRM')
                            MAPSET('ACCTSET')
                     END-EXEC.
           IF EIBAID = DFHPF9 THEN
               MOVE 'XXXX' TO EIBFN,
               MOVE 99 TO EIBRCODE,
               GO TO NO-GOOD,
           ELSE
               IF EIBAID = DFHENTER THEN
                    GO TO MENU-REFRESH,
               ELSE
                    GO TO CONFIRM-SEND.
       MENU-REFRESH.
           MOVE LOW-VALUES TO ACCTMNUO.
           MOVE MENU-MSG (MENU-MSGNO) TO MSGMO.
           EXEC CICS SEND MAP('ACCTMNU') MAPSET('ACCTSET') ERASE FREEKB
               END-EXEC.
           EXEC CICS RETURN TRANSID('AC01') END-EXEC.
       INPUT-REDISPLAY.
           MOVE DTL-MSG (DTL-MSGNO) TO MSGDO.
           IF DTL-MSGNO = 2 OR 4 OR 5, MOVE -1 TO SNAMEDL.
           EXEC CICS SEND MAP('ACCTDTL') MAPSET('ACCTSET') DATAONLY
               CURSOR FREEKB END-EXEC.
           EXEC CICS RETURN TRANSID('AC02') COMMAREA(DFHCOMMAREA)
               LENGTH(6) END-EXEC.
       NO-OWN.
           IF EIBAID = DFHCLEAR GO TO MENU-REFRESH.
           MOVE 2 TO DTL-MSGNO, GO TO INPUT-REDISPLAY.
       NO-MAP.
           IF REQC = 'X' MOVE 1 TO DTL-MSGNO, MOVE -1 TO VFYDL
              ELSE MOVE 5 TO DTL-MSGNO.
           GO TO INPUT-REDISPLAY.
       PA-KEY.
           MOVE 4 TO DTL-MSGNO, GO TO INPUT-REDISPLAY.
       NO-GOOD.
           MOVE EIBFN TO ERR-FN, MOVE EIBRCODE TO ERR-RCODE.
           EXEC CICS HANDLE CONDITION ERROR END-EXEC.
           EXEC CICS LINK PROGRAM('ACCT04')
               COMMAREA(COMMAREA-FOR-ACCT04) LENGTH(10) END-EXEC.
           GOBACK.

