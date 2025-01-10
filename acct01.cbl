       IDENTIFICATION DIVISION.                                         00000300
       PROGRAM-ID. ACCT01.                                              00000400
       REMARKS. THIS PROGRAM IS THE FIRST INVOKED BY THE 'AC01'         00000500
                TRANSACTION. IT ANALYZES ALL REQUESTS, AND COMPLETES    00000600
                THOSE FOR NAME INQUIRIES AND RECORD DISPLAYS.  FOR      00000700
                UPDATE TRANSACTIONS, IT SENDS THE APPROPRIATE DATA ENTRY00000800
                SCREEN AND SETS THE NEXT TRANSACTION IDENTIFIER TO      00000900
                'AC02', WHICH COMPLETES THE UPDATE OPERATION. FOR PRINT 00001000
                REQUESTS, IT STARTS TRANSACTION 'AC03' TO DO THE ACTUAL 00001100
                PRINTING.                                               00001200
       ENVIRONMENT DIVISION.                                            00001300
       DATA DIVISION.                                                   00001400
       WORKING-STORAGE SECTION.                                         00001500
       01  MISC.                                                        00001600
           02  MSG-NO                 PIC S9(4) COMP VALUE +0.          00001700
           02  ACCT-LNG               PIC S9(4) COMP VALUE +383.        00001800
           02  DTL-LNG                PIC S9(4) COMP VALUE +751.        00002000
           02  STARS                  PIC X(12) VALUE '************'.   00002100
           02  USE-QID.                                                 00002200
               04  USE-QID1           PIC X(3) VALUE 'AC0'.             00002300
               04  USE-QID2           PIC X(5).                         00002400
           02  USE-REC.                                                 00002500
               04  USE-TERM           PIC X(4) VALUE SPACES.            00002600
               04  USE-TIME           PIC S9(7) COMP-3.                 00002700
               04  USE-DATE           PIC S9(7) COMP-3.                 00002800
           02  USE-LIMIT              PIC S9(7) COMP-3 VALUE +1000.     00002900
           02  USE-ITEM               PIC S9(4) COMP VALUE +1.          00003000
           02  USE-LNG                PIC S9(4) COMP VALUE +12.         00003100
           02  IN-AREA.                                                 00003200
               04  IN-TYPE            PIC X VALUE 'R'.                  00003300
               04  IN-REQ.                                              00003400
                   06  REQC           PIC X VALUE SPACES.               00003500
                   06  ACCTC          PIC X(5) VALUE SPACES.            00003600
                   06  PRTRC          PIC X(4) VALUE SPACES.            00003700
               04  IN-NAMES.                                            00003800
                   06  SNAMEC         PIC X(18) VALUE SPACES.           00003900
                   06  FNAMEC         PIC X(12) VALUE SPACES.           00004000
           02  COMMAREA-FOR-ACCT04.                                     00004100
               04  ERR-PGRMID         PIC X(8) VALUE 'ACCT01'.          00004200
               04  ERR-FN             PIC X.                            00004300
               04  ERR-RCODE          PIC X.                            00004400
           02  LINE-CNT               PIC S9(4) COMP VALUE +0.          00004500
           02  MAX-LINES              PIC S9(4) COMP VALUE +6.          00004600
           02  IX                     PIC S9(4) COMP.                   00004700
           02  SRCH-CTRL.                                               00004800
               04  FILLER             PIC X VALUE 'S'.                  00004900
               04  BRKEY.                                               00005000
                   06  BRKEY-SNAME    PIC X(12).                        00005100
               04  BRKEY-ACCT         PIC X(5).                         00005200
               04  MAX-SNAME          PIC X(12).                        00005300
               04  MAX-FNAME          PIC X(7).                         00005400
               04  MIN-FNAME          PIC X(7).                         00005500
           02  SUM-LINE.                                                00005600
               04  ACCTDO             PIC X(5).                         00005700
               04  FILLER             PIC X(3) VALUE SPACES.            00005800
               04  SNAMEDO            PIC X(12).                        00005900
               04  FILLER             PIC X(2) VALUE SPACES.            00006000
               04  FNAMEDO            PIC X(7).                         00006100
               04  FILLER             PIC X(2) VALUE SPACES.            00006200
               04  MIDO               PIC X(1).                         00006300
               04  FILLER             PIC X(2) VALUE SPACES.            00006400
               04  TTLDO              PIC X(4).                         00006500
               04  FILLER             PIC X(2) VALUE SPACES.            00006600
               04  ADDR1DO            PIC X(24).                        00006700
               04  FILLER             PIC X(2) VALUE SPACES.            00006800
               04  STATDO             PIC X(2).                         00006900
               04  FILLER             PIC X(3) VALUE SPACES.            00007000
               04  LIMITDO            PIC X(8).                         00007100
           02  PAY-LINE.                                                00007200
               04  BAL                PIC X(8).                         00007300
               04  FILLER             PIC X(6) VALUE SPACES.            00007400
               04  BMO                PIC 9(2).                         00007500
               04  FILLER             PIC X VALUE '/'.                  00007600
               04  BDAY               PIC 9(2).                         00007700
               04  FILLER             PIC X VALUE '/'.                  00007800
               04  BYR                PIC 9(2).                         00007900
               04  FILLER             PIC X(4) VALUE SPACES.            00008000
               04  BAMT               PIC X(8).                         00008100
               04  FILLER             PIC X(7) VALUE SPACES.            00008200
               04  PMO                PIC 9(2).                         00008300
               04  FILLER             PIC X VALUE '/'.                  00008400
               04  PDAY               PIC 9(2).                         00008500
               04  FILLER             PIC X VALUE '/'.                  00008600
               04  PYR                PIC 9(2).                         00008700
               04  FILLER             PIC X(4) VALUE SPACES.            00008800
               04  PAMT               PIC X(8).                         00008900
           COPY DFHBMSCA.                                               00009000
           COPY DFHAID.                                                 00009100
       01  ACCTREC.
           COPY ACCTREC.
           COPY ACCTSET.                                                00009400
       01  MSG-LIST.                                                    00009500
           02  FILLER                PIC X(70) VALUE                    00009600
               'NAMES MUST BE ALPHABETIC, AND SURNAME IS REQUIRED.'.    00009700
           02  FILLER                PIC X(70) VALUE                    00009800
               'ENTER SOME INPUT AND USE ONLY "CLEAR" OR "ENTER".'.     00009900
           02  FILLER                PIC X(70) VALUE                    00010000
           'REQUEST TYPE REQUIRED; MUST BE "D", "P", "A", "M" OR "X".'. 00010100
           02  FILLER                PIC X(70) VALUE                    00010200
               'PRINTER NAME REQUIRED ON PRINT REQUESTS'.               00010300
           02  FILLER                PIC X(70) VALUE                    00010400
               'ACCOUNT NUMBER REQUIRED (BETWEEN 10000 AND 79999)'.     00010500
           02  FILLER                PIC X(70) VALUE                    00010600
               'ACCOUNT NO. MUST BE NUMERIC AND FROM 10000 TO 79999'.   00010700
           02  FILLER                PIC X(70) VALUE                    00010800
               'NO NAMES ON LOCAL FILE MATCHING YOUR REQUEST, HOST LINK 00010900
      -    'NOT AVAILABLE'.                                             00011000
           02  FILLER                PIC X(70) VALUE                    00011100
               'ENTER EITHER NAME OR A REQUEST TYPE AND ACCOUNT NUMBER'.00011200
           02  FILLER                PIC X(70) VALUE                    00011300
               'THIS ACCOUNT NUMBER ALREADY EXISTS'.                    00011400
           02  FILLER                PIC X(70) VALUE                    00011500
               'NO LOCAL RECORD OF THIS ACCOUNT NUMBER, HOST LINK NOT AV00011600
      -    'AILABLE'.                                                   00011700
           02  FILLER                PIC X(47) VALUE                    00011800
               'THIS ACCOUNT NUMBER ALREADY IN USE AT TERMINAL '.       00011900
           02  MSG-TERM              PIC X(23).                         00012000
           02  FILLER                PIC X(70) VALUE                    00012100
               'PRINT REQUEST SCHEDULED'.                               00012200
           02  FILLER                PIC X(70) VALUE                    00012300
               'PRINTER NAME NOT RECOGNIZED'.                           00012400
           02  FILLER                PIC X(70) VALUE                    00012500
           'INPUT ERROR; PLEASE RETRY; USE ONLY "CLEAR" OR "ENTER" KEY'.00012600
           02  FILLER                PIC X(70) VALUE                    00012700
               'THERE ARE MORE MATCHING NAMES. PRESS PA2 TO CONTINUE.'. 00012800
       01  FILLER REDEFINES MSG-LIST.                                   00012900
           02  MSG-TEXT              PIC X(70) OCCURS 15.               00013000
       LINKAGE SECTION.                                                 00013100
       01  DFHCOMMAREA.                                                 00013200
           02  SRCH-COMM             PIC X(44).                         00013300
           02  IN-COMM REDEFINES SRCH-COMM PIC X(41).                   00013400
           02  CTYPE REDEFINES SRCH-COMM PIC X.                         00013500
       PROCEDURE DIVISION.                                              00013700
           EXEC CICS HANDLE CONDITION MAPFAIL(NO-MAP)                   00014100
               NOTFND(SRCH-ANY)                                         00014200
               ENDFILE(SRCH-DONE)                                       00014300
               QIDERR(RSRV-1)                                           00014400
               TERMIDERR(TERMID-ERR)                                    00014500
               ERROR(OTHER-ERRORS) END-EXEC.                            00014600
           EXEC CICS IGNORE CONDITION DUPKEY END-EXEC.
           MOVE LOW-VALUES TO ACCTMNUI, ACCTDTLI.                       00014700
           IF EIBAID = DFHCLEAR                                         00015000
               IF EIBCALEN = 0,                                         00015100
                   EXEC CICS XCTL PROGRAM('ACCTMENU') END-EXEC
               ELSE GO TO NEW-MENU.                                     00015400
           IF EIBAID = DFHPA2 AND EIBCALEN > 0 AND CTYPE = 'S',         00015500
               MOVE SRCH-COMM TO SRCH-CTRL, GO TO SRCH-RESUME.          00015600
           IF EIBCALEN > 0 AND CTYPE = 'R', MOVE IN-COMM TO IN-AREA.    00015700
           EXEC CICS RECEIVE MAP('ACCTMNU') MAPSET('ACCTSET') END-EXEC. 00016000
           IF REQML > 0 MOVE REQMI TO REQC.                             00016100
           IF REQMF NOT = LOW-VALUE, MOVE SPACE TO REQC.                00016200
           IF ACCTML > 0 MOVE ACCTMI TO ACCTC.                          00016300
           IF ACCTMF NOT = LOW-VALUE, MOVE SPACES TO ACCTC.             00016400
           IF PRTRML > 0 MOVE PRTRMI TO PRTRC.                          00016500
           IF PRTRMF NOT = LOW-VALUE, MOVE SPACES TO PRTRC.             00016600
           IF SNAMEML > 0 MOVE SNAMEMI TO SNAMEC.                       00016700
           IF SNAMEMF NOT = LOW-VALUE, MOVE SPACES TO SNAMEC.           00016800
           IF FNAMEML > 0 MOVE FNAMEMI TO FNAMEC.                       00016900
           IF FNAMEMF NOT = LOW-VALUE, MOVE SPACES TO FNAMEC.           00017000
           MOVE LOW-VALUES TO ACCTMNUI.                                 00017100
           IF IN-NAMES = SPACES GO TO CK-ANY.                           00017200
           IF FNAMEC NOT ALPHABETIC, MOVE 1 TO MSG-NO,                  00017600
               MOVE -1 TO FNAMEML, MOVE DFHBMBRY TO FNAMEMA.            00017700
           IF SNAMEC = SPACES, MOVE STARS TO SNAMEMO,                   00017800
           ELSE IF SNAMEC ALPHABETIC, GO TO CK-NAME.                    00017900
           MOVE 1 TO MSG-NO.                                            00018000
           MOVE -1 TO SNAMEML, MOVE DFHBMBRY TO SNAMEMA.                00018100
       CK-NAME.                                                         00018200
           IF MSG-NO > 0 GO TO MENU-RESEND.                             00018300
       SRCH-INIT.                                                       00018600
           MOVE SNAMEC TO BRKEY-SNAME, MAX-SNAME.                       00018700
           MOVE LOW-VALUES TO BRKEY-ACCT.                               00018800
           TRANSFORM MAX-SNAME FROM SPACES TO HIGH-VALUES.              00018900
           MOVE FNAMEC TO MIN-FNAME, MAX-FNAME.                         00019000
           TRANSFORM MIN-FNAME FROM SPACES TO LOW-VALUES.               00019100
           TRANSFORM MAX-FNAME FROM SPACES TO HIGH-VALUES.              00019200
       SRCH-RESUME.                                                     00019500
           EXEC CICS STARTBR DATASET('ACCTIX') RIDFLD(BRKEY) GTEQ       00019600
               END-EXEC.                                                00019700
       SRCH-LOOP.                                                       00020100
           EXEC CICS READNEXT DATASET('ACCTIX') INTO(ACCTREC)           00020200
               LENGTH(ACCT-LNG) RIDFLD(BRKEY) END-EXEC.                 00020300
           IF SNAMEDO IN ACCTREC > MAX-SNAME GO TO SRCH-DONE.           00020400
           IF FNAMEDO IN ACCTREC < MIN-FNAME OR                         00020500
               FNAMEDO IN ACCTREC > MAX-FNAME, GO TO SRCH-LOOP.         00020600
           ADD 1 TO LINE-CNT.                                           00020700
           IF LINE-CNT > MAX-LINES,                                     00020800
               MOVE MSG-TEXT (15) TO MSGMO,                             00020900
               MOVE DFHBMBRY TO MSGMA, GO TO SRCH-DONE.                 00021000
           MOVE CORRESPONDING ACCTREC TO SUM-LINE.                      00021100
           MOVE SUM-LINE TO SUMLNMO (LINE-CNT).                         00021200
           GO TO SRCH-LOOP.                                             00021300
       SRCH-DONE.                                                       00021400
           EXEC CICS ENDBR DATASET('ACCTIX') END-EXEC.                  00021500
       SRCH-ANY.                                                        00021600
           IF LINE-CNT = 0, MOVE 7 TO MSG-NO,                           00021700
               MOVE -1 TO SNAMEML, GO TO MENU-RESEND.                   00021800
           MOVE DFHBMUNP TO SUMLNMA (1), SUMLNMA (2), SUMLNMA (3),      00022100
               SUMLNMA (4), SUMLNMA (5), SUMLNMA (6).                   00022200
           MOVE DFHBMBRY TO MSGMA, MOVE DFHBMASB TO SUMTTLMA.           00022300
           EXEC CICS SEND MAP('ACCTMNU') MAPSET('ACCTSET')              00022400
               FREEKB DATAONLY ERASEAUP END-EXEC.                       00022500
           IF LINE-CNT NOT > MAX-LINES,                                 00022600
               EXEC CICS RETURN TRANSID('AC01') END-EXEC                00022700
           ELSE
                EXEC CICS RETURN TRANSID('AC01') COMMAREA(SRCH-CTRL)    00022800
                   LENGTH(44) END-EXEC.                                 00022900
       CK-ANY.                                                          00023300
           IF IN-REQ = SPACES, MOVE -1 TO SNAMEML,                      00023400
               MOVE 8 TO MSG-NO, GO TO MENU-RESEND.                     00023500
       CK-ACCTNO-1.                                                     00023600
           IF ACCTC = SPACES, MOVE STARS TO ACCTMO,                     00023700
               MOVE 5 TO MSG-NO, GO TO ACCT-ERR.                        00023800
           IF (ACCTC < '10000' OR ACCTC > '79999' OR ACCTC NOT NUMERIC),00023900
               MOVE 6 TO MSG-NO, GO TO ACCT-ERR.                        00024000
       CK-ACCTNO-2.                                                     00024100
           EXEC CICS HANDLE CONDITION NOTFND(NO-ACCT-RECORD) END-EXEC.  00024200
           EXEC CICS READ DATASET('ACCTFIL') RIDFLD(ACCTC)              00024300
               INTO(ACCTREC) LENGTH(ACCT-LNG) END-EXEC.                 00024400
           IF REQC = 'A',                                               00024500
               MOVE 9 TO MSG-NO, GO TO ACCT-ERR,                        00024600
           ELSE GO TO CK-REQ.                                           00024700
       NO-ACCT-RECORD.                                                  00024800
           IF REQC = 'A', GO TO CK-REQ.                                 00024900
           MOVE 10 TO MSG-NO.                                           00025000
       ACCT-ERR.                                                        00025100
           MOVE -1 TO ACCTML, MOVE DFHBMBRY TO ACCTMA.                  00025200
       CK-REQ.                                                          00025500
           IF REQC =  'D' OR 'P' OR 'A' OR 'M' OR 'X',                  00025600
               IF MSG-NO = 0 GO TO CK-USE, ELSE GO TO MENU-RESEND.      00025700
           IF REQC = SPACE, MOVE STARS TO REQMO.                        00025800
           MOVE -1 TO REQML, MOVE DFHBMBRY TO REQMA,                    00025900
           MOVE 3 TO MSG-NO.                                            00026000
           GO TO MENU-RESEND.                                           00026100
       CK-USE.                                                          00026400
           IF REQC = 'P' OR 'D' GO TO BUILD-MAP.                        00026500
           MOVE ACCTC TO USE-QID2.                                      00026600
           EXEC CICS READQ TS QUEUE(USE-QID) INTO(USE-REC)              00026700
               ITEM(USE-ITEM) LENGTH(USE-LNG) END-EXEC.                 00026800
           ADD USE-LIMIT TO USE-TIME.                                   00026900
           IF USE-TIME > 236000, ADD 1 TO USE-DATE,                     00027000
               SUBTRACT 236000 FROM USE-TIME.                           00027100
           IF USE-DATE > EIBDATE OR                                     00027200
               (USE-DATE = EIBDATE AND USE-TIME NOT < EIBTIME)          00027300
               MOVE USE-TERM TO MSG-TERM, MOVE 11 TO MSG-NO,            00027400
               MOVE -1 TO ACCTML, MOVE DFHBMBRY TO ACCTMA,              00027500
               GO TO MENU-RESEND.                                       00027600
       RSRV.                                                            00027900
           MOVE EIBTRMID TO USE-TERM, MOVE EIBTIME TO USE-TIME.         00028000
           MOVE EIBDATE TO USE-DATE.                                    00028100
           EXEC CICS WRITEQ TS QUEUE(USE-QID) FROM(USE-REC)             00028200
               LENGTH(12) ITEM(USE-ITEM) REWRITE END-EXEC.              00028300
           GO TO BUILD-MAP.                                             00028400
       RSRV-1.                                                          00028500
           MOVE EIBTRMID TO USE-TERM, MOVE EIBTIME TO USE-TIME.         00028600
           MOVE EIBDATE TO USE-DATE.                                    00028700
           EXEC CICS WRITEQ TS QUEUE(USE-QID) FROM(USE-REC)             00028800
               LENGTH(12) END-EXEC.                                     00028900
       BUILD-MAP.                                                       00029200
           IF REQC = 'X' MOVE 'DELETION' TO TITLEDO,                    00029300
               MOVE -1 TO VFYDL, MOVE DFHBMUNP TO VFYDA,                00029400
               MOVE 'ENTER "Y" TO CONFIRM OR "CLEAR" TO CANCEL'         00029500
                   TO MSGDO,                                            00029600
           ELSE MOVE -1 TO SNAMEDL.                                     00029700
           IF REQC = 'A' MOVE 'NEW RECORD' TO TITLEDO,                  00029800
               MOVE DFHPROTN TO STATTLDA, LIMTTLDA, HISTTLDA,           00029900
               MOVE ACCTC TO ACCTDI,                                    00030000
               MOVE 'FILL IN AND PRESS "ENTER," OR "CLEAR" TO CANCEL'   00030100
                   TO MSGDO,                                            00030200
               GO TO SEND-DETAIL.                                       00030300
           IF REQC = 'M' MOVE 'RECORD CHANGE' TO TITLEDO,               00030400
               MOVE 'MAKE CHANGES AND "ENTER" OR "CLEAR" TO CANCEL'     00030500
                   TO MSGDO,                                            00030600
           ELSE IF REQC = 'D',                                          00030700
                   MOVE 'PRESS "CLEAR" OR "ENTER" WHEN FINISHED'        00030800
                       TO MSGDO.                                        00030900
           MOVE CORRESPONDING ACCTREC TO ACCTDTLO.                      00031000
           MOVE CORRESPONDING PAY-HIST (1) TO PAY-LINE.                 00031100
           MOVE PAY-LINE TO HIST1DO.                                    00031200
           MOVE CORRESPONDING PAY-HIST (2) TO PAY-LINE.                 00031300
           MOVE PAY-LINE TO HIST2DO.                                    00031400
           MOVE CORRESPONDING PAY-HIST (3) TO PAY-LINE.                 00031500
           MOVE PAY-LINE TO HIST3DO.                                    00031600
           IF REQC  = 'M' GO TO SEND-DETAIL,                            00031700
           ELSE IF REQC = 'P' GO TO PRINT-PROC.                         00031800
           MOVE DFHBMASK TO                                             00031900
               SNAMEDA, FNAMEDA, MIDA, TTLDA, TELDA, ADDR1DA,           00032000
               ADDR2DA, ADDR3DA, AUTH1DA, AUTH2DA, AUTH3DA,             00032100
               AUTH4DA, CARDSDA, IMODA, IDAYDA, IYRDA, RSNDA,           00032200
               CCODEDA, APPRDA, SCODE1DA, SCODE2DA, SCODE3DA.           00032300
       SEND-DETAIL.                                                     00032600
           EXEC CICS SEND MAP('ACCTDTL') MAPSET('ACCTSET') ERASE FREEKB 00032700
               CURSOR END-EXEC.                                         00032800
           IF REQC = 'D', EXEC CICS RETURN TRANSID('ACCT') END-EXEC,    00032900
           ELSE
                EXEC CICS RETURN TRANSID('AC02')                        00033000
                   COMMAREA(IN-REQ) LENGTH(6) END-EXEC.                 00033100
       PRINT-PROC.                                                      00033400
           IF PRTRC = SPACES, MOVE STARS TO PRTRMO                      00033500
               MOVE 4 TO MSG-NO, GO TO TERMID-ERR1.                     00033600
           EXEC CICS START TRANSID('AC03') FROM(ACCTDTLO)               00033700
               LENGTH(DTL-LNG) TERMID(PRTRC) END-EXEC.                  00033800
           MOVE MSG-TEXT (12) TO MSGMO.                                 00033900
           EXEC CICS SEND MAP('ACCTMNU') MAPSET ('ACCTSET') DATAONLY    00034000
                ERASEAUP FREEKB END-EXEC.                               00034100
           EXEC CICS RETURN TRANSID('AC01') END-EXEC.                   00034200
       TERMID-ERR.                                                      00034300
           MOVE 13 TO MSG-NO.                                           00034400
       TERMID-ERR1.                                                     00034500
           MOVE -1 TO PRTRML, MOVE DFHBMBRY TO PRTRMA.                  00034600
       MENU-RESEND.                                                     00035000
           MOVE MSG-TEXT (MSG-NO) TO MSGMO.                             00035100
           EXEC CICS SEND MAP('ACCTMNU') MAPSET('ACCTSET')              00035200
               CURSOR FRSET FREEKB END-EXEC.
           EXEC CICS RETURN TRANSID('AC01') COMMAREA(IN-AREA)           00035400
                   LENGTH(41) END-EXEC.                                 00035500
       NO-MAP.                                                          00035800
           IF (EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3 OR DFHENTER)         00035900
               MOVE 2 TO MSG-NO, MOVE -1 TO SNAMEML, GO TO MENU-RESEND. 00036000
           MOVE MSG-TEXT (14) TO MSGMO.                                 00036100
       NEW-MENU.                                                        00036200
           EXEC CICS SEND MAP('ACCTMNU') MAPSET('ACCTSET')              00036300
               FREEKB ERASE END-EXEC.                                   00036400
           EXEC CICS RETURN TRANSID ('AC01') END-EXEC.                  00036500
       OTHER-ERRORS.                                                    00036800
           MOVE EIBFN TO ERR-FN, MOVE EIBRCODE TO ERR-RCODE.            00036900
           EXEC CICS HANDLE CONDITION ERROR END-EXEC.                   00037000
           EXEC CICS LINK PROGRAM('ACCT04')                             00037100
               COMMAREA(COMMAREA-FOR-ACCT04) LENGTH(10) END-EXEC.       00037200
           GOBACK.                                                      00037300
