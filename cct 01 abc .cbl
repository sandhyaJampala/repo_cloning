000100*************************                                         00010000
000200 IDENTIFICATION DIVISION.                                         00020000
000300*************************                                         00030000
000400                                                                  00040000
000500 PROGRAM-ID.    MXBPB510.                                         00050000
000600 AUTHOR.        IMAX TEAM.                                        00060000
000700 INSTALLATION.  TEST.                                             00070000
000800 DATE-WRITTEN.                                                    00080000
000900 DATE-COMPILED.                                                   00090000
001000                                                                  00100000
001100*================================================================*00110000
001200*                                                                ¦00120000
001300* MAX SYSTEM STANDARD PROGRAM DESCRIPTION AREA                   ¦00130000
001400*                                                                ¦00140000
001500*================================================================*00150000
001600* PROGRAM  ¦ MXBPB510 ¦ TRANS ID ¦  N/A ¦                        ¦00160000
001700*----------+----------+----------+------+------------------------*00170000
001800*                                                                ¦00180000
001900* DESCRIPTION:                                                   ¦00190000
002000*                                                                ¦00200000
002100*      XEROX CONTROL FILE PROGRAM                                ¦00210000
002200*      --------------------------                                ¦00220000
002300*                                                                ¦00230000
002400*  THIS PROGRAM IS CALLED BY MXBPB503, AND CREATES A CONTROL     ¦00240000
002500*  REPORT FOR THE XEROX REPORT PROGRAMS.  THE CONTROL REPORT     ¦00250000
002600*  SHOWS BY HANDLING CODE, THE NUMBER OF PAGES PRINTED, THE      ¦00260000
002700*  NUMBER OF DOCUMENTS, AND THE NUMBER OF ENVELOPES.  IN         ¦00270000
002800*  ADDITION, THE REPORT ALSO SHOWS A CONTROL TOTAL BY HANDLING   ¦00280000
002900*  CODE TO VERIFY THE DOCUMENTS SENT OUT.                        ¦00290000
003000*****************************************************************¦00300000
003100* IMPORTANT:                                                     ¦00310000
003200* A COPY OF THIS PROGRAM WAS MADE.  IT IS MXBPB511.  ANY CHANGES ¦00320000
003300* MADE TO MXBPB510 MUST BE MADE TO MXBPB511.                     ¦00330000
003400*****************************************************************¦00340000
003500*================================================================*00350000
003600/================================================================*00360000
003700*   CALL   ¦ PROGRAM DESCRIPTION                                 ¦00370000
003800*----------+-----------------------------------------------------*00380000
003900*          ¦                                                     ¦00390000
004000*----------+-----------------------------------------------------*00400000
004100*                                                                 00410000
004200*================================================================*00420000
004300*  TABLE   ¦   VIEW   ¦     DESCRIPTION                          ¦00430000
004400*----------+-----------------------------------------------------*00440000
004500*          ¦          ¦                                          ¦00450000
004600*----------+-----------------------------------------------------*00460000
004700*                                                                 00470000
004800*================================================================*00480000
004900*   PLAN   ¦ BIND INCLUDE MEMBERS                                ¦00490000
005000*----------+-----------------------------------------------------*00500000
005100*          ¦                                                     ¦00510000
005200*-----------------------------------------------------------------00520000
005300*                                                                 00530000
005400*================================================================*00540000
005500*   XCTL   ¦ SEC ¦ ENTITY ¦ PROGRAM DESCRIPTION                  ¦00550000
005600*----------+-----+--------+--------------------------------------*00560000
005700*   N/A    ¦     ¦        ¦                                      ¦00570000
      *-----------------------------------------------------------------00580000
010200/                                                                 00820000
010300**********************                                            00830000
010400 ENVIRONMENT DIVISION.                                            00840000
010500**********************                                            00850000
010600                                                                  00860000
010700 CONFIGURATION SECTION.                                           00870000
010800                                                                  00880000
010900 SOURCE-COMPUTER.   IBM-3090.                                     00890000
011000 OBJECT-COMPUTER.   IBM-3090.                                     00900000
011100                                                                  00910000
011200 SPECIAL-NAMES.                                                   00920000
011300                                                                  00930000
011400         C01                          IS TO-TOP-OF-PAGE.          00940000
011500                                                                  00950000
011600 INPUT-OUTPUT SECTION.                                            00960000
011700                                                                  00970000
011800 FILE-CONTROL.                                                    00980000
011900                                                                  00990000
012000       SELECT XEROX-CONTROL-REPORT                                01000000
012100          ASSIGN TO MXPB510T                                      01010000
012200          FILE STATUS IS WS-FILE-STATUS.                          01020000
012300                                                                  01030000
012400/                                                                 01040000
012500*************************                                         01050000
012600 DATA DIVISION.                                                   01060000
012700*************************                                         01070000
012800 FILE SECTION.                                                    01080000
012900                                                                  01090000
013000 FD  XEROX-CONTROL-REPORT                                         01100000
013100     RECORDING MODE F                                             01110000
013200     BLOCK CONTAINS 0 RECORDS                                     01120000
013300     DATA RECORD IS XEROX-CNTL-RECORD.                            01130000
013400                                                                  01140000
013500 01  XEROX-CNTL-RECORD.                                           01150000
013600     05  FILLER                      PIC X(80).                   01160000
013700                                                                  01170000
013800                                                                  01180000
013900                                                                  01190000
014000/************************                                         01200000
014100 WORKING-STORAGE SECTION.                                         01210000
014200*************************                                         01220000
014300*                                                                 01230000
014400 01  WS-DB2DATE-LITERALS.
014500     05  WS-DB2DATE-ALL-ZEROES  PIC  X(10) VALUE '0000000000'.
014600     05  WS-DB2DATE-ZEROES-6    PIC  X(06) VALUE '000000'.
014700     05  WS-DB2DATE-ZEROES-8    PIC  X(08) VALUE '00000000'.
014800     05  WS-DB2DATE-ZEROES-10   PIC  X(10) VALUE '00/00/0000'.
014900
015000 01  REPORT-TABLE.                                                01240000
015100     05  TABLE-ENTRY     OCCURS 100 TIMES                         01250000
015200                         DEPENDING ON WS-SUB-MAX.                 01260000
015300        10  TBL-SPECIAL-HANDLING    PIC X(2) VALUE SPACES.        01270000
015400        10  TBL-PAGE-CNT            PIC 9(9)  COMP-3 VALUE 0.     01280000
015500        10  TBL-DOC-CNT             PIC 9(9)  COMP-3 VALUE 0.     01290000
015600        10  TBL-ENV-CNT             PIC 9(9)  COMP-3 VALUE 0.     01300000
015700        10  TBL-CONTROL-TOTAL       PIC 9(15) COMP-3 VALUE 0.     01310000
015800        10  TBL-FORM-NAME           PIC X(8)  VALUE SPACES.       01320000
015900        10  TBL-RPT-DATE            PIC X(10) VALUE SPACES.       01330000
016000        10  TBL-RUN-DATE            PIC X(10) VALUE SPACES.       01340000
016100        10  TBL-RUN-TIME            PIC X(6)  VALUE SPACES.       01350000
016200                                                                  01360000
016300 01  CONTROL-RPT-ZZZ                     PIC X(3)  VALUE 'ZZZ'.   01370000
016400                                                                  01380000
016500 01  CONTROL-RPT-DTL1.                                            01390000
016600     05  DTL-HANDLING-CODE               PIC X(2)  VALUE SPACES.  01400000
016700     05  DTL-PAGE-CNT                    PIC 9(9)  VALUE 0.       01410000
016800     05  DTL-DOC-CNT                     PIC 9(9)  VALUE 0.       01420000
016900     05  DTL-ENV-CNT                     PIC 9(9)  VALUE 0.       01430000
017000     05  DTL-CONTROL-TOT                 PIC 9(15) VALUE 0.       01440000
017100     05  DTL-FORM-NAME                   PIC X(8)  VALUE SPACES.  01450000
017200     05  DTL-RPT-DATE                    PIC X(10) VALUE SPACES.  01460000
017300     05  DTL-RUN-DATE                    PIC X(10) VALUE SPACES.  01470000
017400     05  DTL-RUN-TIME                    PIC X(6)  VALUE SPACES.  01480000
017500     05  FILLER                          PIC X(2)  VALUE SPACES.  01490000
017600                                                                  01500000
017700 01  WS-FIELDS.                                                   01510000
017800     05  WS-DOC-KEY.                                              01520000
017900         10  WS-DOC-SH                   PIC X(2)  VALUE SPACE.   01530000
018000         10  WS-DOC-FORM                 PIC X(8)  VALUE SPACE.   01530000
018100         10  WS-DOC-DATE                 PIC X(8)  VALUE SPACE.   01540000
018200         10  WS-DOC-COMP                 PIC 9(4)  VALUE ZERO.    01550000
018300         10  WS-DOC-CUST                 PIC 9(6)  VALUE ZERO.    01560000
018400         10  WS-DOC-CUST-LOC             PIC 9(4)  VALUE ZERO.    01570000
018500         10  WS-DOC-FILLER               PIC X(6)  VALUE SPACE.   01620000
018600     05  WS-DOC-KEY-OBLIGOR REDEFINES WS-DOC-KEY.                 01580000
018700         10  WS-DOC-SH-O                 PIC X(2).                01530000
018800         10  WS-DOC-FORM-O               PIC X(8).                01530000
018900         10  WS-DOC-DATE-O               PIC X(8).                01540000
019000         10  WS-DOC-COMP-O               PIC 9(4).                01550000
019100         10  WS-DOC-CUST-O               PIC 9(6).                01560000
019200         10  WS-DOC-LOC-O                PIC 9(4).                01570000
019300         10  WS-DOC-DIST-O               PIC 9(6).                01570000
019400     05  WS-DOC-KEY-OTHER   REDEFINES WS-DOC-KEY.                 01580000
019500         10  WS-DOC-SH-X                 PIC X(2).                01530000
019600         10  WS-DOC-FORM-X               PIC X(8).                01590000
019700         10  WS-DOC-DLR-NO-X             PIC 9(6).                01600000
019800         10  WS-DOC-TRUST-NO-X           PIC X(11).               01610000
019900         10  WS-DOC-FILLER-X             PIC X(11).               01620000
020000                                                                  01630000
020100     05  WS-CURR-DATE.                                            01640000
020200         10  WS-CURR-CC                  PIC X(2)  VALUE SPACES.  01650000
020300         10  WS-CURR-YY                  PIC X(2)  VALUE SPACES.  01660000
020400         10  FILLER                      PIC X     VALUE '-'.     01670000
020500         10  WS-CURR-MM                  PIC X(2)  VALUE SPACES.  01680000
020600         10  FILLER                      PIC X     VALUE '-'.     01690000
020700         10  WS-CURR-DD                  PIC X(2)  VALUE SPACES.  01700000
020800                                                                  01710000
020900     05  WS-CURR-TIME                    PIC X(6)  VALUE SPACES.  01720000
021000                                                                  01730000
021100     05  WS-RPT-DATE.                                             01740000
021200         10  WS-RPT-CC                   PIC X(2)  VALUE SPACES.  01750000
021300         10  WS-RPT-YY                   PIC X(2)  VALUE SPACES.  01760000
021400         10  FILLER                      PIC X     VALUE '-'.     01770000
021500         10  WS-RPT-MM                   PIC X(2)  VALUE SPACES.  01780000
021600         10  FILLER                      PIC X     VALUE '-'.     01790000
021700         10  WS-RPT-DD                   PIC X(2)  VALUE SPACES.  01800000
021800                                                                  01810000
021900     05  WS-RUN-DATE                     PIC X(10) VALUE SPACES.  01820000
022000     05  WS-REPORT-DATE                  PIC X(10) VALUE SPACES.  01830000
022100                                                                  01840000
022200     05  WS-PREV-DOC-KEY                 PIC X(38) VALUE SPACE.   01850000
022300     05  WS-PREV-ENV-KEY                 PIC X(27) VALUE SPACE.   01860000
022400     05  WS-ENV-KEY.                                              01870000
022500         10  WS-ENV-KEY-CUST             PIC X(25) VALUE SPACE.   01880000
022600         10  WS-ENV-SPC-HNDL             PIC X(2)  VALUE SPACE.   01890000
022700     05  WS-SUB                          PIC 9(3)  COMP   VALUE 0.01900000
022800     05  WS-SUB-MAX                      PIC 9(3)  COMP   VALUE 0.01910000
022900     05  WS-TOTAL-RECORDS                PIC 9(6)  VALUE 0.       01920000
023000     05  WS-TOTAL-ENV                    PIC 9(6)  VALUE 0.       01930000
023100     05  WS-TOTAL-DOCS                   PIC 9(6)  VALUE 0.       01940000
023200     05  WS-FILE-STATUS                  PIC 9(2)  VALUE 0.       01950000
023300     05  WS-RETURN-CODE                  PIC S9(2) COMP  VALUE 0. 01960000
023400     05  WS-MATCH-FLAG                   PIC X     VALUE 'N'.     01970000
023500         88  WS-MATCH-FOUND                        VALUE 'Y'.     01980000
023600         88  WS-NO-MATCH                           VALUE 'N'.     01990000
023700*                                                                 02000000
023800     05  WS-SYSTEM-DATE.                                          02010000
023900         10  WS-SYSTEM-YEAR           PIC 9(02) VALUE 0.          02020000
024000         10  WS-SYSTEM-MONTH          PIC 9(02) VALUE 0.          02030000
024100         10  WS-SYSTEM-DAY            PIC 9(02) VALUE 0.          02040000
024200/                                                                 02050000
024300* RIS 00.636 START                                                02120007
024400     COPY MXWW18.                                                 02180010
024500/                                                                 02200000
024600     COPY MXWW30.                                                 02201010
024700/                                                                 02202010
024800     COPY MXWW31.                                                 02210000
024900/                                                                 02220003
025000     COPY MXWW03.                                                 02230003
025100                                                                  02240000
025200* RIS 00.636 END                                                  02250000
025300/                                                                 02260000
025400 LINKAGE SECTION.                                                 02270000
025500                                                                  02280000
025600     COPY MXBW510.                                                02290000
025700*                                                                 02300000
025800/*******************                                              02310000
025900 PROCEDURE DIVISION  USING TRIPLE-ZERO-RECORD                     02320000
026000                           MXBW510-LINK.                          02330000
026100********************                                              02340000
026200*                                                                 02350000
026300 0000-MAINLINE.                                                   02360000
026400                                                                  02370000
026500     IF MXBW510-FIRST-TIME                                        02380000
026600        PERFORM 0100-INITIALIZATION    THRU 0100-EXIT             02390000
026700     END-IF.                                                      02400000
026800                                                                  02410000
026900     IF NOT MXBW510-EOJ                                           02420000
027000         PERFORM 0200-PROCESS-TABLE     THRU 0200-EXIT            02430000
027100      ELSE                                                        02440000
027200         PERFORM 0300-PROCESS-EOJ       THRU 0300-EXIT            02450000
027300     END-IF.                                                      02460000
027400                                                                  02470000
027500     GOBACK.                                                      02480000
027600                                                                  02490000
027700 0000-EXIT.  EXIT.                                                02500000
027800/**************************************************************** 02510000
027900*  INITIALIZE                                                   * 02520000
028000***************************************************************** 02530000
028100 0100-INITIALIZATION.                                             02540000
028200                                                                  02550000
028300     INITIALIZE  CONTROL-RPT-DTL1                                 02560000
028400                 WS-FIELDS.                                       02570000
028500                                                                  02580000
028600     MOVE 'N'                      TO MXBW510-RUN-FLAG.           02590000
028700     MOVE 1                        TO WS-SUB-MAX.                 02600000
028800                                                                  02610000
028900     ACCEPT WS-SYSTEM-DATE       FROM DATE.                       02620000
029000     ACCEPT WS-CURR-TIME         FROM TIME.                       02630000
029100                                                                  02640000
029200     IF WS-SYSTEM-YEAR > '50'                                     02650000
029300           MOVE '19'             TO WS-CURR-CC                    02660000
029400      ELSE                                                        02670000
029500           MOVE '20'             TO WS-CURR-CC                    02680000
029600     END-IF.                                                      02690000
029700     MOVE WS-SYSTEM-YEAR         TO WS-CURR-YY                    02700000
029800     MOVE WS-SYSTEM-MONTH        TO WS-CURR-MM                    02710000
029900     MOVE WS-SYSTEM-DAY          TO WS-CURR-DD                    02720000
030000                                                                  02730000
030100* RIS 00.636 START                                                02740000
030200     MOVE WS-CURR-DATE           TO  MXWW31-DB2-DATE.             02750000
030300     MOVE 10                     TO  MXWW31-FIELD-LENGTH.         02760000
030400     MOVE 1                      TO  MXWW31-CNTL-ENT-NO.          02770000
030500     PERFORM MXWP31-ENVIRONMENT-DATE  THRU  MXWP31-EXIT.          02780001
030600* SAH START TRACKER #2157
030700**IF RETURNED DATE IS SPACES, ZERO FILL BASED ON LENGTH.
030800     EVALUATE TRUE
030900        WHEN (MXWW31-FIELD-LENGTH NOT = 6  AND
031000              MXWW31-FIELD-LENGTH NOT = 8  AND
031100              MXWW31-FIELD-LENGTH NOT = 10)
031200            MOVE WS-DB2DATE-ALL-ZEROES TO
031300                 MXWW31-RETURNED-DATE
031400        WHEN MXWW31-RETURNED-DATE = SPACES
031500            DISPLAY '*********************************'
031600            DISPLAY '* BLANK DATE RETURNED BY MXWP31 *'
031700            DISPLAY '* THIS DATE WILL BE ZERO FILLED *'
031800            DISPLAY '*********************************'
031900            DISPLAY '* DATE PASSED:   ' WS-CURR-DATE
032000            DISPLAY '* DATE RETURNED: ' MXWW31-RETURNED-DATE
032100            DISPLAY '*********************************'
032200            EVALUATE TRUE
032300                WHEN MXWW31-FIELD-LENGTH = 6
032400                    MOVE WS-DB2DATE-ZEROES-6  TO
032500                         MXWW31-RETURNED-DATE
032600                WHEN MXWW31-FIELD-LENGTH = 8
032700                    MOVE WS-DB2DATE-ZEROES-8  TO
032800                         MXWW31-RETURNED-DATE
032900                WHEN MXWW31-FIELD-LENGTH = 10
033000                    MOVE WS-DB2DATE-ZEROES-10 TO
033100                         MXWW31-RETURNED-DATE
033200            END-EVALUATE
033300     END-EVALUATE.
033400* SAH END   TRACKER #2157
033500     IF NOT MXWW30-STATUS-SUCCESS                                 02790004
033600        MOVE +16 TO MXBW510-STATUS                                02800002
033700     END-IF.                                                      02810002
033800     MOVE MXWW31-RETURNED-DATE   TO  WS-RUN-DATE.                 02820000
033900* RIS 00.636 END                                                  02830000
034000                                                                  02840000
034100     MOVE WS-RUN-DATE            TO  TBL-RUN-DATE (1).            02850000
034200     MOVE FORM-NAME              TO  TBL-FORM-NAME (1).           02860000
034300                                                                  02870000
034400     MOVE  DATE-000(1:2)         TO  WS-RPT-CC.                   02880000
034500     MOVE  DATE-000(3:2)         TO  WS-RPT-YY.                   02890000
034600     MOVE  DATE-000(5:2)         TO  WS-RPT-MM.                   02900000
034700     MOVE  DATE-000(7:2)         TO  WS-RPT-DD.                   02910000
034800                                                                  02920000
034900* RIS 00.636 START                                                02930000
035000     MOVE WS-RPT-DATE           TO MXWW31-DB2-DATE.               02940000
035100     MOVE 10                    TO MXWW31-FIELD-LENGTH.           02950000
035200     MOVE 1                     TO MXWW31-CNTL-ENT-NO.            02960000
035300     PERFORM   MXWP31-ENVIRONMENT-DATE THRU MXWP31-EXIT.          02970001
035400* SAH START TRACKER #2157
035500**IF RETURNED DATE IS SPACES, ZERO FILL BASED ON LENGTH.
035600     EVALUATE TRUE
035700        WHEN (MXWW31-FIELD-LENGTH NOT = 6  AND
035800              MXWW31-FIELD-LENGTH NOT = 8  AND
035900              MXWW31-FIELD-LENGTH NOT = 10)
036000            MOVE WS-DB2DATE-ALL-ZEROES TO
036100                 MXWW31-RETURNED-DATE
036200        WHEN MXWW31-RETURNED-DATE = SPACES
036300            DISPLAY '*********************************'
036400            DISPLAY '* BLANK DATE RETURNED BY MXWP31 *'
036500            DISPLAY '* THIS DATE WILL BE ZERO FILLED *'
036600            DISPLAY '*********************************'
036700            DISPLAY '* DATE PASSED:   ' WS-RPT-DATE
036800            DISPLAY '* DATE RETURNED: ' MXWW31-RETURNED-DATE
036900            DISPLAY '*********************************'
037000            EVALUATE TRUE
037100                WHEN MXWW31-FIELD-LENGTH = 6
037200                    MOVE WS-DB2DATE-ZEROES-6  TO
037300                         MXWW31-RETURNED-DATE
037400                WHEN MXWW31-FIELD-LENGTH = 8
037500                    MOVE WS-DB2DATE-ZEROES-8  TO
037600                         MXWW31-RETURNED-DATE
037700                WHEN MXWW31-FIELD-LENGTH = 10
037800                    MOVE WS-DB2DATE-ZEROES-10 TO
037900                         MXWW31-RETURNED-DATE
038000            END-EVALUATE
038100     END-EVALUATE.
038200* SAH END   TRACKER #2157
038300     IF NOT MXWW30-STATUS-SUCCESS                                 02980004
038400        MOVE +16 TO MXBW510-STATUS                                02990002
038500     END-IF.                                                      03000002
038600     MOVE MXWW31-RETURNED-DATE  TO WS-REPORT-DATE.                03010000
038700* RIS 00.636 END                                                  03020000
038800                                                                  03030000
038900     MOVE WS-REPORT-DATE         TO  TBL-RPT-DATE (1).            03040000
039000     MOVE WS-CURR-TIME           TO  TBL-RUN-TIME (1).            03050000
039100                                                                  03060000
039200 0100-EXIT.                                                       03070000
039300     EXIT.                                                        03080000
039400/*****************************************************************03090000
039500* CREATE CONTROL TABLE                                           *03100000
039600******************************************************************03110000
039700 0200-PROCESS-TABLE.                                              03120000
039800                                                                  03130000
039900     PERFORM 0210-PROCESS-DOC-KEY      THRU 0210-EXIT.            03140000
040000                                                                  03150000
040100     PERFORM WITH TEST AFTER                                      03160000
040200        VARYING WS-SUB                                            03170000
040300        FROM 1 BY 1                                               03180000
040400        UNTIL WS-SUB  >  WS-SUB-MAX                               03190000
040500           OR  WS-MATCH-FOUND                                     03200000
040600                                                                  03210000
040700          IF TBL-SPECIAL-HANDLING (WS-SUB) = SPECIAL-HANDLING     03220000
040800             MOVE  'Y'                 TO  WS-MATCH-FLAG          03230000
040900          END-IF                                                  03240000
041000                                                                  03250000
041100     END-PERFORM.                                                 03260000
041200                                                                  03270000
041300     MOVE  UNIQUE-KEY           TO WS-ENV-KEY-CUST.               03280000
041400     MOVE  SPECIAL-HANDLING     TO WS-ENV-SPC-HNDL.               03290000
041500                                                                  03300000
041600     IF WS-MATCH-FOUND                                            03310000
041700           PERFORM 0250-EXISTING-ENTRY    THRU 0250-EXIT          03320000
041800       ELSE                                                       03330000
041900           PERFORM 0220-NEW-ENTRY         THRU 0220-EXIT          03340000
042000     END-IF.                                                      03350000
042100                                                                  03360000
042200     ADD  1             TO  WS-TOTAL-RECORDS.                     03370000
042300                                                                  03380000
042400 0200-EXIT.  EXIT.                                                03390000
042500/*****************************************************************03400000
042600* PROCESS DOC KEY                                                *03410000
042600*                                                                *03410000
042600* TRACKER 12748/PROJECT 05.933: ADDED RECEIVED CREDIT MEMO FORM  *03410000
042600* NAME, RECVCRED, TO THE BELOW EVALUATE.                         *03410000
042700******************************************************************03420000
042800 0210-PROCESS-DOC-KEY.                                            03430000
042900                                                                  03440000
043000     INITIALIZE WS-DOC-KEY.                                       03450000
043100                                                                  03460000
043200     EVALUATE FORM-NAME                                           03470000
043300       WHEN 'DLRSTMTS'                                            03480000
043400       WHEN 'DSTSTMTS'                                            03490000
043500       WHEN 'SCHEDLIQ'                                            03500000
043600       WHEN 'RETMERCH'                                            03510000
043700       WHEN 'ADVOFPAY'                                            03520000
043800       WHEN 'CRDNOTE '                                            03530000
043900       WHEN 'REMITADV'                                            03540000
044000       WHEN 'DLRDISC '                                            03550000
044100       WHEN 'SELFCERT'                                            03560000
044200       WHEN 'APPLCRED'                                            03570000
044300       WHEN 'DBNOTE  '                                            03580000
044400           MOVE SPECIAL-HANDLING       TO WS-DOC-SH               03590000
044500           MOVE FORM-NAME              TO WS-DOC-FORM             03590000
044600           MOVE DATE-000               TO WS-DOC-DATE             03600000
044700           MOVE COMPANY-NO-000         TO WS-DOC-COMP             03610000
044800           MOVE CUST-NO-000            TO WS-DOC-CUST             03620000
044900           MOVE CUST-LOC-NO-000        TO WS-DOC-CUST-LOC         03630000
045000           MOVE SPACES                 TO WS-DOC-FILLER           01620000
045100                                                                  03640000
045200       WHEN 'FCOBLG01'                                            03580000
045300           MOVE SPECIAL-HANDLING       TO WS-DOC-SH-O             03590000
045400           MOVE FORM-NAME              TO WS-DOC-FORM-O           03590000
045500           MOVE DATE-000               TO WS-DOC-DATE-O           03600000
045600           MOVE COMPANY-NO-000         TO WS-DOC-COMP-O           03610000
045700           MOVE CUST-NO-000            TO WS-DOC-CUST-O           03620000
045800           MOVE DIST-NO-000            TO WS-DOC-DIST-O           03630000
045900           MOVE ZERO                   TO WS-DOC-LOC-O            01570000
046000                                                                  03640000
046100       WHEN 'CRDTAPPL'                                            03650000
046200       WHEN 'TRUSTPRT'                                            03660000
046300       WHEN 'MULTIMFG'                                            03670000
046400       WHEN 'AVRYINVC'                                            03680000
046500       WHEN 'LINVOICE'                                            03680000
046600       WHEN 'CRDTPRIN'                                            03680000
046600       WHEN 'ERACHG01'                                            03680000
044300       WHEN 'RECVCRED'                                            03580000
042700*86554 - B                                                        03420000
044300       WHEN 'FIXPTPRT'                                            03580000
042700*86554 - E                                                        03420000
046700           MOVE SPECIAL-HANDLING       TO WS-DOC-SH-X             03590000
046800           MOVE FORM-NAME              TO WS-DOC-FORM-X           03690000
046900           MOVE DLR-NO-000             TO WS-DOC-DLR-NO-X         03700000
047000           MOVE TRUST-NO-000           TO WS-DOC-TRUST-NO-X       03710000
047100           MOVE SPACES                 TO WS-DOC-FILLER-X         01620000
047200                                                                  03720000
047300       WHEN OTHER                                                 03730000
047400           MOVE SPECIAL-HANDLING       TO WS-DOC-SH               03590000
047500           MOVE FORM-NAME              TO WS-DOC-FORM             03590000
047600           MOVE DATE-000               TO WS-DOC-DATE             03600000
047700           MOVE COMPANY-NO-000         TO WS-DOC-COMP             03610000
047800           MOVE CUST-NO-000            TO WS-DOC-CUST             03620000
047900           MOVE CUST-LOC-NO-000        TO WS-DOC-CUST-LOC         03630000
048000           MOVE SPACES                 TO WS-DOC-FILLER           01620000
048100                                                                  03720000
048200     END-EVALUATE.                                                03760000
048300                                                                  03770000
048400 0210-EXIT.  EXIT.                                                03780000
048500/*****************************************************************03790000
048600* PROCESS NEW HANDLING CODE                                      *03800000
048700******************************************************************03810000
048800 0220-NEW-ENTRY.                                                  03820000
048900                                                                  03830000
049000     ADD 1     TO WS-SUB-MAX.                                     03840000
049100     MOVE WS-SUB-MAX        TO WS-SUB.                            03850000
049200                                                                  03860000
049300     MOVE  FORM-NAME            TO TBL-FORM-NAME (WS-SUB).        03870000
049400     MOVE  WS-RUN-DATE          TO TBL-RUN-DATE (WS-SUB).         03880000
049500     MOVE  WS-CURR-TIME         TO TBL-RUN-TIME (WS-SUB).         03890000
049600     MOVE  WS-REPORT-DATE       TO TBL-RPT-DATE (WS-SUB).         03900000
049700                                                                  03910000
049800     MOVE  SPECIAL-HANDLING     TO TBL-SPECIAL-HANDLING (WS-SUB). 03920000
049900     ADD   1                    TO TBL-PAGE-CNT (WS-SUB).         03930000
050000     ADD   1                    TO TBL-DOC-CNT (WS-SUB).          03940000
050100     ADD   1                    TO TBL-ENV-CNT (WS-SUB).          03950000
050200     ADD   HASH-TOTAL           TO TBL-CONTROL-TOTAL (WS-SUB).    03960000
050300                                                                  03970000
050400     MOVE  WS-DOC-KEY           TO WS-PREV-DOC-KEY.               03980000
050500     MOVE  WS-ENV-KEY           TO WS-PREV-ENV-KEY.               03990000
050600                                                                  04000000
050700     ADD   1                    TO WS-TOTAL-ENV.                  04010000
050800     ADD   1                    TO WS-TOTAL-DOCS.                 04020000
050900                                                                  04030000
051000 0220-EXIT.  EXIT.                                                04040000
051100/*****************************************************************04050000
051200* PROCESS EXISTING HANDLING CODE                                 *04060000
051300******************************************************************04070000
051400 0250-EXISTING-ENTRY.                                             04080000
051500                                                                  04090000
051600     ADD   1                    TO TBL-PAGE-CNT (WS-SUB).         04100000
051700                                                                  04110000
051800     IF WS-DOC-KEY        =   WS-PREV-DOC-KEY                     04120000
051900          IF WS-ENV-KEY NOT = WS-PREV-ENV-KEY                     04130000
052000*            SET MXBW510-ERROR        TO  TRUE                    04140000
052100             DISPLAY '** DOCUMENT / ENVELOPE ERROR ** '           04150000
052200                     '  ENVELOPE KEY.....'  UNIQUE-KEY            04160000
052300                     '  WS-ENV-KEY  .....'  WS-ENV-KEY            04170000
052400                     '  WS-PREV-ENV-KEY .'  WS-PREV-ENV-KEY       04180000
052500          END-IF                                                  04190000
052600      ELSE                                                        04200000
052700          ADD   1                  TO TBL-DOC-CNT (WS-SUB)        04210000
052800          MOVE  WS-DOC-KEY         TO WS-PREV-DOC-KEY             04220000
052900          ADD 1                    TO WS-TOTAL-DOCS               04230000
053000                                                                  04240000
053100          IF  WS-ENV-KEY  =  WS-PREV-ENV-KEY                      04250000
053200             CONTINUE                                             04260000
053300           ELSE                                                   04270000
053400             ADD   1               TO TBL-ENV-CNT (WS-SUB)        04280000
053500             MOVE  WS-ENV-KEY      TO WS-PREV-ENV-KEY             04290000
053600             ADD 1                 TO WS-TOTAL-ENV                04300000
053700          END-IF                                                  04310000
053800     END-IF.                                                      04320000
053900                                                                  04330000
054000                                                                  04340000
054100     ADD   HASH-TOTAL           TO TBL-CONTROL-TOTAL (WS-SUB).    04350000
054200                                                                  04360000
054300     MOVE 'N'                   TO WS-MATCH-FLAG.                 04370000
054400                                                                  04380000
054500                                                                  04390000
054600 0250-EXIT.  EXIT.                                                04400000
054700/*****************************************************************04410000
054800*  END-OF-JOB PROCESSING                                         *04420000
054900******************************************************************04430000
055000 0300-PROCESS-EOJ.                                                04440000
055100                                                                  04450000
055200     OPEN OUTPUT    XEROX-CONTROL-REPORT.                         04460000
055300                                                                  04470000
055400     PERFORM  VARYING WS-SUB                                      04480000
055500       FROM 1 BY 1                                                04490000
055600       UNTIL WS-SUB >  WS-SUB-MAX                                 04500000
055700                                                                  04510000
055800       IF TBL-SPECIAL-HANDLING (WS-SUB)  =  SPACES                04520000
055900         AND TBL-PAGE-CNT (WS-SUB)       = ZERO                   04530000
056000           CONTINUE                                               04540000
056100       ELSE                                                       04550000
056200         MOVE TBL-SPECIAL-HANDLING (WS-SUB)  TO DTL-HANDLING-CODE 04560000
056300         MOVE TBL-PAGE-CNT (WS-SUB)          TO DTL-PAGE-CNT      04570000
056400         MOVE TBL-DOC-CNT  (WS-SUB)          TO DTL-DOC-CNT       04580000
056500         MOVE TBL-ENV-CNT  (WS-SUB)          TO DTL-ENV-CNT       04590000
056600         MOVE TBL-CONTROL-TOTAL (WS-SUB)     TO DTL-CONTROL-TOT   04600000
056700         MOVE TBL-FORM-NAME (WS-SUB)         TO DTL-FORM-NAME     04610000
056800         MOVE TBL-RPT-DATE (WS-SUB)          TO DTL-RPT-DATE      04620000
056900         MOVE TBL-RUN-DATE (WS-SUB)          TO DTL-RUN-DATE      04630000
057000         MOVE TBL-RUN-TIME (WS-SUB)          TO DTL-RUN-TIME      04640000
057100                                                                  04650000
057200         WRITE  XEROX-CNTL-RECORD   FROM  CONTROL-RPT-DTL1        04660000
057300       END-IF                                                     04670000
057400                                                                  04680000
057500     END-PERFORM.                                                 04690000
057600                                                                  04700000
057700     WRITE  XEROX-CNTL-RECORD   FROM  CONTROL-RPT-ZZZ.            04710000
057800                                                                  04720000
057900     DISPLAY '*************************************************'. 04730000
058000     DISPLAY '***** MXBPB510 - CONTROL TOTAL PROCESSING  ******'. 04740000
058100     DISPLAY 'TOTAL PAGES PROCESSED.......' WS-TOTAL-RECORDS.     04750000
058200     DISPLAY 'TOTAL DOCS  PROCESSED.......' WS-TOTAL-DOCS.        04760000
058300     DISPLAY 'TOTAL ENVS  PROCESSED.......' WS-TOTAL-ENV.         04770000
058400     DISPLAY '*************************************************'. 04780000
058500                                                                  04790000
058600     CLOSE  XEROX-CONTROL-REPORT.                                 04800000
058700                                                                  04810000
058800                                                                  04820000
058900 0300-EXIT.  EXIT.                                                04830000
059000*                                                                 04840000
059100* RIS 00.636 START                                                04850000
059200     COPY MXWP31.                                                 04860000
059300                                                                  04870003
059400     COPY MXWP02.                                                 04880003
059500* RIS 00.636 END                                                  04890003
059600*                                                                 04900003