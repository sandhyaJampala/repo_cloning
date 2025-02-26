000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID. MXBPCABT.                                            00020000
000300 DATE-COMPILED.                                                   00030000
000400* ------------------------------------------------------------ *  00040000
000500*                                                              *  00050000
000600*  THIS ROUTINE PROCESSES ABNORMAL TERMINATION REQUESTS IN     *  00060000
000700*  THE BATCH ENVIRONMENT.                                      *  00070000
000800*                                                              *  00080000
000900*                                                              *  00090000
001000*  LINKAGE EDITOR CONTROL INPUT:                               *  00100000
001100*     ENTRY MXBPCABT                                           *  00110000
001200*     NAME  MXBPCABT(R)                                        *  00120000
001300*                                                              *  00130000
001400*  THIS ROUTINE MUST EXIST IN A LOAD LIBRARY ALLOCATED TO      *  00140000
001500*  THE BATCH JOB.                                              *  00150000
001600*                                                              *  00160000
004400 ENVIRONMENT DIVISION.                                            00440000
004500 CONFIGURATION SECTION.                                           00450000
004600 DATA DIVISION.                                                   00460000
004700 EJECT                                                            00470000
004800 WORKING-STORAGE SECTION.                                         00480000
004900 01  WK-VARIABLES.                                                00490000
005000     05 WK-SQL-ERRMSG-TEXT-LEN     PIC S9(9) COMP VALUE +72.      00500000
005100     05 WK-SUB                     PIC S9(4) COMP VALUE +0.       00510000
005200     05 WS-DISPLAY-SQLCODE         PIC +999.                      00520000
005300* SCR 3524 CHANGE BEGIN                                           00530000
005400 01     ABD-CODE                   PIC S9(09) BINARY.             00540000
005500 01     TIMING                     PIC S9(09) BINARY.             00550000
005600 01     CEE3ABD                    PIC X(08) VALUE 'CEE3ABD'.     00560000
005700* SCR 3524 CHANGE END                                             00570000
005800 EJECT                                                            00580000
005900     EXEC SQL                                                     00590000
006000         INCLUDE SQLCA                                            00600000
006100     END-EXEC.                                                    00610000
006200 SKIP2                                                            00620000
006300     EXEC SQL                                                     00630000
006400         INCLUDE VWMER00                                          00640000
006500     END-EXEC.                                                    00650000
006600 SKIP3                                                            00660000
006700* TRACKER 3524 CHANGE BEGIN                                       00670000
006800     EXEC SQL                                                     00680000
006900         INCLUDE VWMCX00                                          00690000
007000     END-EXEC.                                                    00700000
007100* TRACKER 3524 CHANGE BEGIN                                       00710000
007200 EJECT                                                            00720000
007300 LINKAGE SECTION.                                                 00730000
007400 01  ABNORMAL-TERMINATION-AREA.                                   00740000
007500     EXEC SQL                                                     00750000
007600         INCLUDE MXADDABT                                         00760000
007700     END-EXEC.                                                    00770000
007800 EJECT                                                            00780000
007900 01  LK-SQLCA.                                                    00790000
008000     05 LK-SQLCAID            PIC X(8).                           00800000
008100     05 LK-SQLCABC            PIC S9(9) COMP-4.                   00810000
008200     05 LK-SQLCODE            PIC S9(9) COMP-4.                   00820000
008300     05 LK-SQLERRM.                                               00830000
008400         49  LK-SQLERRML      PIC S9(4) COMP-4.                   00840000
008500         49  LK-SQLERRMC      PIC X(70).                          00850000
008600     05 LK-SQLERRP            PIC X(8).                           00860000
008700     05 LK-SQLERRD     OCCURS 6 TIMES                             00870000
008800                              PIC S9(9)  COMP-4.                  00880000
008900     05 LK-SQLWARN.                                               00890000
009000         10  LK-SQLWARN0      PIC X.                              00900000
009100         10  LK-SQLWARN1      PIC X.                              00910000
009200         10  LK-SQLWARN2      PIC X.                              00920000
009300         10  LK-SQLWARN3      PIC X.                              00930000
009400         10  LK-SQLWARN4      PIC X.                              00940000
009500         10  LK-SQLWARN5      PIC X.                              00950000
009600         10  LK-SQLWARN6      PIC X.                              00960000
009700         10  LK-SQLWARN7      PIC X.                              00970000
009800     05 LK-SQLEXT             PIC X(8).                           00980000
009900 SKIP2                                                            00990000
010000*                                                                 01000000
010100 01  BATCH-ERROR-AREA.                                            01010000
010200     05  BATCH-ERROR-KEY-L            PIC S9(4) COMP.             01020000
010300     05  BATCH-ERROR-CMNT-L           PIC S9(4) COMP.             01030000
010400     05  BATCH-ABEND-CONTROL          PIC X.                      01040000
010500     05  BATCH-ERROR-KEY.                                         01050000
010600         10  BATCH-ERROR-KEY-1        PIC X(72).                  01060000
010700         10  BATCH-ERROR-KEY-2        PIC X(72).                  01070000
010800         10  BATCH-ERROR-KEY-3        PIC X(72).                  01080000
010900         10  BATCH-ERROR-KEY-4        PIC X(38).                  01090000
011000     05  BATCH-ERROR-CMNT.                                        01100000
011100         10  BATCH-ERROR-CMNT-1       PIC X(72).                  01110000
011200         10  BATCH-ERROR-CMNT-2       PIC X(72).                  01120000
011300         10  BATCH-ERROR-CMNT-3       PIC X(72).                  01130000
011400         10  BATCH-ERROR-CMNT-4       PIC X(38).                  01140000
011500 SKIP2                                                            01150000
011600*                                                                 01160000
011700 01  SQL-ERRMSG.                                                  01170000
011800     05 SQL-ERRMSG-LEN                PIC S9(4)  COMP VALUE +576. 01180000
011900     05 SQL-ERRMSG-GROUP.                                         01190000
012000         10 SQL-ERRMSG-TEXT           PIC X(072)                  01200000
012100                                      OCCURS 8 TIMES.             01210000
012200 EJECT                                                            01220000
012300 PROCEDURE DIVISION USING    ABNORMAL-TERMINATION-AREA            01230000
012400                             LK-SQLCA                             01240000
012500                             BATCH-ERROR-AREA                     01250000
012600                             SQL-ERRMSG.                          01260000
012700* ------------------------------------------------------------ *  01270000
012800*  PERFORM MAIN LINE PROCESSING OF CUSTOM ABEND HANDLING       *  01280000
012900*  MODULE.                                                     *  01290000
013000* ------------------------------------------------------------ *  01300000
013100 MAIN-LINE               SECTION.                                 01310000
013200                                                                  01320000
013300     DISPLAY '*+++++++++++++++++++++++++++++++++++++*'            01330000
013400     DISPLAY '*++++ MXBPCABT ROUTINE CALLED +++++++++*'           01340000
013500     DISPLAY '*+++++++++++++++++++++++++++++++++++++*'            01350000
013600                                                                  01360000
013700     IF NOT ABT-PGM-IS-BATCHPGM                                   01370000
013800        CALL 'ADRABND' USING ABT-ERROR-ABEND-CODE                 01380000
013900     END-IF.                                                      01390000
014000                                                                  01400000
014100***  PLEASE REMOVE THE CHANGE BEFORE PROMOTING                    01410000
014200     DISPLAY 'START'                                              01420000
014300***  END OF CHANGE                                                01430000
014400     DISPLAY 'PROGRAM:       ',ABT-PGM-NAME                       01440000
014500     DISPLAY 'ACTIVITY:      ',ABT-ERROR-ACTIVITY                 01450000
014600     DISPLAY 'ABEND CODE:    ',ABT-ERROR-ABEND-CODE               01460000
014700     DISPLAY 'SECTION:       ',ABT-ERROR-SECTION                  01470000
014800     DISPLAY 'DATA ACCESS:   ',ABT-DA-ACCESS-NAME                 01480000
014900     DISPLAY 'DATA FUNCTION: ',ABT-DA-FUNCTION                    01490000
015000                                                                  01500000
015100     EVALUATE TRUE                                                01510000
015200       WHEN ABT-ERROR-IS-DB2                                      01520000
015300           CALL 'DSNTIAR' USING LK-SQLCA                          01530000
015400                                SQL-ERRMSG                        01540000
015500                                WK-SQL-ERRMSG-TEXT-LEN            01550000
015600           PERFORM VARYING WK-SUB FROM 1 BY 1                     01560000
015700             UNTIL WK-SUB > 8                                     01570000
015800                OR SQL-ERRMSG-LEN < ZERO                          01580000
015900                  DISPLAY SQL-ERRMSG-TEXT(WK-SUB)                 01590000
016000                  SUBTRACT 72 FROM SQL-ERRMSG-LEN                 01600000
016100           END-PERFORM                                            01610000
016200       WHEN ABT-ERROR-IS-VSAM                                     01620000
016300       WHEN ABT-ERROR-IS-SEQ                                      01630000
016400           DISPLAY 'FILE STATUS:   ', ABT-BATCH-STATUS            01640000
016500           DISPLAY 'GENERIC STATUS:', ABT-DA-GENERIC-STATUS       01650000
016600     END-EVALUATE.                                                01660000
016700                                                                  01670000
016800     IF  BATCH-ERROR-KEY-L > ZERO                                 01680000
016900         DISPLAY 'ERROR KEY:'                                     01690000
017000         IF  BATCH-ERROR-KEY-1 NOT = SPACES                       01700000
017100             DISPLAY BATCH-ERROR-KEY-1                            01710000
017200         END-IF                                                   01720000
017300         IF  BATCH-ERROR-KEY-2 NOT = SPACES                       01730000
017400             DISPLAY BATCH-ERROR-KEY-2                            01740000
017500         END-IF                                                   01750000
017600         IF  BATCH-ERROR-KEY-3 NOT = SPACES                       01760000
017700             DISPLAY BATCH-ERROR-KEY-3                            01770000
017800         END-IF                                                   01780000
017900         IF  BATCH-ERROR-KEY-4 NOT = SPACES                       01790000
018000             DISPLAY BATCH-ERROR-KEY-4                            01800000
018100         END-IF                                                   01810000
018200     END-IF.                                                      01820000
018300     IF  BATCH-ERROR-CMNT-L > ZERO                                01830000
018400         DISPLAY 'ERROR CMNT:'                                    01840000
018500         IF  BATCH-ERROR-CMNT-1 NOT = SPACES                      01850000
018600             DISPLAY BATCH-ERROR-CMNT-1                           01860000
018700         END-IF                                                   01870000
018800         IF  BATCH-ERROR-CMNT-2 NOT = SPACES                      01880000
018900             DISPLAY BATCH-ERROR-CMNT-2                           01890000
019000         END-IF                                                   01900000
019100         IF  BATCH-ERROR-CMNT-3 NOT = SPACES                      01910000
019200             DISPLAY BATCH-ERROR-CMNT-3                           01920000
019300         END-IF                                                   01930000
019400         IF  BATCH-ERROR-CMNT-4 NOT = SPACES                      01940000
019500             DISPLAY BATCH-ERROR-CMNT-4                           01950000
019600         END-IF                                                   01960000
019700     END-IF.                                                      01970000
019800*********************************************************         01980000
019900* MAX CODE TO:                                          *         01990000
020000*  IF ABEND FUNCTION (ABT-DO-ABEND IS TRUE)             *         02000000
020100*     (1). PERFORM A ROLLBACK                           *         02010000
020200*     (2). PRODUCE A DUMP                               *         02020000
020300*  IF NOT ABEND FUNCTION (ABT-DO-ABEND IS FALSE)        *         02030000
020400*     (1). WRITE OUT AN ERROR ROW TO THE TABLE          *         02040000
020500*********************************************************         02050000
020600     IF ABT-DO-ABEND                                              02060000
020700        EXEC SQL                                                  02070000
020800             ROLLBACK                                             02080000
020900        END-EXEC                                                  02090000
021000     END-IF.                                                      02100000
021100                                                                  02110000
021200     IF LK-SQLCODE EQUAL -922 OR -923                             02120000
021300        MOVE LK-SQLCODE    TO WS-DISPLAY-SQLCODE                  02130000
021400        DISPLAY ' '                                               02140000
021500        DISPLAY '*+++++++++++++++++++++++++++++++++++++*'         02150000
021600        DISPLAY '*+++++++++   WARNING !!  +++++++++++++*'         02160000
021700        DISPLAY '*+++++++++++++++++++++++++++++++++++++*'         02170000
021800        DISPLAY '* MXBPCABT - DB2 AUTHORIZATION ERROR   *'        02180000
021900        DISPLAY '*        SQLCODE = ' WS-DISPLAY-SQLCODE          02190000
022000                                       '               *'         02200000
022100        DISPLAY '*+++++++++++++++++++++++++++++++++++++*'         02210000
022200        DISPLAY ' '                                               02220000
022300     ELSE                                                         02230000
022400         INITIALIZE DCLVWMER00                                    02240000
022500         MOVE ABT-PGM-NAME              TO PGM-NAME               02250000
022600                                        OF DCLVWMER00             02260000
022700         MOVE 'BATCH'                   TO LOGON-ID               02270000
022800         MOVE ABT-ERROR-ACTIVITY        TO ERROR-TYPE-CODE        02280000
022900         EVALUATE TRUE                                            02290000
023000           WHEN ABT-ERROR-IS-DB2                                  02300000
023100               MOVE LK-SQLCODE          TO ERROR-SQLCODE          02310000
023200               MOVE LK-SQLERRML         TO ERROR-SQLERRMC-LEN     02320000
023300               MOVE LK-SQLERRMC         TO ERROR-SQLERRMC-TEXT    02330000
023400               MOVE LK-SQLERRP          TO ERROR-SQLERRP          02340000
023500               MOVE LK-SQLERRD(1)       TO ERROR-SQLERRD1         02350000
023600               MOVE LK-SQLERRD(2)       TO ERROR-SQLERRD2         02360000
023700               MOVE LK-SQLERRD(5)       TO ERROR-SQLERRD5         02370000
023800               MOVE LK-SQLERRD(6)       TO ERROR-SQLERRD6         02380000
023900               MOVE LK-SQLWARN1         TO ERROR-SQLWARN1         02390000
024000               MOVE LK-SQLWARN2         TO ERROR-SQLWARN2         02400000
024100               MOVE LK-SQLWARN3         TO ERROR-SQLWARN3         02410000
024200               MOVE LK-SQLWARN4         TO ERROR-SQLWARN4         02420000
024300               MOVE LK-SQLWARN5         TO ERROR-SQLWARN5         02430000
024400               MOVE LK-SQLWARN6         TO ERROR-SQLWARN6         02440000
024500               MOVE ABT-DA-FUNCTION     TO SQL-FUNC-CODE          02450000
024600               MOVE ABT-DA-ACCESS-NAME  TO VIEW-NAME              02460000
024700           WHEN ABT-ERROR-IS-VSAM                                 02470000
024800           WHEN ABT-ERROR-IS-SEQ                                  02480000
024900               MOVE ABT-BATCH-STATUS    TO ERROR-SQLCODE          02490000
025000               MOVE ABT-DA-FUNCTION     TO SQL-FUNC-CODE          02500000
025100               MOVE ABT-DA-ACCESS-NAME  TO VIEW-NAME              02510000
025200         END-EVALUATE                                             02520000
025300         MOVE ABT-ERROR-ABEND-CODE      TO CICS-ABEND-CODE        02530000
025400         MOVE ABT-ERROR-SECTION         TO PGH-ID                 02540000
025500         MOVE BATCH-ERROR-CMNT-L        TO ERROR-COMMENT-TEXT-LEN 02550000
025600         MOVE BATCH-ERROR-CMNT          TO ERROR-COMMENT-TEXT-TEXT02560000
025700         MOVE BATCH-ERROR-KEY-L         TO KEY-DATA-LEN           02570000
025800         MOVE BATCH-ERROR-KEY           TO KEY-DATA-TEXT          02580000
025900         IF ABT-DO-ABEND                                          02590000
026000            EXEC SQL                                              02600000
026100                 COMMIT                                           02610000
026200            END-EXEC                                              02620000
026300         END-IF                                                   02630000
026400         EXEC SQL INSERT                                          02640000
026500                  INTO VWMER00                                    02650000
026600                        (PGM_NAME,                                02660000
026700                         LOGON_ID,                                02670000
026800                         CICS_TRANS_CODE,                         02680000
026900                         CICS_TERM_ID,                            02690000
027000                         ERROR_TYPE_CODE,                         02700000
027100                         ERROR_SQLCODE,                           02710000
027200                         SQL_FUNC_CODE,                           02720000
027300                         ERROR_SQLERRMC,                          02730000
027400                         ERROR_SQLERRP,                           02740000
027500                         ERROR_SQLERRD1,                          02750000
027600                         ERROR_SQLERRD2,                          02760000
027700                         ERROR_SQLERRD5,                          02770000
027800                         ERROR_SQLERRD6,                          02780000
027900                         ERROR_SQLWARN1,                          02790000
028000                         ERROR_SQLWARN2,                          02800000
028100                         ERROR_SQLWARN3,                          02810000
028200                         ERROR_SQLWARN4,                          02820000
028300                         ERROR_SQLWARN5,                          02830000
028400                         ERROR_SQLWARN6,                          02840000
028500                         VIEW_NAME,                               02850000
028600                         CICS_ABEND_CODE,                         02860000
028700                         CICS_REGION_ID,                          02870000
028800                         CICS_EIBFN_CODE,                         02880000
028900                         CICS_EIBR_CODE,                          02890000
029000                         PGH_ID,                                  02900000
029100                         KEY_DATA,                                02910000
029200                         ERROR_COMMENT_TEXT)                      02920000
029300                VALUES (:DCLVWMER00.PGM-NAME,                     02930000
029400                        :DCLVWMER00.LOGON-ID,                     02940000
029500                        :DCLVWMER00.CICS-TRANS-CODE,              02950000
029600                        :DCLVWMER00.CICS-TERM-ID,                 02960000
029700                        :DCLVWMER00.ERROR-TYPE-CODE,              02970000
029800                        :DCLVWMER00.ERROR-SQLCODE,                02980000
029900                        :DCLVWMER00.SQL-FUNC-CODE,                02990000
030000                        :DCLVWMER00.ERROR-SQLERRMC,               03000000
030100                        :DCLVWMER00.ERROR-SQLERRP,                03010000
030200                        :DCLVWMER00.ERROR-SQLERRD1,               03020000
030300                        :DCLVWMER00.ERROR-SQLERRD2,               03030000
030400                        :DCLVWMER00.ERROR-SQLERRD5,               03040000
030500                        :DCLVWMER00.ERROR-SQLERRD6,               03050000
030600                        :DCLVWMER00.ERROR-SQLWARN1,               03060000
030700                        :DCLVWMER00.ERROR-SQLWARN2,               03070000
030800                        :DCLVWMER00.ERROR-SQLWARN3,               03080000
030900                        :DCLVWMER00.ERROR-SQLWARN4,               03090000
031000                        :DCLVWMER00.ERROR-SQLWARN5,               03100000
031100                        :DCLVWMER00.ERROR-SQLWARN6,               03110000
031200                        :DCLVWMER00.VIEW-NAME,                    03120000
031300                        :DCLVWMER00.CICS-ABEND-CODE,              03130000
031400                        :DCLVWMER00.CICS-REGION-ID,               03140000
031500                        :DCLVWMER00.CICS-EIBFN-CODE,              03150000
031600                        :DCLVWMER00.CICS-EIBR-CODE,               03160000
031700                        :DCLVWMER00.PGH-ID,                       03170000
031800                        :DCLVWMER00.KEY-DATA,                     03180000
031900                        :DCLVWMER00.ERROR-COMMENT-TEXT)           03190000
032000         END-EXEC                                                 03200000
032100         IF SQLCODE = 0                                           03210000
032200            DISPLAY ' '                                           03220000
032300            DISPLAY '*+++++++++++++++++++++++++++++++++++++*'     03230000
032400            DISPLAY '* MXBPCABT - ERROR ROW ADDED VWMER00   *'    03240000
032500            DISPLAY '*+++++++++++++++++++++++++++++++++++++*'     03250000
032600            DISPLAY ' '                                           03260000
032700         ELSE                                                     03270000
032800            MOVE SQLCODE TO WS-DISPLAY-SQLCODE                    03280000
032900            DISPLAY ' '                                           03290000
033000            DISPLAY '*+++++++++++++++++++++++++++++++++++++*'     03300000
033100            DISPLAY '*+++++++++   WARNING !!  +++++++++++++*'     03310000
033200            DISPLAY '*+++++++++++++++++++++++++++++++++++++*'     03320000
033300            DISPLAY '* MXBPCABT - INVALID SQL CODE RETURNED *'    03330000
033400            DISPLAY '* FROM ATTEMPT TO INSERT INTO VWMER00 *'     03340000
033500            DISPLAY '*        SQLCODE = ' WS-DISPLAY-SQLCODE      03350000
033600                                           '               *'     03360000
033700            DISPLAY '*+++++++++++++++++++++++++++++++++++++*'     03370000
033800            MOVE SQLCA TO LK-SQLCA                                03380000
033900            CALL 'DSNTIAR' USING LK-SQLCA                         03390000
034000                                 SQL-ERRMSG                       03400000
034100                                 WK-SQL-ERRMSG-TEXT-LEN           03410000
034200            DISPLAY ' '                                           03420000
034300         END-IF                                                   03430000
034400         DISPLAY 'ABEND PROGRAM  : ' ABT-PGM-NAME                 03440000
034500         IF ABT-DO-ABEND                                          03450000
034600            EXEC SQL                                              03460000
034700                 COMMIT                                           03470000
034800            END-EXEC                                              03480000
034900* 3524 TRACKER CHANGE BEGIN                                       03490000
035000                                                                  03500000
035100            EXEC SQL                                              03510000
035200              SELECT VALUE(MIN(HARD_ABEND_FLAG),'Y')              03520000
035300              INTO   :HARD-ABEND-FLAG                             03530000
035400              FROM VWMCX00                                        03540000
035500              WHERE PGM_NAME = :ABT-PGM-NAME                      03550000
035600              AND   HARD_ABEND_FLAG = 'N'                         03560000
035700              WITH UR                                             03570000
035800            END-EXEC                                              03580000
035900                                                                  03590000
036000            DISPLAY 'HARD ABEND FLAG: ' HARD-ABEND-FLAG           03600000
036100            IF SQLCODE NOT = +0                                   03610000
036200               DISPLAY '****************************************' 03620000
036300               DISPLAY 'VWMCX00 SELECT FAILED'                    03630000
036400               DISPLAY 'VWMCX00 SELECT SQLCODE: ' SQLCODE         03640000
036500               MOVE SQLCA TO LK-SQLCA                             03650000
036600               CALL 'DSNTIAR' USING LK-SQLCA                      03660000
036700                                    SQL-ERRMSG                    03670000
036800                                    WK-SQL-ERRMSG-TEXT-LEN        03680000
036900               DISPLAY '****************************************' 03690000
037000            END-IF                                                03700000
037100                                                                  03710000
037200* DEFAULT IS TO ALWAYS FORCE A HARD ABEND, UNLESS UNDER W010'S    03720000
037300* (911 RESTART PROGRAM) CONTROL AND A SQLCODE OF -911 IS NOTED.   03730000
037400                                                                  03740000
037500            IF HARD-ABEND-FLAG = 'N' AND LK-SQLCODE = -911        03750000
037600               CONTINUE                                           03760000
037700            ELSE                                                  03770000
037800               EVALUATE LK-SQLCODE                                03780000
037900                 WHEN -911                                        03790000
038000                   MOVE 911            TO ABD-CODE                03800000
038100                 WHEN OTHER                                       03810000
038200                   MOVE 238            TO ABD-CODE                03820000
038300               END-EVALUATE                                       03830000
038400               MOVE 0                 TO TIMING                   03840000
038500               CALL CEE3ABD  USING ABD-CODE,TIMING                03850000
038600            END-IF                                                03860000
038700         END-IF                                                   03870000
038800* 3524 TRACKER CHANGE END                                         03880000
038900     END-IF.                                                      03890000
039000                                                                  03900000
039100     GOBACK.                                                      03910000