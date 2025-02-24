000600*----------+----------------------+-------------------------------00060000
000700 01 ABNORMAL-TERMINATION-AREA.                                    00070000
000800     05 FILLER                        PIC X(8)  VALUE 'ABT AREA'. 00080000
000900     05 ABT-TEST-FACILITY-AREA.                                   00090000
001000        10 ABT-TEST-FACILITY-IND      PIC X     VALUE 'N'.        00100000
001100                 88 ABT-TEST-FACILITY-ACTIVE       VALUE 'Y'.     00110000
001200                 88 ABT-TEST-FACILITY-NOT-ACTIVE   VALUE 'N'.     00120000
001300        10 ABT-TEST-MODE-IND          PIC X     VALUE 'P'.        00130000
001400                 88 ABT-TEST-MODE-ABT              VALUE 'A'.     00140000
001500                 88 ABT-TEST-MODE-PGM              VALUE 'P'.     00150000
001600*   IF THE TELON TEST FACILITY IS ACTIVE                          00160000
001700*        AND THE ABT-TEST-MODE IS "P"                             00170000
001800*   THEN THE ABT-CONTROL-INDICATOR WILL BE FORCED TO SPACE.       00180000
001900        10 FILLER                     PIC X(2)  VALUE LOW-VALUES. 00190000
002000        10 ABT-TEST-FACILITY-RESERVE  PIC X(4)  VALUE LOW-VALUES. 00200000
002100     05 ABT-CONTROL-INFO.                                         00210000
002200        10 ABT-CONTROL-INDICATOR      PIC X     VALUE 'E'.        00220000
002300           88 ABT-DO-ABEND            VALUE 'A'.                  00230000
002400           88 ABT-DO-WRITE            VALUE 'E'.                  00240000
002500           88 ABT-DO-TRANSFER         VALUE 'R'.                  00250000
002600           88 ABT-CONTINUE-PROCESS    VALUE ' '.                  00260000
002700        10 ABT-IN-PROGRESS            PIC X     VALUE 'N'.        00270000
002800* P0509538-S                                                      00280000
002900*       10 ABT-DYNAMIC-CONTROL-PGM    PIC X(8)  VALUE 'MXBCABT'.  00290000
003000        10 ABT-DYNAMIC-CONTROL-PGM    PIC X(8)  VALUE 'MXBPCABT'. 00300000
003100* P0509538-E                                                      00310000
003200        10 ABT-DYNAMIC-CONTROL-RC     PIC S9(4) COMP VALUE +0.    00320000
003300        10 ABT-NUMBER-OF-STD-PARMS    PIC 9(2)  COMP VALUE 7.     00330000
003400        10 ABT-NUMBER-OF-USER-PARMS   PIC 9(2)  COMP VALUE 0.     00340000
003500        10 ABT-NUMBER-OF-DA-PARMS     PIC 9(2)  COMP VALUE 0.     00350000
003600        10 ABT-PGM-GEN-TYPE           PIC X(4)  VALUE 'BATC'.     00360000
003700           88 ABT-PGM-IS-TSOPGM       VALUE 'TSO '.               00370000
003800           88 ABT-PGM-IS-IMSDYN       VALUE 'IDYN'.               00380000
003900           88 ABT-PGM-IS-IMSSTAT      VALUE 'ISTA'.               00390000
004000           88 ABT-PGM-IS-IMSDRVR      VALUE 'IDRV'.               00400000
004100           88 ABT-PGM-IS-CICSPGM      VALUE 'CICS'.               00410000
004200           88 ABT-PGM-IS-BATCHPGM     VALUE 'BATC'.               00420000
004300        10 ABT-PGM-GEN-REL-LEVEL      PIC X(4)  VALUE '2.1A'.     00430000
004400        10 ABT-PGM-NAME               PIC X(8).                   00440000
004500        10 ABT-PGM-TRAN-CODE          PIC X(8).                   00450000
004600        10 ABT-PGM-MAP-NAME           PIC X(8).                   00460000
004700        10 ABT-NEXT-PROGRAM-NAME.                                 00470000
004800           15 ABT-NEXT-PROGRAM-NAME-HDR  PIC X(4).                00480000
004900           15 ABT-NEXT-PROGRAM-NAME-ID   PIC X(4).                00490000
005000        10 FILLER                     PIC X(2) VALUE LOW-VALUES.  00500000
005100        10 ABT-ERROR-MESSAGE          PIC X(80) VALUE SPACES.     00510000
005200        10 FILLER                     PIC X(8)  VALUE LOW-VALUES. 00520000
005300 SKIP1                                                            00530000
005400     05 ABT-PGM-ERROR-DATA.                                       00540000
005500        10 ABT-ERROR-SECTION.                                     00550000
005600           15 ABT-ERROR-SECTION-NAME  PIC X(5)  VALUE SPACES.     00560000
005700           15 ABT-ERROR-SECTION-SUB   PIC X(3)  VALUE SPACES.     00570000
005800        10 ABT-PROGRAM-FUNCTION  REDEFINES                        00580000
005900                   ABT-ERROR-SECTION  PIC X(8).                   00590000
006000        10 ABT-ERROR-ACTIVITY         PIC X(4).                   00600000
006100           88 ABT-ERROR-IS-TP-IMS     VALUE 'IMS '.               00610000
006200           88 ABT-ERROR-IS-TP-CICS    VALUE 'CICS'.               00620000
006300           88 ABT-ERROR-IS-TP-TSO     VALUE 'TSO '.               00630000
006400           88 ABT-ERROR-IS-SEQ        VALUE 'SEQ '.               00640000
006500           88 ABT-ERROR-IS-VSAM       VALUE 'VSAM'.               00650000
006600           88 ABT-ERROR-IS-DLI        VALUE 'DLI '.               00660000
006700           88 ABT-ERROR-IS-EXECDLI    VALUE 'XDLI'.               00670000
006800           88 ABT-ERROR-IS-DB2        VALUE 'DB2 '.               00680000
006900           88 ABT-ERROR-IS-CQUEUE     VALUE 'CQUE'.               00690000
007000           88 ABT-ERROR-IS-CJOURNAL   VALUE 'CJRL'.               00700000
007100        10 ABT-ERROR-ABEND-CODE       PIC S9(4) COMP VALUE +0.    00710000
007200        10 FILLER                     PIC X(16) VALUE LOW-VALUES. 00720000
007300 SKIP1                                                            00730000
007400     05 ABT-DATA-ACCESS-INFO.                                     00740000
007500        10 ABT-DA-FUNCTION            PIC X(8)  VALUE SPACES.     00750000
007600        10 ABT-DA-FUNCTION-DLI  REDEFINES  ABT-DA-FUNCTION.       00760000
007700           15 ABT-DA-FUNC-DLI         PIC X(4).                   00770000
007800           15 ABT-DA-FUNC-PCB-TYPE    PIC X(4).                   00780000
007900        10 ABT-U100-SUB               PIC X(3)  VALUE SPACES.     00790000
008000        10 FILLER                     PIC X(1)  VALUE LOW-VALUES. 00800000
008100        10 ABT-DA-ACCESS-NAME         PIC X(8).                   00810000
008200        10 ABT-DA-GENERIC-STATUS      PIC X(3)  VALUE SPACES.     00820000
008300           88 ABT-DA-OK               VALUE 'OK '.                00830000
008400           88 ABT-DA-DUPLICATE        VALUE 'DUP'.                00840000
008500           88 ABT-DA-NOTAVAIL         VALUE 'NAV'.                00850000
008600           88 ABT-DA-NOTFOUND         VALUE 'NFD'.                00860000
008700           88 ABT-DA-ENDFILE          VALUE 'EOF' 'NFD'.          00870000
008800           88 ABT-DA-LOGICERR         VALUE 'LOG'.                00880000
008900           88 ABT-DA-SECURITY         VALUE 'SEC'.                00890000
009000           88 ABT-DA-DBMERROR         VALUE 'DBM'.                00900000
009100           88 ABT-DA-ANYERROR         VALUE 'DUP' 'NAV'           00910000
009200                                            'NFD' 'EOF'           00920000
009300                                            'LOG' 'SEC'           00930000
009400                                            'DBM'.                00940000
009500        10 FILLER                     PIC X(1)  VALUE LOW-VALUES. 00950000
009600        10 ABT-DA-SPECIFIC-STATUS     PIC X(6)  VALUE LOW-VALUES. 00960000
009700        10 FILLER               REDEFINES ABT-DA-SPECIFIC-STATUS. 00970000
009800           15 ABT-DLI-STATUS          PIC X(2).                   00980000
009900           15 FILLER                  PIC X(4).                   00990000
010000        10 FILLER               REDEFINES ABT-DA-SPECIFIC-STATUS. 01000000
010100           15 ABT-DB2-STATUS          PIC S9(9) COMP-4.           01010000
010200           15 FILLER                  PIC X(2).                   01020000
010300        10 FILLER               REDEFINES ABT-DA-SPECIFIC-STATUS. 01030000
010400           15 ABT-VSAM-CICS-STATUS    PIC X(1).                   01040000
010500           15 FILLER                  PIC X(5).                   01050000
010600        10 FILLER               REDEFINES ABT-DA-SPECIFIC-STATUS. 01060000
010700           15 ABT-BATCH-STATUS        PIC X(2).                   01070000
010800           15 FILLER                  PIC X(4).                   01080000
010900        10 FILLER                     PIC X(16).                  01090000
011000*                                                                 01100000
011100 01  SQL-ERRMSG.                                                  01110000
011200     05 SQL-ERRMSG-LEN                PIC S9(4)  COMP VALUE +960. 01120000
011300     05 SQL-ERRMSG-TEXT               PIC X(120)                  01130000
011400                                      OCCURS 8 TIMES.             01140000
011500 01  SQL-ERRMSG-RE REDEFINES SQL-ERRMSG.                          01150000
011600     05 FILLER                        PIC S9(4)  COMP.            01160000
011700     05 SQL-ERRMSG-TEXT-RE            PIC X(60)                   01170000
011800                                      OCCURS 16 TIMES.            01180000
011900*                                                                 01190000
012000 01  I-O-EXCEPTION-CODES.                                         01200000
012100     05  DATASET-STATUS-CODES.                                    01210000
012200         10  DATASET-OK               PIC XX    VALUE '00'.       01220000
012300         10  DATASET-DUPKEY           PIC XX    VALUE '02'.       01230000
012400         10  DATASET-ENDFILE          PIC XX    VALUE '10'.       01240000
012500         10  DATASET-SEQERR           PIC XX    VALUE '21'.       01250000
012600         10  DATASET-DUPREC           PIC XX    VALUE '22'.       01260000
012700         10  DATASET-NOTFND           PIC XX    VALUE '23'.       01270000
012800         10  DATASET-INVREQ           PIC XX    VALUE '91'.       01280000
012900         10  DATASET-ILLOGIC          PIC XX    VALUE '92'.       01290000
013000         10  DATASET-NOTOPEN          PIC XX    VALUE '95'.       01300000
013100         10  DATASET-VERIFIED         PIC XX    VALUE '97'.       01310000
013200*                                                                 01320000
013300     05  DATA-ACCESS-STATUS-LITS.                                 01330000
013400         10  DA-OK-LIT                PIC X(3)  VALUE 'OK '.      01340000
013500         10  DA-DUPLICATE-LIT         PIC X(3)  VALUE 'DUP'.      01350000
013600         10  DA-NOTAVAIL-LIT          PIC X(3)  VALUE 'NAV'.      01360000
013700         10  DA-NOTFOUND-LIT          PIC X(3)  VALUE 'NFD'.      01370000
013800         10  DA-ENDFILE-LIT           PIC X(3)  VALUE 'EOF'.      01380000
013900         10  DA-LOGICERR-LIT          PIC X(3)  VALUE 'LOG'.      01390000
014000         10  DA-SECURITY-LIT          PIC X(3)  VALUE 'SEC'.      01400000
014100         10  DA-DBMERROR-LIT          PIC X(3)  VALUE 'DBM'.      01410000
014200*                                                                 01420000
014300     05  DATA-ACCESS-STATUS.                                      01430000
014400         10  DA-STATUS                PIC X(3).                   01440000
014500             88  DA-OK                VALUE 'OK '.                01450000
014600             88  DA-DUPLICATE         VALUE 'DUP'.                01460000
014700             88  DA-NOTAVAIL          VALUE 'NAV'.                01470000
014800             88  DA-NOTFOUND          VALUE 'NFD'.                01480000
014900             88  DA-ENDFILE           VALUE 'EOF'                 01490000
015000                                            'NFD'.                01500000
015100             88  DA-LOGICERR          VALUE 'LOG'.                01510000
015200             88  DA-SECURITY          VALUE 'SEC'.                01520000
015300             88  DA-DBMERROR          VALUE 'DBM'.                01530000
015400             88  DA-ANYERROR          VALUE 'DUP' 'NAV' 'NFD'     01540000
015500                                            'EOF' 'LOG' 'SEC'.    01550000
015600*                                                                 01560000
015700****************************************************************  01570000
015800* IF YOU ARE INVOKING THE ABEND ROUTINE FOR SEQUENTIAL OR VSAM *  01580000
015900* ERRORS THE STATUS CODE RECIEVED MUST BE MOVED TO             *  01590000
016000* DA-STATUS-FILE.                                              *  01600000
016100****************************************************************  01610000
016200     05  DA-STATUS-FILE               PIC X(2).                   01620000
016300*                                                                 01630000
016400****************************************************************  01640000
016500* THE FOLLOWING AREA CAN BE USED TO INSERT YOUR OWN COMMENTS   *  01650000
016600* INTO THE DB2 ERROR INCIDENCE TABLE.  IT IS USED THE SAME WAY *  01660000
016700* THAT THE ONLINE XFER AREA IS USED.                           *  01670000
016800****************************************************************  01680000
016900 01  BATCH-ERROR-AREA.                                            01690000
017000     05  BATCH-ERROR-KEY-L            PIC S9(4) COMP VALUE +0.    01700000
017100     05  BATCH-ERROR-CMNT-L           PIC S9(4) COMP VALUE +0.    01710000
017200     05  BATCH-ABEND-CONTROL          PIC X.                      01720000
017300     05  BATCH-ERROR-KEY                        VALUE SPACES.     01730000
017400         10  BATCH-ERROR-KEY-1        PIC X(72).                  01740000
017500         10  BATCH-ERROR-KEY-2        PIC X(72).                  01750000
017600         10  BATCH-ERROR-KEY-3        PIC X(72).                  01760000
017700         10  BATCH-ERROR-KEY-4        PIC X(38).                  01770000
017800     05  BATCH-ERROR-CMNT                       VALUE SPACES.     01780000
017900         10  BATCH-ERROR-CMNT-1       PIC X(72).                  01790000
018000         10  BATCH-ERROR-CMNT-2       PIC X(72).                  01800000
018100         10  BATCH-ERROR-CMNT-3       PIC X(72).                  01810000
018200         10  BATCH-ERROR-CMNT-4       PIC X(38).                  01820000
