       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CBLPGM01.                                            00020000
       AUTHOR. SOWMYA SRIDHAR.                                          00030000
      *                                                                 00040000
      * PURPOSE:  THIS IS A SAMPLE PROGRAM FOR USING SEARCH OPTION      00050000
      * -------                                                         00060000
      *                                                                 00070000
       DATA DIVISION.                                                   00080000
       WORKING-STORAGE SECTION.                                         00090000
       01 ARRAY-TABLE.                                                  00100002
          05 FILLER.                                                    00110002
             10 FILLER   PIC X(02) VALUE '01'.                          00120002
             10 FILLER   PIC X(20) VALUE 'JANUARY'.                     00130002
          05 FILLER.                                                    00140002
             10 FILLER   PIC X(02) VALUE '02'.                          00150002
             10 FILLER   PIC X(20) VALUE 'FEBRAUARY'.                   00160002
          05 FILLER.                                                    00170002
             10 FILLER   PIC X(02) VALUE '03'.                          00180002
             10 FILLER   PIC X(20) VALUE 'MARCH'.                       00190002
          05 FILLER.                                                    00200002
             10 FILLER   PIC X(02) VALUE '04'.                          00210002
             10 FILLER   PIC X(20) VALUE 'APRIL'.                       00220002
          05 FILLER.                                                    00230002
             10 FILLER   PIC X(02) VALUE '05'.                          00240002
             10 FILLER   PIC X(20) VALUE 'MAY'.                         00250002
          05 FILLER.                                                    00260002
             10 FILLER   PIC X(02) VALUE '06'.                          00270002
             10 FILLER   PIC X(20) VALUE 'JUNE'.                        00280002
          05 FILLER.                                                    00290002
             10 FILLER   PIC X(02) VALUE '07'.                          00300002
             10 FILLER   PIC X(20) VALUE 'JULY'.                        00310002
          05 FILLER.                                                    00320002
             10 FILLER   PIC X(02) VALUE '08'.                          00330002
             10 FILLER   PIC X(20) VALUE 'AUGUST'.                      00340002
          05 FILLER.                                                    00350002
             10 FILLER   PIC X(02) VALUE '09'.                          00360002
             10 FILLER   PIC X(20) VALUE 'SEPTEMBER'.                   00370002
          05 FILLER.                                                    00380002
             10 FILLER   PIC X(02) VALUE '10'.                          00390002
             10 FILLER   PIC X(20) VALUE 'OCTOBER'.                     00400002
          05 FILLER.                                                    00410002
             10 FILLER   PIC X(02) VALUE '11'.                          00420002
             10 FILLER   PIC X(20) VALUE 'NOVEMBER'.                    00430002
          05 FILLER.                                                    00440002
             10 FILLER   PIC X(02) VALUE '12'.                          00450002
             10 FILLER   PIC X(20) VALUE 'DECEMBER'.                    00460002
      *                                                                 00470001
       01 WS-ARRAY-TABLE REDEFINES ARRAY-TABLE.                         00480002
          05 WS-ARRAY OCCURS 12 TIMES INDEXED BY WS-IDX.                00490002
             10  WS-MNTH-NUM  PIC X(02).                                00500002
             10  WS-MNTH      PIC X(20).                                00510002
      *                                                                 00520002
       01 WS-NUM         PIC X(02) VALUE ZEROES.                        00530002
       01 WS-ARRAY-DATA  PIC X(20) VALUE SPACES.                        00540001
       01 WS-OUTPUT-DATA PIC X(60) VALUE SPACES.                        00550001
      *                                                                 00560001
       LINKAGE SECTION.                                                 00570001
       01 LINK-DATA.                                                    00580001
          05 LINK-LENGTH      PIC S9(04) COMP.                          00590001
          05 LINK-NUM         PIC X(02).                                00600002
      *                                                                 00610001
       PROCEDURE DIVISION USING LINK-DATA.                              00620001
      *                                                                 00630001
           MOVE LINK-NUM TO WS-NUM.                                     00640001
      *                                                                 00650001
           IF WS-NUM NOT NUMERIC                                        00660001
              DISPLAY 'PARM INCORRECT. VALUE NOT NUMERIC'               00670001
           ELSE                                                         00680001
              IF WS-NUM <= 0                                            00690001
                 DISPLAY 'PARM VALUE CANNOT BE LESS THAN ZERO'          00700001
              ELSE                                                      00710001
                 IF WS-NUM > 12                                         00720001
                    DISPLAY 'PARM CANNOT BE GREATER THAN 12'            00730001
                 ELSE                                                   00740001
                    SEARCH WS-ARRAY                                     00750002
                        AT END DISPLAY 'NO MONTH FOUND'                 00760001
                        WHEN WS-NUM = WS-MNTH-NUM(WS-IDX)               00770002
                             STRING WS-NUM,': ',                        00780001
                             WS-MNTH(WS-IDX) DELIMITED BY SIZE INTO     00790002
                             WS-OUTPUT-DATA                             00800001
                             DISPLAY WS-OUTPUT-DATA                     00810001
                    END-SEARCH                                          00820001
                 END-IF                                                 00830001
              END-IF                                                    00840001
           END-IF.                                                      00850001
      *                                                                 00860001
           STOP RUN.                                                    00870001
