000010*************************                                         12/05/99
000020 IDENTIFICATION DIVISION.                                         MXBPW021
000030*************************                                            LV003
000040                                                                     CL**2
000050 PROGRAM-ID.    MXBPW021.                                            CL**2
000060 INSTALLATION.  TEST.                                                CL**2
000070 DATE-WRITTEN.  APRIL 1999.                                          CL**2
000080 DATE-COMPILED.                                                      CL**2
000090*================================================================*   CL**2
000100*                                                                |   CL**2
000110* MAX SYSTEM STANDARD PROGRAM DESCRIPTION AREA                   |   CL**2
000120*                                                                |   CL**2
000130*================================================================*   CL**2
000140* PROGRAM  | MXBPW021 | TRANS ID | N/A  |                        |   CL**2
000150*----------+----------+----------+------+------------------------*   CL**2
000160* DESCRIPTION:                                                   |   CL**2
000170*                                                                |   CL**2
000180*      LANGUAGE TRANSLATION BATCH MODULE                         |   CL**2
000190*               COMMON BATCH CALLED SUBROUTINE                   |   CL**2
000200*                                                                |   CL**2
000210*                                                                |   CL**2
000220* THE PROGRAM WILL DETERMINE WHETHER A REPORT WILL BE PRINTED    |   CL**2
000230* IN A LANGUAGE BASED ON THE ENTITY IT IS PRINTING OR WHETHER    |   CL**2
000240* THE ENTIRE REPORT SHOULD BE GENERATED IN A PARTICULAR LANGUAGE |   CL**2
000250*                                                                |   CL**2
000260* THIS IS THE BATCH VERSION. THE ONLINE VERSION ALSO EXISTS.     |   CL**2
000270*                                                                |   CL**2
000280* THE ONLINE VERSION IS MXCPW021.                                |   CL**2
000290*                                                                |   CL**2
000300*================================================================*   CL**2
000310/                                                                    CL**2
000320*                                                                    CL**2
000330*================================================================*   CL**2
000340*   CALL   | PROGRAM DESCRIPTION                                 |   CL**2
000350*----------+-----------------------------------------------------*   CL**2
000360* MXBCABT  |  MAX STANDARD ABORT ROUTINE.                        |   CL**2
000370*================================================================*   CL**2
000380/                                                                    CL**2
000390*================================================================*   CL**2
000400*  TABLE   |   VIEW   |     DESCRIPTION                          |   CL**2
000410*----------+-----------------------------------------------------*   CL**2
000420* NONE.    |     -    |           -                              |   CL**2
000430*================================================================*   CL**2
000440*   PLAN   | BIND INCLUDE MEMBERS                                |   CL**2
000450*----------+-----------------------------------------------------*   CL**2
000460*          |                                                     |   CL**2
000470*-----------------------------------------------------------------   CL**2
000480/                                                                    CL**2
000550/*********************                                               CL**2
000560 ENVIRONMENT DIVISION.                                               CL**2
000570**********************                                               CL**2
000580                                                                     CL**2
000590 CONFIGURATION SECTION.                                              CL**2
000600                                                                     CL**2
000610 SOURCE-COMPUTER.   IBM-3090.                                        CL**2
000620 OBJECT-COMPUTER.   IBM-3090.                                        CL**2
000630                                                                     CL**2
000640                                                                     CL**2
000650*************************                                            CL**2
000660 DATA DIVISION.                                                      CL**2
000670*************************                                            CL**2
000680                                                                     CL**2
000690/************************                                            CL**2
000700 WORKING-STORAGE SECTION.                                            CL**2
000710*************************                                            CL**2
000720/                                                                    CL**2
000730     EXEC SQL                                                        CL**2
000740          INCLUDE MXWW021                                            CL**2
000750     END-EXEC.                                                       CL**2
000760/                                                                    CL**2
000770*+-------------------------------------------------+                 CL**2
000780*|      C O N S T A N T S                          |                 CL**2
000790*+-------------------------------------------------+                 CL**2
000800 01 WS-CONSTANTS.                                                    CL**2
000810         10  WS-PGM-NAME              PIC X(08) VALUE 'MXBPW021'.    CL**2
000820                                                                     CL**2
000830                                                                     CL**2
000840/**************************************************************      CL**2
000850***             ERROR MESSAGE HANDLING                      ***      CL**2
000860***************************************************************      CL**2
000870     EXEC SQL                                                        CL**2
000880          INCLUDE MXWW03                                             CL**2
000890     END-EXEC.                                                       CL**2
000900                                                                     CL**3
000910/+---------------------------------------------+                     CL**3
000920*|  D C L G E N S  A N D   C O P Y B O O K S   |                     CL**2
000930*+---------------------------------------------+                     CL**2
000940                                                                     CL**3
000950     EXEC SQL                                                        CL**2
000960        INCLUDE SQLCA                                                CL**2
000970     END-EXEC.                                                       CL**2
000980                                                                     CL**3
000990/  LANGUAGE TRANSLATION TABLE                                        CL**3
001000                                                                     CL**2
001010     EXEC SQL                                                        CL**2
001020        INCLUDE VWMCTRLT                                             CL**2
001030     END-EXEC.                                                       CL**2
001040                                                                     CL**3
001050/  CONTROL ENTITY TABLE                                              CL**3
001060                                                                     CL**3
001070     EXEC SQL                                                        CL**3
001080        INCLUDE VWMCN00                                              CL**3
001090     END-EXEC.                                                       CL**3
001100                                                                     CL**3
001110/****************                                                    CL**2
001120 LINKAGE SECTION.                                                    CL**2
001130*****************                                                    CL**2
001140     EXEC SQL                                                        CL**2
001150          INCLUDE MXCW021                                            CL**2
001160     END-EXEC.                                                       CL**2
001170                                                                     CL**2
001180/**********************************************************          CL**2
001190 PROCEDURE DIVISION USING MXCW021-LANGUAGE-TRANSLATION.              CL**2
001200**********************************************************           CL**2
001210                                                                     CL**2
001220**************************************                               CL**2
001230 000-MAINLINE.                                                       CL**2
001240**************************************                               CL**2
001250                                                                     CL**2
001260     EXEC SQL                                                        CL**2
001270          INCLUDE MXPW021                                            CL**2
001280     END-EXEC.                                                       CL**2
001290                                                                     CL**2
001300 400-DISPLAY.                                                        CL**2
001310                                                                     CL**2
001320     MOVE    SEQ-NO          OF DCLVWMCTRLT TO DISP-SEQ-NO           CL**2
001330     DISPLAY PGM-NAME        OF DCLVWMCTRLT '-'                      CL**2
001340             DISP-SEQ-NO                    '-'                      CL**2
001350             PGM-NAME-QLF-ID OF DCLVWMCTRLT.                         CL**2
001360                                                                     CL**2
001370     SET CLEAR-DISPLAY       TO TRUE.                                CL**2
001380                                                                     CL**2
001390 400-EXIT.  EXIT.                                                    CL**2
001400/**************************************************************      CL**2
001410***             ERROR MESSAGE HANDLING                      ***      CL**2
001420***************************************************************      CL**2
001430                                                                     CL**2
001440     EXEC SQL                                                        CL**2
001450          INCLUDE MXWP02                                             CL**2
001460     END-EXEC.                                                       CL**2
001470                                                                     CL**2
