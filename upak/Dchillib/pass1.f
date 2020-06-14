C$PROG PASS1
      SUBROUTINE PASS1
C
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40),NF
C
      COMMON/CCC/ ISYN(2,100),ISYT(100),ISYD(100),ISYP(100),
     &            ISPF(100),ISYV(16384),NSYM,NSYV
      CHARACTER*4 ISPF
C
      COMMON/FFF/ LIST(2002),LPAR(2002),IPSP(2002),IPSI(2002),
     &            MXPAR,NPAR,MINIP,MAXIP,LPARF
C
      COMMON/JJJ/ LIN,LOU,LU6,LER,NEREC,NSOL,LHLN,NERR
C
      COMMON/QQQ/ ITEX(20),ITIT(10),NUID
      CHARACTER*4 ITEX
C
      COMMON/XXX/ IBANF,IBAFF,IGATF,IGAFF
      CHARACTER*4 IBANF,IBAFF,IGATF,IGAFF
C
      COMMON/ZZZ/ JHPC(4096),LISTL,IREPACK,JREPACK(3)
C
      INTEGER*4 MSG(10,6),MSER(10)
C
      CHARACTER*40 MSC(6)
C
      INTEGER*4 JWD(32)
C
      CHARACTER*4 KMD,NPRDUN,LPRDUN,LKMD,IWD1,CIWD(20)
C
      EQUIVALENCE (KMD,IWD(1)),(CIWD,IWD)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/'SYMBOLIC PARAMETER OUT OF RANGE         ',
     2         'SYMBOLIC PARAMETER MULTIPLY DEFINED     ',
     3         'SYNTAX ERROR OR ILLEGAL VALUE           ',
     4         'MULTIPLE DEFINITION OF #-OF-PARAMETERS  ',
     5         'FRAGMENTED PARM-LENGTH SPECIFICATION    ',
     6         'PARM-LENGTH SPECIFICATION INCOMPLETE    '/
C
      INTEGER*4  DO,DONE
      character*4 cdo, cdone
      equivalence (cdo,do), (cdone,done)
      DATA       cDO,cDONE/'DO  ','DONE'/
C
      SAVE
C
C     **************************************************************
C     READ INPUT LINE, LIST & DETERMINE TYPE
C     **************************************************************
C
      IBANF='NO  '
      IBAFF='NO  '
      IGATF='NO  '
      IGAFF='NO  '
C
      NPRDUN='NO  '
      LPRDUN='NO  '
      IWD1='    '
C
  100 LKMD=IWD1
      READ(LIN,105,END=1900)IWD
  105 FORMAT(20A4)
      IWD1=KMD
      IERR=0
      NSOL=NSOL+1
      CALL LODJWD(IWD,JWD)
      JWD(30)=NSOL
C
      IF(KMD.EQ.'$NPR') GO TO 120       !PROCESS COMPLETELY IN PASS1
C
      IF(LKMD.EQ.'$LPR'.AND.KMD.NE.'$LPR') THEN
                                           LPRDUN='YES '
                                           IF(LPARF.LE.0) GO TO 1440
                                           ENDIF
C
      IF(KMD.EQ.'$LPR') GO TO 210       !PROCESS COMPLETELY IN PASS1
      IF(KMD.EQ.'$LST') GO TO 120       !PROCESS COMPLETELY IN PASS1
      IF(KMD.EQ.'$DIM') GO TO 220       !PROCESS COMPLETELY IN PASS1
      IF(KMD.EQ.'$DIP') GO TO 220       !PROCESS COMPLETELY IN PASS1
      IF(KMD.EQ.'$ASS') GO TO 230       !PROCESS COMPLETELY IN PASS1
      IF(KMD.EQ.'$DAT') GO TO 230       !PROCESS COMPLETELY IN PASS1
C
      IF(KMD.EQ.'$BAN') GO TO 240       !PROCESS COMPLETELY IN PASS1
      IF(KMD.EQ.'$BAF') GO TO 245       !PROCESS COMPLETELY IN PASS1
      IF(KMD.EQ.'$GAT') GO TO 250       !PROCESS COMPLETELY IN PASS1
      IF(KMD.EQ.'$GAF') GO TO 255       !PROCESS COMPLETELY IN PASS1
C
      IF(KMD.EQ.'$MAP') GO TO 260       !PROCESS COMPLETELY IN PASS1
      IF(KMD.EQ.'$GLS') GO TO 260       !PROCESS COMPLETELY IN PASS1
      IF(KMD.EQ.'$TEX') GO TO 270       !PROCESS COMPLETELY IN PASS1
C
      IF(KMD.EQ.'$H16') GO TO 1510      !SAVE FOR PASS2 NO PRE-PROC
      IF(KMD.EQ.'$H32') GO TO 1510      !SAVE FOR PASS2 NO PRE-PROC
      IF(KMD.EQ.'$TIT') GO TO 1510      !SAVE FOR PASS2 NO PRE-PROC
      IF(KMD.EQ.'$HID') GO TO 1510      !SAVE FOR PASS2 NO PRE-PROC
      IF(KMD.EQ.'$HWD') GO TO 1510      !SAVE FOR PASS2 NO PRE-PROC
      IF(KMD.EQ.'$FWD') GO TO 1510      !SAVE FOR PASS2 NO PRE-PROC
      IF(KMD.EQ.'$ASP') GO TO 1510      !SAVE FOR PASS2 NO PRE-PROC
C
      GO TO 400                         !PRE-PROCESS OTHER TYPES
C
C     **************************************************************
C     PROCESS SOME $DIR-TYPES ON FIRST PASS
C     **************************************************************
C
  120 CALL REFOR(IWD,LWD,ITYP,NF,2,80,NTER)
      IF(NTER.NE.0) GO TO 1000
      IF(NF.LT.2)   GO TO 1000
C
      IF(KMD.EQ.'$LST') GO TO 130
C
      IF(NPRDUN.EQ.'YES ') THEN
                           IERR=4
                           GO TO 1450
                           ENDIF
C
      NPRDUN='YES '
      CALL LIMIV(LWD(1,2),1,MXPAR,NPAR,IERR)
      IF(IERR.NE.0) GO TO 1000
      MINIP=1
      MAXIP=NPAR
      IF(NF.LT.3) GO TO 1500
      CALL LIMIV(LWD(1,3),1,NPAR,MINIP,IERR)
      IF(IERR.NE.0) GO TO 1000
      IF(NF.LT.4) GO TO 1500
      CALL LIMIV(LWD(1,4),MINIP,NPAR,MAXIP,IERR)
      IF(IERR.NE.0) GO TO 1000
      GO TO 1500
C
  130 CALL LIMIV(LWD(1,2),256,262144,LISTL,IERR)
      IF(IERR.NE.0) GO TO 1000
      IF(2*(LISTL/2).NE.LISTL) GO TO 1000
      GO TO 1500
C
  210 IF(LPRDUN.EQ.'YES ') THEN
                           IERR=5
                           GO TO 1450
                           ENDIF
C
      CALL PARLIN(IWD,IERR,MSER)
      GO TO 1500
C
  220 CALL DIMEN(KMD,IERR,MSER)
      GO TO 1500
C
  230 CALL ASSYM(IERR,MSER)
      GO TO 1500
C
  240 CALL BAFFER(IERR,MSER)
      IBANF='YES '
      IF(IBAFF.EQ.'YES ') IBANF='OERR'
      GO TO 1500
C
  245 CALL NUBAN(IERR,MSER)
      IBAFF='YES '
      GO TO 1500
C
  250 CALL GAFFER(IERR,MSER)
      IGATF='YES '
      IF(IGAFF.EQ.'YES ') IGATF='OERR'
      GO TO 1500
C
  255 CALL NUGAT(IERR,MSER)
      IGAFF='YES '
      GO TO 1500
C
  260 CALL MAPPER(IERR,MSER)
      GO TO 100
C
  270 DO 272 I=1,19
      ITEX(I)=CIWD(I+1)
  272 CONTINUE
      ITEX(20)='    '
      GO TO 1500
C
C     **************************************************************
C     PRE-PROCESS ALL OTHER TYPES AND SAVE ON TEMP-FILE
C     **************************************************************
C
  400 CALL CLINE(JWD,IERR,MSER)
      JWD(30)=NSOL
C
      IF(JWD(21).NE.DO) GO TO 1510
C
      IF(IERR.EQ.0) GO TO 420
      CALL SAVLIN(JWD,MSER,IERR,'DONE',NSOL,1)
C
  420 CALL LOOPER(JWD,IERR,MSER)
      GO TO 100
C
C     **************************************************************
C     SET UP ANY ERROR MESSAGE GENERATED IN PASS1 PROPER
C     **************************************************************
C
 1000 DO 1010 I=1,10
      MSER(I)=MSG(I,3)
 1010 CONTINUE
      IERR=1
      GO TO 1500
C
C     **************************************************************
C     RECORD ALL SOURCE LINES  ON  CHIL.IMF AND
C     RECORD ANY ERROR MESSAGES ON CHIL.ERR
C     **************************************************************
C
 1440 IERR=6
C
 1450 DO 1460 I=1,10
      MSER(I)=MSG(I,IERR)
 1460 CONTINUE
C
 1500 JWD(21)=DONE
 1510 JWD(31)=1
      IF(IERR.EQ.0) GO TO 1530
      NEREC=NEREC+1
      JWD(22)=NEREC
      WRITE(LER,IOSTAT=IOS)MSER
      CALL IOERR(IOS)
C
 1530 WRITE(LOU,IOSTAT=IOS)JWD
      CALL IOERR(IOS)
      GO TO 100
C
C     **************************************************************
C     BUILD SYMBOLIC PARAMETER POINTER TABLE
C     **************************************************************
C
 1900 DO 1905 I=1,NPAR                  !SET POINTER ARRAY TO ZERO
      IPSP(I)=0
 1905 CONTINUE
      DO 1930 I=1,NSYM                  !LOOP ON # OF SYMBOLS
      IF(ISPF(I).NE.'$DIP') GO TO 1930  !TST FOR "PARAMETER"
      N=ISYP(I)-1                       !POINTER IN VALUE ARRAY
      NDO=ISYD(I)                       !# OF VALUES
      DO 1920 J=1,NDO                   !LOOP ON # OF VALUES
      N=N+1                             !INDEX IN VALUE ARRAY
      NDX=ISYV(N)                       !INDEX IN SYMBOLIC PARM TABL
C
      IF(NDX.GT.0.AND.NDX.LE.NPAR) GO TO 1912
C
      WRITE(LU6,1910)(MSG(K,1),K=1,10),ISYN(1,I),ISYN(2,I),J
 1910 FORMAT(1H ,10A4,' = ',2A4,'(',I3,')')
      NERR=NERR+1
      GO TO 1920
C
 1912 IF(IPSP(NDX).EQ.0) GO TO 1916     !ERR IF ALREADY DEFINED
C
      WRITE(LU6,1910)(MSG(K,2),K=1,10),ISYN(1,I),ISYN(2,I),J
      NERR=NERR+1
      GO TO 1920
C
 1916 IPSP(NDX)=I                       !POINTS TO SYMBOL #
      IPSI(NDX)=J                       !GIVES SYMBOL SUBSCRIPT
 1920 CONTINUE
 1930 CONTINUE
C
      RETURN
C
      END
