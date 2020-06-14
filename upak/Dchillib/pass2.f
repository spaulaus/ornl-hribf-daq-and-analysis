C$PROG PASS2
      SUBROUTINE PASS2
C
      LOGICAL Q
C
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40),NF
C
      COMMON/CCC/ ISYN(2,100),ISYT(100),ISYD(100),ISYP(100),
     &            ISPF(100),ISYV(16384),NSYM,NSYV
C
      COMMON/FFF/ LIST(2002),LPAR(2002),IPSP(2002),IPSI(2002),
     &            MXPAR,NPAR,MINIP,MAXIP,LPARF
C
      COMMON/JJJ/ LIN,LOU,LU6,LER,NEREC,NSOL,LHLN,NERR
C
      COMMON/KKK/ MILF(262144),MILMF(32768),MILC,MILCF,NHWPC,NXDAD,Q,
     &            NUBPC,IGATOF,IMAPOF,NGATL,NMAPL,MXGATL,MXMAPL
      CHARACTER*4 NUBPC
C
      COMMON/PPP/ LPATCH(500),LAPAT(500),LPATI
C
      COMMON/QQQ/ ITEX(20),ITIT(10),NUID
C
      COMMON/YYY/ NAMHIL(20),LISKIN,LTA,LICO,NOHIS,ITRACE
      CHARACTER*4            LISKIN
C
      COMMON/ZZZ/ JHPC(4096),LISTL,IREPACK,JREPACK(3)
      CHARACTER*4                  IREPACK,JREPACK
C
      BYTE BYHPC(16384)
C
      INTEGER*2 MIL(65536)
C
      INTEGER*4 MSER(10),JWD(32)
C
      CHARACTER*4  KMD,CJWD(32)
C
      EQUIVALENCE (BYHPC(1),JHPC(1))
      EQUIVALENCE (MIL(1),MILF(1)),(KMD,JWD(21)),(CJWD,JWD)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(LISKIN.NE.'NULL') WRITE(LU6,40)NAMHIL
   40 FORMAT(1H1,'LISTING OF CHIL SOURCE FROM FILE - ',20A4/)
C
      REWIND LOU
      REWIND LER
      NOL=0
      NCC=0
   50 READ(LOU,IOSTAT=IOS,END=55)JWD
      IF(IOS.EQ.0)  GO TO 60
      CALL IOERR(IOS)
   55 RETURN
   60 CONTINUE
      NOL=NOL+1
C
      IF(LISKIN.EQ.'NULL') GO TO 70
C
      IF(LISKIN.EQ.'FULL') THEN
                           WRITE(LU6,62)NOL,(JWD(I),I=1,30)
                           GO TO 70
                           ENDIF
C
      IF(JWD(31).EQ.1) WRITE(LU6,64)JWD(30),(JWD(I),I=1,20)
C
   62 FORMAT(1H ,I4,2X,20A4,A4,4I3,4(1X,A4),I4)
   64 FORMAT(1H ,I4,2X,20A4)
C
   70 IF(JWD(22).EQ.0) GO TO 80
C
      READ(LER,IOSTAT=IOS)MSER
      CALL IOERR(IOS)
      WRITE(6,75)MSER
   75 FORMAT(1H ,'* * * ',10A4)
      NERR=NERR+1
      GO TO 50
C
   80 IF(CJWD(26).EQ.'    ') GO TO 90   !TST FOR STATEMENT LABEL
      NXBY=2*MILC                       !CALC BYTE# OF NEXT INS
      CALL LASAV(JWD(26),NXBY,IERR,MSER)!SAVE THE LABEL & VALUE
      IF(IERR.EQ.0) GO TO 85            !TST FOR ERROR
      NERR=NERR+1
      WRITE(LU6,1005)MSER
C
   85 CONTINUE
C
   90 IF(KMD.NE.'EQU ')GO TO 100
C
C     **************************************************************
C     PROCESS  -  EQUATE STATEMENT
C     **************************************************************
C
      CALL EQUATE(JWD,JWD(23),JWD(24),JWD(25),IERR,MSER)
      IF(IERR.NE.0) GO TO 1000
      GO TO 50
C
C     PROCESS OTHER TYPES ******************************************
C
  100 IF(KMD.EQ.'C   ') GO TO 50
      IF(KMD.EQ.'DO  ') GO TO 50
      IF(KMD.EQ.'DONE') GO TO 50
      IF(KMD.EQ.'CONT') GO TO 50
      IF(KMD.EQ.'LC  ') GO TO 50
      IF(JWD(22).NE.0)  GO TO 1010
C
      IF(KMD.EQ.'CALL') GO TO 200
      IF(KMD.EQ.'GOTO') GO TO 220
C
      IF(KMD.EQ.'IFS ') GO TO 300
      IF(KMD.EQ.'IFU ') GO TO 300
      IF(KMD.EQ.'IFX ') GO TO 300
      IF(KMD.EQ.'IFN ') GO TO 300
      IF(KMD.EQ.'IFC ') GO TO 300
      IF(KMD.EQ.'IFP ') GO TO 300
      IF(KMD.EQ.'BTAB') GO TO 300
C
      IF(KMD.EQ.'H   ') GO TO 400
      IF(KMD.EQ.'OH  ') GO TO 400
C
      IF(KMD.EQ.'$H32') GO TO 500
      IF(KMD.EQ.'$H16') GO TO 510
      IF(KMD.EQ.'$TIT') GO TO 550
      IF(KMD.EQ.'$HID') GO TO 560
      IF(KMD.EQ.'$HWD') GO TO 600
      IF(KMD.EQ.'$FWD') GO TO 600
      IF(KMD.EQ.'ASP ') GO TO 650
C
      GO TO 1010
C
C     **************************************************************
C     PROCESS  -  CALL STATEMENT
C     **************************************************************
C
  200 MILC=MILC+1
      MIL(MILC)=32
                                                   CALL QQ(0,32)
      ICO=1
      IF(CJWD(27).EQ.'SUB2') ICO=2
      IF(CJWD(27).EQ.'SUB3') ICO=3
      IF(CJWD(27).EQ.'CK1 ') ICO=4
      IF(CJWD(27).EQ.'CK2 ') ICO=5
      IF(CJWD(27).EQ.'CK3 ') ICO=6
      IF(ICO.GT.3) GO TO 205
C
      MILC=MILC+1
      MIL(MILC)=ICO
                                                   CALL QQ(0,46)
      GO TO 50
C
  205 IREPACK='YES '
      JREPACK(ICO-3)='YES '
      LCO=IFIND(JWD,z'2C',JWD(23),JWD(24))
      IF(LCO.LE.0) GO TO 1020
      MINP=ITERV(JWD,JWD(23),LCO-1,IERR,MSER)
      IF(IERR.NE.0) GO TO 1000
      MAXP=ITERV(JWD,LCO+1,JWD(24),IERR,MSER)
      IF(IERR.NE.0) GO TO 1000
      IF(MINP.LE.0.OR.MINP.GT.NPAR)    GO TO 1020
      IF(MAXP.LT.MINP.OR.MAXP.GT.NPAR) GO TO 1020
C
      MILC=MILC+1
      MIL(MILC)=ICO
                                                  CALL QQ(0,47)
      MILC=MILC+1
      MIL(MILC)=MINP
                                                  CALL QQ(0,48)
      MILC=MILC+1
      MIL(MILC)=MAXP
                                                  CALL QQ(0,49)
      GO TO 50
C
C     **************************************************************
C     PROCESS  -  GOTO STATEMENT
C     **************************************************************
C
  220 CONTINUE
C
  230 MILC=MILC+1                       !INC THE MIL-CNTR
      MIL(MILC)=31                      !OPCODE FOR GOTO
                                                   CALL QQ(0,31)
      MARK=2*(MILC-1)                   !MARK LOCATION OF OPCODE
      MILC=MILC+1                       !INC THE MIL-CNTR
      MIL(MILC)=MARK                    !LOAD MARK IN "DEST WORD"
                                                   CALL QQ(0,1)
      LPATI=LPATI+1                     !INC PATCH LOCATION CNTR
      LPATCH(LPATI)=MILC                !SAVE LOCATION TO PATCH
      LAPAT(LPATI)=JWD(27)              !SAVE ASSOCIATED LABEL NAME
      GO TO 50
C
C     **************************************************************
C     PROCESS  -  IFS, IFU, IFX, IFN, IFC, IFP, BTAB
C     **************************************************************
C
  300 CALL IFPRO(NOL,JWD,IERR,MSER)
      IF(IERR.NE.0) GO TO 1000
      GO TO 50
C
C     **************************************************************
C     PROCESS  -  H-STATEMENT  OR  OH-STATEMENT
C     **************************************************************
C
  400 LHLN=JWD(30)
      CALL HISPRO(NOL,JWD,IERR,MSER)
C
      IF(IERR.LE.0) GO TO 410
      NERR=NERR+1
      WRITE(LU6,1005)MSER
  410 IF(KMD.EQ.'END ') RETURN
      GO TO 60
C
C     **************************************************************
C     PROCESS  -  DIRECTIVES  $H32, $H16 & SET JHPC-ARRAY
C     **************************************************************
C
  500 NHWPC=2                           !SET TO 32 BITS/CHANNEL
      IVSET=1
      GO TO 520
  510 NHWPC=1                           !SET TO 16 BITS/CHANNEL
      IVSET=0
C
  520 NUBPC='YES '                      !SET NEW-BIT-PER-CHAN FLAG
      NBF=(NXDAD+32767)/32768           !ROUND UP NXDAD TO MULTIPLE
      NXDAD=32768*NBF                   !OF 32768 HALF WORDS
      NBF=NBF+1                         !NEXT BUF#
C
      DO 530 I=NBF,1024                 !LOOP TO SET REMAINING FLAGS
      BYHPC(I)=IVSET                    !TO NEW VALUE
  530 CONTINUE
      GO TO 50
C
C     **************************************************************
C     PROCESS  -  DIRECTIVES  $TIT & $HID
C     **************************************************************
C
  550 DO 552 I=1,10                     !PICK UP NEW TITLE
      ITIT(I)=JWD(I+1)
  552 CONTINUE
      GO TO 50
C
  560 CALL REFOR(JWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0) GO TO 1020
      CALL LIMIV(LWD(1,1),1,65535,NUID,IERR)
      IF(IERR.NE.0) GO TO 1020
      NUID=NUID-1
      GO TO 50
C
C     **************************************************************
C     PROCESS  -  DIRECTIVES  $HWD, $FWD
C     **************************************************************
C
  600 CALL REFOR(JWD,LWD,ITYP,NF,5,80,NTER)
      IF(NTER.NE.0) GO TO 1020
C
      IF(KMD.EQ.'$FWD') GO TO 620
C
      DO 610 J=1,NF
      CALL LIMIV(LWD(1,J),-32767,32767,IV,IERR)
      IF(IERR.NE.0) GO TO 1020
      MILC=MILC+1
      MIL(MILC)=IV
                                                   CALL QQ(0,44)
  610 CONTINUE
      GO TO 50
C
  620 IF(2*(MILC/2).NE.MILC) GO TO 1030
      MILCF=MILC/2
      DO 630 J=1,NF
      CALL LIMIV(LWD(1,J),0,40000000,IV,IERR)
      IF(IERR.NE.0) GO TO 1020
      MILC=MILC+2
      MILCF=MILCF+1
      MILF(MILCF)=IV
                                                   CALL QQ(0,-45)
  630 CONTINUE
      GO TO 50
C
C     **************************************************************
C     PROCESS  -  ASP(IP,IV)
C     **************************************************************
C
  650 IA=JWD(23)
      IB=JWD(24)
      IF(IA.LE.0)  GO TO 1020
      IF(IB.LE.IA) GO TO 1020
      LCO=IFIND(JWD,Z'2C',IA,IB)
      IF(LCO.LE.0) GO TO 1020
C
      IPARN=ITERV(JWD,IA,LCO-1,IERR,MSER)
      IF(IERR.NE.0) GO TO 1000
      IPARV=ITERV(JWD,LCO+1,IB,IERR,MSER)
      IF(IERR.NE.0) GO TO 1000
      IF(IPARN.LT.1.OR.IPARN.GT.NPAR) GO TO 1020
C
      MILC=MILC+1
      MIL(MILC)=28
                                                   CALL QQ(0,28)
      MILC=MILC+1
      MIL(MILC)=IPARV
                                                   CALL QQ(0,1)
      MILC=MILC+1
      MIL(MILC)=17
                                                   CALL QQ(0,17)
      MILC=MILC+1
      MIL(MILC)=2*(IPARN-1)
                                                   CALL QQ(0,2)
      GO TO 50
C
C     **************************************************************
C     LIST ERROR MESSAGES AND INCREMENT "NERR"
C     **************************************************************
C
 1000 WRITE(LU6,1005)MSER
 1005 FORMAT(1H ,'* * * ',10A4)
      GO TO 1500
C
 1010 WRITE(LU6,1015)
 1015 FORMAT(1H ,'* * * UNRECOGNIZED STATEMENT')
      GO TO 1500
C
 1020 WRITE(LU6,1025)
 1025 FORMAT(1H ,'* * * SYNTAX ERROR OR ILLEGAL VALUE')
      GO TO 1500
C
 1030 WRITE(LU6,1035)
 1035 FORMAT(1H ,'* * * FULL-WD CODE ON HALF-WD BOUNDRY')
C
 1500 NERR=NERR+1
      GO TO 50
      END
