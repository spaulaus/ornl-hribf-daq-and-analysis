C$PROG NUBAN
      SUBROUTINE NUBAN(IERR,MSER)
C
      INTEGER*2   BNID,BLEN
C
      INTEGER*4   BLOC
C
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40),NF
C
      COMMON/BBB/ BNID(880),BLEN(880),BLOC(880),MXBAN,NBAN
C
      INTEGER*4    IX(64),JY(64),KPAR(9),NAMF(20)
C
      INTEGER*4    RECLVALU
C
      CHARACTER*80 CNAMF
C
      CHARACTER*40 MSER
C
      EQUIVALENCE (LXD,KPAR(3)),(LXG,KPAR(4)),
     &            (LYD,KPAR(5)),(LYG,KPAR(6))
      EQUIVALENCE (CNAMF,NAMF)
C
      DATA LU/13/
C
      INTEGER*4    BLANK
      character*4  cblank
      equivalence (cblank, blank)
      DATA         cBLANK/'    '/
C
      SAVE
C
C     **************************************************************
C     PROCESS  -  $BAF FILENAM.BAN/ACT
C     **************************************************************
C
      IERR=0
C
      CNAMF=' '
C
      MSER=' '
C
      IA=NXNB(IWD,5,80)
      IF(IA.LE.0) GO TO 510
      IB=NXBL(IWD,IA,80)-1
      IF(IB.LE.0) GO TO 510
      CALL LODUP(IWD,IA,IB,NAMF,1)
C
      OPEN(UNIT      = LU,
     &     FILE      = CNAMF,
     &     STATUS    = 'OLD',
     &     ACCESS    = 'DIRECT',
     &     RECL      = RECLVALU(80),
     &     IOSTAT    = IOS)
C
      CALL OPERR(IOS)
      IF(IOS.NE.0) THEN
                   WRITE(0,25)
                   RETURN
                   ENDIF
C
   25 FORMAT(1H ,'ERROR TRYING TO OPEN BAN-FILE')
C
      IBN=0
      CALL BANIN(LU,IBN,IX,JY,NXY,KPAR,KERR,MSER)
      IF(KERR.NE.0) THEN
                    WRITE(0,30)MSER,IBN
                    WRITE(6,30)MSER,IBN
                    GO TO 110
                    ENDIF
C
      DO 100 I=1,NBAN
      IBN=BNID(I)
      LENG=BLEN(I)
      LOC=BLOC(I)
C
      CALL BANIN(LU,IBN,IX,JY,NXY,KPAR,KERR,MSER)
      IF(KERR.GT.1) IERR=KERR
      IF(KERR.NE.0) THEN
                    WRITE(0,30)MSER,IBN
                    WRITE(6,30)MSER,IBN
                    GO TO 100
                    ENDIF
   30 FORMAT(1H ,'* * * ',A,'  BAN-ID =',I6)
C
      CALL FREG1(IX,JY,NXY,LXD,LXG,LENG,LYD,LYG,LOC)
C
  100 CONTINUE
C
  110 CLOSE(LU)
      RETURN
C
  510 WRITE(MSER,512)
  512 FORMAT('SYNTAX ERROR IN BAN-FILE SPECIFICATION  ')
      IERR=1
      RETURN
      END
