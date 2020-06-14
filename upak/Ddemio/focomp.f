C$PROG FOCOMP
C     
C     PROGRAM TO DEMONSTRATE THE USE OF ROUTINE SPKIO FOR OUTPUT
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4 IDAT(8192),IHED(32),NDX(4)
C
      INTEGER*4 NAMF(20)
C
      DATA NDX/1,0,0,0/
C
      DATA      NAMPROG/'FOCO','MP  '/
C
      CHARACTER*4  MODE,IACP
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CMSSG=' '
      LOGUT=6
      LOGUP=7
      LISFLG='LON '
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'focomp.log',
     &     STATUS     = 'UNKNOWN')
C
      CLOSE(UNIT=LOGUP,DISP='DELETE')
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'focomp.log',
     &     STATUS     = 'NEW')
C
  100 WRITE(6,105)
  105 FORMAT(1H ,'ENTER NAME OF SPK-FILE TO BE CREATED')
      READ(5,110)NAMF
  110 FORMAT(20A4)
      MODE='CREA'
      IACP='RW  '
      LUS=2
      CALL SPKMAN(MODE,NAMF,LUS,IACP,IERR)
      IF(IERR.NE.0) GO TO 100
C
      DO 200 ID=1,10
C
      CALL INPUT(ID,IDAT,NCH)
C
      DO 120 I=1,32
      IHED(I)=0
  120 CONTINUE
C
      IHED(1)=ID
      IHED(9)=64
      IHED(12)=NCH
C
      CALL SPKIO(2,LUS,DUM,IHED,DUM,IDAT,DUM,DUM,IERR)
      CALL SPKERR(IERR)
C
      IF(IERR.NE.0) STOP
      WRITE(CMSSG,130)ID
  130 FORMAT('ID NUMBER STORED =',I6)
      CALL MESSLOG(LOGUT,LOGUP)
C
  200 CONTINUE
      STOP
      END
C$PROG INPUT
      SUBROUTINE INPUT(ID,IDAT,NCH)
C
      INTEGER*4 IDAT(*),NCALL
C
      REAL*4    COMPSUM(720),FAC(300),CALL,SIGN
C
      DATA      SIGN,CALL/-1.0,0.0/
C
      DATA      NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0) GO TO 50
C
      NCH=720
C
      DO 20 I=1,250
      FAC(I)=1.0
   20 CONTINUE
C
      FAC(1)=1.0  !+
      FAC(2)=0.40 !-
      FAC(3)=0.18 !+
      FAC(4)=0.06 !-
C
   50 NCALL=NCALL+1
C
      CALL=CALL+1.0
C
      DENO=FAC(NCALL)*CALL
C
      WRITE(6,777)CALL,FAC(NCALL)
  777 FORMAT(1H ,'CALL,FAC =',2F10.3)
C
      SIGN=-SIGN
C
      DTOR=3.1415927/180.0
C
      DO 100 I=1,NCH
C
      ANG=CALL*FLOAT(I)
C
      RAD=DTOR*ANG
C
C
      COMPSUM(I)=COMPSUM(I)+SIGN*1000.0*SIN(RAD)*FAC(NCALL)
C
C     IDAT(I)=1000.0*SIN(RAD)
C
      IDAT(I)=COMPSUM(I)
  100 CONTINUE
      RETURN
      END
