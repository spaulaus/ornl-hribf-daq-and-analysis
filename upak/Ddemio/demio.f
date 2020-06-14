C$PROG DEMIO
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
      REAL*4 CAL(3)
C
      EQUIVALENCE (CAL(1),IHED(18))
C
CX    INTEGER*4 NAMF(20)
C
      CHARACTER*80 NAMF
C
      CHARACTER*40 TITLE
C
      CHARACTER*40 TITMSG(5)
C
      DATA NDX/1,0,0,0/
C
      character*4 cnamprog(2)
      equivalence (cnamprog, namprog)
      DATA     cNAMPROG/'DEMI','O   '/
C
      CHARACTER*4  MODE,IACP
C
      EQUIVALENCE (TITLE,IHED(23))
C
      DATA  TITMSG/'This is Title-A baseball fans',
     &             'This is Title-B ping-pong fans',
     &             'This is Title-C golf fans',
     &             'This is Title-D basketball fans',
     &             'This is Title-E football fans'/
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
     &     FILE       = 'demio.log',
     &     STATUS     = 'UNKNOWN')
C
C     CLOSE(UNIT=LOGUP,DISP='DELETE')
      CLOSE(UNIT=LOGUP)
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'demio.log',
     &     STATUS     = 'REPLACE')
C
  100 WRITE(6,105)
  105 FORMAT(1H ,'ENTER NAME OF SPK-FILE TO BE CREATED')
      READ(5,110)NAMF
  110 FORMAT(A)
      MODE='CREA'
      IACP='RW  '
      LUS=2
      CALL SPKMAN(MODE,NAMF,LUS,IACP,IERR)
      IF(IERR.NE.0) GO TO 100
C
      DO 200 ID=1,5
C
      CALL INPUT(ID,IDAT,NCH)
C
      DO 120 I=1,32
      IHED(I)=0
  120 CONTINUE
C
      CAL(1)=0.0
      CAL(2)=0.5
      CAL(3)=0.001
C
      TITLE=' '
C
      WRITE(TITLE,125)CAL
  125 FORMAT('13byte Title:',3F9.4)
C
      IHED(1)=ID
      IHED(9)=64
      IHED(12)=NCH
C
      CALL SPKIO(2,LUS,DUM,IHED,DUM,IDAT,DUM,DUM,IERR)
      CALL SPKERR(IERR)
C
      IF(IERR.NE.0) CALL EXIT(1)
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
      INTEGER*4 IDAT(1)
C
      NCH=720
C 
      DTOR=3.1415927/180.0
C
      DO 10 I=1,NCH
      RAD=DTOR*FLOAT(I)
      IDAT(I)=55.0+50.0*SIN(RAD)+10.0*FLOAT(ID-1)
   10 CONTINUE
      RETURN
      END
