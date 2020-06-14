C$PROG DEMIOX
C     
C     PROGRAM TO DEMONSTRATE THE USE OF ROUTINE SPKIO FOR OUTPUT
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C
      CHARACTER*8  CNAMPROG
      EQUIVALENCE (CNAMPROG,NAMPROG)
      DATA         C NAMPROG/'DEMIOX'/
C     ------------------------------------------------------------------
      INTEGER*4 IDAT(8192),IHED(32)
C
      REAL*4 CAL(3)                 !REAL*4 calibration constants
      EQUIVALENCE (CAL(1),IHED(18)) !Equivalence to INED(18)
C
      CHARACTER*40 TITLE            !CHARACTER*40 Title
      EQUIVALENCE (TITLE,IHED(23))  !Equivalence to IHED(23)
C
      CHARACTER*80 NAMF             !For file name
C
      CHARACTER*4  MODE,IACP
C
      INTEGER*4    DATIM(6),JTIME(2),YR,MO,DA
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
C     ------------------------------------------------------------------
C     Create and open a new log-file
C     ------------------------------------------------------------------
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'demiox.log',
     &     STATUS     = 'UNKNOWN')
C
      CLOSE(UNIT=LOGUP,DISP='DELETE')
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'demiox.log',
     &     STATUS     = 'NEW')
C
C     ------------------------------------------------------------------
C     Generate a filename which includes YR,MO,DA,HR,MN,SEC
C     ------------------------------------------------------------------
C
      CALL MILYMDHMS(DATIM)        !Get YR,MO,DA  in INTEGER
C
      CALL MILTIME(JTIME)          !Get HR,MN,SEC in ASCII
C
      YR=DATIM(1)
      MO=DATIM(2)
      DA=DATIM(3)
C
      NAMF=' '                     !Clear filename & generate a new one
C                                  !via internal WRITE
C
      WRITE(NAMF,10)YR,MO,DA,JTIME
   10 FORMAT('demio-',I4.4,'-',I2.2,'-',I2.2,'_',2A4,'.spk')
C
C
      MODE='CREA'                  !Set MODE = CREATE
      IACP='RW  '                  !Access = Read/Write
      LUS=2                        !Logical unit = 2
C
      CALL SPKMAN(MODE,NAMF,LUS,IACP,IERR) !Call SPKMAN to do it
      IF(IERR.NE.0) STOP
C
      DO 200 ID=1,5                 !Loop to output 5 spectra
C
      CALL INPUT(ID,IDAT,NCH)       !Get a data array
C
      DO 120 I=1,32                 !Clear the header
      IHED(I)=0
  120 CONTINUE
C
      CAL(1)=0.0                    !Define calibration constants
      CAL(2)=0.5                    !This puts them in the header
      CAL(3)=0.001                  !but DAMM doesn't display them
C
      TITLE=' '                     !Clear the title
C
      WRITE(TITLE,125)CAL           !Define a new title which includes
  125 FORMAT('13byte Title:',3F8.3) !calibration constants which will
                                    !be displayed in the banner
C
      IHED(1)=ID                    !Load ID into IHED(1)
      IHED(9)=64                    !Header length
      IHED(12)=NCH                  !# of channels of data
C
      CALL SPKIO(2,LUS,DUM,IHED,DUM,IDAT,DUM,DUM,IERR) !Output it
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
