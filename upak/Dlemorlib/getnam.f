C$PROG GETNAM    - Loads 1st contig string (IA to IB) from IWD to NAMFIL
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE GETNAM(IWD,IA,IB,NAMFIL,KFD,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      CHARACTER*4  KFD
C
      INTEGER*4    IWD(*),NAMFIL(20),IA,IB,IERR
C
      INTEGER*4    NAM1(20), NAM2(20)
C
      CHARACTER*80 CNAM1, CNAM2, TNAM
C
      EQUIVALENCE (CNAM1,NAM1),(CNAM2,NAM2)
C
      INTEGER*4    JA,JB,IT1,ISCOLON,ISFILE,IT,INT,I
C
      INTEGER*4    NXNB, FIND_ANY
C
      INTEGER*4    BLANK
      character*4  cblank
      equivalence  (cblank, blank)
      DATA         cBLANK/'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     LOADS FIRST CONTIGUOUS BYTE-STRING (IA,IB) FROM IWD TO NAMFIL
C     TERMINATES ON BLANK OR !
C     IF 1ST 5 BYTES IN NAMFIL ARE     "/DEV/" - KFD = 'TAPE'
C        OR IF LAST BYTE IN NAMFIL IS  ":"     - KFD = 'TAPE'
C     OTHERWISE                                - KFD = 'FILE'
C     ------------------------------------------------------------------
C
      IERR=0
      DO 10 I=1,20
      NAMFIL(I)=BLANK
      NAM1(I)=IWD(I) 
   10 CONTINUE
      CNAM2=' '
C
      JA=NXNB(IWD,IA,IB)
      IF(JA.LE.0) GO TO 100
C
      CNAM2=CNAM1(JA:IB)
      CNAM1=' '
      JB=FIND_ANY(CNAM2,' !')
      JB = JB-1
      IF(JB.EQ.0)GOTO 100
      IT=INDEX(CNAM2(1:JB),'MT')
      IT1=INDEX(CNAM2(1:JB),'MTH')
      IF(IT1.NE.0)THEN
          IT=IT+3
      ELSE
          IT=IT+2
      ENDIF
      ISCOLON=0
      ISFILE=0
      IF(CNAM2(JB:JB).EQ.":")THEN		!last char : means tape
          ISCOLON=1
          INT=JB-IT
          IF(INT.LE.0)GOTO 100
          TNAM(1: )=CNAM2(IT:JB-1)
      ENDIF
      IF((ISCOLON).EQ.0.) THEN
          KFD='FILE'
          CNAM1=CNAM2(1:JB)
      ELSE
          KFD='TAPE'
          CNAM1(1:)=TNAM(1:INT)
      ENDIF
      DO I=1,20
         NAMFIL(I)=NAM1(I)
      ENDDO
      RETURN
C
  100 IERR=1
      WRITE(CMSSG,105)
  105 FORMAT('SYNTAX ERROR DECODING FILE-NAME')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
