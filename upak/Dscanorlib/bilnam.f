C$PROG BILNAM    - Builds device names (probably obsolete)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 05/29/98
C     ******************************************************************
C
      SUBROUTINE BILNAM(IWD,DSK,EXT,NAMFD,IEXT,LDOT,INEW,IERR)
C   
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE  (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4   IWD(20),NAMFD(20),DSK(2),EXT
C
      INTEGER*4   IEXT,LDOT,INEW,IERR
C
      INTEGER*4   NXBL,NXNB,LSNB,LEXT,IFIND
C
      INTEGER*4   LLO,LHI,LCOM,NCL,IA,IB
C
      INTEGER*4   ICOM,IDOT,ICOL,ISEM,NEX,LC,BLANK,I
C
      character*4 cblank
      equivalence (cblank, blank)
      DATA cBLANK/'    '/
C   
      DATA ICOM,IDOT,ICOL,ISEM/'2C'X,'2E'X,'3A'X,'3B'X/
C
      SAVE
C   
C     ==================================================================
C     ON ENTRY  - IWD   CONTAINS - CMD FILNAM<.EXT><!VER><,NEW>
C                 DSK   CONTAINS - DEFAULT DEV:
C                 EXT   CONTAINS - DEFAULT .EXT
C   
C     ON RETURN - NAMFD CONTAINS - FILNAM<.EXT>
C                 IEXT  CONTAINS - <.EXT> ACTUALLY USED
C                 INEW  CONTAINS - <NEW>
C     ==================================================================
C   
      DO 10 I=1,20                      !LOOP TO BLANK FIL/DEV NAME
      NAMFD(I)=BLANK
   10 CONTINUE
      IEXT=BLANK                        !BLANK FIL/DEV EXT WORD
      INEW=BLANK                        !BLANK NEW-FLAG
      LDOT=0                            !LOC OF DOT (1ST EXT BYTE)
      IERR=0                            !ERROR FLAG
C   
      LLO=NXBL(IWD,1,80)                !LOC OF 1ST BLANK AFTER CMD
      IF(LLO.LE.0) GO TO 200            !ERROR IF NOT FOUND
      LLO=NXNB(IWD,LLO,80)              !LOC OF 1ST FIL/DEV BYTE
      IF(LLO.LE.0) GO TO 200            !ERROR IF NOT FOUND
      LHI=LSNB(IWD,LLO,80)              !LOC OF LAST NON-BLANK
      IF(LHI.LE.0) GO TO 200            !ERROR IF NOT FOUND
C   
      LDOT=LEXT(IWD,LLO,LHI)            !LOC OF DOT (1ST EXT BYTE)
C
      LCOM=IFIND(IWD,ICOM,LLO,LHI)      !LOC OF , (1ST  NEW-BYTE)
C   
      NCL=0                             !INIT # BYTES LOADED IN NAMFD
C   
      IA=LLO
      IB=LHI                            !LAST BYTE IN IWD FOR FILNAM
      IF(LCOM.GT.0) IB=LCOM-1           !OMIT NEW-FIELD
      CALL LODUP(IWD,IA,IB,NAMFD,NCL+1) !LOAD FILNAM
      NCL=NCL+IB-IA+1                   !# BYTES LOADED
      IF(LDOT.GT.0) GO TO 40            !TST FOR EXTENSION SPECIFIED
      NEX=LSNB(EXT,1,4)                 !#BYTES IN DEFAULT EXTENSION
      IF(NEX.LE.0)  GO TO 40            !TST FOR DEFAULT EXT GIVEN
      CALL LODUP(EXT,1,NEX,NAMFD,NCL+1) !LOAD DEFAULT EXT
C   
   40 IF(LDOT.EQ.0) THEN                !SET EXT-NAME IN IEXT
                    IEXT=EXT
                    GO TO 50
                    ENDIF
C   
      CALL LODUP(IWD,LDOT,LDOT+3,IEXT,1)
C   
   50 LDOT=LEXT(NAMFD,1,80)             !LOC OF 1ST EXT-BYTE IN NAMFD
C   
      IF(LCOM.LE.0) RETURN              !TST FOR COMMA (NEW-FIELD)
      LC=LCOM
      CALL LODUP(IWD,LC+1,LC+3,INEW,1)  !IF YES, LOAD "INEW"
      RETURN
C   
C     ==================================================================
C     RETURN error messages
C     ==================================================================
C
  200 WRITE(CMSSG,205)
  205 FORMAT('Syntax error or illegal file/dev specification')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
