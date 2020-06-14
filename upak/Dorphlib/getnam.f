C$PROG GETNAM    - Returns filename from command line
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE GETNAM(IWD,NAMFD,IEXT,LDOT,INEW,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      INTEGER*4 IWD(20),NAMFD(20)
C
      INTEGER*4 BLANK
      CHARACTER*4 cBLANK
      EQUIVALENCE (cBLANK, BLANK)
C
      DATA     cBLANK/'    '/
C
      DATA IDOT,ICOM/Z'2E',Z'2C'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     DECODES  -  CMD FILDEV<.EXT><,NEW>
C     ON RETURN:  NAMFD CONTAINS - FILDEV<.EXT>
C                 IEXT  CONTAINS - <.EXT>
C                 INEW  CONTAINS - <NEW>
C     ------------------------------------------------------------------
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
      LD=LEXT(IWD,LLO,LHI)              !LOC OF DOT (1ST EXT BYTE)
C
      LC=IFIND(IWD,ICOM,LLO,LHI)        !LOC OF COMMA
C
      MAX=LHI                           !MAX BYTE FOR FIL/DEV NAME
      IF(LC.GT.0) MAX=LC-1              !DON'T LOAD ",NEW"
      MAX=LSNB(IWD,LLO,MAX)
C
      CALL LODUP(IWD,LLO,MAX,NAMFD,1)   !LOAD FIL/DEV NAME
C
      IF(LD.LE.0) GO TO 50              !TST FOR DOT
      LDOT=LD-LLO+1                     !LOC OF DOT IN NAMFD
C
      IHI=LD+3                          !IF YES,
      IF(IHI.GT.MAX) IHI=MAX            !LOAD FILENAME EXTENSION
      CALL LODUP(IWD,LD,IHI,IEXT,1)     !INTO IEXT
C
   50 IF(LC.LE.0) RETURN                !TST FOR COMMA
      CALL LODUP(IWD,LC+1,LC+3,INEW,1)  !IF YES, LOAD "INEW"
      RETURN
C
  200 WRITE(CMSSG,205)                  !ERROR RETURN
  205 FORMAT('SYNTAX ERROR OR ILLEGAL FIL/DEV SPECIFICATION')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
