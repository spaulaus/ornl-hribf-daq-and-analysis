C$PROG LDFINP    - List-Data-File input routine for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/26/99
C     ******************************************************************
C
      SUBROUTINE LDFINP(IGO,JFIR,JLAS,IERR)
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
      COMMON/SC04/ JCNF,IHEDN,MBFL
      INTEGER*4         IHEDN,MBFL
      CHARACTER*4  JCNF
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      COMMON/SC06/ LIST(16384,2)
      INTEGER*2    LIST
C     ------------------------------------------------------------------
      COMMON/SC15/ NCEOF,LAUTO
      INTEGER*4    NCEOF
      CHARACTER*4        LAUTO
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      INTEGER*4    LISTF(8192,2)
C
      CHARACTER*4  CLISTF(8192,2)
C
      EQUIVALENCE (LISTF(1,1),LIST(1,1)),(CLISTF,LISTF)
C
      INTEGER*4    IGO,JFIR,JLAS,IERR
C
      INTEGER*4    LREC,LP,LR,NBYT,NUM,NEV,I
C
      INTEGER*4    MOD,ISWAF
C
      DATA         LREC/16384/
C
      CHARACTER*4  KIND
C     ------------------------------------------------------------------
      SAVE
C   
C     ------------------------------------------------------------------
C     IGO  = 1/2 FOR INITIAL/SUBSEQUENT READ (not used here)
C     JFIR = FIRST WORD IN LSTL TO PROCESS
C     JLAS = LAST  WORD IN LSTL TO PROCESS
C     IERR = ERROR CODE
    
C     LR = LIST BUFFER # (2ND INDEX OF LIST) BEING READ
C     LP = LIST BUFFER # (2ND INDEX OF LIST) BEING PROCESSED
C     ------------------------------------------------------------------
C   
      IERR=0                                    !Reset error flag
C
      LR=1                                      !Set to READ-1
      LP=1                                      !Set to PROC-2
      JFIR=1
      JLAS=LREC
C 
  100 CALL LDFREAD(LUINF,INRECI,LIST(1,LR),KIND,NBYT,IERR)
C
      INRECI=INRECI+1
C
      IF(IERR.NE.0) RETURN
C
C
      NCEOF=0                                   !ZOT CONTIG EOF CNTR
C
C   
      IF(MSGF.NE.'    ') RETURN                 !Tsfr Ctrl/C
C
      IF(MOD(NBYT,2).NE.0) GO TO 100    !Skip odd# bytes
C
      IF(CLISTF(1,LP).EQ.'SCAL') GO TO 100 !Skip Scaler record
C
      IF(KIND.EQ.'DATA')   GO TO 200    !Tst for DATA
C
      IF(KIND.EQ.'HEAD')   GO TO 220    !Tst for HEADER
C
      IF(KIND.EQ.'DEAD')   GO TO 240    !Tst for DEADTIME
C
      GO TO 100                         !Otherwise, skip it
C
C
C     ------------------------------------------!Process data 
C
  200 IF(ISWAB.EQ.'YES ') THEN                  !Tst for 
      CALL SWAPB(LIST(1,LP),1,LSTL)             !Byte-swap request
      ENDIF
C   
      RETURN
C
C     ------------------------------------------!Display Header
  220 IHEDN=LISTF(33,LP)
      IF(ISWAB.EQ.'YES ') IHEDN=ISWAF(IHEDN)    !Tst for byte-swap
      WRITE(CMSSG,225)(LISTF(I,LP),I=9,23),IHEDN 
      CALL MESSLOG(LOGUT,LOGUP)
  225 FORMAT(15A4,I6)
      GO TO 100
C
C     ------------------------------------------!Display dead-time
  240 DO 245 I=1,28
      MSSG(I)=LISTF(I,LP)
  245 CONTINUE
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
C
      END
