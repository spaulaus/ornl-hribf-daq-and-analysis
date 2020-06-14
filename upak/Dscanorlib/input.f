C$PROG INPUT     - Input routine for SCANOR (MIPS version)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/26/99
C     ******************************************************************
C
      SUBROUTINE INPUT(IGO,JFIR,JLAS,IERR)
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
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
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
      COMMON/SC20/ SCANTYP
      CHARACTER*4  SCANTYP
C     ------------------------------------------------------------------
      INTEGER*4    LISTF(8192,2)
C
      CHARACTER*4  CLISTF(8192,2)
C
      EQUIVALENCE (LISTF(1,1),LIST(1,1)),(CLISTF,LISTF),(C,LUC(1))
C
      INTEGER*4    C,IGO,JFIR,JLAS,IERR
C
      INTEGER*4    LR,LP,LTM,NEV,NBYT,NUM,I,ISWAF
C
      SAVE
C
C     ==================================================================
C     C    = TAPE CHANNEL NUMBER
C     IGO  = 1/2 FOR INITIAL/SUBSEQUENT READ
C     JFIR = FIRST WORD IN LLIST (EQUIVALENCED TO LIST) TO PROCESS
C     JLAS = LAST  WORD IN LLIST (IN SCANU MAIN)        TO PROCESS
C     IERR = ERROR CODE
    
C     LR = LIST BUFFER # (2ND INDEX OF LIST) BEING READ
C     LP = LIST BUFFER # (2ND INDEX OF LIST) BEING PROCESSED
C     ==================================================================
C   
      IERR=0                                    !Reset error flag
C
C     ==================================================================
C     Test for input from LDF-file
C     ==================================================================
C
      IF(INTYP.EQ.'LDF ') THEN                  !Tst for LDF type
C
      LSTL=16384
      LNBY=32768
C
      CALL LDFINP(IGO,JFIR,JLAS,IERR)
C
      RETURN
      ENDIF
C
C     ==================================================================
C     Test for input from UDF-file
C     ==================================================================
C
      IF(INTYP.EQ.'UDF ') THEN                  !Tst for UDF type
C
      LSTL=16384
      LNBY=32768
C
      CALL UDFINP(IGO,JFIR,JLAS,IERR)
C
      RETURN
      ENDIF
C
C     ==================================================================
C     Otherwise input from tape
C     ==================================================================
C
      LR=1                                      !Set to READ-1
      LP=1                                      !Set to PROC-2
C
      JFIR=1
      JLAS=LSTL
C
      IF(IGO.NE.1) GO TO 100                    !Tst for first read
C   
      IF(LAUTO.EQ.'YES ') THEN                  !Tst for auto RECL
                          LNBY=32768            !detection
                          LSTL=16384            !enabled
                          ENDIF
C
C     ------------------------------------------------------------------
C     Read 1 record
C     ------------------------------------------------------------------
C
  100 CALL MT_READW(C,LIST(1,LR),LNBY,NBYT,IERR)
C
      IF(IERR.NE.0) THEN
      CALL IOSERR(IERR)
      RETURN
      ENDIF
C
      NCEOF=0
C
      IF(MSGF.NE.'    ') RETURN                 !Tsfr Ctrl/C
C
      IF(CLISTF(1,LP).EQ.'SCAL')      GO TO 100 !Skip scaler
      IF(NBYT.EQ.LNBY.AND.IGO.EQ.2)   GO TO 200 !Tsfr data
      IF(NBYT.EQ.LNBY.AND.IGO.EQ.1)   GO TO 150 !Tsfr 1st read
C
      IF(NBYT.EQ.256)                 GO TO 220 !Tsfr header
      IF(NBYT.EQ.128)                 GO TO 240 !Tsfr deadtime
      IF(NBYT.LT.2048)                GO TO 100 !Skip PAC records
      IF(MOD(NBYT,2).NE.0)            GO TO 100 !Skip odd# bytes
      IF(IGO.NE.1.OR.LAUTO.NE.'YES ') GO TO 100 !Tsfr autorecl
      
C
C     ------------------------------------------------------------------
C     Do auto-recl detect
C     ------------------------------------------------------------------
C
  150 IF(LAUTO.EQ.'YES ') THEN
      LNBY=NBYT
      LSTL=NBYT/2
      JLAS=JFIR+LSTL-1
      WRITE(CMSSG,155)LNBY
  155 FORMAT('Using detected  data RECL =',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 200
      ENDIF
C
      WRITE(CMSSG,160)LNBY
  160 FORMAT('Using specified data RECL =',I6)
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Process data
C     ------------------------------------------------------------------
C
  200 IF(ISWAB.EQ.'YES ') THEN                  !Tst for 
      CALL SWAPB(LIST(1,LP),1,LSTL)             !Byte-swap request
      ENDIF
C   
      RETURN
C
C     ------------------------------------------------------------------
C     Display header
C     ------------------------------------------------------------------
C
  220 IHEDN=LISTF(33,LP)
      IF(ISWAB.EQ.'YES ') IHEDN=ISWAF(IHEDN)    !Tst for byte-swap
      WRITE(CMSSG,225)(LISTF(I,LP),I=9,23),IHEDN 
      CALL MESSLOG(LOGUT,LOGUP)
  225 FORMAT(15A4,I6)
      GO TO 100
C
C     ------------------------------------------------------------------
C     Display dead-time
C     ------------------------------------------------------------------
C
  240 DO 245 I=1,28
      MSSG(I)=LISTF(I,LP)
  245 CONTINUE
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
C
      END
