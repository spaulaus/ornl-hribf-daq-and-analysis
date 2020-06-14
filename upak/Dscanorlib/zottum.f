C$PROG ZOTTUM    - Zeros individual in-core histograms
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/17/99
C     ******************************************************************
C
      SUBROUTINE ZOTTUM(IWD)
C   
      IMPLICIT NONE
C
      INTEGER*4    MXLST
C
      PARAMETER   (MXLST=8000)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SC17/ IOFF(8000),IOFH(8000),NDIM(8000),NHPC(8000),
     &             LENX(8000),LENH(8000)
C
      INTEGER*2    LENX,                 NDIM,      NHPC
      INTEGER*4    IOFF,      IOFH
      INTEGER*4               LENH
C     ------------------------------------------------------------------
      INTEGER*4   IWD(20),LWD(2,40),ITYP(40),NF
C
      INTEGER*4   IDLST(MXLST)
C
      INTEGER*4   NID,NTER,IERR,IT,INC,IA,IB,ID,N,I,IV
C
      DATA NID/0/
C
      CHARACTER*4  KMZ
C
      EQUIVALENCE (KMZ,LWD(1,3))
C
C     ------------------------------------------------------------------
C     SUPPORTED FORMS ARE:  Z IDA,IDB,IDC... (explicit list)
C                           Z IDA to IDB     (in steps of 1)
C                           Z IDA to IDB INC (in steps of INC)
C     ------------------------------------------------------------------
C
      CALL CASEUP(IWD)                           !MAKE UPPER CASE
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)      !REFORMAT
      IF(NTER.NE.0) GO TO 1000                   !TST FOR ERROR
C
      IF(NF.EQ.1.AND.NID.GT.0) GO TO 100         !TST FOR "REPEAT"
      IF(NF.LT.2)              GO TO 1000        !TST FOR ERROR
C
      NID=0                                      !# IDs TO ZERO
      IF(KMZ.EQ.'TO  ')GOTO 50                   !TST FOR FORM
C
      DO 10 I=2,NF                               !LOOP ON DATA FIELDS
      CALL LIMIV(LWD(1,I),1,8000,IV,IERR)        !GET VALUE
      IF(IERR.NE.0) GO TO 1000                   !TST FOR ERROR
      NID=NID+1                                  !INC ID COUNTER
      IDLST(NID)=IV
   10 CONTINUE
      GO TO 100                                  !GO ZERO LIST
C
   50 INC=1                                      !DEFAULT LOOP INC
      CALL LIMIV(LWD(1,5),0,8000,IT,IERR)        !GET LOOP INC
      IF(IERR.NE.0) GO TO 1000                   !TST FOR ERROR
      IF(IT.GT.0) INC=IT                         !TST FOR SPECIFIED
      CALL LIMIV(LWD(1,2),1,8000,IA,IERR)        !GET LO-LIMIT
      IF(IERR.NE.0) GO TO 1000                   !TST FOR ERROR
      CALL LIMIV(LWD(1,4),1,8000,IB,IERR)        !GET HI-LIMIT
      IF(IERR.NE.0) GO TO 1000                   !TST FOR ERROR
      IF(IA.GT.IB)  GO TO 1000                   !TST FOR ERROR
C
      DO 60 I=IA,IB,INC                          !GENERATE IN LIST
      NID=NID+1                                  !INC LIST INDEX
      IF(NID.GT.MXLST) GO TO 1000                !TST FOR OVERFLO
      IDLST(NID)=I                               !STORE ID
   60 CONTINUE
C
  100 DO 200 N=1,NID                             !LOOP ON ID LIST
C
      ID=IDLST(N)                                !ACTUAL ID
C
      IF(LENX(ID).LE.0) GO TO 200                !TEST FOR UNUSED
C
      IF(NHPC(ID).EQ.2) GO TO 120                !TEST FOR 32-BIT/CH
C
      IA=IOFH(ID)                                !DO IT FOR 16-BITS/CH
      IB=IA+LENH(ID)-1
C
      CALL MEM_ZOT_HW(IA,IB)                     !ZERO HALF-WDS IA-IB
      GO TO 200
C
  120 IA=IOFF(ID)                                !DO IT FOR 16-BITS/CH
      IB=IA+LENH(ID)-1
C
      CALL MEM_ZOT_FW(IA,IB)                     !ZERO FULL-WDS IA-IB
C
  200 CONTINUE
C
      RETURN
C
C     ------------------------------------------------------------------
C     Error return
C     ------------------------------------------------------------------
C
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Syntax error or illegal value - command ignored!')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
      END
