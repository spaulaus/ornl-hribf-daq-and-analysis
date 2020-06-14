C$PROG ZOTMAN    - Zeros individual in-core histograms
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE ZOTMAN
C   
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (MXLST=8000)
C
      LOGICAL SHMFLG,SHMUSE
C
      COMMON/SharedMem/ SHMID(20),SHMFLG(20),SHMUSE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      INTEGER*4    IOFF(MXLST),IOFH(MXLST),LENH(MXLST)
C
      INTEGER*2    NHPC(MXLST)
C
      INTEGER*4    IDLST(MXLST),LSHMID(20)
C
      INTEGER*4    IBUF(32),STAT,S
C
      INTEGER*2    IBUH(64)
C
      CHARACTER*4  KMD,KMX,CLWD(2,40)
C
      EQUIVALENCE (IBUH,IBUF)
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMX,LWD(1,2)),(CLWD,LWD)
C
      DATA NID,LSHMID/0,20*0/
C
      DATA IOFF,IOFH,LENH/MXLST*0,MXLST*0,MXLST*0/
      DATA NHPC          /MXLST*0/
C
      CHARACTER*4  IZOT,DROK
C
      DATA LU,IZOT,DROK/9,'OFF ','NO  '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO ZERO INDIVIDUAL IN-CORE HISTOGRAMS
C     ------------------------------------------------------------------
C     SUPPORTED FORMS ARE:  Z ON             (enable  zero command)
C                           Z OFF            (disable zero command)
C
C                           Z IDA,IDB,IDC... (explicit list)
C                           Z IDA to IDB     (in steps of 1)
C                           Z IDA to IDB INC (in steps of INC)
C     ------------------------------------------------------------------
C
      IF(KMX.EQ.'ON  ')  THEN                    !TST FOR Z-ENABLE
                         IZOT='ON  '
                         RETURN
                         ENDIF
C
      IF(KMX.EQ.'OFF ')  THEN                    !TST FOR Z-DISABLE
                         IZOT='OFF '
                         RETURN
                         ENDIF
C
      IF(IZOT.NE.'ON  ') GO TO 1000              !TST FOR Z-ENABLE
      IF(.NOT.SHMUSE)    GO TO 1010              !TST SHM IN USE
      IF(.NOT.SHMFLG(9)) GO TO 1020              !TST FOR N-file
      IF(SHMID(9).EQ.LSHMID(9)) GO TO 300        !TST FOR NEW FILE
C
C     ------------------------------------------------------------------
C     BUILD A NEW DIRECTORY IF SHMID HAS CHANGED
C     ------------------------------------------------------------------
C
      DO 20 I=1,MXLST
      LENH(I)=0
   20 CONTINUE
      DROK='NO  '
C
      READ(LU,REC=1,IOSTAT=S)IBUF
      IF(S.NE.0) THEN
      CALL IOFERR(S)
      GO TO 1040
      ENDIF
C
      NH=IBUF(4)
      NDO=(NH+31)/32
      NRN=NH+1
      JB=0
C
      DO 120 N=1,NDO
      NRN=NRN+1
      JA=JB+1
      JB=JA+31
      READ(LU,REC=NRN,IOSTAT=S)(IDLST(J),J=JA,JB)
      IF(S.NE.0) THEN
      CALL IOFERR(S)
      GO TO 1040
      ENDIF
  120 CONTINUE
C
      NRN=1
      DO 150 N=1,NH
      NRN=NRN+1
      READ(LU,REC=NRN,IOSTAT=S)IBUF
      IF(S.NE.0) THEN
      CALL IOFERR(S)
      GO TO 1040
      ENDIF
C
      NDX=IDLST(N)
C
      IF(NDX.LT.1.OR.NDX.GT.MXLST) GO TO 1050
C
      IOFF(NDX)=IBUF(12)/2+1
      IOFH(NDX)=IBUF(12)+1
      NHPC(NDX)=IBUH(2)
      NDIM=IBUH(1)
      LENCH=1
C
      DO 130 I=1,NDIM
      IMIN=IBUH(14+I)
      IMAX=IBUH(18+I)
      LENCH=LENCH*(IMAX-IMIN+1)
  130 CONTINUE
      LENH(NDX)=LENCH
C
  150 CONTINUE
      LSHMID(9)=SHMID(9)
      DROK='YES '
C
C     ------------------------------------------------------------------
C     EXECUTE THE ZERO REQUEST
C     ------------------------------------------------------------------
C  
  300 IF(NTER.NE.0) GO TO 1030                   !TST FOR ERROR
C
      IF(DROK.NE.'YES ') GO TO 1060              !TST FOR DIR-OK
C
      IF(NF.EQ.1.AND.NID.GT.0) GO TO 400         !TST FOR "REPEAT"
      IF(NF.LT.2)              GO TO 1000        !TST FOR ERROR
C
      NID=0                                      !# IDs TO ZERO
      IF(CLWD(1,3).EQ.'TO  ')GOTO 350            !TST FOR FORM
C
      DO 310 I=2,NF                              !LOOP ON DATA FIELDS
      CALL LIMIV(LWD(1,I),1,MXLST,IV,IERR)       !GET VALUE
      IF(IERR.NE.0) GO TO 1000                   !TST FOR ERROR
      NID=NID+1                                  !INC ID COUNTER
      IDLST(NID)=IV
  310 CONTINUE
      GO TO 400                                  !GO ZERO LIST
C
  350 INC=1                                      !DEFAULT LOOP INC
      CALL LIMIV(LWD(1,5),0,MXLST,IT,IERR)       !GET LOOP INC
      IF(IERR.NE.0) GO TO 1000                   !TST FOR ERROR
      IF(IT.GT.0) INC=IT                         !TST FOR SPECIFIED
      CALL LIMIV(LWD(1,2),1,MXLST,IA,IERR)       !GET LO-LIMIT
      IF(IERR.NE.0) GO TO 1000                   !TST FOR ERROR
      CALL LIMIV(LWD(1,4),1,MXLST,IB,IERR)       !GET HI-LIMIT
      IF(IERR.NE.0) GO TO 1000                   !TST FOR ERROR
      IF(IA.GT.IB)  GO TO 1000                   !TST FOR ERROR
C
      DO 360 I=IA,IB,INC                         !GENERATE IN LIST
      NID=NID+1                                  !INC LIST INDEX
      IF(NID.GT.MXLST) GO TO 1000                !TST FOR OVERFLO
      IDLST(NID)=I                               !STORE ID
  360 CONTINUE
C
  400 DO 500 N=1,NID                             !LOOP ON ID LIST
C
      ID=IDLST(N)                                !ACTUAL ID
C
      IF(LENH(ID).LE.0) GO TO 500                !TEST FOR UNUSED
C
      IF(NHPC(ID).EQ.2) GO TO 420                !TEST FOR 32-BIT/CH
C
      IA=IOFH(ID)                                !DO IT FOR 16-BITS/CH
      IB=IA+LENH(ID)-1
C
      CALL MEM_ZOT_HW(IA,IB)                     !ZERO HALF-WDS IA-IB
      GO TO 500
C
  420 IA=IOFF(ID)                                !DO IT FOR 16-BITS/CH
      IB=IA+LENH(ID)-1
C
      CALL MEM_ZOT_FW(IA,IB)                     !ZERO FULL-WDS IA-IB
C
  500 CONTINUE
C
      RETURN
C
C     ------------------------------------------------------------------
C     SEND ERROR MESSAGES AND RETURN
C     ------------------------------------------------------------------
C  
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('The Z-command has not been enabled - cmd ignored')
      GO TO 2000
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('Shared Memory not in use - Z-cmd not supported')
      GO TO 2000
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('Shared Memory not in use for N-file - Z-cmd ',
     &'not supported')
      GO TO 2000
 1030 WRITE(CMSSG,1035)
 1035 FORMAT('Syntax error or illegal value - cmd ignored!')
      GO TO 2000
 1040 WRITE(CMSSG,1045)
 1045 FORMAT('Error reading DRR-file - Z-cmd unavailable')
      GO TO 2000
 1050 WRITE(CMSSG,1055)
 1055 FORMAT('Histogram ID out-of-range - Z-cmd unavailable')
      GO TO 2000
 1060 WRITE(CMSSG,1065)
 1065 FORMAT('Memory Directory unavailable - cmd ignored')
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
      END
