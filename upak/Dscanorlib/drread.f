C$PROG DRREAD    - Processes drr-file & sets program parameters
C
C     ******************************************************************
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WT MILNER 02/17/99
C     ******************************************************************
C
      SUBROUTINE DRREAD(LU)
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
      COMMON/SC02/ NAMH(20)
      INTEGER*4    NAMH
C     ------------------------------------------------------------------
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      COMMON/SC17/ IOFF(8000),IOFH(8000),NDIM(8000),NHPC(8000),
     &             LENX(8000),LENH(8000)
C
      INTEGER*2    LENX,                 NDIM,      NHPC
      INTEGER*4    IOFF,      IOFH
      INTEGER*4               LENH
C     ------------------------------------------------------------------
      COMMON/SC18/ ICMP(4,8000),IMIN(4,8000),IMAX(4,8000),MAXOFF
C
      INTEGER*2    ICMP,        IMIN,        IMAX
      INTEGER*4                                           MAXOFF
C     ------------------------------------------------------------------
      INTEGER*4    MXNH
C   
      PARAMETER   (MXNH=6144)
C
      INTEGER*4    IDLST(MXNH),IBUF(32),JCMP(4),STAT,S,LU,N,J,LCMP
C   
      INTEGER*2    IBUH(64)
C
      CHARACTER*80 CNAMH,CNAMLIST
C
      CHARACTER*40 TIT
C   
      EQUIVALENCE (IBUH,IBUF),(IBUF(23),TIT)
C
      EQUIVALENCE (CNAMH,NAMH)
C
      INTEGER*4    LSNB,I1,IHIS,IHIS2,I,NH,NHW,NRN,JA,JB,NDO,K,NDX,LENCH
C
      INTEGER*4    NREC
C
      REAL*4       XCMP
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      I1=LSNB(NAMH,1,80)
      CNAMLIST=CNAMH
      IHIS=INDEX(CNAMLIST,'.HIS')
      IHIS2=INDEX(CNAMLIST,'.his')
      IF(IHIS.EQ.0.AND.IHIS2.NE.0)IHIS=IHIS2
C
      IF(I1.EQ.0)THEN
      CMSSG='HIS name error: using XnoNameX.list for DRR listing'
      CALL MESSLOG(LOGUT,LOGUP)
      CNAMLIST='XnoNameX'
      I1=8
      ELSE IF(IHIS.NE.0)THEN
      CNAMLIST=CNAMH(1:IHIS-1)
      I1=IHIS-1
      ENDIF
C
      CNAMLIST(I1+1:)='.list'
      WRITE(CMSSG,5)CNAMLIST(1:I1+6)
    5 FORMAT('DRR list file name = ',A)
      CNAMLIST(I1+6:I1+6)=CHAR(0)
      OPEN(UNIT=11,STATUS='UNKNOWN',FILE=CNAMLIST)
C
      DO 10 I=1,8000
      NDIM(I)=0
   10 CONTINUE
C   
      NRN=1
      READ(LU,REC=NRN,IOSTAT=S)IBUF
      IF(S.NE.0) THEN
                 CALL IOFERR(S)
                 GO TO 200
                 ENDIF
C   
      NH=IBUF(4)
      NHWH=IBUF(5)
C
      NREC=NHWH/32768               !Make his-file size a multiple
      IF(32768*NREC.LT.NHWH) THEN   !of 65536 bytes
      NREC=NREC+1                   !to make it more efficient to
      NHWH=32768*NREC               !add one his-file to another
      ENDIF
C
      WRITE(11,'(1H ,I6,'' HISTOGRAMS, '',I12,'' HALF-WORDS'')')NH,NHWH
      NDO=(NH+31)/32
      NRN=NH+1
      JB=0
C   
      DO 20 N=1,NDO
      NRN=NRN+1
      JA=JB+1
      JB=JA+31
      READ(LU,REC=NRN,IOSTAT=S)(IDLST(J),J=JA,JB)
      IF(S.NE.0) THEN
                 CALL IOFERR(S)
                 GO TO 200
                 ENDIF
   20 CONTINUE
C
      WRITE(11,25)
      WRITE(11,30)(IDLST(K),K=1,NH)
      WRITE(11,35)
   25 FORMAT(' ID-LIST:')
   30 FORMAT(8I8)
   35 FORMAT('  HID  DIM HWPC  LEN(CH)   COMPR  MIN',
     &          '   MAX   OFFSET    TITLE')
C   
      NRN=1
      DO 100 N=1,NH
      NRN=NRN+1
      READ(LU,REC=NRN,IOSTAT=S)IBUF
      IF(S.NE.0) THEN
                 CALL IOFERR(S)
                 GO TO 200
                 ENDIF
C   
      NDX=IDLST(N)
C   
      IF(NDX.LT.1.OR.NDX.GT.8000) GO TO 220
C   
      IOFF(NDX)=IBUF(12)/2+1
      IOFH(NDX)=IBUF(12)+1
      NDIM(NDX)=IBUH(1)
      NHPC(NDX)=IBUH(2)
      NHW=IBUH(2)
      LENCH=1
      LENX(NDX)=IBUH(19)-IBUH(15)+1
C
      DO 50 I=1,NDIM(NDX)
      IMIN(I,NDX)=IBUH(14+I)
      IMAX(I,NDX)=IBUH(18+I)
      LENCH=LENCH*(IMAX(I,NDX)-IMIN(I,NDX)+1)
      ICMP(I,NDX)=0
      JCMP(I)=0
C
      IF(IBUH(10+I).NE.0)XCMP=IBUH(6+I)/IBUH(10+I)
C
      IF(XCMP.GE.1.)THEN
      XCMP=LOG(XCMP)/LOG(2.)
      LCMP=NINT(ABS(XCMP))
      ICMP(I,NDX)=LCMP
      JCMP(I)=2**(ICMP(I,NDX))
      ENDIF
C
   50 CONTINUE
C
      LENH(NDX)=LENCH
      WRITE(11,60)NDX,NDIM(NDX),NHPC(NDX),LENCH,JCMP(1),
     &            IMIN(1,NDX),IMAX(1,NDX),IBUF(12),TIT(1:20)
   60 FORMAT(I5,I5,I4,I9,I8,2I6,I9,2x,A)
C
      IF(NDIM(NDX).GT.1)THEN
      DO 80 J=2,NDIM(NDX)
      WRITE(11,75)JCMP(J),IMIN(J,NDX),IMAX(J,NDX)
   75 FORMAT(14X,9X,I8,2I6)
   80 CONTINUE
                        ENDIF
C
  100 CONTINUE
C
      CLOSE(UNIT=LU,IOSTAT=STAT)
      CLOSE(11)
      RETURN
C   
  200 WRITE(CMSSG,205)NRN
  205 FORMAT('ERROR READING DRR-FILE NRN =',I4)
      GO TO 230
C   
  220 WRITE(CMSSG,225)NDX
  225 FORMAT('ILLEGAL HISTOGRAM ID =',I8,'  LEGAL VALUES ARE 1-8000')
  230 CALL MESSLOG(LOGUT,LOGUP)
      STOP
      END
