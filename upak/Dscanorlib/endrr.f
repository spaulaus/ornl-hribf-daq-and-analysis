C$PROG ENDRR     - Windup call - outputs directory & first record
C
C     ******************************************************************
C     BY J.R. BEENE AT HRIBF - LAST MODIFIED by WT MILNER 11/17/99
C     ******************************************************************
C
      SUBROUTINE ENDRR
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    MXNH
C
      PARAMETER   (MXNH=6144)
C
C     ------------------------------------------------------------------
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      COMMON/SC18/ ICMP(4,8000),IMIN(4,8000),IMAX(4,8000),MAXOFF
C
      INTEGER*2    ICMP,        IMIN,        IMAX
      INTEGER*4                                           MAXOFF
C     ------------------------------------------------------------------
      COMMON/SC24/ IDIRF(32),IDLST(MXNH),NXOFF,IREC,NHID,LSOFF,NCALL
      INTEGER*4    IDIRF,    IDLST,      NXOFF,IREC,NHID,LSOFF,NCALL
      CHARACTER*4  CDIRF(32)
      EQUIVALENCE (CDIRF,IDIRF)
C     ------------------------------------------------------------------
      INTEGER*4    IDATIM(6),ILO,IHI,IOS,I,LDR
C
      EQUIVALENCE (LDR,LUC(9))
C
      SAVE
C
C     ------------------------------------------------------------------
C     OUTPUT "DIRECTORY" TO .DRR-FILE
C     ------------------------------------------------------------------
C
      MAXOFF=NXOFF
      ILO=1                                 !INIT ID-CNTR
  200 IF(ILO.GT.NHID) GOTO 250              !TST FOR DONE
      IHI=ILO+31
      IREC=IREC+1
      WRITE(LDR,REC=IREC,IOSTAT=IOS)(IDLST(I),I=ILO,IHI)
      IF(IOS.NE.0)CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1000               !TST FOR ERROR
      ILO=ILO+32                            !INC ID-CNTR
      GO TO 200                             !GO BACK FOR MORE
C
C     ------------------------------------------------------------------
C     SET-UP AND OUTPUT FIRST BLOCK OF .DIR-FILE
C     ------------------------------------------------------------------
C
  250 CDIRF(1)='HHIR'
      CDIRF(2)='FDIR'
      CDIRF(3)='0001'
      IDIRF(4)=NHID                         !# OF ID'S ON FILE
      IDIRF(5)=NXOFF                        !# OF HALF-WDS ON FILE
      IDIRF(6)=0                            !NOT USED
      CALL MILYMDHMS(IDATIM)                !GET DATE & TIME
C
      IDIRF(7)= IDATIM(1)                   !YEAR
      IDIRF(8)= IDATIM(2)                   !MONTH
      IDIRF(9)= IDATIM(3)                   !DAY
      IDIRF(10)=IDATIM(4)                   !HOUR
      IDIRF(11)=IDATIM(5)                   !MINUTE
      IDIRF(12)=IDATIM(6)                   !SECOND
C
      IREC=1                                !LOCATE FIRST RECORD
      WRITE(LDR,REC=IREC,IOSTAT=IOS)IDIRF   !OUTPUT FIRST RECORD
      CLOSE(LDR)
      CALL IOFERR(IOS)
      IF(IOS.NE.0) GO TO 1000               !TST FOR ERROR
      RETURN 
C            
C     ------------------------------------------------------------------
C     Error return
C     ------------------------------------------------------------------
C            
c1000 NERR=NERR+1
C
 1000 RETURN 
      END    
