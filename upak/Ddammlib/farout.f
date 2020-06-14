C$PROG FAROUT    - Outputs an array of FP numbers 
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE FAROUT(X,NDEC,N)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      CHARACTER*8  CCMSSG(14)
      EQUIVALENCE (CCMSSG,MSSG)
C   
      DIMENSION    X(*)
C   
C     ------------------------------------------------------------------
C   
      IHI=0
C   
   10 ILO=IHI+1
      IF(ILO.GT.N) RETURN
      IHI=ILO+3
      IF(IHI.GT.N)IHI=N
      L=1
C   
      DO 50 I=ILO,IHI
      WRITE(CCMSSG(L),30)X(I)
   30 FORMAT(F8.2)
      L=L+1
   50 CONTINUE
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 10
      END
