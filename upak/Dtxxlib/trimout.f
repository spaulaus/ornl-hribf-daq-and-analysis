C$PROG TRIMOUT
C
      SUBROUTINE TRIMOUT(IBY,NBY)
C
      COMMON/BBB/  IFMTA,IFMTB,IDATE(3),IFODX,JUSTON
      CHARACTER*4                             JUSTON
      CHARACTER*16 IFMTA,IFMTB
C
      COMMON/TOGG/ KTOG
C
      COMMON/YYY/ KLIS,KAUTOSP,KPAGLAB,KTOFLAG,NPAGSP
      CHARACTER*4 KLIS,KAUTOSP,KPAGLAB,KTOFLAG
C
      BYTE IBY(*),X20
C
      DATA        X20/'20'X/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      II=NBY+1
C
   10 II=II-1
      IF(II.LT.1)        GO TO 30
      IF(IBY(II).EQ.X20) GO TO 10
C
      IF(KTOG.EQ.1) WRITE(7,IFMTA)(IBY(I),I=1,II)
      IF(KTOG.NE.1) WRITE(7,IFMTB)(IBY(I),I=1,II)
      RETURN
C
   30 WRITE(7,40)
   40 FORMAT(1H )
      RETURN
      END
