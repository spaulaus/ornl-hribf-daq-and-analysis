C$PROG GETBETA   - Gets adjusted BETA & A(J,J) for rel-intensity case
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE GETBETA(JP,JX,BETAJ,AJJ)
C   
C
C     ------------------------------------------------------------------
      COMMON/SM04/ KXF(4,44),KFUN(44),BETA(50),IVF(4)
C     ------------------------------------------------------------------
      COMMON/SM28/ RELI(44),NREL,NPKK,KRELF
C     ------------------------------------------------------------------
      COMMON/MAINV8/ A(50,50),B(50,50),DET,IFS
      REAL*8         A,       B,       DET
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     GET ADJUSTED BETA(J) AND A(J,J) FOR CASES WHERE
C     RELATIVE INTENSITY HAS BEEN SPECIFIED  
C     ------------------------------------------------------------------
C
      IF(NREL.LE.1)       THEN
                          BETAJ=BETA(JP) 
                          AJJ=A(JP,JP)
                          JX=JP
                          RETURN
                          ENDIF
C
      IF(RELI(JP).GT.0.0) THEN
                          BETAJ=BETA(1)*RELI(JP)
                          AJJ=A(1,1)
                          JX=1
                          RETURN
                          ENDIF
C
      JX=1
      DO 20 I=1,JP
      IF(RELI(I).LE.0.0) JX=JX+1
   20 CONTINUE
C
      BETAJ=BETA(JX)
      AJJ=A(JX,JX)
C
      RETURN
      END
