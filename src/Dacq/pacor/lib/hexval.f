C$PROG HEXVAL
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      SUBROUTINE HEXVAL(IWD,IV,IERR)
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      INTEGER*4 IWD(2),JWD(2)
C
      CHARACTER*8   CJWD
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CJWD,JWD),(CMSSG,MSSG)
C
      INTEGER*4    X20,X30,X39,X41,X46,X48
      DATA X20,X30,X39,X41,X46,X48/'20'X,'30'X,'39'X,'41'X,'46'X,'48'X/
C
      SAVE
C
C     ------------------------------------------------------------------
C     "IWD" CONTAINS RIGHT-JUSTIFIED ASCII STRING REPRESENTING A
C     HEXADECIMAL NUMBER
C     FIRST CHARACTER MUST BE A DECIMAL DIGIT
C     LAST  CHARACTER MUST BE CHARACTER "H"
C     THE NUMERICAL VALUE IS RETURNED IN "IV"
C     ------------------------------------------------------------------
C
      IV=0
      IERR=0
      CJWD=' '
C
      CALL ILBYTE(IT,IWD,7)
      IF(IT.NE.X48) GO TO 50
C
      I=6
      J=7
      DO 40 N=1,7
      CALL ILBYTE(IT,IWD,I)
      IF(IT.EQ.X20) GO TO 30
      IF(IT.GE.X30.AND.IT.LE.X39) GO TO 20
      IF(IT.GE.X41.AND.IT.LE.X46) GO TO 20
      GO TO 50
C
   20 CALL ISBYTE(IT,JWD,J)
      J=J-1
C
   30 I=I-1
   40 CONTINUE
C
      READ(CJWD,45,ERR=50)IV
   45 FORMAT(Z8)
      RETURN
C
   50 WRITE(CMSSG,55)IWD
   55 FORMAT('ILLEGAL HEX-VALUE SPECIFICATION = ',2A4)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
