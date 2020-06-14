C$PROG PAC120
C
      SUBROUTINE PAC120(ICH,IWD,IFIR,NUM)
C
      COMMON/XXX/ BOLDFLG,UNDLFLG,BOLDON,UNDLON,BOLDOF(2),UNDLOF(2)
      CHARACTER*4 BOLDFLG,UNDLFLG,BOLDON,UNDLON,BOLDOF,   UNDLOF
C
      COMMON/YYY/ KLIS,KAUTOSP,KPAGLAB,KTOFLAG,NPAGSP
      CHARACTER*4 KLIS,KAUTOSP,KPAGLAB,KTOFLAG
C
      BYTE      BBOLDON(4),BUNLDON(4),BBOLDOF(8),BUNDLOF(8)
C
      EQUIVALENCE (BBOLDON,BOLDON),
     &            (BUNDLON,UNDLON),
     &            (BBOLDOF,BOLDOF),
     &            (BUNDLOF,UNDLOF)
C
      INTEGER*4 ICH(120,2),IWD(1),JTEMP
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
C     ROUTINE TO PACK LO-ORDER BYTES FROM ICH INTO IWD
C
      I=IFIR-1
      NDO=LASTC(ICH,NUM)
C
      DO 100 N=1,NDO
      IT=ICH(N,1)
      IFLG=ICH(N,2)
      IF(IFLG.LE.0)      GO TO 50
      IF(KLIS.NE.'LN03') GO TO 50
C
      IF(IFLG.EQ.1) THEN
                    CALL LODUP(BOLDON,1,4,IWD,I+1)
                    I=I+4
                    GO TO 50
                    ENDIF
C
      IF(IFLG.EQ.2) THEN
                    CALL LODUP(UNDLON,1,4,IWD,I+1)
                    I=I+4
                    GO TO 50
                    ENDIF
C
      IF(IFLG.EQ.3) THEN
                    CALL LODUP(BOLDON,1,4,IWD,I+1)
                    CALL LODUP(UNDLON,1,4,IWD,I+5)
                    I=I+8
                    GO TO 50
                    ENDIF
C
   50 I=I+1
      CALL ISBYTE(IT,IWD,I-1)
      IF(KLIS.NE.'LN03') GO TO 100
      IF(IFLG.NE.-1)     GO TO 100
C
      IF(BOLDFLG.NE.'BON$') THEN
                            CALL LODUP(BOLDOF,1,6,IWD,I+1)
                            I=I+6
                            ENDIF
C
      IF(UNDLFLG.NE.'UON$') THEN
                            CALL LODUP(UNDLOF,1,6,IWD,I+1)
                            I=I+6
                            ENDIF
C
  100 CONTINUE
      RETURN
      END
