C$PROG ADJUST
C
      SUBROUTINE ADJUST(IC,NLO,NHI)
C
      INTEGER*4 IC(120,2),JC(120,2),NBLAD(60)
C
      INTEGER*4  X20
C
      DATA       X20/'20'X/
C
      DATA ITOG/1/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IGAP=0
      NGAP=0
      LNB=0
      N=NLO-1
   10 N=N+1
      IF(N.GT.NHI) GO TO 50
      IF(IC(N,1).NE.X20) GO TO 30
      IGAP=IGAP+1
   20 N=N+1
      IF(N.GT.NHI) GO TO 50
      IF(IC(N,1).EQ.X20) GO TO 20
   30 LNB=N
      NGAP=IGAP
      GO TO 10
   50 NTOAD=NHI-LNB
      IF(NTOAD.LE.0) RETURN
      DO 60 I=1,60
      NBLAD(I)=0
   60 CONTINUE
      NADED=0
   70 NS=1
      IF(ITOG.NE.1) NS=NGAP
      JAD=1
      IF(ITOG.NE.1) JAD=-JAD
      N=NS
      IF(NGAP.LT.1) GO TO 90
      DO 80 I=1,NGAP
      NBLAD(N)=NBLAD(N)+1
      NADED=NADED+1
      IF(NADED.GE.NTOAD) GO TO 100
      N=N+JAD
   80 CONTINUE
   90 ITOG=-ITOG
      GO TO 70
  100 ITOG=-ITOG
      N=NLO
      M=NLO
      NDX=0
  110 IF(M.GT.NHI) GO TO 200
      JC(M,1)=IC(N,1)
      JC(M,2)=IC(N,2)
      N=N+1
      M=M+1
      IF(IC(N-1,1).NE.X20) GO TO 110
C   
C     STUFF IN EXTRA BLANKS
C   
      NDX=NDX+1
      NDO=NBLAD(NDX)
      IF(NDO.LE.0) GO TO 110
      DO 120 I=1,NDO
      JC(M,1)=X20
      JC(M,2)=0
      M=M+1
  120 CONTINUE
C   
C     PICK UP ANY MORE BLANKS IN INPUT STRING
C   
  130 IF(IC(N,1).NE.X20) GO TO 110
      JC(M,1)=IC(N,1)
      JC(M,2)=IC(N,2)
      M=M+1
      N=N+1
      IF(M.GT.NHI) GO TO 200
      GO TO 130
C   
C     LOAD ADJUSTED LINE BACK INTO IC
C   
  200 IF(NLO.GT.NHI) GO TO 220
      DO 210 I=NLO,NHI
      IC(I,1)=JC(I,1)
      IC(I,2)=JC(I,2)
  210 CONTINUE
  220 RETURN
      END
