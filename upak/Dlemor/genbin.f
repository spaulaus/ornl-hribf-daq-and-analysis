C$PROG GENASCII - Generates an ASCII list-data file for scanmo
C
      IMPLICIT NONE
C
      INTEGER*4    LIST(8194)
C
      INTEGER*2    LIST2(2,8192),BLANK(16)
C
      CHARACTER*4  KIND
C
      INTEGER*4    LINE(16),IREC
C
      INTEGER*4    RECLVALU,NREC,IOS,ILO,IHI,JLO,JHI,JMAX,I,J,N
C
      EQUIVALENCE (LIST2(1,1),LIST(3)),(KIND,LIST(1))
C
      DATA         BLANK/16*0/
C
      SAVE
C
      OPEN(UNIT      = 1,
     &     FILE      = '/tera/milner/DDlinux/Dscanorx/rinl3.ldf',
     &     STATUS    = 'OLD',
     &     ACCESS    = 'DIRECT',
     &     RECL      = RECLVALU(32776),
     &     IOSTAT    = IOS)
C
      IF(IOS.NE.0) THEN
      WRITE(6,10)IOS
   10 FORMAT(' Error opening LDF file - IOS =',Z8)
      STOP
      ENDIF
C
      OPEN(UNIT      = 2,
     &     FILE      = 'rinl3.bin',
     &     ACCESS    = 'DIRECT',
     &     RECL      = RECLVALU(64),
     &     STATUS    = 'UNKNOWN',
     &     IOSTAT    = IOS)
C
      IF(IOS.NE.0) THEN
      WRITE(6,20)IOS
   20 FORMAT(' Error opening ASK file - IOS =',Z8)
      STOP
      ENDIF
C
      NREC=0
C
      IREC=0
C
  100 NREC=NREC+1
C
      READ(1,REC=NREC,ERR=500)LIST
C
      IF(KIND.NE.'DATA') GO TO 100
C
      JLO=1
      JHI=8192
  110 DO 120 J=JLO,JHI
      IF(LIST2(1,J).EQ.-1.AND.LIST2(2,J).EQ.-1) GO TO 130
      IF(LIST2(1,J).EQ.-1) THEN
      LIST2(1,J)=0
      GO TO 120
      ENDIF
      LIST2(1,J)=IAND(LIST2(1,J),'0FFF'X)
  120 CONTINUE
      GO TO 100
C
  130 IF(J.EQ.JLO) THEN
      JLO=JLO+1
      GO TO 110
      ENDIF
C
      ILO=JLO
      JMAX=J-1

  140 IHI=ILO+7
      IF(IHI.GT.JMAX) IHI=JMAX
C
      DO 142 I=1,16
      LINE(I)=0
  142 CONTINUE
C
      N=0
      DO 144 I=ILO,IHI
      N=N+1
      LINE(N)=LIST2(1,I)
      N=N+1
      LINE(N)=LIST2(2,I)
  144 CONTINUE
C
      IREC=IREC+1
      WRITE(2,REC=IREC)LINE
C
      ILO=IHI+1
      IF(ILO.LE.JMAX) GO TO 140
C
      DO 146 I=1,16
      LINE(I)=0
  146 CONTINUE
C
      IREC=IREC+1
      WRITE(2,REC=IREC)LINE
C
      JLO=JMAX+2
C
      IF(JLO.GT.8192) GO TO 100
C
      GO TO 110
C
  500 STOP
      END
