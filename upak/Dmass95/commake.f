C$PROG MASSIO
C
      INTEGER*4 MASS(4),ERR(4),LOZ(112),HIZ(112)
C
      INTEGER*4 A(9,2932)
C
      DATA LOZ,HIZ/224*0/
C
      OPEN(UNIT       = 1,
     &     FILE       = 'mas95.dat',
     &     STATUS     = 'OLD')
C
      OPEN(UNIT       = 7,
     &     FILE       = 'commake.log',
     &     STATUS     = 'UNKNOWN',
     &     DISP       = 'DELETE',
     &     ACCESS     = 'APPEND')
C
      CLOSE(UNIT=7)
C
      OPEN(UNIT       = 7,
     &     FILE       = 'commake.log',
     &     STATUS     = 'UNKNOWN',
     &     ACCESS     = 'APPEND')
C
      LASZ=-1
C
      NN=0
  100 READ(1,105,END=400)IZ,IA,MASS,ERR
  105 FORMAT(I10,I8,2(3A4,A1))
      NN=NN+1
      IF(IZ.NE.LASZ) THEN
                     NDX=IZ+1
                     LOZ(NDX)=NN
                     LASZ=IZ
                     ENDIF
C
      HIZ(NDX)=NN
C
C     WRITE(7,125)NN,IZ,IA,MASS,ERR
C 125 FORMAT(3I8,2(3A4,A2))
C
      A(1,NN)=IA
      DO 130 I=1,4
      A(I+1,NN)=MASS(I)
      A(I+5,NN)=ERR(I)
  130 CONTINUE
      GO TO 100
C
  400 WRITE(7,405)
  405 FORMAT(6X,'DATA LZLO/')
      WRITE(7,420)LOZ
      WRITE(7,410)
  410 FORMAT(6X,'DATA LZHI/')
      WRITE(7,420)HIZ
      WRITE(7,430)
  420 FORMAT(5X,'&',I4,',',I4,',',I4,',',I4,',',I4,',',
     &              I4,',',I4,',',I4,',',I4,',',I4,',')
  430 FORMAT(1H )
C
      JLO=1
      JHI=90
      WRITE(7,440)JLO,JHI
  440 FORMAT(6X,'DATA (XBUF(I),I=',I4,',',I4,')/')
C
      NLN=0
      DO 550 J=1,2932,2
      NLN=NLN+1
C
      IF(NLN.EQ.15) THEN
      NLN=0
      WRITE(7,530)(A(I,J),I=1,9),(A(I,J+1),I=1,9)
      JLO=JHI+1
      JHI=JLO+89
      WRITE(7,440)JLO,JHI
      GO TO 550
      ENDIF
C
      WRITE(7,525)(A(I,J),I=1,9),(A(I,J+1),I=1,9)
C
  525 FORMAT(5X,'&',I3,',',2(3A4,A1,','),I3,',',2(3A4,A1,','))
  530 FORMAT(5X,'&',I3,',',2(3A4,A1,','),
     &              I3,',',  3A4,A1,',',3A4,A1,'/')
C
  550 CONTINUE
      CALL EXIT
      END
