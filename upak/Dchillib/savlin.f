C$PROG SAVLIN
      SUBROUTINE SAVLIN(JWD,MSER,IERR,J21,J30,J31)
C
      COMMON/JJJ/ LIN,LOU,LU6,LER,NEREC,NSOL,LHLN,NERR
C
      INTEGER*4 JWD(32),MSER(10)
C
      SAVE
C
C     **************************************************************
C     SAVES JWD ON CHIL.IMF & ANY ERROR MESSAGE ON CHIL.ERR
C     **************************************************************
C
      IF(IERR.EQ.0) GO TO 20
      NEREC=NEREC+1
      NERR=NERR+1
      JWD(22)=NEREC
C
      WRITE(LER,IOSTAT=IOS)MSER
      CALL IOERR(IOS)
C
   20 K21=JWD(21)
      K30=JWD(30)
      K31=JWD(31)
      JWD(21)=J21
      JWD(30)=J30
      JWD(31)=J31
C
      WRITE(LOU,IOSTAT=IOS)JWD
      CALL IOERR(IOS)
C
      JWD(21)=K21
      JWD(30)=K30
      JWD(31)=K31
      RETURN
      END
