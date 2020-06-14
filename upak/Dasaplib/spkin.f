C$PROG SPKIN
      SUBROUTINE SPKIN(KFIL,LUS,LUH,LUD,ID,IDAT,NCH,IERR)
C
      COMMON/DIR/KLOC(6),JHSP(4),LENG(4),ND,NHW,LENH,LENT,IOF,LDF,
     &NHIS,LEND(4),LENS(4),MINC(4),MAXC(4),CONS(4),ITEX(20),
     &ITIT(10),LABX(3),LABY(3),MSER(10),KFILT
C
      INTEGER*4 IDAT(1),NDX(4)
      INTEGER*4 IHED(32)
      INTEGER*2 ITMH(2)
C
      CHARACTER*4  KFIL
C
      EQUIVALENCE (ITMF,ITMH(1))
C
      DATA NDX/1,0,0,0/
C
      SAVE
C
C     *************************************************************
C     ROUTINE TO READ IN DATA FROM EITHER .SPK OF .HIS FILES
C     *************************************************************
C
      IF(KFIL.EQ.'HIS ') GO TO 100                   !TST FOR .HIS
C
      CALL SPKIO(1,LUS,ID,IHED,64,IDAT,NDX,0,IERR)   !READ FROM .SPK
      CALL SPKERR(IERR)
      IF(IERR.NE.0) RETURN
      NCH=IHED(12)                                   !SET # CHANS
      CALL SPKIO(1,LUS,ID,IHED,64,IDAT,NDX,NCH,IERR) !READ FROM .SPK
      CALL SPKERR(IERR)
      RETURN
C
C     *************************************************************
C     THIS SECTION READS DATA FROM .HIS FILES
C     *************************************************************
C
  100 CALL HISIN(LUD,LUH,ID,NDX,0,IDAT,IERR)         !GET ATTRIBUTES
      CALL HISERR(IERR)
      IF(IERR.NE.0) RETURN
C
      IF(ND.NE.1) GO TO 400                          !TST FOR 1-D
      NCH=LENG(1)                                    !SET # CHANS
      IF(NHW.EQ.1) GO TO 200                         !TST FULL/HALF
      CALL HISIN(LUD,LUH,ID,NDX,NCH,IDAT,IERR)       !READ DATA
      CALL HISERR(IERR)                              !TST FOR ERROR
      RETURN
C
  200 IF=NCH+1                                       !READ 1/2 WD DATA I
      CALL HISIN(LUD,LUH,ID,NDX,NCH,IDAT(IF),IERR)   !UPPER PART
      CALL HISERR(IERR)                              !OF BUFFER
      IF(IERR.NE.0) RETURN
C
      N=0                                            !AND LOAD INTO
      M=NCH                                          !LOWER PART
  210 N=N+1
      M=M+1
      IF(N.GT.NCH) RETURN
      ITMF=IDAT(M)
      IDAT(N)=ITMH(1)
      N=N+1
      IF(N.GT.NCH) RETURN
      IDAT(N)=ITMH(2)
      GO TO 210
C
  400 WRITE(6,405)                                   !SEND ERROR MSG
  405 FORMAT(1H ,'REQUESTED ID NOT 1-D DATA')
      IERR=1
      RETURN
      END
