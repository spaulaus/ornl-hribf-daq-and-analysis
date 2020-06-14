C$PROG USERCMP   - User command processor (USERCMP) for FITX
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE USERCMP(IWD)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/FTUSER/ NONEG(64),NV,IFUNK,UTIT
      INTEGER*4      NONEG,    NV,IFUNK
      CHARACTER*40                      UTIT
C     ------------------------------------------------------------------
      DATA NONEG/64*0/     !Default - allow +/- for all coefficients
      DATA NV   /2/        !Default - 2 adjustable coefficients
      DATA IFUNK/2/        !Default - function ID=2
      DATA UTIT /'A+B*X'/  !Default - Banner label
C     ------------------------------------------------------------------
      CHARACTER*40   BANLAB(50)
C
      DATA BANLAB(1)/'A*X'/
      DATA BANLAB(2)/'A+B*X'/
      DATA BANLAB(3)/'A+B*X+C*X*X'/
      DATA BANLAB(4)/'A*DSQRT(X)'/
      DATA BANLAB(5)/'A+B*DSQRT(X)'/
      DATA BANLAB(6)/'A+B*DSQRT(X)+C*X'/
      DATA BANLAB(7)/'DSQRT(A+B*X)+C*X'/
      DATA BANLAB(8)/'A+B*X+C*DEXP(D*X)'/
      DATA BANLAB(9)/'A*(B/(C-B))*(EXP(-B*X)-EXP(-C*X))'/
C     ------------------------------------------------------------------
      INTEGER*4      IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      REAL*4         XV
C
      INTEGER*4      IV,KIND,IERR,I
C
      CHARACTER*8    FLAG
C
      CHARACTER*18   POMFLAG
C
      CHARACTER*4    KMD
C
      EQUIVALENCE    (KMD,LWD(1,1))
C     ------------------------------------------------------------------
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(NTER.NE.0) GO TO 1000
C
      IF(KMD.EQ.'FUNK') GO TO 5
      IF(KMD.EQ.'CPOS') GO TO 400
      IF(KMD.EQ.'CPON') GO TO 400
      IF(KMD.EQ.'CPOM') GO TO 400
      IF(KMD.EQ.'STAT') GO TO 500
C
      GO TO 1010
C
C
    5 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)     !Get function-ID
C
      IF(IERR.NE.0) GO TO 1000                !Tst for error
C
      IF(IV.LT.1)   GO TO 1020                !Tst for in range
      IF(IV.GT.9)   GO TO 1020                !tst for in range
C
C     ------------------------------------------------------------------
C     Set up the specified user function
C     ------------------------------------------------------------------
C
      GO TO (10,20,30,40,50,60,70,80,90) IV   !Go set it up
C
C
   10 UTIT=BANLAB(1)                          !Display banner title
      NONEG(1)=0                              !Allow negative coeff-1
      IFUNK=1                                 !Function ID
      NV=1                                    !No. of coefficients
      RETURN
C
   20 UTIT=BANLAB(2)                          !Display banner title
      NONEG(1)=0                              !Allow negative coeff-1
      NONEG(2)=0                              !Allow negative coeff-2
      IFUNK=2                                 !Function ID
      NV=2                                    !No. of coefficients
      RETURN
C
   30 UTIT=BANLAB(3)                          !Display banner title
      NONEG(1)=0                              !Allow negative coeff-1
      NONEG(2)=0                              !Allow negative coeff-2
      NONEG(3)=0                              !Allow negative coeff-3
      IFUNK=3                                 !Function ID
      NV=3                                    !No. of coefficients
      RETURN
C
   40 UTIT=BANLAB(4)                          !Display banner title
      NONEG(1)=0                              !Allow negative coeff-1
      IFUNK=4                                 !Function ID
      NV=1                                    !No. of coefficients
      RETURN
C
   50 UTIT=BANLAB(5)                          !Display banner title
      NONEG(1)=0                              !Allow negative coeff-1
      NONEG(2)=0                              !Allow negative coeff-2
      IFUNK=5                                 !Function ID
      NV=2                                    !No. of coefficients
      RETURN
C
   60 UTIT=BANLAB(6)                          !Display banner title
      NONEG(1)=0                              !Allow negative coeff-1
      NONEG(2)=0                              !Allow negative coeff-2
      NONEG(3)=0                              !Allow negative coeff-3
      IFUNK=6                                 !Function ID
      NV=3                                    !No. of coefficients
      RETURN
C
   70 UTIT=BANLAB(7)                          !Display banner title
      NONEG(1)=1                              !Disallow neg   coeff-1
      NONEG(2)=1                              !Disallow neg   coeff-2
      NONEG(3)=0                              !Allow negative coeff-3
      IFUNK=7                                 !Function ID
      NV=3                                    !No. of coefficiemts
      RETURN
C
   80 UTIT=BANLAB(8)                          !Display banner title
      NONEG(1)=0                              !Allow negative coeff-1
      NONEG(2)=0                              !Allow negative coeff-2
      NONEG(3)=0                              !Allow negative coeff-3
      NONEG(4)=1                              !Disallow neg   coeff-4
      IFUNK=8                                 !Function ID
      NV=4                                    !No. of coefficients
      RETURN
C
   90 UTIT=BANLAB(9)                          !Display banner title
      NONEG(1)=0                              !Allow negative coeff-1
      NONEG(2)=0                              !Allow negative coeff-2
      NONEG(3)=0                              !Allow negative coeff-3
      IFUNK=9                                 !Function ID
      NV=3                                    !No. of coefficients
      RETURN
C
C     ------------------------------------------------------------------
C     Display User Status
C     ------------------------------------------------------------------
C
  400 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
C
      IF(IERR.NE.0) GO TO 1000
C
      IF(IV.LT.1)   GO TO 1030
      IF(IV.GT.NV)  GO TO 1030
C
      IF(KMD.EQ.'CPOS') NONEG(IV)=1
      IF(KMD.EQ.'CPON') NONEG(IV)=0
      IF(KMD.EQ.'CPOM') NONEG(IV)=0
C
      RETURN
C
C     ------------------------------------------------------------------
C     Display User Status
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('FUNK#  Function                                 Status')
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,510)
  510 FORMAT('--------------------------------------------------------')
      CALL MESSLOG(LOGUT,LOGUP)
      DO 530 I=1,9
      FLAG=' '
      IF(I.EQ.IFUNK) FLAG='SELECTED'
      WRITE(CMSSG,520)I,BANLAB(I),FLAG
  520 FORMAT(I5,'  ',A,' ',A)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
  530 CONTINUE
C
      WRITE(CMSSG,510)
      CALL MESSLOG(LOGUT,LOGUP)
      DO 550 I=1,NV
      POMFLAG='Pos or neg allowed'
      IF(NONEG(I).EQ.1) THEN
      POMFLAG='Pos only   allowed'
      ENDIF
      WRITE(CMSSG,535)POMFLAG,I
  535 FORMAT(A,' for coefficient# ',I2)
      CALL MESSLOG(LOGUT,LOGUP)
  550 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Syntax error or illegal value - command ignored')
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('User command not recognized - command ignored')
      GO TO 2000
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('User function ID out of range - command ignored')
      GO TO 2000
C
 1030 WRITE(CMSSG,1035)
 1035 FORMAT('User coefficient ID out of range - command ignored')
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
