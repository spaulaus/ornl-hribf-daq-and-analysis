C$PROG LDFSWAP   - Byte-swaps ldf-file & replaces on disk
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 11/14/2002
C     ******************************************************************
C
      SUBROUTINE LDFSWAP(KMD)
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
      COMMON/BBB/ LUS,LUD,LUH,LUF
      INTEGER*4   LUS,LUD,LUH,LUF
C     ------------------------------------------------------------------
      CHARACTER*4 KMD,KIND,KORD
C
      INTEGER*4   IBUF(8192),NREC,NFW,IREC,I,IOS
C
      INTEGER*4   IERR,FSTAT,STATF,FILSIZ,STATARA(13)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'TEST') GO TO 100
      IF(KMD.EQ.'SWAP') GO TO 200
C
      RETURN
C
C     ------------------------------------------------------------------
C     Test byte order for compatibility with current platform
C     ------------------------------------------------------------------
C
  100 IREC=1
C
      READ(LUF,REC=1,IOSTAT=IOS)KIND,NFW,IBUF
C
      IF(IOS.NE.0)       GO TO 500
      IF(KIND.NE.'DIR ') GO TO 510
C
      IF(NFW.EQ.8192) THEN
      WRITE(CMSSG,110)
  110 FORMAT('Byte order is compatible with this platform')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      ENDIF
C
      IF(NFW.NE.8192) THEN
      WRITE(CMSSG,120)
  120 FORMAT('Byte order is NOT compatible with this platform')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      ENDIF
C
C     ------------------------------------------------------------------
C     Execute a byte swap operation (in place)
C     ------------------------------------------------------------------
C
  200 IREC=1
C
      KORD='OK  '
C
      READ(LUF,REC=IREC,IOSTAT=IOS)KIND,NFW,IBUF
C
      IF(IOS.NE.0)       GO TO 500
      IF(KIND.NE.'DIR ') GO TO 510
C
      IF(NFW.NE.8192)    KORD='NOOK'
C
CX    NREC=IBUF(2)
C
CX    IF(KORD.EQ.'NOOK') CALL SWAPPER(2,NREC)
C
      STATF=FSTAT(LUF,STATARA)
      IF(STATF.EQ.0) THEN
      FILSIZ=STATARA(8)
      ELSE
      FILSIZ=(-1)*STATF
      ENDIF
      NREC=FILSIZ/32776
C
      CALL SWAPPER(2,NFW)
C
      DO 210 I=1,8192
      CALL SWAPPER(2,IBUF(I))
  210 CONTINUE
C
      WRITE(LUF,REC=IREC,IOSTAT=IOS)KIND,NFW,IBUF
C
      IF(IOS.NE.0) GO TO 520
C
  300 IREC=IREC+1
C
      IF(IREC.GT.NREC) RETURN
C
      READ(LUF,REC=IREC,IOSTAT=IOS)KIND,NFW,IBUF
C
      IF(IOS.NE.0) GO TO 500
C
      CALL SWAPPER(2,NFW)
C
      IF(KIND.EQ.'DATA') THEN
      DO 310 I=1,8192
      CALL SWAPPER(1,IBUF(I))
  310 CONTINUE
      ENDIF
C
      IF(KIND.EQ.'HEAD') THEN
      DO 320 I=33,64
      CALL SWAPPER(2,IBUF(I))
  320 CONTINUE
      ENDIF
C
      WRITE(LUF,REC=IREC,IOSTAT=IOS)KIND,NFW,IBUF
C
      IF(IOS.NE.0)       GO TO 520
C
      GO TO 300
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)IOS,IREC
  505 FORMAT('Read error - zstat = ',Z8,'  at record# ',I10)
      GO TO 1000
C
  510 WRITE(CMSSG,515)
  515 FORMAT('Directory record not identified - try another file')
      GO TO 1000
C
  520 WRITE(CMSSG,525)IOS,IREC
  525 FORMAT('Write error - zstat = ',Z8,'  at record# ',I10)
      GO TO 1000
C
C
 1000 CALL MESSLOG(LOGUT,LOGUP)
C
      RETURN
      END
