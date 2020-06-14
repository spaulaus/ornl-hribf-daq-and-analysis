C$PROG HISSWAP   - Byte-swaps his-file & replaces on disk
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE HISSWAP(KMD)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/AAA/ ILO(8500),IHI(8500),NHW(8500),NHIS,LENT
C     ------------------------------------------------------------------
      COMMON/BBB/ LUS,LUD,LUH,LUF
C     ------------------------------------------------------------------
      COMMON/CCC/ IBUF(4096)
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD,IOSC
C
      INTEGER*4    STAT,IOS
C
      EQUIVALENCE (IOSC,IOS)
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      CALL DRRSWAP(KMD)
C   
      NDO=(LENT+8191)/8192
C
      JHI=0
      IREC=0
C   
      DO 20 I=1,NDO
C
      JLO=JHI+1
      JHI=JLO+8191
      IREC=IREC+1
C
      CALL DREAD(LUH,IBUF,IREC,16384,IOS)
C
      IF(IOSC.EQ.'EOF ') GO TO 15
C
      IF(IOS.NE.0) THEN
                   WRITE(CMSSG,5)
                   CALL MESSLOG(LOGUT,LOGUP)
                   RETURN
                   ENDIF
C
    5 FORMAT('Error reading his-file')
C   
   10 CONTINUE
C
   15 CALL SWAPUM(JLO,JHI)
C   
      CALL DRITE(LUH,IBUF,IREC,16384,STAT)
      IF(STAT.NE.0) RETURN
C
   20 CONTINUE
C   
      RETURN
      END
