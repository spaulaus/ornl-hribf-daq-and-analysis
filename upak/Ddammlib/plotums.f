C$PROG PLOTUMS   - Display routine for 1-D fits
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE PLOTUMS(IDW,ILO,NCHL,KINDSP)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/SM07/ WINL(4),KSCAL,LASIDW
C
      COMMON/SM25/ DATX(16384),IFI,NCH
C
      COMMON/SM27/ LTITL(20),KSOR,IDD,KDISPOK,JDSPL,NCHD
C
      INTEGER*4    ISPK(16384)
C
      CHARACTER*4  KINDSP,KDISPOK
C
      EQUIVALENCE (XLO ,WINL(1)),(XHI ,WINL(2)),
     &            (YMIN,WINL(3)),(YMAX,WINL(4))
C
      EQUIVALENCE (ISPK,DATX)
C
      SAVE
C     ==================================================================
C
      IHI=ILO+NCHL-1
C
      CALL SPKINS(KSOR,IDD,ISPK,ILO,NCHL,NCHD,IERR)
C
      IF(IERR.NE.0) RETURN
      IF(IHI.GT.NCHD) GO TO 200
C
      CALL PEEKMAN(ISPK,ILO,NCHL)
C   
      DO 20 I=1,NCHL
      DATX(I)=ISPK(I)
   20 CONTINUE
C   
      CALL DATMM(DATX(1),1,NCHL,1.0E7,1.0,YMIN,YMAX)
C   
      CALL DISTY(YMIN,JDSPL,KINDSP,KSCAL)
C   
      XLO=FLOAT(ILO-1)
      XHI=XLO+FLOAT(NCHL)-1.0
C   
C
      IF(KINDSP.EQ.'LOG ') THEN
                           IF(YMIN.LT.1.0)      YMIN=1.0
                           IF(YMAX.LT.YMIN+2.0) YMAX=YMIN+2.0
                           ENDIF
C
      IF(KINDSP.EQ.'LIN ') THEN
                           IF(YMAX.LT.YMIN+2.0) YMAX=YMIN+2.0
                           ENDIF
C
      LASIDW=IDW
C
      CALL BOXIT(IDW,KINDSP,LTITL,XLO,YMIN,XHI,YMAX)
C
      WINFLC(3,IDW)='SAM '
C   
      CALL PLOTY(IDW,'HIST','WHIT',XLO,1.0,DATX,NCHL)
C   
      CALL DMAR(IDW)
      CALL PLOB(IDW,'INIT')
      CALL PLOB(IDW,'PLOT')
      KDISPOK='YES '
      RETURN
C
  200 WRITE(CMSSG,205)
  205 FORMAT('INVALID RANGE OF DATA')
      CALL MESSLOG(LOGUT,LOGUP)
      END
