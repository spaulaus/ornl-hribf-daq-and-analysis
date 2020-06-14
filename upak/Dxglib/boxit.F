C$PROG BOXIT     - Erase & re-create window with specified axis, etc
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE BOXIT(IDW,KDSP,WLAB,X1,Y1,X2,Y2)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
#if (PTRSIZE == 64)
      INTEGER*8    DPY,WDID                                 !Alpha STAR8
#else
      INTEGER*4    DPY,WDID                                 !Alpha STAR8
#endif
      INTEGER*4                 XN,    YN,              WN
      CHARACTER*4                             NUWIN
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL,    IBANNER
C     ------------------------------------------------------------------
      INTEGER*4    IDW,WLAB(*)
C
      REAL*4       X1,Y1,X2,Y2
C
      CHARACTER*4  KDSP
C
      INTEGER*4    IWD(20),LUM,IERR,I
      CHARACTER*4  CIWD(20)
      EQUIVALENCE (CIWD, IWD)
C
      DATA CIWD/'FIG ','   1',18*'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ERASE AND RE-CREATE WINDOW-IDW WITH AXIS SPECIFIED BY
C     X1,Y1,X2,Y2, ETC
C     ------------------------------------------------------------------
C
      IF(WINFLG(1,IDW).EQ.0.OR.ISOPEN.NE.'YES ') THEN
      CALL NEWFIG(LUM,IWD,IERR)
      CALL XX_WINMAN('FIGG',0)
                                                 ENDIF
C
      DO 10 I=1,20
      ITITL(I)=WLAB(I)
   10 CONTINUE
C
crlv      CALL XX_WINMAN('ERAS',IDW)
C
      WINFLG(1,IDW)=0
      WINFLC(3,IDW)='1D  '
      WINFLC(4,IDW)=KDSP
C
      WINDAT(5,IDW)=X1
      WINDAT(6,IDW)=Y1
      WINDAT(7,IDW)=X2
      WINDAT(8,IDW)=Y2
C
      CALL XX_WINMAN('WIN ',IDW)
C
      RETURN
      END
