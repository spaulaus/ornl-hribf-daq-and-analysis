C$PROG EXPAN2    - Expands 2-D display
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE EXPAN2(IDW)
C
C     ------------------------------------------------------------------
      COMMON/DML3/ IDL(33,20),KFL(33,20),CNO(33,20),         !/DML3
     &             NCHL(33,20),KOLR(33,20),NNID(20),KDST(20) !/DML3
      REAL*4       CNO                                       !/DML3
      INTEGER*4    IDL,NCHL,KOLR,NNID                        !/DML3
      CHARACTER*4             KFL,                  KDST     !/DML3
C     ------------------------------------------------------------------
      COMMON/PL04/ ILOCX(20),IHICX(20),ILOCY(20),IHICY(20),  !/PL04
     &             KLOCX(20),KHICX(20),KLOCY(20),KHICY(20),  !/PL04
     &             KDDP(20),MINZZ,MAXZZ                      !/PL04
C     ------------------------------------------------------------------
C
      ID=IDL(1,IDW)
      KPLO=KDDP(IDW)
      KLX=KLOCX(IDW)
      KHX=KHICX(IDW)
      KLY=KLOCY(IDW)
      KHY=KHICY(IDW)
      KRX=1
      KRY=1
C
      CALL PLOTUM2(IDW,ID,KPLO,KLX,KHX,KLY,KHY,KRX,KRY)
C
      RETURN
      END
