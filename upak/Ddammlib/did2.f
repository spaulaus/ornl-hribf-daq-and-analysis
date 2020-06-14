C$PROG DID2      - Displays ID# and filename for current 2-D display
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/12/02
C     ******************************************************************
C
      SUBROUTINE DID2(IDW)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      COMMON/DML3/ IDL(33,20),KFL(33,20),CNO(33,20),         !/DML3
     &             NCHL(33,20),KOLR(33,20),NNID(20),KDST(20) !/DML3
      REAL*4       CNO                                       !/DML3
      INTEGER*4    IDL,NCHL,KOLR,NNID                        !/DML3
      CHARACTER*4             KFL,                  KDST     !/DML3
C     ------------------------------------------------------------------
C
      CHARACTER*4  KODL(7),KODE
C
      INTEGER*4    LDX(7)
C   
      DATA KODL/'    ','N   ','O   ','P   ','Q   ','R   ','S   '/
C
      DATA  LDX/ 8,     8,     10,    18,    12,    14,    16   /
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      KODE=KFL(1,IDW)
C   
      DO 10 I=1,7
      IF(KODE.EQ.KODL(I)) GO TO 20
   10 CONTINUE
      RETURN
C   
   20 NDX=LDX(I)
C
      WRITE(CMSSG,30)IDL(1,IDW)
   30 FORMAT('ID=',I8)
      CALL MESSLOG(LOGUT,0)
C
      WRITE(CMSSG,40)(NAMFIL(I,NDX),I=1,20)
      CALL MESSLOG(LOGUT,0)
   40 FORMAT(19A4,A1)
      RETURN
      END
