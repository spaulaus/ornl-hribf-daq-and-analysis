C$PROG MODCOD
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ******************************************************************
C     Modified by JWK to add AD413 codes  09 and 29  06/28/00
C
      SUBROUTINE MODCOD(IWD,ICODE,IERR)
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      COMMON/PACO/ KODLO(4),KODHI(4)
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 IWD(3),NAME(3,37),CODE(37)
C
      CHARACTER*12 CNAME(37)
C
      EQUIVALENCE (CNAME,NAME)
C
      DATA (CNAME(I),CODE(I),I=1,37)/
     &'LRS_4300    ',1,              !FERRA   - LECROY   ADC
     &'GAN_812F    ',2,              !FERRA   - GANELEC  TDC
     &'SILENA_4418 ',3,              !FERRA   - SILENA   ADC
     &'LRS_3351    ',3,              !FERRA   - like SILENA-4418
     &'LRS_3371    ',3,              !FERRA   - like SILENA-4418
     &'MCSQ_FER    ',4,              !FERRA   - test-only---
     &'LRS_3377    ',5,              !FERRA
     &'BAKLASH     ',6,              !FERRA   - Clover   ADC
     &'BAKLASH2    ',7,              !FERRA   - Clover   ADC
     &'BAKLASH3    ',8,              !FERRA   - Clover   ADC
     &'AD_413      ',9,              !FERA    - ORTEC    ADC, JWK Added
     &'LRS_1885    ',11,             !FASTBUS - LECROY   ADC
     &'PHIL_10C6   ',12,             !FASTBUS - PHILLIPS TDC
     &'LRS_1872    ',13,             !FASTBUS - LECROY   TDC
     &'LRS_1875    ',13,             !FASTBUS - LECROY   TDC
     &'LRS_1881    ',14,             !FASTBUS - LECROY   ADC
     &'LRS_1877    ',15,             !FASTBUS - LECROY   ADC
     &'MCSQ_FAS    ',16,             !FASTBUS - test-only---
     &'PHIL_7164   ',21,             !CAMAC   - PHILLIPS ADC
     &'PHIL_7166   ',21,             !CAMAC   - PHILLIPS ADC
     &'PHIL_7167   ',21,             !CAMAC   - PHILLIPS ADC
     &'PHIL_7186   ',22,             !CAMAC   - PHILLIPS TDC
     &'PHIL_7187   ',22,             !CAMAC   - PHILLIPS TDC
     &'LRS_2277    ',23,             !CAMAC   - LECROY   TDC
     &'SILENA_4418C',24,             !CAMAC   - SILENA   ADC
     &'LRS_3351C   ',24,             !CAMAC   - like SILENA-4418C
     &'LRS_3371C   ',24,             !CAMAC   - like SILENA-4418C
     &'MCSQ_CAM    ',25,             !CAMAC   - test-only---
     &'LRS_4300C   ',26,             !CAMAC   - LECROY   ADC
     &'LRS_3377C   ',27,             !CAMAC   - LECROY   ADC
     &'XIA_TIME    ',28,             !CAMAC   - XIA TIME
     &'AD_413C     ',29,             !CAMAC   - ORTEC    ADC, JWK added
     &'CAEN-775    ',41,             !VME     - CAEN 32-CHANNEL TDC
     &'CAEN-785    ',42,             !VME     - CAEN 32-CHANNEL ADC
     &'CAEN-792    ',43,             !VME     - CAEN 32-CHANNEL QDC
     &'SIS_3820    ',44,             !VME     - SIS 3820 scaler
     &'MYRIAD      ',45/             !VME     - Myriad module
C
      DATA NNAME/37/
C
      DATA KODLO/1,  11,  21,   41/
      DATA KODHI/9,  16,  29,   45/
C
      SAVE
C
C     ------------------------------------------------------------------
C     TESTS IWD AGAINST MODULE-TYPE TABLE & RETURNS CODE OR ERROR
C     ------------------------------------------------------------------
C
      DO 20 J=1,NNAME
      DO 10 I=1,3
      IF(IWD(I).NE.NAME(I,J)) GO TO 20
   10 CONTINUE
      IERR=0
      ICODE=CODE(J)
      RETURN
C
   20 CONTINUE
C
      WRITE(CMSSG,30)IWD
   30 FORMAT('ILLEGAL MODULE NAME = ',3A4)
      CALL ERRLOG(LOGUT,LOGUP)
      ICODE=0
      IERR=1
      RETURN
      END
