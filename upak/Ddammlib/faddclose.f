C$PROG FADDCLOSE - Closes his-files for hisadd feature of damm
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 08/20/2005
C     ******************************************************************
C
      SUBROUTINE FADDCLOSE(MODE)
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
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/HIS1/ NAMI,NAMO,LUHI,LUDI,LUHO,LUDO,INOP,OUOP
      CHARACTER*160 NAMI,NAMO
      INTEGER*4              LUHI,LUDI,LUHO,LUDO
      CHARACTER*4                                INOP,OUOP
C     ------------------------------------------------------------------
      COMMON/HIS2/ NRDI,NRHI,NRDO,NRHO
      INTEGER*4    NRDI,NRHI,NRDO,NRHO
C     ------------------------------------------------------------------
C
      CHARACTER*4  MODE
C   
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(MODE.EQ.'IN  ') THEN
      CLOSE(UNIT=LUHI)                       !Close input his-file
      CLOSE(UNIT=LUDI)                       !Close input drr-file
      INOP='NO  '                            !Set closed flag
      NAMI=' '                               !Clear filename
      NRDI=0                                 !Reset # drr-records
      NRHI=0                                 !Reset # his-records
      RETURN
      ENDIF
C
      IF(MODE.EQ.'OU  ') THEN
      CLOSE(UNIT=LUHO)                       !Close output his-file
      CLOSE(UNIT=LUDO)                       !Close output drr-file
      OUOP='NO  '                            !Set closed flag
      NAMO=' '                               !Clear filename
      NRDO=0                                 !Reset # drr-records
      NRHO=0                                 !Reset # his-records
      RETURN
      ENDIF
C
      RETURN
C
      END
