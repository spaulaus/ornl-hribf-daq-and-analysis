C$PROG FADDOPEN  - Opens HIS & DRR files for hisadd feature of damm
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 08/20/2005
C     ******************************************************************
C
      SUBROUTINE FADDOPEN(MODE,IERR)
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/HIS1/ NAMI,NAMO,LUHI,LUDI,LUHO,LUDO,INOP,OUOP
      CHARACTER*160 NAMI,NAMO
      INTEGER*4              LUHI,LUDI,LUHO,LUDO
      CHARACTER*4                                INOP,OUOP
C     ------------------------------------------------------------------
      COMMON/HIS2/ NRDI,NRHI,NRDO,NRHO
      INTEGER*4    NRDI,NRHI,NRDO,NRHO
C
      DATA         NRDI,NRHI,NRDO,NRHO/4*0/
C     ------------------------------------------------------------------
C
      INTEGER*4    FSTUFF(13),NRECD,NRECH
C
      CHARACTER*4  IX,MODE,ANS
C   
      INTEGER*4    NAMH(40),NAMD(40),LD,STAT
C
      INTEGER*4    IERR,LUH,LUD,N,I,JB,RECLVALU,STRLEN
C
      CHARACTER*160 CNAMH,CNAMD
C
      EQUIVALENCE (CNAMH,NAMH),(CNAMD,NAMD)
C
      CHARACTER*4  CKEXT,CIEXT
      INTEGER*4     KEXT, IEXT
      EQUIVALENCE (CKEXT,KEXT),(CIEXT,IEXT)
C
      INTEGER*4    DIRF(32)
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO OPEN HIS- & SPK-FILES
C     ------------------------------------------------------------------
C   
      IERR=0                                 !RESET ERROR FLAG
C
      IF(MODE.EQ.'IN  ') THEN
      CALL FADDCLOSE('IN  ')
      LUH=LUHI
      LUD=LUDI
      ENDIF
C
      IF(MODE.EQ.'OU  ') THEN
      CALL FADDCLOSE('OU  ')
      LUH=LUHO
      LUD=LUDO
      ENDIF
C
      IF(MODE.EQ.'INIT') THEN
      CALL FADDCLOSE('OU  ')
      LUH=LUHO
      LUD=LUDO
      ENDIF

C
      CALL MAKFNAM(IWDRAW,NAMH,IX,IERR)  !Make full-path filename
C   
      IF(IERR.NE.0)    GO TO 900         !TST FOR ERROR
C
      CNAMD=CNAMH
C   
      IF(IX.EQ.'.HIS') GO TO 100         !TST FOR HIS-FILE
      IF(IX.EQ.'.his') GO TO 100         !TST FOR his-file
                       GO TO 900         !OTHERWISE ERROR
C
  100 LD=INDEX(CNAMH,'.')                !LOCATE .EXT
      CKEXT='    '
      CALL LODUP(NAMH,LD,LD+3,KEXT,1)    !GET HIS-FILE EXT
      CIEXT='.DRR'
      IF(CKEXT.EQ.'.his') CIEXT='.drr'
      CALL LODUP(IEXT,1,4,NAMD,LD)       !INSERT .DRR-EXTENSION
      JB=LD+3
      CALL ISBYTE(0,NAMD,JB)             !STUFF IN A NULL
      CALL ISBYTE(0,NAMH,JB)             !STUFF IN A NULL
C
      IF(MODE.EQ.'INIT') GO TO 200
C
      IF(MODE.EQ.'OU  ') THEN
      IF(CNAMH.EQ.NAMI)  GO TO 930
      CALL DINGER(3)
      WRITE(6,105)
  105 FORMAT('YOU HAVE REQUESTED THAT '/)
      WRITE(6,110)CNAMH(1:STRLEN(CNAMH))
  110 FORMAT(A/)
      WRITE(6,115)
  115 FORMAT('BE OPENED FOR OUTPUT'/)
      CALL YESNO(ANS)
      IF(ANS.NE.'YES ') THEN
      WRITE(6,120)
  120 FORMAT(/'OUTPUT FILE NOT OPENED!')
      RETURN
      ENDIF
      ENDIF
C   
C     ==================================================================
C     OPEN THE DRR-FILE
C     ==================================================================
C
      OPEN(UNIT       = LUD,
     &     FILE       = CNAMD,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(128),
     &     IOSTAT     = STAT)
C
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    GO TO 1000
                    ENDIF
C
      CALL FSTAT(LUD,FSTUFF,STAT)
      NRECD=FSTUFF(8)/128
C
C     ==================================================================
C     OPEN THE HIS-FILE
C     ==================================================================
C
      OPEN(UNIT       = LUH,
     &     FILE       = CNAMH,
     &     STATUS     = 'OLD',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(65536),
     &     IOSTAT     = STAT)
C
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    GO TO 1040
                    ENDIF
C
      CALL FSTAT(LUH,FSTUFF,STAT)
      NRECH=FSTUFF(8)/65536
      IF(65536*NRECH.LT.FSTUFF(8)) NRECH=NRECH+1
C
      IF(MODE.EQ.'IN  ') THEN
      INOP='YES '
      NAMI=CNAMH
      NRDI=NRECD
      NRHI=NRECH
      ENDIF
C
      IF(MODE.EQ.'OU  ') THEN
      OUOP='YES '
      NAMO=CNAMH
      NRDO=NRECD
      NRHO=NRECH
      ENDIF
C
      CALL FADDCHECK(IERR)
C
      IF(IERR.EQ.0) RETURN
C
      IF(MODE.EQ.'IN  ') THEN
      CALL FADDCLOSE('IN  ')
      GO TO 910
      ENDIF
C
      IF(MODE.EQ.'OU  ') THEN
      CALL FADDCLOSE('OU  ')
      GO TO 920
      ENDIF
C
      RETURN
C
C     ==================================================================
C     Create & init new his- & drr-files with same attributes as input
C     ==================================================================
C
  200 IF(INOP.NE.'YES ') GO TO 1050
C
      OPEN(UNIT       = LUD,
     &     FILE       = CNAMD,
     &     STATUS     = 'NEW',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(128),
     &     IOSTAT     = STAT)
C
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    GO TO 1060
                    ENDIF
C
      OPEN(UNIT       = LUH,
     &     FILE       = CNAMH,
     &     STATUS     = 'NEW',
     &     ACCESS     = 'DIRECT',
     &     RECL       = RECLVALU(65536),
     &     IOSTAT     = STAT)
C
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    GO TO 1070
                    ENDIF
C
      OUOP='YES '
      NAMO=CNAMH
      NRDO=NRDI
      NRHO=NRHI
C
      DO 220 N=1,NRDI
      READ(LUDI,REC=N)DIRF
      WRITE(LUDO,REC=N)DIRF
  220 CONTINUE
C
      CALL FADDZOT(IERR)
C
      RETURN
C
C     ------------------------------------------------------------------
C     ERROR RETURNS
C     ------------------------------------------------------------------
C
  900 WRITE(CMSSG,905)
  905 FORMAT('Syntax error processing filename - aborted')
      GO TO 2000
C   
  910 WRITE(CMSSG,915)
  915 FORMAT('Processing input filename aborted')
      GO TO 2000
C
  920 WRITE(CMSSG,925)
  925 FORMAT('Processing output filename aborted')
      GO TO 2000
C
  930 WRITE(CMSSG,935)
  935 FORMAT('Output filename same as Input filename - not allowed!')
      GO TO 2000
C
 1000 WRITE(CMSSG,1005)STAT
 1005 FORMAT('Error trying to open DRR-file - zstat =',Z8)
      GO TO 2000
C
 1040 WRITE(CMSSG,1045)
 1045 FORMAT('Error trying to open HIS-file or file not found')
      GO TO 2000
C
 1050 WRITE(CMSSG,1055)
 1055 FORMAT('Input file not open - cannot do init')
      GO TO 2000
C
 1060 WRITE(CMSSG,1065)
 1065 FORMAT('Error trying to open NEW drr-file for init')
      GO TO 2000
C
 1070 WRITE(CMSSG,1075)
 1075 FORMAT('Error trying to open NEW his-file for init')
      GO TO 2000
C
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
