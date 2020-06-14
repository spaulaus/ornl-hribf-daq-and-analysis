C$PROG DSTAT     - Displays status of fit-parameters
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/12/02
C     ******************************************************************
C
      SUBROUTINE DSTAT
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C   
      COMMON/SM01/ I1,I2,NPK,NCOMP,IFBGD,QFN,QFLO,NVLU,NVLUI,DXFAC
C   
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM04/ KXF(4,44),KFUN(44),BETA(50),IVF(4)
C   
      COMMON/SM19/ ISKIP(2,4),JSKIP(2,4),PMIN(4),PMAX(4),KVAR(4)
C   
C     ------------------------------------------------------------------
      COMMON/SM23/ JYLOG,JDSP,KDSP,MXDL,MXFR,MAXH,
     &             NOP,NBC,KINBAK,MARKS,ECA,ECB,ECC,
     &             DEL,DELFAC,NMUL,XSTEP,DXMAX,FWLO,FWHI,FALO,FAHI,
     &             MAXNC,MAXVP,KFUNS,NWOOD,KPPL,ASUM,ID,NSKIP,RESOK,
     &             ILO,IHI,SETW
      CHARACTER*4  JDSP,KDSP,KINBAK,NWOOD,KPPL,MARKS,SETW,RESOK
C     ------------------------------------------------------------------
C   
      WRITE(CMSSG,10)
   10 FORMAT('      DEL   DELFAC     NDEL    XSTEP    DXMAX',
     &       '      ECA      ECB      ECC')
      CALL MESSLOG(LOGUT,LOGUP)
C   
      WRITE(CMSSG,20)DEL,DELFAC,NMUL,XSTEP,DXMAX,ECA,ECB,ECC
   20 FORMAT(2F9.4,I9,5F9.4)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
C   
      WRITE(CMSSG,30)
   30 FORMAT('      FWA      FWB      FWC     FWLO     FWHI',
     &       '    KBACK      NBC     WOOD')
      CALL MESSLOG(LOGUT,LOGUP)
C   
      WRITE(CMSSG,40)FWA,FWB,FWC,FWLO,FWHI,KINBAK,NBC,NWOOD
   40 FORMAT(5F9.4,5X,A4,I9,5X,A4)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
C   
      WRITE(CMSSG,50)
   50 FORMAT('     ASLO     ASHI     FALO     FAHI       VX',
     &       '       VW     VALO     VAHI')
      CALL MESSLOG(LOGUT,LOGUP)
C   
      WRITE(CMSSG,60)ASLO,ASHI,FALO,FAHI,KVAR
   60 FORMAT(4F9.4,4(5X,A4))
      CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,LOGUP)
C   
      DO 70 I=1,4
      IF(ISKIP(1,I).NE.0) GO TO 80
      IF(ISKIP(2,I).NE.0) GO TO 80
   70 CONTINUE
      GO TO 100
C   
   80 WRITE(CMSSG,90)ISKIP
   90 FORMAT('SKIP-REGIONS=',4('(',2I6,')'))
      CALL MESSLOG(LOGUT,LOGUP)
C
  100 II1=NPK+1
      II2=NCOMP
      WRITE(CMSSG,110)(BETA(II),II=II1,II2)
  110 FORMAT('BGD COEFS =',1P5E12.4)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
