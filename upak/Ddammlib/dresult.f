C$PROG DRESULT   - Displays fit results
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE DRESULT
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SM01/ I1,I2,NPK,NCOMP,IFBGD,QFN,QFLO,NVLU,NVLUI,DXFAC
C     ------------------------------------------------------------------
      COMMON/SM12/ LTITL(20)
      CHARACTER*4  LTITL
C     ------------------------------------------------------------------
      COMMON/SM17/ IORD(500),ITEM(500),LOUXX(14,50)
C     ------------------------------------------------------------------
      COMMON/SM23/ JYLOG,JDSP,KDSP,MXDL,MXFR,MAXH,
     &             NOP,NBC,KINBAK,MARKS,ECA,ECB,ECC,
     &             DEL,DELFAC,NMUL,XSTEP,DXMAX,FWLO,FWHI,FALO,FAHI,
     &             MAXNC,MAXVP,KFUNS,NWOOD,KPPL,ASUM,ID,NSKIP,RESOK,
     &             ILO,IHI,SETW
      CHARACTER*4  JDSP,KDSP,KINBAK,NWOOD,KPPL,MARKS,SETW,RESOK
C     ------------------------------------------------------------------
C   
      IF(RESOK.NE.'YES ') GO TO 600
C
      WRITE(LOGUT,5)
    5 FORMAT(1H )
C   
      WRITE(CMSSG,10)LTITL
   10 FORMAT(20A4)
      CALL MESSLOG(LOGUT,0)
C   
      WRITE(CMSSG,562)
  562 FORMAT('   #       X      AREA    %ERR    FWHM',
     &       '     ASL     ASH')
      CALL MESSLOG(LOGUT,0)
C   
      DO 578 KK=1,NPK
      DO 572 I=1,14
      MSSG(I)=LOUXX(I,KK)
  572 CONTINUE
  576 CALL MESSLOG(LOGUT,0)
  578 CONTINUE
      RETURN
C   
  600 WRITE(CMSSG,610)
  610 FORMAT('NO RESULTS AVAILABLE')
      CALL MESSLOG(LOGUT,0)
      RETURN
      END
