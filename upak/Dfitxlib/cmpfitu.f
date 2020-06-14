C$PROG CMPFITU   - Command processor for Non-Linear Fit operations
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE CMPFITU(IDONE,IERR)
C
      IMPLICIT NONE 
C
      INTEGER*4  MXDAT
C
      PARAMETER (MXDAT=500)
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
      COMMON/FT02/ LIN,LCM,LCI
      INTEGER*4    LIN,LCM,LCI
C     ------------------------------------------------------------------
      COMMON/FT03/ XIN(MXDAT),YIN(MXDAT),UIN(MXDAT),NDAT
      REAL*8       XIN,       YIN,       UIN
      INTEGER*4                                     NDAT
C     ------------------------------------------------------------------
      COMMON/FT04/ ITSP(20),KINT(20),NTERMS
      INTEGER*4    ITSP,    KINT,    NTERMS
C     ------------------------------------------------------------------
      COMMON/FT05/ LWD4(4,20)
      INTEGER*4    LWD4
C     ------------------------------------------------------------------
      COMMON/FT06/ F(64)
      REAL*8       F
C     ------------------------------------------------------------------
      COMMON/FT07/ YCAL(MXDAT),KFOX
C
      REAL*8       YCAL
C
      INTEGER*4                KFOX
C     ------------------------------------------------------------------
      COMMON/FT08/ NAMF(20)
      INTEGER*4    NAMF
C     ------------------------------------------------------------------
      COMMON/FT09/ SKIPF(MXDAT),TCHISQ
      CHARACTER*4  SKIPF
      REAL*8                    TCHISQ
C     ------------------------------------------------------------------
      COMMON/FT12/ TITLA,TITLB
      CHARACTER*80 TITLA,TITLB
C     ------------------------------------------------------------------
      COMMON/FTUSER/ NONEG(64),NV,IFUNK,UTIT
      INTEGER*4      NONEG,    NV,IFUNK
      CHARACTER*40                      UTIT
C     ------------------------------------------------------------------
      CHARACTER*80 CNAMF
      EQUIVALENCE (CNAMF,NAMF)
C     ------------------------------------------------------------------
      REAL*4       UNCERT(512),PCER,SV
C
      REAL*8       ALIM(128),GUESS(64),X(64),DEL,DELFAC,FOFX,CHISQ,PCD
C
      REAL*8       FX(64),FSAV(64)
C
      REAL*8       FOX,USERFOX,TQFN,QFN,DENO,TCHISQSAV,CHISQLO
C
      REAL*8       YCALSAV(MXDAT),SUMA(64),SUMASQ(64),STD(64),XNDO
C
      REAL*8       XLO,XHI,DELX,XNOW
C
      REAL*4       XLIN(MXDAT),YLIN(MXDAT)
C
      INTEGER*4    NLIN
C
      CHARACTER*8  FLAG
C
      INTEGER*4    IV(MXDAT),NSKIP,NN,N
C
      INTEGER*4    IVN,LASFUNK
C
      INTEGER*4    KK,ITPF,LENG,STRLEN
C
      INTEGER*4    IERR,IDN,KIND,NCALL,NLN,II,I
C
      REAL*4       RAN
C
      INTEGER*4    ISEED,NITF,NITE,JV
C
      DATA         ISEED,NITF,NITE/-1,20,10/
C
      CHARACTER*4  ONOF(64)
C
      CHARACTER*4  IDONE,KMD
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      DATA         ITPF/Z'0C202020'/
C
      DATA         ONOF/64*'ON  '/
C
      DATA         FSAV/64*1.0/
C
      DATA         LASFUNK/0/
C
      DATA         NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IDONE='NO  '
C
      IERR=0
C
      IF(IFUNK.EQ.LASFUNK) GO TO 50
C
      DO 10 I=1,NV
      GUESS(I)=1.0
   10 CONTINUE
C
      DO 20 I=1,NV
      ALIM(I)   =-10.0
      ALIM(I+NV)= 10.0
      IF(NONEG(I).NE.0) ALIM(I)=0.010
   20 CONTINUE
      LASFUNK=IFUNK
      NCALL=1
C
   50 CALL GREAD(IWD,LWD4,ITYP,NF,'SETW',16,NTER)
C
      IF(KMD.EQ.'TON ') GO TO 100       !Turn variable ON
      IF(KMD.EQ.'TOF ') GO TO 100       !Turn variable OFF
      IF(KMD.EQ.'NITF') GO TO 130       !No. of fit   iterations
      IF(KMD.EQ.'NITE') GO TO 130       !No. of error iterations
      IF(KMD.EQ.'SKPI') GO TO 200       !Define points to skip in fit
      IF(KMD.EQ.'GO  ') GO TO 300       !Do non-linear fit
      IF(KMD.EQ.'GOMO') GO TO 325       !Continue non-linear search
C
      GO TO 2510
C
C     ------------------------------------------------------------------
C     Turn variables ON/OFF
C     ------------------------------------------------------------------
C
  100 CALL MILV(LWD(1,2),IVN,SV,KIND,IERR)
C
      IF(IERR.NE.0)             GO TO 2010
C
      IF(IVN.LT.1.OR.IVN.GT.NV) GO TO 2020
C
      IF(KMD.EQ.'TON ') GO TO 110
      IF(KMD.EQ.'TOF ') GO TO 120
C
  110 ALIM(   IVN)=-10.0
      ALIM(IVN+NV)= 10.0
      IF(NONEG(IVN).NE.0) ALIM(IVN)=0.010
      ONOF(IVN)='ON  '
      GUESS(IVN)=1.0
      GO TO 2500
C
  120 ALIM(   IVN)=0.0
      ALIM(NV+IVN)=0.0
      ONOF(IVN)='OFF '
      GUESS(IVN)=0.0
      GO TO 2500
C
  130 CALL MILV(LWD(1,2),JV,SV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      IF(JV.LT.1)   GO TO 2030
      IF(JV.GT.100) GO TO 2030
      IF(KMD.EQ.'NITF') NITF=JV
      IF(KMD.EQ.'NITE') NITE=JV
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Define points to be omitted in computing sum-of-squared-residuals
C     ------------------------------------------------------------------
C
  200 IF(NF.LT.2) THEN
      NN=0
      GO TO 225
      ENDIF
C
      NN=0
      DO 220 N=2,NF
      NN=NN+1
      CALL MILV(LWD(1,N),IV(NN),SV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      IF(IV(NN).LT.1.OR.IV(NN).GT.MXDAT) GO TO 2010
  220 CONTINUE
C
  225 DO 230 I=1,MXDAT
      SKIPF(I)='NO  '
  230 CONTINUE
C
      DO 240 I=1,NN
      SKIPF(IV(I))='YES '
  240 CONTINUE
C
      GO TO 2500
C
C     ------------------------------------------------------------------
C     FIT REQUEST - IS NEXT FIELD NUMERIC INDICATING DATA SET #
C     ------------------------------------------------------------------
C
  300 IF(NF.LT.2) GO TO 2000
      CALL MILV(LWD(1,2),IDN,SV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
C
      CALL GETDATX(IDN,IERR)
C
      IF(IERR.EQ.0) GO TO 315
C
      WRITE(6,310)IDN
  310 FORMAT(' FITN REQUESTED DATA SET #',I6,'  NOT FOUND')
      GO TO 2500
C
  315 KK=7
      DEL=0.10
      DELFAC=0.25
C
      DO 320 I=1,NV
      F(I)=1.0
  320 CONTINUE
C
  325 CALL WTCOMP
C
      IF(KMD.EQ.'GOMO') THEN
      DO 330 I=1,NV
      F(I)=FSAV(I)
  330 CONTINUE
      ENDIF
C
C     ------------------------------------------------------------------
C     Do iterations on the non-linear search
C     ------------------------------------------------------------------
C
      CHISQLO=1.0E32
C
      DO 340 II=1,NITF
C
      CALL SMIN(NV,KK,DEL,DELFAC,ALIM,GUESS,X,FOFX)
C
      CHISQ=FOX(X)
C
      DO 335 I=1,NV
      F(I)=F(I)*X(I)
      IF(GUESS(I).GT.0.0) GUESS(I)=1.50-RAN(ISEED)
  335 CONTINUE
C
      IF(CHISQ.GE.CHISQLO) GO TO 340
C
      DO 336 I=1,NV
      FSAV(I)=F(I)
  336 CONTINUE
C
      DO 338 I=1,NDAT
      YCALSAV(I)=YCAL(I)
  338 CONTINUE
C
      CHISQLO=CHISQ
C
      TCHISQSAV=TCHISQ
C
  340 CONTINUE
C
      DO 342 I=1,NV
      GUESS(I)=1.0
  342 CONTINUE
C
C     ------------------------------------------------------------------
C     Now modify YIN as per uncertainties & estimate uncert in A's
C     ------------------------------------------------------------------
C
      DO 350 I=1,NV
      SUMA(I)=0.0
      SUMASQ(I)=0.0
  350 CONTINUE
C
      CALL YINMOD('SAVE')
      XNDO=NITE
      DO 400 II=1,NITE
      CALL YINMOD('MODI')
      CALL SMIN(NV,KK,DEL,DELFAC,ALIM,GUESS,X,FOFX)
      CALL YINMOD('REST')
      DO 360 I=1,NV
      SUMA(I)  =SUMA(I)+F(I)*X(I)
      SUMASQ(I)=SUMASQ(I)+F(I)*F(I)*X(I)*X(I)
  360 CONTINUE
  400 CONTINUE
C
      DO 410 I=1,NV
      STD(I)=DSQRT((XNDO*SUMASQ(I)-SUMA(I)*SUMA(I))/(XNDO*(XNDO-1.0)))
  410 CONTINUE
C
C     ------------------------------------------------------------------
C     Display/log the results
C     ------------------------------------------------------------------
C
      WRITE(CMSSG,545)ITPF
  545 FORMAT(A4)
      CALL TABLOG(0,LOGUP)
C
      WRITE(CMSSG,550)IDN,CNAMF(1:STRLEN(CNAMF))
  550 FORMAT('Non-linear fit of ID =',I3,' - from file - ',A)
      CALL TABLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,580)
      CALL TABLOG(LOGUT,LOGUP)
  580 FORMAT(8('----------'))
C
      WRITE(CMSSG,585)UTIT
  585 FORMAT('User label = ',A)
      CALL TABLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,580)
      CALL TABLOG(LOGUT,LOGUP)
C
      DENO=NDAT-NV
      IF(DENO.LT.1.0) DENO=1.0
      TQFN=TCHISQSAV/DENO
C
      NSKIP=0
      DO 600 I=1,NDAT
      IF(SKIPF(I).NE.'YES ') GO TO 600
      NSKIP=NSKIP+1
  600 CONTINUE
C
      DENO=NDAT-NSKIP-NV
      IF(DENO.LT.1.0) DENO=1.0
C
      QFN=CHISQLO/DENO
C
      WRITE(CMSSG,610)
      CALL TABLOG(LOGUT,LOGUP)
      WRITE(CMSSG,620)
      CALL TABLOG(LOGUT,LOGUP)
      WRITE(CMSSG,630)TCHISQSAV,CHISQLO,TQFN,QFN
      CALL TABLOG(LOGUT,LOGUP)
      WRITE(CMSSG,580)
      CALL TABLOG(LOGUT,LOGUP)
      WRITE(CMSSG,640)
      CALL TABLOG(LOGUT,LOGUP)
C
  610 FORMAT('     Total       Fit     Total       Fit')
  620 FORMAT('     Chisq     Chisq       QFN       QFN')
  630 FORMAT(4F10.2)
  640 FORMAT('Coeff   Coeff-value  Est-err(%)  State')
C
      DO 760 I=1,NV
      PCER=0.0
      IF(FSAV(I).NE.0.0) PCER=100.0*STD(I)/FSAV(I)
      PCER=ABS(PCER)
      WRITE(CMSSG,770)I,FSAV(I),PCER,ONOF(I)
      CALL TABLOG(LOGUT,LOGUP)
  760 CONTINUE
C
  770 FORMAT('A(',I2.2,')  ',1PD12.4,0PF12.2,4X,A4)
C
      WRITE(CMSSG,580)
C
      CALL TABLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,780)
  780 FORMAT('No.           X           Y   GIVEN-ERR    %ERR',
     &       '        YCAL    %DIF')
C
      CALL TABLOG(LOGUT,LOGUP)
C
C
      CALL ERRCOMP(UNCERT)
C
      NLN=0
      DO 800 I=1,NDAT
      NLN=NLN+1
      IF(NLN.GT.5) THEN
      CALL TABLOG(LOGUT,LOGUP)
      NLN=1
      ENDIF
      PCER=100.0*UNCERT(I)/YIN(I)
      PCD=100.0*(YCALSAV(I)-YIN(I))/YIN(I)
      FLAG=' '
      IF(SKIPF(I).EQ.'YES ') FLAG='Not used'
      WRITE(CMSSG,790)I,XIN(I),YIN(I),UIN(I),PCER,YCALSAV(I),PCD,FLAG
  790 FORMAT(I3,3(1PD12.3),0PF8.2,1PD12.3,0PF8.2,1X,A)
      CALL TABLOG(LOGUT,LOGUP)
  800 CONTINUE
C
      WRITE(CMSSG,580)
      CALL TABLOG(LOGUT,LOGUP)
      CALL TABLOG(LOGUT,LOGUP)
C
      CALL GREAD(IWD,LWD4,ITYP,NF,'SETW',8,NTER)
C
      LENG=STRLEN(CNAMF)
C
      IF(LENG.GT.20) LENG=20
C
      WRITE(TITLB,805)IDN,CNAMF(1:LENG),UTIT(1:STRLEN(UTIT))
C
  805 FORMAT('NLfit_ID=',I3,' of ',A,' - ',A)
C
      XLO=1.0E10
      XHI=-1.0E10
      DO 810 I=1,NDAT
      IF(XIN(I).LT.XLO) XLO=XIN(I)
      IF(XIN(I).GT.XHI) XHI=XIN(I)
  810 CONTINUE
      DELX=(XHI-XLO)/100.0
      XNOW=XLO-DELX
      DO 820 I=1,101
      XNOW=XNOW+DELX
      IF(XNOW.EQ.0.0) XNOW=1.0E-20
      XLIN(I)=XNOW
      YLIN(I)=USERFOX(FSAV,XNOW)
  820 CONTINUE
      NLIN=101
C
CX    CALL PLOTFIT(2,XIN,YIN,YCALSAV,NDAT,XLIN,YLIN,NLIN)
C
      GO TO 2500
C
C     ------------------------------------------------------------------
C     LIST DIAGNOSTIC MESSAGES
C     ------------------------------------------------------------------
C
 2000 WRITE(CMSSG,2005)
 2005 FORMAT('Data ID number not entered - no longer supported')
      GO TO 2400
C
 2010 WRITE(CMSSG,2015)
 2015 FORMAT('Syntax error or illegal value - command not processed')
      GO TO 2400
C
 2020 WRITE(CMSSG,2025)IVN
 2025 FORMAT('Variable number out of range = ',I6)
      GO TO 2400
C
 2030 WRITE(CMSSG,2035)
 2035 FORMAT('# of iterations specified out of range 1 - 100 - ignored')
      GO TO 2400
C
 2400 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
C
 2500 IDONE='YES '
 2510 CALL GREAD(IWD,LWD4,ITYP,NF,'SETW',8,NTER)
      RETURN
      END
