C$PROG CMPIOMAN  - Command processor for LEMOR - I/O manager
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE CMPIOMAN(IDONE,IERR)
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
      COMMON/LM01/ LIN,KFI,LINO,LOU(3),KFO(3),LOUO(3),LTX,LML,LMLO,
     &             KMDS,LUCI,LUCO(3)
C
      INTEGER*4    LIN,         LOU,                  LTX,LML,
     &                  LUCI,LUCO
C
      CHARACTER*4      KFI,LINO,       KFO,   LOUO,           LMLO
      CHARACTER*4  KMDS
C     ------------------------------------------------------------------
      COMMON/LM09/ KINPUT,INRECN
      CHARACTER*4  KINPUT
      INTEGER*4           INRECN
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM22/ NBRED,ICNF,INFLG
      INTEGER*4    NBRED
      CHARACTER*4        ICNF,INFLG
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      COMMON/LM26/ LDFONAM,LDFINAM
      CHARACTER*80 LDFONAM,LDFINAM
C     ------------------------------------------------------------------
      INTEGER*4    IERR,KL,NDO,LUC,IV
C
      CHARACTER*4  IDONE,KMD,KMX,KMM,KSTAT
C
      INTEGER*4    KOUTN
C
      character*80  ciwd,ciwdraw,paccmd,tmpstr,filedir
      data  filedir /' '/
      integer*4    pacflag,indx(2,10),itype(10),numf,inext
C
C       Our protocol can lose packets, so keep track of events
C
      real*8      strbufevt,numbufevts,buf_num,lastevt
      real*8      sumevts,beginevt

      COMMON/ORPHAS/ strbufevt,numbufevts,buf_num,lastevt,sumevts,
     &              beginevt
      common/autofile/ nextfile
      integer*4  nextfile 
      character*4 cnextfile
      equivalence (cnextfile, nextfile)
      data cnextfile/'    '/
C
C       Scaler data stuff.
C
      integer*4 scatbuf,nscat,scatdmp,scatclr,scaterr
      COMMON/SCAT0/ scatbuf(8000,2),nscat,scatdmp,scatclr,scaterr
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMX,LWD(1,2)),(CIWD,IWD)
      EQUIVALENCE (CIWDRAW,IWDRAW)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C     
C     ------------------------------------------------------------------
C     Process commands
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'PSAV') GO TO 400
      if(KMD.eq.'SCAT') go to 1000
      IF(KMD.EQ.'TRUN') GO TO 510
      IF(KMD.EQ.'TCON') GO TO 510
      IF(KMD.EQ.'TON ') GO TO 510
      IF(KMD.EQ.'AFON') GO TO 520
      IF(KMD.EQ.'AFOF') GO TO 520
*
      IF(KMD.EQ.'INFI') GO TO 160
      IF(KMD.EQ.'INEV') GO TO 160
C
      KMM=KMD
C
      IF(KMD.EQ.'IN  ') GO TO 165
      IF(KMD.EQ.'INF ') GO TO 175
      IF(KMD.EQ.'OUF ') GO TO 175
      IF(KMD.EQ.'FDIR') GO TO 800
C
      CALL ISBYTE('20'X,KMM,2)
      CALL ISBYTE('20'X,KMM,3)
C
      IF(KMM.EQ.'OU  ') GO TO 170
      IF(NTER.NE.0)     GO TO 4000
C
      IF(KMD.EQ.'CLI ') GO TO 180
      IF(KMD.EQ.'ULI ') GO TO 182
      IF(KMD.EQ.'FRI ') GO TO 200
      IF(KMD.EQ.'BRI ') GO TO 200
      IF(KMD.EQ.'FFI ') GO TO 200
      IF(KMD.EQ.'BFI ') GO TO 200
      IF(KMD.EQ.'RWI ') GO TO 200
      IF(KMD.EQ.'BTI ') GO TO 750
C
      KMM=KMD
      CALL ISBYTE('20'X,KMM,3)
C
      IF(KMM.EQ.'CLO ') GO TO 190
      IF(KMM.EQ.'ULO ') GO TO 192
      IF(KMM.EQ.'FRO ') GO TO 210
      IF(KMM.EQ.'BRO ') GO TO 210
      IF(KMM.EQ.'FFO ') GO TO 210
      IF(KMM.EQ.'BFO ') GO TO 210
      IF(KMM.EQ.'RWO ') GO TO 210
      IF(KMM.EQ.'EOF ') GO TO 210
      IF(KMM.EQ.'BTO ') GO TO 755
C
      IF(KMD.EQ.'DTIT') GO TO 1400
      IF(KMD.EQ.'FIND') GO TO 2900
C
      RETURN
C
C     ------------------------------------------------------------------
C     Open input & output tapes & files
C     ------------------------------------------------------------------
C
  160 KMX='DMOU'                              !OPEN INPUT FILE 
      ICNF='NO  '
      KINPUT='    '
      INTYP ='    '
      INFOP ='    '
      CALL CKCLI(KMX)
      CALL EVELOPEN(LIN,IERR)
      IF(IERR.NE.0) GO TO 6000
      INTYP='EVEL'
      GO TO 5500
C
  165 KINPUT='    '
      INTYP ='    '
      INFOP='    '
      GO TO 171
C
  170 OUTYP='    '
      OUFOP='    '
  171 CALL TAPOPEN(KMD,KMM,IERR)              !OPEN INPUT & OUTPUT TAPES
      IF(IERR.NE.0) GO TO 6000
      IF(KMD.EQ.'IN  ') INTYP='TAPE'
      IF(KMD.EQ.'OU  ') OUTYP='TAPE'
      GO TO 5500
C
  175 call strparse(ciwdraw,indx,itype,numf,inext)
      if (numf .le. 1) then
        if (oufop .ne. 'YES ') go to 5500
        inquire(luouf,name=tmpstr)
        write(cmssg,7000) tmpstr
7000    format('Current LDF file is: ',a)
        call messlog(logut,logup)
        go to 5500
      endif
      if (ciwdraw(indx(1,2):indx(1,2)) .ne. '/') then
        tmpstr = filedir
        if (filedir(1:1) .ne. ' ') call strappend(tmpstr,'/')
      else
        tmpstr = ' '
      endif
      call strappend(tmpstr,ciwdraw(indx(1,2):indx(2,2)))
      do numf=1,80
        if (tmpstr(numf:numf) .eq. '.') then
          if (tmpstr(numf+1:) .eq. 'ldf') go to 176
          write(cmssg,*) "Only 'ldf' extension allowed"
          call messlog(logut,logup)
          go to 6000
        endif
      enddo
      call strappend(tmpstr,'.ldf')
  176 write(cmssg,7001) tmpstr
7001  format('Open LDF file: ',a)
      call messlog(logut,logup)
      ciwdraw(5:) = tmpstr
      CALL LDFOPEN
      nextfile = 0
      GO TO 5500
C
C     ------------------------------------------------------------------
C     DO TAPE CLOSE, UNLOAD ETC
C     ------------------------------------------------------------------
C
  180 IF(INTYP.EQ.'LDF ') THEN
      CLOSE(UNIT=LUINF)
      INTYP='    '
      INFOP='    '
      LDFINAM=' '
      GO TO 5500
      ENDIF
C
      KMX='DMOU'
      GO TO 185
  182 KMX='DMUL'
C
  185 ICNF='NO  '
      KINPUT='    '
      CALL CKCLI(KMX)
      GO TO 5500
C
  190 IF(OUTYP.EQ.'LDF ') THEN
      CLOSE(UNIT=LUOUF)
      OUTYP='    '
      OUFOP='    '
      LDFONAM=' '
      GO TO 5500
      ENDIF
C
      KMX='DMOU'
      GO TO 195
  192 KMX='DMUL'
C
  195 KL=KOUTN(KMD,4,IERR)
      IF(IERR.NE.0) GO TO 3500
C
      CALL CKCLO(KMX,KL)
      GO TO 5500
C
C     ------------------------------------------------------------------
C     DO TAPE CONTROL FUNCTIONS FR, BR, FF, BF, REW, ETC
C     ------------------------------------------------------------------
C
  200 IF(INTYP.EQ.'LDF ') THEN
      CALL LDFMAN(IERR)
      GO TO 5500
      ENDIF
C
      ICNF='NO  '
      INFLG='YES '
      GO TO 215
C
  210 IF(OUTYP.EQ.'LDF ') THEN
      CALL LDFMAN(IERR)
      GO TO 5500
      ENDIF
C
      INFLG='NO  '
  215 NDO=1
      IF(NF.GT.2) GO TO 4000
      IF(NF.NE.2) GO TO 220
      CALL IVALU(LWD(1,2),NDO,IERR)
      IF(IERR.NE.0) GO TO 4000
C
  220 IF(INFLG.NE.'YES '.OR.KINPUT.NE.'FILE') GO TO 225
      CALL FILHAN(KMD,NDO)
      GO TO 5500
C
  225 CALL TAPHANS(KMD,NDO,KSTAT)
      IF(KSTAT.NE.'GOOD') GO TO 6000
      GO TO 5500
C
C   *************************************************************************
C   Save PAC file name for next call to COPY
C   *************************************************************************
C
400   paccmd = ciwdraw
      pacflag = 1
      go to 5500
C
C     *************************************************************
C     COPY  FROM INPUT TO OUTPUT
C     *************************************************************
C
510   if (outyp .eq. 'LDF ') go to 530
      if (pacflag .ne. 0) then
         if (louo(1) .ne. 'YES') then
           cmssg = 'OUTPUT TAPE/FILE NOT ASSIGNED - COMMAND IGNORED'
           call messlog(logut,logup)
           go to 5500
         endif
         ciwd = paccmd
*****         call pacsav(ierr)
         nbred = 0
         if (ierr .ne. 0) go to 6000
         call filmar(luco(1),2,1,ierr)
         if (ierr .ne. 0) go to 6000
         pacflag = 0
         KMD = 'TRUN'
         ciwd(1:80) = 'TRUN'
      endif
      ICNF='NO  '
520   if (outyp .eq. 'LDF ') go to 540
      CALL COPY(KMD,NF,IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
C
530   if (pacflag .ne. 0) then
         ciwd = paccmd
         call pacsavf(ierr)
         nbred = 0
         if (ierr .ne. 0) go to 6000
         call filmar(luouf,2,1,ierr)
         if (ierr .ne. 0) go to 6000
         pacflag = 0
         KMD = 'TRUN'
         ciwd(1:80) = 'TRUN'
      endif
      ICNF='NO  '
540   CALL copyipc(KMD,NF,IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
C
C     ------------------------------------------------------------------
C     Set up to find DBL-EOF and list headers along the way
C     ------------------------------------------------------------------
C
  750 IF(INTYP.EQ.'LDF ') THEN
      CALL LDFMAN(IERR)
      GO TO 5500
      ENDIF
C
      LUC=LUCI
      IV=-1
      IF(LINO.NE.'YES ') GO TO 4020
      CALL HEDLOC(LUC,IV,IERR)
      GO TO 5500
C
  755 IF(OUTYP.EQ.'LDF ') THEN
      CALL LDFMAN(IERR)
      GO TO 5500
      ENDIF
C
      KL=KOUTN(KMD,4,IERR)
      IF(IERR.NE.0) GO TO 3500
      LUC=LUCO(KL)
      IV=-1
      IF(LOUO(KL).NE.'YES ') GO TO 4020
      CALL HEDLOC(LUC,IV,IERR)
      GO TO 5500
*
*     *************************************************************
*      FDIR Display or Set directory for LDF files
*     *************************************************************
*
800   call strparse(ciwdraw,indx,itype,numf,inext)
      if (numf .lt. 2) then
        write(cmssg,801) filedir
801     format('File directory is: ',a)
        call messlog(logut,logup)
        go to 5500
      endif
      filedir = ciwdraw(indx(1,2):indx(2,2))
      if (filedir(1:1) .ne. '/') filedir = ' '
      go to 5500
C
C     *************************************************************
C     SNIT - Scaler readout and recording
C     *************************************************************
C
1000  continue
      call scatcmp(ciwdraw)
      go to 5500
C

C
C     ------------------------------------------------------------------
C     DISPLAY NEXT TITLE FROM INPUT TAPE
C     ------------------------------------------------------------------
C
 1400 IF(INTYP.EQ.'LDF ') THEN
      CALL LDFMAN(IERR)
      GO TO 5500
      ENDIF
C
      IF(LINO.NE.'YES ') GO TO 4020
      CALL HEDLOC(LUCI,0,IERR)
      IF(IERR.NE.0) GO TO 6000
      GO TO 5500
C
C     ------------------------------------------------------------------
C     FIND REQUESTED HEADER NUMBER - LIST "TITLES" ALONG THE WAY
C     ------------------------------------------------------------------
C
 2900 IF(INTYP.EQ.'LDF ') THEN
      CALL LDFMAN(IERR)
      GO TO 5500
      ENDIF
C
      IF(LINO.NE.'YES ') GO TO 4020
      IF(NF.NE.2) GO TO 4000
      CALL IVALU(LWD(1,2),IV,IERR)
      IF(IERR.NE.0) GO TO 4000
      IF(IV.LE.0)   GO TO 4010
      CALL HEDLOC(LUCI,IV,IERR)
      GO TO 5500
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 3500 WRITE(CMSSG,3505)
 3505 FORMAT('Syntax error - command ignored')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 6000
C
 4000 WRITE(CMSSG,4005)
      CALL MESSLOG(LOGUT,LOGUP)
 4005 FORMAT('SYNTAX ERROR - COMMAND IGNORED')
      GO TO 6000
C
 4010 WRITE(CMSSG,4015)
      CALL MESSLOG(LOGUT,LOGUP)
 4015 FORMAT('ILLEGAL VALUE OR SYNTAX ERROR - CMD IGNORED')
      GO TO 6000
C
 4020 WRITE(CMSSG,4025)
 4025 FORMAT('REQUESTED TAPE OR FILE NOT ASSIGNED - CMD IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 6000
C
C     ------------------------------------------------------------------
C     NORMAL RETURN
C     ------------------------------------------------------------------
C
 5500 IDONE='YES '
      IERR=0
      RETURN
C
C     ------------------------------------------------------------------
C     ERROR RETURN
C     ------------------------------------------------------------------
C
 6000 IDONE='YES '
      IERR=1
      RETURN
      END
