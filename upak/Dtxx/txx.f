C$PROG TXX
C   
      COMMON/BBB/  IFMTA,IFMTB,IDATE(3),IFODX,JUSTON
      CHARACTER*16 IFMTA,IFMTB
      CHARACTER*4                             JUSTON
C
      COMMON/CCC/ ICHAP,NCHAP(10)
C
      COMMON/YYY/ KLIS,KAUTOSP,KPAGLAB,KTOFLAG,NPAGSP
      CHARACTER*4 KLIS,KAUTOSP,KPAGLAB,KTOFLAG
C
      CHARACTER*80 CARG(2),NAMFIL,CNAMOU
C
      INTEGER*4    NARG(20,2),NAMOU(20)
C
      EQUIVALENCE (NARG,CARG),(NAMOU,CNAMOU)
C
      DATA LU/1/
C
      character*4 cichap, cnchap(10)
      equivalence (cichap, ichap), (cnchap, nchap)
      DATA cICHAP,cNCHAP/11*'    '/
C
C
      NUMARG=IARGC()
      CARG(1)=' '
      CARG(2)=' '
      IF(NUMARG.GT.2) GO TO 100
      CALL GETARG(1,CARG(1))
      CALL GETARG(2,CARG(2))
C
      IA=1
      IB=NXBL(NARG(1,1),IA,79)-1
      NAMFIL=CARG(1)
      CNAMOU=CARG(1)
      NAMFIL(IB+1:)='.doc'
C
      OPEN(UNIT            = 1,
     &     STATUS          = 'OLD',
     &     FILE            = NAMFIL)
C       
      CALL MILDATE2(IDATE)
      WRITE(IFMTA,10)
      WRITE(IFMTB,10)
   10 FORMAT('(3X,120A1)  ')
C   
      KLIS='    '
      IF(CARG(2).NE.'    ') KLIS='LN03'
C
      KAUTOSP='YES '
      KPAGLAB='YES '
      KTOFLAG='YES '
      NPAGSP=0
C
      CALL STRIPATH(NAMOU,80)
      IB=NXBL(NAMOU,1,80)-1
C
      IF(KLIS.EQ.'LN03') CNAMOU(IB+1:)='.txx'
      IF(KLIS.NE.'LN03') CNAMOU(IB+1:)='.tex'
C
c      OPEN(UNIT            = 7,
c     &     FILE            = CNAMOU,
c     &     STATUS          = 'UNKNOWN')
C
c      CLOSE(UNIT=7, DISP='DELETE')
c      CLOSE(UNIT=7)
C
      IF(KLIS.NE.'LN03')   THEN
C
         OPEN(UNIT            = 7,
     &        FILE            = CNAMOU,
     &        STATUS          = 'REPLACE')
C
      ELSE IF(KLIS.EQ.'LN03')   THEN
C
         OPEN(UNIT            = 7,
     &        FILE            = CNAMOU,
     &        STATUS          = 'REPLACE',
     &        ACCESS          = 'SEQUENTIAL')
C     
      ENDIF
C
      JUSTON='ON  '
      IFODX=2
      CALL COMPOZ(LU)
      CALL EXIT
C
  100 WRITE(6,105)
  105 FORMAT(1H ,'ERROR IN FILE SPECIFICATION')
      CALL EXIT
C
  200 WRITE(6,205)
  205 FORMAT(1H ,'ERROR OPENING DOC-FILE')
      CALL EXIT
      END
