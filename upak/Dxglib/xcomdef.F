C$PROG XCOMDEF   - Definition of XGLIB common blocks
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE XCOMDEF
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
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
#if (PTRSIZE == 64)
      INTEGER*8    DPY,WDID                                 !Alpha STAR8
#else
      INTEGER*4    DPY,WDID                                 !Alpha STAR8
#endif
      INTEGER*4                 XN,    YN,              WN
      CHARACTER*4                             NUWIN
C     ------------------------------------------------------------------
      COMMON/XLBB/ AA(2,20),BB(2,20),PLOTYP(20)
      REAL*4       AA,      BB
      CHARACTER*4                    PLOTYP
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLDD/ FIGDAT(4,20)
      INTEGER*4    FIGDAT
C     ------------------------------------------------------------------
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
      INTEGER*4    MASKEV,MODE_OR,MODE_PL
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
#if (PTRSIZE == 64)
      INTEGER*8    GCOR,   GCON                             !Alpha STAR8
#else
      INTEGER*4    GCOR,   GCON                             !Alpha STAR8
#endif
C     ------------------------------------------------------------------
      COMMON/XLGG/ IX(512),JY(512)
      INTEGER*4    IX,     JY
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL,    IBANNER
C     ------------------------------------------------------------------
      COMMON/XLII/ LASWIN,LWIN,WNDX,IVX,IVY
      INTEGER*4    LASWIN,LWIN,WNDX,IVX,IVY
C     ------------------------------------------------------------------
      COMMON/XLJJ/ IRED(40),IGRE(40),IBLU(40),KOLRSET
      INTEGER*4    IRED,    IGRE,    IBLU
      CHARACTER*4                             KOLRSET
C     ------------------------------------------------------------------
      COMMON/XLLL/ NUWFLG1(20),NUWFLG2(20),NUWFLG3(20)
      CHARACTER*4  NUWFLG1,    NUWFLG2,    NUWFLG3
C     ------------------------------------------------------------------
      COMMON/LAXX/ LA7(20,10),LA8(20,10),LA9(20,10),NL7,NL8,NL9,KOL
      INTEGER*4                                     NL7,NL8,NL9
      CHARACTER*4  LA7,       LA8,       LA9,                   KOL
C     ------------------------------------------------------------------
      COMMON/DML3/ IDL(33,20),KFL(33,20),CNO(33,20),         !/DML3
     &             NCHL(33,20),KOLR(33,20),NNID(20),KDST(20) !/DML3
      REAL*4       CNO
      INTEGER*4    IDL,KFL,NCHL,KOLR,NNID
      CHARACTER*4                                   KDST     !/DML3
C     ------------------------------------------------------------------
      COMMON/PL04/ ILOCX(20),IHICX(20),ILOCY(20),IHICY(20),  !/PL04
     &             KLOCX(20),KHICX(20),KLOCY(20),KHICY(20),  !/PL04
     &             KDDP(20), MINZZ,    MAXZZ                 !/PL04
C
      INTEGER*4    ILOCX,    IHICX,    ILOCY,    IHICY
      INTEGER*4    KLOCX,    KHICX,    KLOCY,    KHICY
      INTEGER*4    KDDP,     MINZZ,    MAXZZ
C     ------------------------------------------------------------------
      COMMON/PL16/ LENHIS(4,20)
      INTEGER*4    LENHIS
C     ------------------------------------------------------------------
C
C     COMMON/XLAA/ ***********************************************
C
C     DPY      =  DISPLAY ID      (DEFINED VIA OPEN_DISPLAY)
C     WDID(I)  =  ITH WINDOW  -ID (DEFINED VIA CREATE_WINDOW)
C
C     XN(I)    =  X-COOR OF POINTER FOR ITH DISPLAY (PIXELS)
C     YN(I)    =  Y-COOR OF POINTER FOR ITH DISPLAY (PIXELS)
C
C     NUWIN(I) =  'YES '/'NO  ' FOR NEW-DISPLAY/NO-NEW-DISPLAY
C     WN       =  CURRENT WINDOW NUMBER (WHERE POINTER IS)
C 
C     COMMON/XLBB/ ***********************************************
C
C     AA & BB  -  ARE DATA-TO-PIXEL CONVERSION CONSTANTS, SUCH THAT:  
C              -      XPIX(I)=AA(1,I)+BB(1,I)*XDAT(I)  
C              -  AND YPIX(I)=AA(2,I)+BB(2,I)*YDAT(I)      FOR LIN 
C              -  OR  YPIX(I)=AA(2,I)+BB(2,I)*LOG(YDAT(I)) FOR LOG
C     PLOTYP(I)=  'LIN '/'LOG ' FOR ITH DISPLAY LIN/LOG
C
C     COMMON/XLCC/ ***********************************************
C
C     WINDAT(1,I) = X-COOR OF LL CORNER OF ITH WINDOW (PIXELS)
C     WINDAT(2,I) = Y-COOR OF LL CORNER OF ITH WINDOW (PIXELS)
C     WINDAT(3,I) = WIDTH               OF ITH WINDOW (PIXELS)
C     WINDAT(4,I) = HEIGHT              OF ITH WINDOW (PIXELS)
C     WINDAT(5,I) = X-COOR OF LL CORNER OF ITH WINDOW (USER UNITS)
C     WINDAT(6,I) = Y-COOR OF LL CORNER OF ITH WINDOW (USER UNITS)
C     WINDAT(7,I) = X-COOR OF UR CORNER OF ITH WINDOW (USER UNITS)
C     WINDAT(8,I) = Y-COOR OF UR CORNER OF ITH WINDOW (USER UNITS)
C
C     WINFLG(1,I) = 1/0           SAYS          ITH WINDOW OPEN/NOT
C     WINFLG(2,I) = 'ON  '/'OFF ' SAYS AXIS FOR ITH WINDOW  ON/OFF
C     WINFLG(3,I) = '1D  '/'2D  ' SAYS DISP  IN ITH WINDOW  1D/2D
C     WINFLG(4,I) = 'LIN '/'LOG ' SAYS DISP  IN ITH WINDOW LIN/LOG
C     NUMWIN      = NUMBER OF WINDOWS DEFINED
C     ISOPEN      = 'YES '/'NO  ' SAYS DISPLAY IS OPEN/NOT-OPEN
C
C     COMMON/XLDD/ ***********************************************
C
C     FIGDAT(1,J) = X-COOR(PIX) OF UL-CORNER OF JTH-WINDOW
C     FIGDAT(2,J) = Y-COOR(PIX) OF UL-CORNER OF JTH-WINDOW
C     FIGDAT(3,J) = WIDTH(PIX)               OF JTH-WINDOW
C     FIGDAT(4,J) = HEIGHT(PIX)              OF JTH-WINDOW
C
C     COMMON/XLEE/ ***********************************************
C     
C     MASKEV  = EVENT-MASK  SET BY XX_WINMAN & USED BY XX_EVENTMAN
C     MODE_OR = ORIGIN-MODE SET BY XX_WINMAN & USED BY PLOTPIX
C     MODE_PL = PLOT-MODE (LIN/LOG) SET BY WINDX ONLY
C
C     COMMON/XLFF/ ***********************************************
C
C     GCOR(I)     =(I=1, 7) GRAPHIC CONTEXT-IDS FOR EXCLUSIVE-OR 
C     GCON(I)     =(I=1,35) GRAPHIC CONTEXT-IDS FOR VARIOUS COLORS
C
C     COMMON/XLGG/ ***********************************************
C
C     IX(I)       = XPIX-ARRAY (TEMP USE) BY PLOTY & PLOTXY
C     JY(I)       = YPIX-ARRAY (TEMP USE) BY PLOTY & PLOTXY
C
C     COMMON/XLHH/ ***********************************************
C
C     ITITL       = CONTAINS BANNER TITLE OF WINDOW TO BE CREATED
C     IBANNER     = BANNER FLAG (NOT USED FOR NOW)
C
C     COMMON/XLII/ ***********************************************
C
C     LASWIN      = WINDOW-ID    FOR LAST    CURSOR-MOTION EVENT 
C     LWIN        = WINDOW-ID    FOR CURRENT CURSOR-MOTION EVENT
C     WNDX        = WINDOW-INDEX FOR CURRENT CURSOR-MOTION EVENT 
C     IVX         = XPIX-CURSOR-COORD FOR CURRENT EVENT
C     IVY         = YPIX-CURSOR-COORD FOR CURRENT EVENT
C
C     COMMON/XLJJ/ ***********************************************
C
C     IRED        = ARRAY OF RED  -VALUES FOR GRAPHICS CONTEXT USE
C     IGRE        = ARRAY OF GREEN-VALUES FOR GRAPHICS CONTEXT USE
C     IBLU        = ARRAY OF BLUE -VALUES FOR GRAPHIVS CONTEXT USE
C
C     KOLRSET     = 'YES '/'NO  ' SAYS IRED,IGRE,IBLU SET/NOT-SET
C
C     ************************************************************
      RETURN
      END
