/*
C$PROG XGLIBC    - For Linux - doesn't destroy/recreate windows
*/
/*****************************************************************************
*
*                            HHIRF COMPUTER GROUP
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*                                Operated By
*                    Martin Marietta Energy Systems, Inc.
*
*                          Copyright(C) 1990, 1991
*
*
*
*                         IMPORTANT NOTICE TO USER
*
*     All statements, technical information and recommendations contained
*     herein are based on tests we believe to be reliable, but the accuracy
*     or completeness thereof is not guaranteed, and the following  is
*     made in lieu of all warranties expressed or implied:
*
*     Our only obligation shall be to correct or replace, at our convenience,
*     any part of the product proven to be defective.  We shall not be liable
*     for any loss or damage, direct or consequential, arrising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  RISC DECStations, RISC FORTRAN, RISC C, XWindow
*
*    File:         /usr/users/mcsq/win/xglibc.c
*
*    Description:
*                  These routines are the interface between WT Milner's display
*                  and analysis codes and the XWindow Library.  On VAXStations
*                  under VMS, Milner implemented these in FORTRAN.  However,
*                  for RISC DECStations it is necessary to interface to the
*                  XWindow Library with C calling conventions.
*
*                  This is a direct conversion from FORTRAN to C.  The routines
*                  herein are called from FORTRAN.  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    12/27/90    MCSQ         Converted WT Milner's FORTRAN version to C
*                             for use on a DECStation 5000.
*
*    12/29/90    MCSQ         Added routine zz_flush_.  This routine forces
*                             output on Logical Units 0 or 6.  It appears that
*                             FORTRAN buffers output until a newline occurs or
*                             a FORTRAN call for input occurs.  The subroutine
*                             flush(3f) reads as if it should do this. However,
*                             my tests using it don't work!
*
*     1/1/91     MCSQ         1) xx_winman -  Windows created with BackingStore
*                                attribute.  Wait for ExposeEvent before
*                                drawing axis.
*                             2) xx_eventman - On entry, flush the event queue
*                                using XSync.  InputFocus follows the window
*                                the pointer is in.
*
*     1/7/93     RLV 
*     xx_winman - remove unnecessary XSync, used only after redrawing axes.
*     Changed event handling to use VisibilityNotify events, and
*     combined the 3 loops of the mapping into one to better track
*     events and clarify the logic.  The efficiency loss is minimal.
*     All this work was to make one version to work on Sun and
*     DECstation
*
*     5/21/95    MCSQ         The FIG command sometimes puts windows in the
*                             wrong position on the screen.  This occurs most
*                             often when an Xterminal is used with a DECstation
*                             or a DECstation is the display for an ALPHA.
*                             Added a wait for a CreateNotify event in the
*                             loop which creates windows.  This added code
*                             is conditionally included only for DECstations
*                             and ALPHAs.
*
*     9/ 6/95    MCSQ         Redo flow in xx_eventman so that an
*                             XQueryPointer call is always done after the
*                             last motion event is removed from the queue.
*                             Makes cursor mode work better on Xterminals.
*****************************************************************************/

#include <stdlib.h>
#include <X11/Xlib.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

/*
*     ************************************************************
*     BY WT MILNER AT HHIRF - LAST MODIFIED 03/31/90
*     ************************************************************
*/
/*
*     common/xlaa/ ***********************************************
*
*     dpy        =  DISPLAY ID      (DEFINED VIA OPEN_DISPLAY)
*     wdid[i-1]  =  ITH WINDOW  -ID (DEFINED VIA CREATE_WINDOW)
*
*     xn[i-1]    =  X-COOR OF POINTER FOR ITH DISPLAY (PIXELS)
*     yn[i-1]    =  Y-COOR OF POINTER FOR ITH DISPLAY (PIXELS)
*
*     nuwin[i-1] =  'YES '/'NO  ' FOR NEW-DISPLAY/NO-NEW-DISPLAY
*     wn         =  CURRENT WINDOW NUMBER (WHERE POINTER IS)
* 
*      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
*/
struct
     {
       Display  *dpy;
       Window   wdid[20];
       int      xn[20];
       int      yn[20];
       char     nuwin[20][4];
       int      wn;
     } xlaa_ ;

/*
*     common/xlcc/ ***********************************************
*
*     windat[i-1][0] = X-COOR OF LL CORNER OF ITH WINDOW (PIXELS)
*     windat[i-1][1] = Y-COOR OF LL CORNER OF ITH WINDOW (PIXELS)
*     windat[i-1][2] = WIDTH               OF ITH WINDOW (PIXELS)
*     windat[i-1][3] = HEIGHT              OF ITH WINDOW (PIXELS)
*     windat[i-1][4] = X-COOR OF LL CORNER OF ITH WINDOW (USER UNITS)
*     windat[i-1][5] = Y-COOR OF LL CORNER OF ITH WINDOW (USER UNITS)
*     windat[i-1][6] = X-COOR OF UR CORNER OF ITH WINDOW (USER UNITS)
*     windat[i-1][7] = Y-COOR OF UR CORNER OF ITH WINDOW (USER UNITS)
*
*     wflg[i-1].winflg1 = 1/0           SAYS          ITH WINDOW OPEN/NOT
*     wflg[i-1].winflg2 = 'ON  '/'OFF ' SAYS AXIS FOR ITH WINDOW  ON/OFF
*     wflg[i-1].winflg3 = '1D  '/'2D  ' SAYS DISP  IN ITH WINDOW  1D/2D
*     wflg[i-1].winflg4 = 'LIN '/'LOG ' SAYS DISP  IN ITH WINDOW LIN/LOG
*     numwin      = NUMBER OF WINDOWS DEFINED
*     isopen      = 'YES '/'NO  ' SAYS DISPLAY IS OPEN/NOT-OPEN
*
*      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
*/

struct 
     {
       float        windat[20][10];
       struct 
            {
             int      winflg1;
             char     winflg2[4];
             char     winflg3[4];
             char     winflg4[4];
             int      winflg5;
             int      winflg6;
            } wflg[20];
       int          numwin;
       char         isopen[4];
     } xlcc_ ;

/*
*     common/xldd/ ***********************************************
*
*     figdat[j-1][0] = X-COOR(PIX) OF UL-CORNER OF JTH-WINDOW
*     figdat[j-1][1] = Y-COOR(PIX) OF UL-CORNER OF JTH-WINDOW
*     figdat[j-1][2] = WIDTH(PIX)               OF JTH-WINDOW
*     figdat[j-1][3] = HEIGHT(PIX)              OF JTH-WINDOW
*
*      COMMON/XLDD/ FIGDAT(4,20)
*/
struct
     {
       int      figdat[20][4];
     } xldd_ ;

/*
*     common/xlee/ ***********************************************
*     
*     maskev  = EVENT-MASK  SET BY XX_WINMAN & USED BY XX_EVENTMAN
*     mode_or = ORIGIN-MODE SET BY XX_WINMAN & USED BY PLOTPIX
*     mode_pl = PLOT-MODE (LIN/LOG) SET BY WINDX ONLY
*
*      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
*/
struct
     {
       int      maskev;
       int      mode_or;
       char     mode_pl[4];
     } xlee_ ;

/*
*     common/xlff/ ***********************************************
*
*     gcor[i]     =(i=0, 6) GRAPHIC CONTEXT-IDS FOR EXCLUSIVE-OR 
*     gcon[i]     =(i=0,34) GRAPHIC CONTEXT-IDS FOR VARIOUS COLORS
*
*      COMMON/XLFF/ GCOR(7),GCON(35)
*/
struct
     {
       GC       gcor[7];
       GC       gcon[35];
     } xlff_ ;

/*
*     common/xlhh/ ***********************************************
*
*     winame      = CONTAINS BANNER TITLE OF WINDOW TO BE CREATED
*     ibanner     = BANNER FLAG (NOT USED FOR NOW)
*
*      COMMON/XLHH/ ITITL(20),IBANNER
*/
struct
     {
       char     winame[80];
       int      ibanner;
     } xlhh_ ;

/*
*     common/xlii/ ***********************************************
*
*     laswin      = WINDOW-ID    FOR LAST    CURSOR-MOTION EVENT 
*     lwin        = WINDOW-ID    FOR CURRENT CURSOR-MOTION EVENT
*     wndx        = WINDOW-INDEX FOR CURRENT CURSOR-MOTION EVENT 
*     ivx         = XPIX-CURSOR-COORD FOR CURRENT EVENT
*     ivy         = YPIX-CURSOR-COORD FOR CURRENT EVENT
*
*      COMMON/XLII/ LASWIN,LWIN,WNDX,IVX,IVY
*/
struct
     {
       int      laswin;
       int      lwin;
       int      wndx;
       int      ivx;
       int      ivy;
     } xlii_ ;

/*
*     common/xljj/ ***********************************************
*
*     ired        = ARRAY OF RED  -VALUES FOR GRAPHICS CONTEXT USE
*     igre        = ARRAY OF GREEN-VALUES FOR GRAPHICS CONTEXT USE
*     iblu        = ARRAY OF BLUE -VALUES FOR GRAPHIVS CONTEXT USE
*
*     kolrset     = 'YES '/'NO  ' SAYS IRED,IGRE,IBLU SET/NOT-SET
*
*      COMMON/XLJJ/ IRED(40),IGRE(40),IBLU(40),KOLRSET
*/
struct
     {
       int      ired[40];
       int      igre[40];
       int      iblu[40];
       char     kolrset[4];
     } xljj_ ;

/*      COMMON/XLKK/ KURTY,MBKEY(3,5)                         */
struct
     {
       char    kurty[4];
       int     mbkey[5][3];
     } xlkk_ ;

/*      COMMON/XLLL/ nuwflg1(20),nuwflg2(20),nuwflg3(20)         */
struct
     {
       char     nuwflg1[20][4];
       char     nuwflg2[20][4];
       char     nuwflg3[20][4];
     } xlll_ ;

/*      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF */
struct
     {
       char    mssg[112];
       int     namprog[2];
       int     logut;
       int     logup;
       int     lisflg;
       int     msgf;
     } lll_ ;

/*  Function Prototypes for WT Milner routines */

void nucur_(void);
void messlog_(int *,int *);
void doaxis_(int *,int *,float *,float *,float *,float *,float *,float *);
void cussman_(int *,int *,int *,int *,int *);
void colrset_(int *,int *,int *);
int  xx_defcol__(Display **, Screen **, Visual **, int *);
int  xx_keynam__(XKeyEvent **);

/*
C$PROG XX_WINMAN 
*/
/*
*     ************************************************************
*     BY WT MILNER AT HHIRF - LAST MODIFIED 03/31/90
*     ************************************************************
*/
void xx_winman__(int *kmdptr,int *idxptr)
{
 int          i,idx,j,err=0;
 char        *cptr,*kmd;
 static union
          {
            int   mode;
            char  modepl[4];
          } M;
 static int   ncall = 0;

 static Screen               *screen;   /*SCREEN ID         */
 static unsigned long         atrmsk;   /*ATTRIBUTES MASK   */
 static int                   depth;    /*NUMBER OF PLANES  */

 static Visual               *visual;   /*VISUAL TYPE       */
 static XSetWindowAttributes  xswda;    /*WINDOW ATTRIBUTES */
 static XGCValues             xgcvl;    /*GC VALUES         */
 static XEvent                event;    /*INPUT EVENT       */
 static XSizeHints            figpos;   /*position and size for twm */
 static Cursor                xcursor;  /* cursor font def */
 static XColor                cursorfg; /* cursor foregrnd color */
 static XColor                cursorbg; /* cursor backgrnd color */
        
/*
*     ************************************************************
*     WINDOW MANAGEMENT ROUTINE - OPENS    DISPLAY
*                               - CREATES  WINDOWS
*                               - MAPS     WINDOWS
*                               - SETS     GRAPHIC CONTEXTS
*                               - DESTROYS WINDOWS, DISPLAYS, ETC
*     ************************************************************
*/
  kmd = (char *)kmdptr;
  idx = *idxptr - 1;
                          
  if(ncall == 0)
   {
     xlee_.mode_or = CoordModeOrigin;
     strncpy(xlcc_.isopen,"NO  ",4);
     strncpy(xlee_.mode_pl,"LIN ",4);
     strncpy(xlkk_.kurty,"LIVE",4);
     ncall=1;
   }
  if(strncmp(kmd,"FIGG",4) == 0)
   {
/*                            
*     *************************************************************
*     INIT DISPLAY-ID AND SCREEN-ID
*     *************************************************************
*/
     if(strncmp(xlcc_.isopen,"YES ",4) == 0)
      {
        for(i=0; i < 20; i++)
         {
           if(xlcc_.wflg[i].winflg1 > 0) 
            {
              XUnmapWindow(xlaa_.dpy,xlaa_.wdid[i]);
              XDestroyWindow(xlaa_.dpy,xlaa_.wdid[i]);
              xlcc_.wflg[i].winflg1 = 0;
            }
         }
        XCloseDisplay(xlaa_.dpy);
        strncpy(xlcc_.isopen,"NO  ",4);
      }
   if(xlcc_.numwin <= 0) return;
   xlaa_.dpy=XOpenDisplay(NULL);
     if(xlaa_.dpy == NULL)
      {
        err = 1020;
      }
     else
      {
        screen=XDefaultScreenOfDisplay(xlaa_.dpy);

/*
*     *************************************************************
*     CREATE WINDOWS
*     *************************************************************
*/
        depth  = XDefaultDepthOfScreen(screen);
        visual = XDefaultVisualOfScreen(screen);
        atrmsk = CWEventMask | CWBackPixel | CWBackingStore;

        if(strncmp(xlkk_.kurty,"LIVE",4) == 0)
         {
           xswda.event_mask= ExposureMask
                             |  ButtonPressMask
                             |  KeyPressMask
                             |  PointerMotionMask ;
         }
        else
         {
           xswda.event_mask= ExposureMask
                             |  ButtonPressMask
                             |  KeyPressMask ;
         }
        xlee_.maskev=xswda.event_mask;

        xswda.backing_store=Always;
        j = 1;
        xswda.background_pixel=xx_defcol__(&xlaa_.dpy,&screen,&visual,&j);

        xcursor = XCreateFontCursor(xlaa_.dpy, XC_crosshair);

        for(i=0; i < xlcc_.numwin; i++)
         {

#ifdef __ultrix
#define __Xdec
#endif
#ifdef __osf__
#define __Xdec
#endif

#ifdef __Xdec
           XSelectInput(xlaa_.dpy, XRootWindowOfScreen(screen),
                        SubstructureNotifyMask);
#endif

           xlaa_.wdid[i]=XCreateWindow(xlaa_.dpy,
                 XRootWindowOfScreen(screen),
                 xldd_.figdat[i][0],
                 xldd_.figdat[i][1],
                 xldd_.figdat[i][2],
                 xldd_.figdat[i][3],
                 4,
                 depth,
                 InputOutput,
                 visual,
                 atrmsk,
                 &xswda);

#ifdef  __Xdec
           XFlush(xlaa_.dpy);
           do
             {
               XWindowEvent(xlaa_.dpy, XRootWindowOfScreen(screen),
                             SubstructureNotifyMask, &event);
             } while(event.type != CreateNotify);
#endif

              /*  Define the cursor visible in the window */
           XDefineCursor(xlaa_.dpy, xlaa_.wdid[i], xcursor);

           figpos.x=xldd_.figdat[i][0];         /* WMHints structure */
           figpos.y=xldd_.figdat[i][1];
           figpos.width=xldd_.figdat[i][2];
           figpos.height=xldd_.figdat[i][3];
           figpos.flags=USPosition|USSize;

           XSetNormalHints(xlaa_.dpy,xlaa_.wdid[i],&figpos); 

           for(j=0, cptr=xlhh_.winame; j < 80; j++) *cptr = ' ';
           sprintf(xlhh_.winame,"WINDOW -%3i",i+1);

           XStoreName(xlaa_.dpy,xlaa_.wdid[i],xlhh_.winame);

           strncpy(xlaa_.nuwin[i],"YES ",4);
           strncpy(xlll_.nuwflg1[i],"YES ",4);
           strncpy(xlll_.nuwflg2[i],"YES ",4);
           strncpy(xlll_.nuwflg3[i],"YES ",4);
           xlii_.laswin=0;

           xlcc_.windat[i][0]=0;
           xlcc_.windat[i][1]=xldd_.figdat[i][3];
           xlcc_.windat[i][2]=xldd_.figdat[i][2];
           xlcc_.windat[i][3]=xldd_.figdat[i][3];
           xlcc_.windat[i][4]=0.0;
           xlcc_.windat[i][5]=1.0;
           xlcc_.windat[i][6]=1000.0;
           xlcc_.windat[i][7]=1000.0;
           xlcc_.wflg[i].winflg1 = 1;
           strncpy(xlcc_.wflg[i].winflg2,"ON  ",4);
           strncpy(xlcc_.wflg[i].winflg3,"1D  ",4);
           strncpy(xlcc_.wflg[i].winflg4,"LIN ",4);
         }
        xgcvl.font = XLoadFont(xlaa_.dpy, "6x13");
        j = 1;
        xgcvl.background = xx_defcol__(&xlaa_.dpy,&screen,&visual,&j);
        j = 34;
        for(i=0; i < 7; i++)
         {
          xgcvl.foreground = xx_defcol__(&xlaa_.dpy,&screen,&visual,&j);
          xlff_.gcor[i]=XCreateGC(xlaa_.dpy,xlaa_.wdid[0],
                               (GCForeground | GCBackground | GCFont),&xgcvl);
          XSetFunction(xlaa_.dpy,xlff_.gcor[i],GXxor);
          j++;
         }

        j = 1;
        for(i=0; i < 35; i++)
         {
          xgcvl.foreground=xx_defcol__(&xlaa_.dpy,&screen,&visual,&j);
          xlff_.gcon[i]=XCreateGC(xlaa_.dpy,xlaa_.wdid[0],
                             (GCForeground | GCBackground | GCFont),&xgcvl);
          j++;
         }

        /* Set the cursor color  */
        cursorbg.flags = 0;
        cursorbg.red   = xljj_.ired[0];
        cursorbg.green = xljj_.igre[0];
        cursorbg.blue  = xljj_.iblu[0];

        cursorfg.flags = 0;
        cursorfg.red   = xljj_.ired[32];
        cursorfg.green = xljj_.igre[32];
        cursorfg.blue  = xljj_.iblu[32];

        XRecolorCursor(xlaa_.dpy, xcursor, &cursorfg, &cursorbg);
/*
*     *************************************************************
*     MAP WINDOWS
*     *************************************************************
*/

/*   Combine three loops into one, to make event tracking more 
     obvious.  Not as efficient, but keeps events from being lost. */

        for(i=0; i < xlcc_.numwin; i++)
        {    
          j=i+1;
          XSelectInput(xlaa_.dpy,xlaa_.wdid[i],
               VisibilityChangeMask);
          XMapWindow(xlaa_.dpy,xlaa_.wdid[i]);

          XWindowEvent(xlaa_.dpy, xlaa_.wdid[i],
                VisibilityChangeMask,
                &event);

          doaxis_(&j,(int *)xlee_.mode_pl,
                  &xlcc_.windat[i][4],
                  &xlcc_.windat[i][5],
                  &xlcc_.windat[i][6],
                  &xlcc_.windat[i][7],
                  &xlcc_.windat[i][2],
                  &xlcc_.windat[i][3]);

          XSelectInput(xlaa_.dpy,xlaa_.wdid[i],NoEventMask);
        }
        XSync(xlaa_.dpy,True);
        strncpy(xlcc_.isopen,"YES ",4);
        return;
      }
   }
  else if(strncmp(kmd,"ERAS",4) == 0)
   {
/*
*     *************************************************************
*     EREASE WINDOW - IDX
*     *************************************************************
*/
    if(idx < 0 || idx >= xlcc_.numwin) err =1000;
    else
     {
      if(xlcc_.wflg[idx].winflg1 == 0) return;
      XUnmapWindow(xlaa_.dpy,xlaa_.wdid[idx]);
      XDestroyWindow(xlaa_.dpy,xlaa_.wdid[idx]);
      XSync(xlaa_.dpy,True);
      xlcc_.wflg[idx].winflg1=0;
      return;
     }
   }

  else if(strncmp(kmd,"WIN ",4) == 0)
   {
/*
*     *************************************************************
*     RE-CREATE WINDOW - IDX
*     *************************************************************
*/
     if(idx < 0 || idx >= xlcc_.numwin || strncmp(xlcc_.isopen,"YES ",4))
       err=1000;
     else {
       if(xlcc_.wflg[idx].winflg1 != 0) return;
       /* remove this call - to discourage change of focus, etc
	  xlaa_.wdid[idx]=XCreateWindow(xlaa_.dpy,
	  XRootWindowOfScreen(screen),
	  xldd_.figdat[idx][0],
	  xldd_.figdat[idx][1],
	  xldd_.figdat[idx][2],
	  xldd_.figdat[idx][3],
	  4,
	  depth,
	  InputOutput,
	  visual,
	  atrmsk,
               &xswda);
       */
       /*  Define the cursor visible in the window */
       XDefineCursor(xlaa_.dpy, xlaa_.wdid[idx], xcursor);
       
       figpos.x=xldd_.figdat[idx][0];         /* WMHints structure */
       figpos.y=xldd_.figdat[idx][1];
       figpos.width=xldd_.figdat[idx][2];
       figpos.height=xldd_.figdat[idx][3];
       figpos.flags=USPosition|USSize;
       
       XSetNormalHints(xlaa_.dpy,xlaa_.wdid[idx],&figpos); 
       
       XStoreName(xlaa_.dpy,xlaa_.wdid[idx],xlhh_.winame);
       /* Need to clear the space */
       XClearWindow(xlaa_.dpy,xlaa_.wdid[idx]);

       /*
	 XSelectInput(xlaa_.dpy,xlaa_.wdid[idx],VisibilityChangeMask);
	 XMapWindow(xlaa_.dpy,xlaa_.wdid[idx]);
	 XWindowEvent(xlaa_.dpy, xlaa_.wdid[idx],
	 VisibilityChangeMask,
	 &event);
	 XSelectInput(xlaa_.dpy,xlaa_.wdid[idx],NoEventMask);
       */
      XSync(xlaa_.dpy, True);
      
      strncpy(M.modepl,xlcc_.wflg[idx].winflg4,4);
      if(strncmp(xlcc_.wflg[idx].winflg3,"2D  ",4) == 0)
	strncpy(M.modepl,"LIN ",4);
      
      j = idx + 1;
      doaxis_(&j,&M.mode,
	      &xlcc_.windat[idx][4],
	      &xlcc_.windat[idx][5],
	      &xlcc_.windat[idx][6],
	      &xlcc_.windat[idx][7],
	      &xlcc_.windat[idx][2],
	      &xlcc_.windat[idx][3]);
      strncpy(xlaa_.nuwin[idx],"YES ",4);
      strncpy(xlll_.nuwflg1[idx],"YES ",4);
      strncpy(xlll_.nuwflg2[idx],"YES ",4);
      strncpy(xlll_.nuwflg3[idx],"YES ",4);
      xlcc_.wflg[idx].winflg1=1;
      xlii_.laswin=0;
      return;
     }
   }            

  else if(strncmp(kmd,"CLR ",4) == 0 || strncmp(kmd,"END ",4) == 0)
   {
/*
*     *************************************************************
*     Unmap and destroy windows
*     *************************************************************
*/
    if(strncmp(xlcc_.isopen,"NO  ",4) != 0)
     {
      for(i=0; i < xlcc_.numwin; i++)
       {
        XUnmapWindow(xlaa_.dpy,xlaa_.wdid[i]);
        XDestroyWindow(xlaa_.dpy,xlaa_.wdid[i]);
        xlcc_.wflg[i].winflg1=0;
       }
      xlcc_.numwin=0;
      XCloseDisplay(xlaa_.dpy);
      strncpy(xlcc_.isopen,"NO  ",4);
     }
    if(strncmp(kmd,"END ",4) != 0) return;
    exit(1);
   }
  else
   {
     err = 1000;
   }
/*
*     *************************************************************
*     SEND ERROR MESSAGES
*     *************************************************************
*/
  for(i=0, cptr=xlhh_.winame; i < 112; i++) *cptr++ = ' ';
  if (err == 1000)
   {
    sprintf(lll_.mssg,"XX_WINMAN SYNTAX ERROR OR ILLEGAL VALUE - IGNORED");
   }
  else if(err == 1010)
   {
    sprintf(lll_.mssg,"%4.4s - IS AN ILLEGAL XX_WINMAN COMMAND - IGNORED",kmd);
   }
  else if(err == 1020)
   {
    sprintf(lll_.mssg,"DISPLAY NOT OPENED");
   }
  else if(err == 1030)
   {
    sprintf(lll_.mssg,"XX_WINMAN ERROR - ZERO WINDOWS!");
   }
  messlog_(&lll_.logut,&lll_.logup);
  if(err == 1020) exit(1);
}

/*
C$PROG XX_EVENTMAN 
*/
/*
*     ************************************************************
*     BY WT MILNER AT HHIRF - LAST MODIFIED 03/31/90
*     ************************************************************
*/
void xx_eventman__(void)
{ 
  Window       irootid,ichilid,win_in;
  int          irootxv,irootyv;
  unsigned int istate;
  int          i,iloop,istat,oloop;
  static union 
            {
              int  keynb;
              char keyna[4];
            } K;
  static union
            {
              int  iretni;
              char iretn[4];
            } I;

  static XEvent       event;    /*INPUT EVENT       */
  XKeyEvent          *evtptr;

/*
*     *************************************************************
*     HANDLE EVENTS
*     *************************************************************
*/
  if(strncmp(xlcc_.isopen,"YES ",4) != 0) return;

  oloop= 1;
  XSync(xlaa_.dpy,True);
  do
   {
    for(i=0; i < xlcc_.numwin; i++)
                        XSelectInput(xlaa_.dpy,xlaa_.wdid[i],
                                     xlee_.maskev | PointerMotionHintMask);

    xlii_.lwin = 0;
    iloop = 1;              
    do
     {
      XNextEvent(xlaa_.dpy,&event);

      if(event.xany.window != xlii_.lwin)
       {
         xlii_.lwin = event.xany.window;
       }
/*
*   Handle Motion events first.  Flush all motion events from the
*   queue, then ALWAYS do the XQueryPointer call.
*/
      if (event.type == MotionNotify)
       {
          while (XCheckTypedEvent(xlaa_.dpy,MotionNotify,&event));
       }
      istat=XQueryPointer(xlaa_.dpy,
                         xlii_.lwin,
                         &irootid,
                         &ichilid,
                         &irootxv,
                         &irootyv,
                         &xlii_.ivx,
                         &xlii_.ivy,
                         &istate);
      if(istat != 0)
       {
        nucur_();
        switch(event.type)
         {
           case ButtonPress:
             strncpy(K.keyna,"    ",4);
             if(event.xbutton.button == Button1) strncpy(K.keyna,"MB1 ",4);
             else if(event.xbutton.button == Button2) strncpy(K.keyna,"MB2 ",4);
             else if(event.xbutton.button == Button3) strncpy(K.keyna,"MB3 ",4);
             if(strncmp(K.keyna,"    ",4) != 0)
              {
               cussman_(&xlii_.wndx,&xlii_.ivx,&xlii_.ivy,&K.keynb,&I.iretni);
               if(strncmp(I.iretn,"RET ",4) == 0) oloop =0, iloop = 0;
               else if(strncmp(I.iretn,"RES ",4) == 0) iloop = 0;
              }
             break;

           case KeyPress:
             evtptr = (XKeyEvent *)&event;
             K.keynb = xx_keynam__(&evtptr);
             cussman_(&xlii_.wndx,&xlii_.ivx,&xlii_.ivy,&K.keynb,&I.iretni);
             if(strncmp(I.iretn,"RET ",4) == 0) oloop = 0, iloop = 0;
             else if(strncmp(I.iretn,"RES ",4) == 0) iloop = 0;
             break;

           default:
             break;
         }      /*  end switch  */
       }
     } while(iloop);
   } while(oloop);
  for(i=0; i < xlcc_.numwin; i++)
                       XSelectInput(xlaa_.dpy,xlaa_.wdid[i],NoEventMask);
  XSync(xlaa_.dpy,True);
}

/*
C$PROG XX_KEYNAM 
*/
/*
*     ************************************************************
*     By RL Varner - to replace keynam by WT Milner
*     ************************************************************
*/
int xx_keynam__(XKeyEvent **event)
{
  KeySym     keysym;
  static union 
            {
             int    keybin;
             char   keyasc[4];
            } K;
/*
*     Lookup the keypress event to get the keysym for the pressed key
*     This looks in the first entry of the keysym table - (special chars
*     numbers and lowercase alphabetic)
*/
      keysym = XLookupKeysym(*event,ShiftMapIndex);
/*
*     If no symbol is defined just return nothing
*/
      if(keysym == NoSymbol) return(0);
/*
*     If alphabetic, lookup the uppercase equivalent in the second
*     Keysym table entry.
*/
      if((keysym >= 97) && (keysym <= 122))
                                 keysym=XLookupKeysym(*event,LockMapIndex);
/*             
*     If a regular key, then pad with blanks and return
*/
      if(keysym < 0xff00)
        {
          strncpy(K.keyasc,"    ",4);
          K.keyasc[0] = (char)keysym;
        }
/*
*     If not printing character, apply our own translation
*/
      else if(keysym == 0xff51) strncpy(K.keyasc,"LF-A",4);
      else if(keysym == 0xff52) strncpy(K.keyasc,"UP-A",4);
      else if(keysym == 0xff53) strncpy(K.keyasc,"RT-A",4);
      else if(keysym == 0xff54) strncpy(K.keyasc,"DN-A",4);
      else if(keysym == 0xff0d) strncpy(K.keyasc,"CR  ",4);
      else  K.keybin = 0;
      return (K.keybin);
}

/*
C$PROG XX_DEFCOL 
*/
/*
*     ************************************************************
*     BY WT MILNER AT HHIRF - LAST MODIFIED 03/31/90
*     ************************************************************
*/
int xx_defcol__(Display **dpy,Screen **screen,Visual **visual,int *nptr)
{
  int            i=0, ierr=0;
  int            flags=0, n;
  char           *cptr;
  Colormap       colormap;
  static union
            {
             int     ifunc;
             char    func[5];
            } Func ;

  static XColor  colors[40];

/*
*     ************************************************************
*     DEFINE COLORS - USED IN SETTING UP GRAPHIC CONTEXTS
*     ************************************************************
*/
  strncpy(Func.func,"INIT",4);
  n = *nptr -1;
  if(strncmp(xljj_.kolrset,"YES ",4) != 0) colrset_(&i,&Func.ifunc,&ierr);
  if(n < 40)
   {
/*      if((*visual)->class == PseudoColor ||
         (*visual)->class == DirectColor ||
         (*visual)->class == GrayScale) */
       if (n<40)
       {
        colormap=XDefaultColormapOfScreen(*screen);
                               
        colors[n].flags = flags;
        colors[n].red   = xljj_.ired[n];
        colors[n].green = xljj_.igre[n];
        colors[n].blue  = xljj_.iblu[n];

        if(XAllocColor(*dpy,colormap,&colors[n])) return colors[n].pixel;
       }
      else
       {
        if(n == 0) return XBlackPixelOfScreen(*screen);
        else   return XWhitePixelOfScreen(*screen);
       }
   }
  for(i=0, cptr=lll_.mssg; i < 112; i++) *cptr = ' ';
  sprintf(lll_.mssg,"COLOR NOT ALLOCATED");
  messlog_(&lll_.logut,&lll_.logup);
  return 0;  
}                   

/*
C$PROG XX_SYNC 
*/

void xx_sync__(Display **idpy,Bool *kind)
{
      XSync(*idpy,*kind);
}
/*
C$PROG XX_DRAWPOINTS 
*/

void xx_drawpoints__(Display **idpy,Drawable *idw,GC *lgc,XPoint *ipoints,
                    int *num,int *mode)
{
      XDrawPoints(*idpy,*idw,*lgc,ipoints,*num,*mode);
}
/*
C$PROG XX_DRAWLINE 
*/

void xx_drawline__(Display **idpy,Drawable *idw,GC *lgc,int *ix1,
                  int *jy1,int *ix2,int *jy2)
{
      XDrawLine(*idpy,*idw,*lgc,*ix1,*jy1,*ix2,*jy2);
}
/*
C$PROG XX_DRAWLINES 
*/

void xx_drawlines__(Display **idpy,Drawable *idw,GC *lgc,XPoint *ipoints,
                   int *num,int *mode)
{
      XDrawLines(*idpy,*idw,*lgc,ipoints,*num,*mode);
}
/*
C$PROG XX_FILLRECTANGLE 
*/
 
void xx_fillrectangle__(Display **idpy,Drawable *idw,GC *lgc,int *ix,
                       int *jy,int *iw,int *ih)
{
      XFillRectangle(*idpy,*idw,*lgc,*ix,*jy,*iw,*ih);
}
/*
C$PROG XX_FILLRECTANGLES 
*/

void xx_fillrectangles__(Display **idpy,Drawable *idw,GC *lgc,
                        XRectangle *irec,int *num,int *mode)
{
      XFillRectangles(*idpy,*idw,*lgc,irec,*num);
}
/*
C$PROG XX_DRAWARC 
*/

void xx_drawarc__(Display **idpy,Drawable *idw,GC *lgc,int *ix,
                 int *jy,int *ih,int *iw,int *ia1,int *ia2)
{
      XDrawArc(*idpy,*idw,*lgc,*ix,*jy,*ih,*iw,*ia1,*ia2);
}
/*
C$PROG XX_DRAWARCS 
*/

void xx_drawarcs__(Display **idpy,Drawable *idw,GC *lgc,XArc *iarks,int *num)
{
      XDrawArcs(*idpy,*idw,*lgc,iarks,*num);
}
/*
C$PROG XX_FILLARC 
*/

void xx_fillarc__(Display **idpy,Drawable *idw,GC *lgc,int *ix,
                 int *jy,int *ih,int *iw,int *ia1,int *ia2)
{
      XFillArc(*idpy,*idw,*lgc,*ix,*jy,*ih,*iw,*ia1,*ia2);
}
/*
C$PROG XX_FILLARCS 
*/

void xx_fillarcs__(Display **idpy,Drawable *idw,GC *lgc,XArc *iarks,int *num)
{
      XFillArcs(*idpy,*idw,*lgc,iarks,*num);
}
/*
C$PROG XX_FILLPOLYGON
*/

void xx_fillpolygon__(Display **idpy, Drawable *idw, GC *lgc, XPoint *ipoints,
                     int *num, int *mode)
{
      XFillPolygon(*idpy,*idw,*lgc,ipoints,*num,Convex,*mode);
}
/*
C$PROG XX_DRAWSTRING 
*/

void xx_drawstring__(Display **idpy,Drawable *idw,GC *lgc,
                    int *ix,int *jy,char *itex,int ltex)
{
      XDrawString(*idpy,*idw,*lgc,*ix,*jy,itex,ltex);
}
/*
C$PROG ZZ_FLUSH
*/
/*
*    FORTRAN callable routine to flush the output buffer for logical unit 6
*    (stdout) or logical unit 0 (stderr).
*/
void zz_flush__(int *lu)
{
  if(*lu == 6) fflush(stdout);
  else if (*lu == 0) fflush(stderr);
}
