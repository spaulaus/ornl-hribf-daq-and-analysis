C$PROG SAMDEF    - Definitions of fit-variables & LUs (all comments)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SAMDEF
C   
C     ------------------------------------------------------------------
C     DEFINITIONS FOR ENTRIES IN PEAK-ATTRIBUTE-TABLE "PAT"
C     AND
C     DEFINITIONS FOR ENTRIES IN LIST-OF-ATTRIBUTES   "LAT"
C   
C     NPAT      = NUMBER OF ENTRIES IN "PAT"
C     MAXPAT    = MAX NUMBER OF ENTRIES ALLOWED
C     NUPAT     = 0 SAYS NO ADDS OR DELETES TO "PAT" SINCE LAST FIT
C     NUPAT     = 1 SAYS THERE HAVE BEEN SUCH ADDS OR DELETES SINCE LAST
C   
C     PAT(I,1)  = X     ITH PEAK POSITION
C     PAT(I,2)  = W     ITH PEAK WIDTH
C     PAT(I,3)  = ASL   ITH LO-SIDE ASYMMETRY
C     PAT(I,4)  = ASH   ITH HI-SIDE ASYMMETRY
C   
C     LAT(I,5)  = FUNO  ITH "FUNCTION NUMBER"
C     LAT(I,6)  = HOLX  ITH "HOLD FLAG" FOR X
C     LAT(I,7)  = HOLW  ITH "HOLD FLAG" FOR W
C     LAT(I,8)  = HOLAL ITH "HOLD FLAG" FOR ASL
C     LAT(I,9)  = HOLAH ITH "HOLD FLAG" FOR ASH
C     LAT(I,10) = IONOF ITH "ON/OFF" FLAG
C     LAT(I,11) = ITH PEAK POSITION IN CURRENT PICTURE (PIXEL #)
C     ------------------------------------------------------------------
C   
C     ECAL = ECO,ECA,ECB POWER SERIES COEFFICIENTS FOR ENERGY CALIBRATIO
C     FW   = FWA,FWB,FWC - STANDARD WIDTH CALIBRATION LIST
C     ASYM = ASLO,ASHI   - STANDARD INITIAL ASYMMETRY VALUES
C     WLIM = FWLO,FWHI   - WIDTH LIMIT FACTORS
C     ALIM = FALO,FAHI   - ASYMMETRY LIMIT FACTORS
C     DX   = XSTEP,DXMAX - STEP SIZE AND LIMIT FOR X-POSITIONING
C     DEL  = DEL,DELFAC,NMUL - STEP SIZE, STEP SIZE MULTIPLIER AND
C                              AND # OF TIMES STEP SIZE IS REDUCED
C     NBC  = NBC         - # OF BACKGROUND COMPONENTS
C     WOOD = 'YES'       - SAYS USE WOODS-SAXON TERM IN BACKGROUND
C     WOOD = 'NO'        - SAYS DO NOT USE WOODS-SAXON TERM IN BACKGROUN
C     FUNO = FUNS - STANDARD FUNCTION # FOR PEAK SHAPE
C   
C     BACK = (X1,Y1),(X2,Y2),(X3,Y3) - THREE BACKGROUND POINTS
C     SKIP = (ILO,IHI) - - UP TO 4 REGIONS TO OMIT FROM FIT
C     FIT  = ID,ILO,IHI  - FIT REQUEST
C   
C     PLOT = 'NONE' SAYS NO PLOTS
C     PLOT = 'FITS' SAYS PLOT "FITS" ONLY
C     PLOT = 'ALL ' SAYS PLOT "FITS" AND "RESIDUALS"
C   
C     KINFD= 'FIT ' SAYS PLOT DATA, FIT AND BACKGROUND
C     KINFD= 'PKS ' SAYS PLOT DATA, FIT, PKS, AND BACKGROUND
C     KINFD= 'PPB ' SAYS PLOT DATA, FIT, (PKS+BGD), AND BGD
C     MARKS= 'ON  ' SAYS DISPLAY PEAK MARKERS WITH FIT
C     MARKS= 'OFF ' SAYS DO NOT
C
C     MINSY= 'VAR ' (AUTOSCALE) OR LO-LIMIT FOR DISPLAY
C     MAXSY= 'VAR ' (AUTOSCALE) OR HI-LIMIT FOR DISPLAY   
C   
C     VX   = UIND  SAYS VARY POSITIONS UNCONDITIONALLY, INDEPENDENTLY
C     VX   = CIND  SAYS VARY POSITIONS CONDITIONALLY AND INDEPENDENTLY
C     VX   = ULOC  SAYS VARY POSITIONS UNCONDITIONALLY, LOCKED
C     VX   = CLOC  SAYS VARY POSITIONS CONDITIONALLY AND LOCKED
C     VX   = FIX   SAYS FIX ALL PEAK POSITIONS
C   
C     VW   - HAS THE SAME MEANING FOR PEAK WIDTHS
C     VALO - HAS THE SAME MEANING FOR LO-SIDE ASYMMETRY
C     VAHI - HAS THE SAME MEANING FOR HI-SIDE ASYMMETRY
C   
C     KVAR(1) CONTAINS "VALUE" FROM "VX-LIST"
C     KVAR(2) CONTAINS "VALUE" FROM "VW-LIST"
C     KVAR(3) CONTAINS "VALUE" FROM "VALO-LIST"
C     KVAR(4) CONTAINS "VALUE" FROM "VAHI-LIST"
C   
C   
C     IVF(I) = 0 SAYS VARY ALL PARMS OF TYPE-I "TOGETHER"
C                 THAT IS MULTIPLY BY THE SAME FACTOR
C     IVF(I) = 1 SAYS VARY ALL PARMS OF TYPE-I INDEPENDENTLY
C                                              OR KEEP FIXED
C   
C     XYP(J,1),XYP(J,2) GIVES JTH (X,Y) BACKGROUND POINT (JMAX=50)
C   
C     JPO(K) GIVES THE PEAK # ASSOCIATED WITH THE KTH PARM TO BE VARIED
C     IPO(K) GIVES THE "PEAK PARM #" (1 TO 4) ASSOCIATED WITH KTH VARIAB
C     KXF(I,J)=0 SAYS PARM I FOR PEAK J IS TO BE VARIED
C     KXF(I,J)=1 SAYS PARM I FOR PEAK J IS NOT TO BE VARIED
C   
C     ------------------------------------------------------------------
C     DEFINITION OF LOGICAL UNITS & CHANNELS
C     ------------------------------------------------------------------
C     LSP = 0/1 IS LOGICAL UNIT FOR INPUT SPK-FILE
C     LUD = 0/2 IS LOGICAL UNIT FOR INPUT DRR-FILE
C     LUH = 0/? IS CHANNEL #    FOR INPUT HIS-FILE
C     LCM = 0/8 IS LOGICAL UNIT FOR COMMAND FILE
C     LIN = 5/8 IS LOGICAL UNIT FOR COMMAND INPUT (INIT @ 5)
C             5 IS ASSIGNED TO CON:
C             7 IS ASSIGNED TO PR:
C            10 IS THE LOGICAL UNIT ASSIGNED TO ANY COLOR MAP FILE
C     ------------------------------------------------------------------
C   
      RETURN
      END
