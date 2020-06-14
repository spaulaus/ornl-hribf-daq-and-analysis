C$PROG TERMS
C
      SUBROUTINE TERMS (DIAG,SDIAG,SIGMA,DEL)
C   
      REAL*4  DIAG,SDIAG,SIGMA,DEL
C   
C                            FROM FITPACK -- AUGUST 31, 1981
C                       CODED BY A. K. CLINE AND R. J. RENKA
C                            DEPARTMENT OF COMPUTER SCIENCES
C                              UNIVERSITY OF TEXAS AT AUSTIN
C   
C THIS SUBROUTINE COMPUTES THE DIAGONAL AND SUPERDIAGONAL
C TERMS OF THE TRIDIAGONAL LINEAR SYSTEM ASSOCIATED WITH
C SPLINE UNDER TENSION INTERPOLATION.
C   
C ON INPUT--
C   
C   SIGMA CONTAINS THE TENSION FACTOR.
C   
C AND
C   
C   DEL CONTAINS THE STEP SIZE.
C   
C ON OUTPUT--
C   
C               (SIGMA*DEL*COSH(SIGMA*DEL) - SINH(SIGMA*DEL)
C   DIAG = DEL*--------------------------------------------.
C                     (SIGMA*DEL)**2 * SINH(SIGMA*DEL)
C   
C                   SINH(SIGMA*DEL) - SIGMA*DEL
C   SDIAG = DEL*----------------------------------.
C                (SIGMA*DEL)**2 * SINH(SIGMA*DEL)
C   
C AND
C   
C   SIGMA AND DEL ARE UNALTERED.
C   
C THIS SUBROUTINE REFERENCES PACKAGE MODULE SNHCSH.
C   
C     ------------------------------------------------------
      SAVE
C     ------------------------------------------------------
C   
      IF (SIGMA .NE. 0.) GO TO 1
      DIAG = DEL/3.
      SDIAG = DEL/6.
      RETURN
    1 SIGDEL = SIGMA*DEL
      CALL SNHCSH (SINHM,COSHM,SIGDEL,0)
      DENOM = DEL/((SINHM+SIGDEL)*SIGDEL*SIGDEL)
      DIAG = DENOM*(SIGDEL*COSHM-SINHM)
      SDIAG = DENOM*SINHM
      RETURN
      END
