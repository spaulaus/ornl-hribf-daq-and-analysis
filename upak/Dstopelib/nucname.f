C$PROG NUCNAME   - Returns nuclear name for a given Z
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      INTEGER*4 FUNCTION NUCNAME(IZ)
C
      INTEGER*4 NUCN(120)
C
      CHARACTER*60 CNUCN(8)
C
      EQUIVALENCE (CNUCN,NUCN)
C
      DATA CNUCN/
     1 'H   He  Li  Be  B   C   N   O   F   Ne  Na  Mg  Al  Si  P   '
     2,'S   Cl  Ar  K   Ca  Sc  Ti  V   Cr  Mn  Fe  Co  Ni  Cu  Zn  '
     3,'Ga  Ge  As  Se  Br  Kr  Rb  Sr  Y   Zr  Nb  Mo  Tc  Ru  Rh  '
     4,'Pd  Ag  Cd  In  Sn  Sb  Te  I   Xe  Cs  Ba  La  Ce  Pr  Nd  '
     5,'Pm  Sm  Eu  Gd  Tb  Dy  Ho  Er  Tm  Yb  Lu  Hf  Ta  W   Re  '
     6,'Os  Ir  Pt  Au  Hg  Tl  Pb  Bi  Po  At  Rn  Fr  Ra  Ac  Th  '
     7,'Pa  U   Np  Pu  Am  Cm  Bk  Cf  Es  Fm  Md  No  Lr  Rf  Ha  '
     8,'Nh  Ns  Hs  Mt  Xa  Xb                                      '/
C
      INTEGER*4  UNKNOWN
C
      character*4 cunknown
      equivalence (cunknown, unknown)
      DATA      cUNKNOWN/'??  '/
C
      IF(IZ.LT.1.OR.IZ.GT.120) GO TO 100
C
      NUCNAME=NUCN(IZ)
C
      RETURN
C
  100 NUCNAME=UNKNOWN

      RETURN
      END
