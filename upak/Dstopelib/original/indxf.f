C$PROG INDXF     - Returns 1-D index in 2-D table
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      FUNCTION INDXF(IX,JY,MX)
      INDXF=(JY-1)*MX+IX
      RETURN
      END
