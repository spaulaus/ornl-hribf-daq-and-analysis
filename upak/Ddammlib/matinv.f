C$PROG MATINV    - Matrix inverter for GFIT
C
C     ******************************************************************
C     From Saltmarsh & Halbert GFIT package
C     ******************************************************************
C
      SUBROUTINE MATINV (N,NFAIL)
C
      COMMON/GF01/ AM(180)
C
      K=1
      IF (N .EQ. 1) THEN
         AM(1)=1.0/AM(1)
         NFAIL=0
         RETURN
      ELSE IF (N .LT. 1) THEN
         NFAIL = K
         RETURN
      ENDIF

      DO 7 M=1,N
         IMAX=M-1
         DO 6 L=M,N
            SUMA=0.0
            KLI=L
            KMI=M
            IF (IMAX .GT. 0) THEN
               DO 2 I=1,IMAX
                  SUMA=SUMA+AM(KLI)*AM(KMI)
                  J=N-I
                  KLI=KLI+J
                  KMI=KMI+J
 2             CONTINUE
            ENDIF
            TERM=AM(K)-SUMA
            IF (L .LE. M) THEN
               IF (TERM .GT. 0) THEN
                  DENOM=SQRT(TERM)
                  AM(K)=DENOM
               ELSE
                  NFAIL=K
                  RETURN
               ENDIF
            ELSE
               AM(K)=TERM/DENOM
            ENDIF
            K=K+1
 6       CONTINUE
 7    CONTINUE

      AM(1)=1.0/AM(1)
      KDM=1
      DO 104 L=2,N
         KDM=KDM+N-L+2
         TERM=1.0/AM(KDM)
         AM(KDM)=TERM
         KMI=0
         KLI=L
         IMAX=L-1
         DO 103 M=1,IMAX
            K=KLI
            SUMA=0.0
            DO 102 I=M,IMAX
               II=KMI+I
               SUMA=SUMA-AM(KLI)*AM(II)
               KLI=KLI+N-I
 102        CONTINUE
            AM(K)=SUMA*TERM
            J=N-M
            KLI=K+J
            KMI=KMI+J
 103     CONTINUE
 104  CONTINUE
      K=1
      DO 203 M=1,N
         KLI=K
         DO 202 L=M,N
            KMI=K
            IMAX=N-L+1
            SUMA=0.0
            DO 201 I=1,IMAX
               SUMA=SUMA+AM(KLI)*AM(KMI)
               KLI=KLI+1
               KMI=KMI+1
 201        CONTINUE
            AM(K)=SUMA
            K=K+1
 202     CONTINUE
 203  CONTINUE
      
 204  CONTINUE
      NFAIL=0
      
 300  RETURN
      END
      
