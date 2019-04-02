* COPYRIGHT (c) 1975 AEA Technology
*######DATE 7 Dec 1992
C       Toolpack tool decs employed.
C       MB01ED reference removed.
C       ZERO and ONE made PARAMETER.
C       FM02AD and W1 references removed.
C       SAVE statements added.
C
C  EAT 21/6/93 EXTERNAL statement put in for block data so will work on VAXs.
C
C
      SUBROUTINE MB01CD(A,M,IA,IND,C)
C     .. Parameters ..
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
C     ..
C     .. Scalar Arguments ..
      INTEGER IA,M
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION A(IA,M),C(M)
      INTEGER IND(M)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AMAX,DIV,SCPROD,STO,W
      INTEGER I,I1,IMAX,IPROD,ISTO,IW,J,J1,K,M1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DABS
C     ..
C     .. Common blocks ..
      COMMON /MB01DD/LP,IFLAG
      INTEGER IFLAG,LP
C     ..
C     .. Data block external statement
      EXTERNAL MB01ED
C     ..
C     .. Save statement ..
      SAVE /MB01DD/
C     ..
C     .. Executable Statements ..
      IFLAG = 0
      IF (M-1) 50,2,3
    2 IF (A(1,1).EQ.ZERO) GO TO 60
      A(1,1) = ONE/A(1,1)
      GO TO 99

    3 M1 = M - 1
      AMAX = ZERO
      DO 32 I = 1,M
        IND(I) = I
        IF (DABS(A(I,1))-DABS(AMAX)) 32,32,31
   31   AMAX = A(I,1)
        IMAX = I
   32 CONTINUE
      IF (AMAX.EQ.ZERO) GO TO 60
      DO 41 J = 1,M1
        IF (IMAX-J) 35,35,33
   33   IW = IND(IMAX)
        IND(IMAX) = IND(J)
        IND(J) = IW
        DO 34 K = 1,M
          W = A(IMAX,K)
          A(IMAX,K) = A(J,K)
          A(J,K) = W
   34   CONTINUE
   35   J1 = J + 1
        IF (J.EQ.1) GO TO 38
CIBMD IGNORE RECRDEPS
        DO 37 I = J1,M
          SCPROD = ZERO
          DO 137 IPROD = 1,J - 1
            SCPROD = SCPROD + A(J,IPROD)*A(IPROD,I)
  137     CONTINUE
          A(J,I) = A(J,I) - SCPROD
   37   CONTINUE
   38   DIV = ONE/AMAX
        DO 440 I = J1,M
          A(I,J) = A(I,J)*DIV
  440   CONTINUE
CIBMD IGNORE RECRDEPS
        DO 40 I = J1,M
          SCPROD = ZERO
          DO 139 IPROD = 1,J
            SCPROD = SCPROD + A(I,IPROD)*A(IPROD,J+1)
  139     CONTINUE
          A(I,J+1) = A(I,J+1) - SCPROD
   40   CONTINUE
        AMAX = ZERO
        DO 240 I = J1,M
          IF (DABS(A(I,J1))-DABS(AMAX)) 240,240,239
  239     AMAX = A(I,J1)
          IMAX = I
  240   CONTINUE
        IF (AMAX.EQ.ZERO) GO TO 60
   41 CONTINUE
      DO 13 I1 = 1,M1
        I = M + 1 - I1
        C(I-1) = -A(I,I-1)
        DO 1011 J = I - 2,1,-1
          SCPROD = ZERO
          DO 1109 IPROD = J + 1,I - 1
            SCPROD = SCPROD + A(IPROD,J)*C(IPROD)
 1109     CONTINUE
          C(J) = -A(I,J) - SCPROD
 1011   CONTINUE
        DO 12 K = 1,I - 1
          A(I,K) = C(K)
   12   CONTINUE
   13 CONTINUE
      W = ONE/A(M,M)
      DO 320 J = 1,M - 1
        C(J) = A(M,J)
  320 CONTINUE
      C(M) = ONE
      DO 21 J = 1,M
        A(M,J) = C(J)*W
   21 CONTINUE
      DO 122 I = M - 1,1,-1
        W = ONE/A(I,I)
        DO 120 J = 1,I - 1
          SCPROD = ZERO
          DO 118 IPROD = I + 1,M
            SCPROD = SCPROD + A(I,IPROD)*A(IPROD,J)
  118     CONTINUE
          C(J) = A(I,J) - SCPROD
  120   CONTINUE
        SCPROD = ZERO
        DO 1118 IPROD = I + 1,M
          SCPROD = SCPROD + A(I,IPROD)*A(IPROD,I)
 1118   CONTINUE
        C(I) = ONE - SCPROD
        DO 2120 J = I + 1,M
          SCPROD = ZERO
          DO 2118 IPROD = I + 1,M
            SCPROD = SCPROD + A(I,IPROD)*A(IPROD,J)
 2118     CONTINUE
          C(J) = -SCPROD
 2120   CONTINUE
        DO 121 J = 1,M
          A(I,J) = C(J)*W
  121   CONTINUE
  122 CONTINUE
      DO 26 I = 1,M
   23   IF (IND(I)-I) 24,26,24
   24   J = IND(I)
        DO 25 K = 1,M
          STO = A(K,I)
          A(K,I) = A(K,J)
          A(K,J) = STO
   25   CONTINUE
        ISTO = IND(J)
        IND(J) = J
        IND(I) = ISTO
        GO TO 23

   26 CONTINUE
      GO TO 99

   50 IF (LP.GT.0) WRITE (LP,FMT=55)

   55 FORMAT (51H ERROR RETURN FROM MB01CD BECAUSE M IS NOT POSITIVE)

      IFLAG = 1
      GO TO 99

   60 IF (LP.GT.0) WRITE (LP,FMT=65)

   65 FORMAT (52H ERROR RETURN FROM MB01CD BECAUSE MATRIX IS SINGULAR)

      IFLAG = 2
   99 RETURN

      END
      BLOCK DATA MB01ED
C     .. Common blocks ..
      COMMON /MB01DD/LP,IFLAG
      INTEGER IFLAG,LP
C     ..
C     .. Save statement ..
      SAVE /MB01DD/
C     ..
C     .. Data statements ..
      DATA LP/6/
C     ..
C     .. Executable Statements ..
      END
