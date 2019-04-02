* COPYRIGHT (c) 1969 AEA Technology
*######DATE 7 Dec 1992
C       Toolpack tool decs employed.
C       Make ZERO and ONE PARAMETER.
C       Change DFLOAT to DBLE.
C       Change arg dimensions to *.
C       Remove MB11CD reference from MB11AD
C       SAVE statements added.
C
C  EAT 21/6/93 EXTERNAL statement put in for block data so will work on VAXs.
C
C
      SUBROUTINE MB11AD(M,N,A,IA,W)
C     PARTITION THE WORKING SPACE ARRAY W
C     THE FIRST PARTITION HOLDS THE FIRST COMPONENTS OF THE VECTORS OF
C     THE ELEMENTARY TRANSFORMATIONS
C     .. Parameters ..
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D0,ZERO=0.0D0)
C     ..
C     .. Scalar Arguments ..
      INTEGER IA,M,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION A(IA,*),W(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AKK,BSQ,RMAX,SIGMA,SUM,WKK
      INTEGER I,IR,J,KK,KP,MMK,N1,N2,N3,N4,N5,N6,NCW,NRW
C     ..
C     .. External Subroutines ..
      EXTERNAL MB11DD,MB11ED,MB11FD
C     ..
C     .. Data block external statement
      EXTERNAL MB11CD
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DABS,DBLE,DSIGN,DSQRT,IDINT
C     ..
C     .. Common blocks ..
      COMMON /MB11BD/LP
      INTEGER LP
C     ..
C     .. Save statement ..
      SAVE /MB11BD/
C     ..
C     .. Executable Statements ..
      NRW = M
C     THE SECOND PARTITION RECORDS ROW INTERCHANGES
      NCW = M + M
C     THE THIRD PARTITION RECORDS COLUMN INTERCHANGES
C     SET THE INITIAL RECORDS OF ROW AND COLUMN INTERCHANGES
      DO 1 I = 1,M
        N1 = NRW + I
        W(N1) = 0.5D0 + DBLE(I)
    1 CONTINUE
      DO 2 I = 1,N
        N1 = NCW + I
        W(N1) = 0.5D0 + DBLE(I)
    2 CONTINUE
C     'KK' COUNTS THE SEPARATE ELEMENTARY TRANSFORMATIONS
      KK = 1
C     FIND LARGEST ROW AND MAKE ROW INTERCHANGES
    3 RMAX = 0.0D0
      DO 4 I = KK,M
        SUM = 0.0D0
        DO 5 J = KK,N
          SUM = SUM + A(I,J)**2
    5   CONTINUE
        IF (RMAX-SUM) 6,4,4
    6   RMAX = SUM
        IR = I
    4 CONTINUE
      IF (RMAX.EQ.0.0D0) GO TO 81
      IF (IR-KK) 7,7,8
    8 N3 = NRW + KK
      SUM = W(N3)
      N4 = NRW + IR
      W(N3) = W(N4)
      W(N4) = SUM
      DO 9 J = 1,N
        SUM = A(KK,J)
        A(KK,J) = A(IR,J)
        A(IR,J) = SUM
    9 CONTINUE
C     FIND LARGEST ELEMENT OF PIVOTAL ROW, AND MAKE COLUMN INTERCHANGES
    7 RMAX = 0.0D0
      SUM = 0.0D0
      DO 10 J = KK,N
        SUM = SUM + A(KK,J)**2
        IF (RMAX-DABS(A(KK,J))) 11,10,10
   11   RMAX = DABS(A(KK,J))
        IR = J
   10 CONTINUE
      IF (IR-KK) 12,12,13
   13 N5 = NCW + KK
      RMAX = W(N5)
      N6 = NCW + IR
      W(N5) = W(N6)
      W(N6) = RMAX
      DO 14 I = 1,M
        RMAX = A(I,KK)
        A(I,KK) = A(I,IR)
        A(I,IR) = RMAX
   14 CONTINUE
C     REPLACE THE PIVOTAL ROW BY THE VECTOR OF THE TRANSFORMATION
   12 SIGMA = DSQRT(SUM)
      BSQ = DSQRT(SUM+SIGMA*DABS(A(KK,KK)))
      W(KK) = DSIGN(SIGMA+DABS(A(KK,KK)),A(KK,KK))/BSQ
      A(KK,KK) = -DSIGN(SIGMA,A(KK,KK))
      KP = KK + 1
      IF (KP-N) 15,15,16
   15 DO 17 J = KP,N
        A(KK,J) = A(KK,J)/BSQ
   17 CONTINUE
C     APPLY THE TRANSFORMATION TO THE REMAINING ROWS OF A
      IF (KP-M) 18,18,16
   18 WKK = W(KK)
      CALL MB11DD(A(KK+1,KK+1),A(KK+1,KK),A(KK,KK+1),IA,WKK,M-KK,N-KK)
      KK = KP
      GO TO 3
C     AT THIS STAGE THE REDUCTION OF A IS COMPLETE
C     NOW WE BUILD UP THE GENERALIZED INVERSE
C     APPLY THE FIRST ELEMENTARY TRANSFORMATION
   16 KK = M
      KP = M + 1
      SUM = -W(M)/A(M,M)
      IF (N-M) 33,33,34
   34 DO 35 J = KP,N
        A(M,J) = SUM*A(M,J)
   35 CONTINUE
   33 A(M,M) = 1.0D0/A(M,M) + SUM*W(M)
C     NOW APPLY THE OTHER (M-1) TRANSFORMATIONS
   36 KP = KK
      KK = KP - 1
      IF (KK) 37,37,38
C     FIRST TRANSFORM THE LAST (M-KK) ROWS
   38 WKK = W(KK)
      CALL MB11ED(A(KK+1,KK+1),A(KK,KK+1),IA,W(KK+1),WKK,M-KK,N-KK)
C     THEN CALCULATE THE NEW ROW IN POSITION KK
      AKK = ONE/A(KK,KK)
      CALL MB11FD(A(KK+1,KK+1),A(KK,KK+1),A(KK+1,KK),IA,WKK,AKK,M-KK,
     +            N-KK)
C     AND REVISE THE COLUMN IN POSITION KK
      SUM = 1.0D0 - WKK**2
      DO 44 I = KP,M
        SUM = SUM - A(I,KK)*W(I)
        A(I,KK) = W(I)
   44 CONTINUE
      A(KK,KK) = SUM/A(KK,KK)
      GO TO 36
C     RESTORE THE ROW INTERCHANGES
   37 DO 45 I = 1,M
   46   N1 = NRW + I
        IR = IDINT(W(N1))
        IF (I-IR) 47,45,45
   47   SUM = W(N1)
        N2 = NRW + IR
        W(N1) = W(N2)
        W(N2) = SUM
        DO 48 J = 1,N
          SUM = A(I,J)
          A(I,J) = A(IR,J)
          A(IR,J) = SUM
   48   CONTINUE
        GO TO 46

   45 CONTINUE
C     RESTORE THE COLUMN INTERCHANGES
      DO 49 J = 1,N
   50   N1 = NCW + J
        IR = IDINT(W(N1))
        IF (J-IR) 51,49,49
   51   SUM = W(N1)
        N2 = NCW + IR
        W(N1) = W(N2)
        W(N2) = SUM
        DO 52 I = 1,M
          SUM = A(I,J)
          A(I,J) = A(I,IR)
          A(I,IR) = SUM
   52   CONTINUE
        GO TO 50

   49 CONTINUE
   80 RETURN

   81 IF (LP.LE.0) GO TO 80
      MMK = M - KK
      WRITE (LP,FMT=82) MMK

   82 FORMAT (1H0,22H *** MB11AD ERROR *** ,I3,8H REDUCED,
     +       22H ROWS FOUND TO BE ZERO)

      STOP

      END
      BLOCK DATA MB11CD
C     .. Common blocks ..
      COMMON /MB11BD/LP
      INTEGER LP
C     ..
C     .. Save statement ..
      SAVE /MB11BD/
C     ..
C     .. Data statements ..
      DATA LP/6/
C     ..
C     .. Executable Statements ..
      END
C@PROCESS DIRECTIVE('IBMD')
      SUBROUTINE MB11DD(A,B,C,IA,WKK,MKK,NKK)
C     .. Scalar Arguments ..
      DOUBLE PRECISION WKK
      INTEGER IA,MKK,NKK
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION A(IA,NKK),B(*),C(IA,NKK)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION SUM
      INTEGER I,J
C     ..
C     .. Executable Statements ..
      DO 19 I = 1,MKK
        SUM = WKK*B(I)
CIBMD PREFER SCALAR
        DO 20 J = 1,NKK
          SUM = SUM + C(1,J)*A(I,J)
   20   CONTINUE
        SUM = -SUM
        B(I) = B(I) + SUM*WKK
CIBMD PREFER SCALAR
        DO 21 J = 1,NKK
          A(I,J) = A(I,J) + SUM*C(1,J)
   21   CONTINUE
   19 CONTINUE
      RETURN

      END
C@PROCESS DIRECTIVE('IBMD')
      SUBROUTINE MB11ED(A,B,IA,W,WKK,MKK,NKK)
C     .. Scalar Arguments ..
      DOUBLE PRECISION WKK
      INTEGER IA,MKK,NKK
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION A(IA,NKK),B(IA,NKK),W(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION SUM
      INTEGER I,J
C     ..
C     .. Executable Statements ..
      DO 39 I = 1,MKK
        SUM = 0.0D0
CIBMD PREFER SCALAR
        DO 40 J = 1,NKK
          SUM = SUM + B(1,J)*A(I,J)
   40   CONTINUE
        SUM = -SUM
CIBMD PREFER SCALAR
        DO 41 J = 1,NKK
          A(I,J) = A(I,J) + SUM*B(1,J)
   41   CONTINUE
        W(I) = SUM*WKK
   39 CONTINUE
      RETURN

      END
C@PROCESS DIRECTIVE('IBMD')
      SUBROUTINE MB11FD(A,B,C,IA,WKK,AKK,MKK,NKK)
C     .. Scalar Arguments ..
      DOUBLE PRECISION AKK,WKK
      INTEGER IA,MKK,NKK
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION A(IA,*),B(IA,*),C(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION SUM
      INTEGER I,J
C     ..
C     .. Executable Statements ..
      DO 42 J = 1,NKK
        SUM = WKK*B(1,J)
        DO 43 I = 1,MKK
          SUM = SUM + C(I)*A(I,J)
   43   CONTINUE
        SUM = -SUM
        B(1,J) = SUM*AKK
   42 CONTINUE
      RETURN

      END
