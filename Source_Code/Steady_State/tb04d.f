* COPYRIGHT (c) 1970 AEA Technology
*######DATE 2 February 1994
C       Toolpack tool decs employed.
C       DATA made PARAMETER.
C
      SUBROUTINE TB04AD(N,X,F,D,A)
C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION A(*),D(N),F(N),X(N)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION H1,H2,P
      INTEGER I,J,K,NP
C     ..
C     .. Data statements ..
      PARAMETER (NP=6)
C     ..
C     .. Executable Statements ..
C F(I) ARE THE FUNCTION VALUES AT THE POINTS X(I) FOR I=1,N AND
C THE SPLINE DERIVATIVES D(I) ARE FOUND.  THE DIMENSION OF A MUST
C NOT BE LESS THAN 3*N. PERIPHERAL NP MUST BE AN OUTPUT MEDIUM.
      DO 5 I = 2,N
        IF (X(I)-X(I-1)) 1,1,5
    1   WRITE (NP,FMT=3) I

    3   FORMAT (29H RETURN FROM TB04AD BECAUSE X,I5,13H OUT OF ORDER)

        A(1) = 1.0D0
        RETURN

    5 CONTINUE
      DO 30 I = 1,N
        J = 2
        IF (I-1) 6,10,6
    6   J = N - 1
        IF (I.EQ.N) GO TO 10
        H1 = 1.0D0/ (X(I)-X(I-1))
        H2 = 1.0D0/ (X(I+1)-X(I))
        A(3*I-2) = H1
        A(3*I-1) = 2.0D0* (H1+H2)
        A(3*I) = H2
        D(I) = 3.0D0* (F(I+1)*H2*H2+F(I)* (H1*H1-H2*H2)-F(I-1)*H1*H1)
        GO TO 30

   10   H1 = 1.0D0/ (X(J)-X(J-1))
        H2 = 1.0D0/ (X(J+1)-X(J))
        A(3*I-2) = H1*H1
        A(3*I-1) = H1*H1 - H2*H2
        A(3*I) = -H2*H2
        D(I) = 2.D0* (H1*H1*H1* (F(J)-F(J-1))+H2*H2*H2* (F(J)-F(J+1)))
   30 CONTINUE
      P = A(4)/A(1)
      A(5) = A(5) - P*A(2)
      A(6) = A(6) - P*A(3)
      D(2) = D(2) - P*D(1)
      DO 50 I = 3,N
        K = 3*I - 4
        P = A(K+2)/A(K)
        A(K+3) = A(K+3) - P*A(K+1)
        D(I) = D(I) - P*D(I-1)
        IF (I.NE.N-1) GO TO 50
        P = A(K+5)/A(K)
        A(K+5) = A(K+6) - P*A(K+1)
        A(K+6) = A(K+7)
        D(N) = D(N) - P*D(N-2)
   50 CONTINUE
      D(N) = D(N)/A(3*N-1)
      DO 60 I = 3,N
        J = N + 2 - I
   60 D(J) = (D(J)-A(3*J)*D(J+1))/A(3*J-1)
      D(1) = (D(1)-D(2)*A(2)-D(3)*A(3))/A(1)
      A(1) = 0.0D0
      RETURN

      END
