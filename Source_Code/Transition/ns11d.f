* COPYRIGHT (c) 1993 AEA Technology and 
* Council for the Central Laboratory of the Research Councils
C       Toolpack tool decs employed.
C       NS11AD a modified version of NS01AD with integer workspace
C         argument IW added.
C       SAVE statements added.
C       NS11CD reference removed.
C       Arg dimensions set to *.
C       DFLOAT -> DBLE.
C       Modified for obsolescent features (Feb 1997)
C
C  EAT 21/6/93 EXTERNAL statement put in for block data so will work on
C
C
      SUBROUTINE NS11AD(CALFUN,N,X,F,AJINV,DSTEP,DMAX,ACC,MAXFUN,
     +							                        IPR,W,IW)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ACC,DMAX,DSTEP
      INTEGER IPR,MAXFUN,N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION AJINV(N,N),F(*),W(*),X(*)
      INTEGER IW(N)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ANMULT,DD,DM,DMM,DMULT,DN,DS,DSS,DTEST,DW,FMIN,
     +                 FNP,FSQ,PA,PJ,SP,SPNS,SS,TINC
      INTEGER I,IC,IPRINT,IS,ITER,J,K,KK,KS,MAXC,MW,ND,NDC,NF,NT,NTEST,
     +        NW,NX
C     ..
C     .. External Subroutines ..
      EXTERNAL CALFUN,MB01CD
C     ..
C     .. Data block external statement
      EXTERNAL NS11CD
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DABS,DBLE,DMAX1,DMIN1,DSQRT,MOD
C     ..
C     .. Common blocks ..
      COMMON /NS11BD/LP,LPD,IERR
      INTEGER IERR,LP,LPD
C     ..
C     .. Save statement ..
      SAVE /NS11BD/
C     ..
C     .. Executable Statements ..
C     SET VARIOUS PARAMETERS
      MAXC = 0
      IPRINT = IPR
      IERR = 0
      ITER = 0
      IF (LP.LE.0) IPRINT = 0
C     'MAXC' COUNTS THE NUMBER OF CALLS OF CALFUN
      NT = N + 4
      NTEST = NT
C    'NT' AND 'NTEST' CAUSE AN ERROR RETURN IF F(X) DOES NOT DECREASE
      DTEST = DBLE(N+N) - 0.5
C     'DTEST' IS USED TO MAINTAIN LINEAR INDEPENDENCE
      NX = N*N
      NF = NX + N
      NW = NF + N
      MW = NW + N
      NDC = MW + N
      ND = NDC + N
C     THESE PARAMETERS SEPARATE THE WORKING SPACE ARRAY W
      FMIN = 0.0D0
C     USUALLY 'FMIN' IS THE LEAST CALCULATED VALUE OF F(X),
C     AND THE BEST X IS IN W(NX+1) TO W(NX+N)
      DD = 0.0D0
C     USUALLY DD IS THE SQUARE OF THE CURRENT STEP LENGTH
      DSS = DSTEP*DSTEP
      DM = DMAX*DMAX
      DMM = 4.0D0*DM
      IS = 5
C     'IS' CONTROLS A 'GO TO' STATEMENT FOLLOWING A CALL OF CALFUN
      TINC = 1.0D0
C     'TINC' IS USED IN THE CRITERION TO INCREASE THE STEP LENGTH
C     START A NEW PAGE FOR PRINTING
      IF (IPRINT.NE.0) WRITE (LP,FMT=86)

   86 FORMAT ('1')
C     CALL THE SUBROUTINE CALFUN
    1 MAXC = MAXC + 1
      CALL CALFUN(N,X,F)
C     TEST FOR CONVERGENCE
      FSQ = 0.0D0
      DO 2 I = 1,N
        FSQ = FSQ + F(I)*F(I)
    2 CONTINUE
      IF(FSQ-ACC.GT.0) GO TO 4
C     PROVIDE PRINTING OF FINAL SOLUTION IF REQUESTED
    3 IF (IPRINT.EQ.0) GO TO 5
      WRITE (LP,FMT=7) MAXC

    7 FORMAT (/,/,/,5X,'THE FINAL SOLUTION CALCULATED BY NS11A REQUIRED'
     +       ,I5,' CALLS OF CALFUN, AND IS')

      WRITE (LP,FMT=8) (I,X(I),F(I),I=1,N)

    8 FORMAT (/,/,4X,'I',7X,'X(I)',12X,'F(I)',/,/, (I5,2D24.16))

      WRITE (LP,FMT=9) FSQ

    9 FORMAT (/,5X,'THE SUM OF SQUARES IS',D24.16)

    5 RETURN
C     TEST FOR ERROR RETURN BECAUSE F(X) DOES NOT DECREASE
    4 GO TO (10,11,11,10,11) IS

   10 IF(FSQ-FMIN.LT.0) GO TO 15
      IF(DD-DSS.GT.0) GO TO 11
      NTEST = NTEST - 1
      IF(NTEST.LT.0) GO TO 13
      IF(NTEST.GT.0) GO TO 11
      IF (LPD.GT.0) WRITE (LPD,FMT=16) NT
      IERR = 1

   16 FORMAT (/,/,/,5X,'ERROR RETURN FROM NS11A BECAUSE',I5,
     +       ' CALLS OF CALFUN FAILED TO IMPROVE THE RESIDUALS')

   17 DO 18 I = 1,N
        X(I) = W(NX+I)
        F(I) = W(NF+I)
   18 CONTINUE
      FSQ = FMIN
      GO TO 3
C     ERROR RETURN BECAUSE A NEW JACOBIAN IS UNSUCCESSFUL
   13 IF (LPD.GT.0) WRITE (LPD,FMT=19)
      IERR = 2

   19 FORMAT (/,/,/,5X,'ERROR RETURN FROM NS11A BECAUSE F(X) ',
     +       'FAILED TO DECREASE USING A NEW JACOBIAN')

      GO TO 17

   15 NTEST = NT
C     TEST WHETHER THERE HAVE BEEN MAXFUN CALLS OF CALFUN
   11 IF(MAXFUN-MAXC.GT.0) GO TO 22
      IF (LPD.GT.0) WRITE (LPD,FMT=23) MAXC
      IERR = 3

   23 FORMAT (/,/,/,5X,'ERROR RETURN FROM NS11A BECAUSE THERE HAVE BEEN'
     +       ,I5,' CALLS OF CALFUN')

      IF(FSQ-FMIN.LT.0) GO TO 3
      GO TO 17
C     PROVIDE PRINTING IF REQUESTED
   22 IF (IPRINT.LE.0) GO TO 24
      IF (MOD(MAXC,IPRINT).NE.0) GO TO 24
      WRITE (LP,FMT=26) MAXC

   26 FORMAT (/,/,/,5X,'AT THE',I5,' TH CALL OF CALFUN WE HAVE')

      WRITE (LP,FMT=8) (I,X(I),F(I),I=1,N)
      WRITE (LP,FMT=9) FSQ
   24 GO TO (27,28,29,87,30) IS
C     STORE THE RESULT OF THE INITIAL CALL OF CALFUN
   30 FMIN = FSQ
      DO 31 I = 1,N
        W(NX+I) = X(I)
        W(NF+I) = F(I)
   31 CONTINUE
C     CALCULATE A NEW JACOBIAN APPROXIMATION
   32 IC = 0
      IS = 3
   33 IC = IC + 1
      X(IC) = X(IC) + DSTEP
      GO TO 1

   29 K = IC
      DO 34 I = 1,N
        W(K) = (F(I)-W(NF+I))/DSTEP
        K = K + N
   34 CONTINUE
      X(IC) = W(NX+IC)
      IF(IC-N.LT.0) GO TO 33
C     CALCULATE THE INVERSE OF THE JACOBIAN AND SET THE DIRECTION MATRIX
      K = 0
      DO 36 I = 1,N
        DO 37 J = 1,N
          K = K + 1
          AJINV(I,J) = W(K)
          W(ND+K) = 0.0D0
   37   CONTINUE
        W(NDC+K+I) = 1.0D0
        W(NDC+I) = 1.0D0 + DBLE(N-I)
   36 CONTINUE
      CALL MB01CD(AJINV,N,N,IW,F)
C     START ITERATION BY PREDICTING THE DESCENT AND NEWTON MINIMA
   38 DS = 0.0D0
      DN = 0.0D0
      SP = 0.0D0
      ITER = ITER + 1
      IF (IPRINT.GE.0) GO TO 6
      IF (MOD(ITER,-IPRINT).NE.0) GO TO 6
      WRITE (LP,FMT=25) ITER

   25 FORMAT (/,/,/,5X,'AT THE',I5,
     + ' TH ITERATION THE LEAST CALCULATED SUM OF SQUARES IS AS FOLLOWS'
     +       )

      WRITE (LP,FMT=8) (I,W(NX+I),W(NF+I),I=1,N)
      WRITE (LP,FMT=9) FMIN
    6 DO 39 I = 1,N
        X(I) = 0.0D0
        F(I) = 0.0D0
        K = I
        DO 40 J = 1,N
          X(I) = X(I) - W(K)*W(NF+J)
          F(I) = F(I) - AJINV(I,J)*W(NF+J)
          K = K + N
   40   CONTINUE
        DS = DS + X(I)*X(I)
        DN = DN + F(I)*F(I)
        SP = SP + X(I)*F(I)
   39 CONTINUE
C     TEST WHETHER A NEARBY STATIONARY POINT IS PREDICTED
      IF(FMIN*FMIN-DMM*DS.LE.0) GO TO 41
C     IF SO THEN RETURN OR REVISE JACOBIAN
      GO TO (43,43,44) IS

   44 IF (LPD.GT.0) WRITE (LPD,FMT=45)
      IERR = 4

   45 FORMAT (/,/,/,5X,'ERROR RETURN FROM NS11A BECAUSE A NEARBY ',
     +       'STATIONARY POINT OF F(X) IS PREDICTED')

      GO TO 17

   43 NTEST = 0
      DO 46 I = 1,N
        X(I) = W(NX+I)
   46 CONTINUE
      GO TO 32
C     TEST WHETHER TO APPLY THE FULL NEWTON CORRECTION
   41 IS = 2
      IF(DN-DD.GT.0) GO TO 48
      DD = DMAX1(DN,DSS)
      DS = 2.5D-1*DN
      TINC = 1.0D0
      IF(DN-DSS.GE.0) GO TO 58
      IS = 4
      GO TO 80
C     CALCULATE THE LENGTH OF THE STEEPEST DESCENT STEP
   48 K = 0
      DMULT = 0.0D0
      DO 51 I = 1,N
        DW = 0.0D0
        DO 52 J = 1,N
          K = K + 1
          DW = DW + W(K)*X(J)
   52   CONTINUE
        DMULT = DMULT + DW*DW
   51 CONTINUE
      DMULT = DS/DMULT
      DS = DS*DMULT*DMULT
C     TEST WHETHER TO USE THE STEEPEST DESCENT DIRECTION
      IF(DS-DD.LT.0) GO TO 53
C     TEST WHETHER THE INITIAL VALUE OF DD HAS BEEN SET
      IF(DD.GT.0) GO TO 56
      DD = DMAX1(DSS,DMIN1(DM,DS))
      DS = DS/ (DMULT*DMULT)
      GO TO 41
C     SET THE MULTIPLIER OF THE STEEPEST DESCENT DIRECTION
   56 ANMULT = 0.0D0
      DMULT = DMULT*DSQRT(DD/DS)
      GO TO 98
C     INTERPOLATE BETWEEN THE STEEPEST DESCENT AND THE NEWTON DIRECTIONS
   53 SP = SP*DMULT
      ANMULT = (DD-DS)/ ((SP-DS)+DSQRT((SP-DD)**2+ (DN-DD)* (DD-DS)))
      DMULT = DMULT* (1.0D0-ANMULT)
C     CALCULATE THE CHANGE IN X AND ITS ANGLE WITH THE FIRST DIRECTION
   98 DN = 0.0D0
      SP = 0.0D0
      DO 57 I = 1,N
        F(I) = DMULT*X(I) + ANMULT*F(I)
        DN = DN + F(I)*F(I)
        SP = SP + F(I)*W(ND+I)
   57 CONTINUE
      DS = 2.5D-1*DN
C     TEST WHETHER AN EXTRA STEP IS NEEDED FOR INDEPENDENCE
      IF(W(NDC+1)-DTEST.LE.0) GO TO 58
      IF(SP*SP-DS.LT.0) GO TO 60
      GO TO 58
C     TAKE THE EXTRA STEP AND UPDATE THE DIRECTION MATRIX
   50 IS = 2
   60 DO 61 I = 1,N
        X(I) = W(NX+I) + DSTEP*W(ND+I)
        W(NDC+I) = W(NDC+I+1) + 1.0D0
   61 CONTINUE
      W(ND) = 1.0D0
      DO 62 I = 1,N
        K = ND + I
        SP = W(K)
        IF (N.LT.2) GO TO 62
        DO 63 J = 2,N
          W(K) = W(K+N)
          K = K + N
   63   CONTINUE
        W(K) = SP
   62 CONTINUE
      GO TO 1
C     EXPRESS THE NEW DIRECTION IN TERMS OF THOSE OF THE DIRECTION
C     MATRIX, AND UPDATE THE COUNTS IN W(NDC+1) ETC.
   58 SP = 0.0D0
      K = ND
      DO 64 I = 1,N
        X(I) = DW
        DW = 0.0D0
        DO 65 J = 1,N
          K = K + 1
          DW = DW + F(J)*W(K)
   65   CONTINUE
        GO TO (68,66) IS

   66   W(NDC+I) = W(NDC+I) + 1.0D0
        SP = SP + DW*DW
        IF(SP-DS.LE.0) GO TO 64
        IS = 1
        KK = I
        X(1) = DW
        GO TO 69

   68   X(I) = DW
   69   W(NDC+I) = W(NDC+I+1) + 1.0D0
   64 CONTINUE
      W(ND) = 1.0D0
C     REORDER THE DIRECTIONS SO THAT KK IS FIRST
      IF(KK-1.LE.0) GO TO 70
      KS = NDC + KK*N
      DO 72 I = 1,N
        K = KS + I
        SP = W(K)
        DO 73 J = 2,KK
          W(K) = W(K-N)
          K = K - N
   73   CONTINUE
        W(K) = SP
   72 CONTINUE
C     GENERATE THE NEW ORTHOGONAL DIRECTION MATRIX
   70 DO 74 I = 1,N
        W(NW+I) = 0.0D0
   74 CONTINUE
      SP = X(1)*X(1)
      K = ND
      IF (N.LT.2) GO TO 99
      DO 75 I = 2,N
        DS = DSQRT(SP* (SP+X(I)*X(I)))
        DW = SP/DS
        DS = X(I)/DS
        SP = SP + X(I)*X(I)
        DO 76 J = 1,N
          K = K + 1
          W(NW+J) = W(NW+J) + X(I-1)*W(K)
          W(K) = DW*W(K+N) - DS*W(NW+J)
   76   CONTINUE
   75 CONTINUE
   99 SP = 1.0D0/DSQRT(DN)
      DO 77 I = 1,N
        K = K + 1
        W(K) = SP*F(I)
   77 CONTINUE
C     CALCULATE THE NEXT VECTOR X, AND PREDICT THE RIGHT HAND SIDES
   80 FNP = 0.0D0
      K = 0
      DO 78 I = 1,N
        X(I) = W(NX+I) + F(I)
        W(NW+I) = W(NF+I)
        DO 79 J = 1,N
          K = K + 1
          W(NW+I) = W(NW+I) + W(K)*F(J)
   79   CONTINUE
        FNP = FNP + W(NW+I)**2
   78 CONTINUE
C     CALL CALFUN USING THE NEW VECTOR OF VARIABLES
      GO TO 1
C     UPDATE THE STEP SIZE
   27 DMULT = 9.0D-1*FMIN + 1.0D-1*FNP - FSQ
      IF(DMULT.GE.0) GO TO 81
      DD = DMAX1(DSS,2.5D-1*DD)
      TINC = 1.0D0
      IF(FSQ-FMIN.LT.0) GO TO 83
      GO TO 28
C     TRY THE TEST TO DECIDE WHETHER TO INCREASE THE STEP LENGTH
   81 SP = 0.0D0
      SS = 0.0D0
      DO 84 I = 1,N
        SP = SP + DABS(F(I)* (F(I)-W(NW+I)))
        SS = SS + (F(I)-W(NW+I))**2
   84 CONTINUE
      PJ = 1. + DMULT/ (SP+DSQRT(SP*SP+DMULT*SS))
      SPNS = 4.0D0
      SP = DMIN1(SPNS,TINC,PJ)
      TINC = PJ/SP
      DD = DMIN1(DM,SP*DD)
      GO TO 83
C     IF F(X) IMPROVES STORE THE NEW VALUE OF X
   87 IF(FSQ-FMIN.GE.0) GO TO 50
   83 FMIN = FSQ
      DO 88 I = 1,N
        SP = X(I)
        X(I) = W(NX+I)
        W(NX+I) = SP
        SP = F(I)
        F(I) = W(NF+I)
        W(NF+I) = SP
        W(NW+I) = -W(NW+I)
   88 CONTINUE
      IF(IS-1.GT.0) GO TO 50
C     CALCULATE THE CHANGES IN F AND IN X
   28 DO 89 I = 1,N
        X(I) = X(I) - W(NX+I)
        F(I) = F(I) - W(NF+I)
   89 CONTINUE
C     UPDATE THE APPROXIMATIONS TO J AND TO AJINV
      K = 0
      DO 90 I = 1,N
        W(MW+I) = X(I)
        W(NW+I) = F(I)
        DO 91 J = 1,N
          W(MW+I) = W(MW+I) - AJINV(I,J)*F(J)
          K = K + 1
          W(NW+I) = W(NW+I) - W(K)*X(J)
   91   CONTINUE
   90 CONTINUE
      SP = 0.0D0
      SS = 0.0D0
      DO 92 I = 1,N
        DS = 0.0D0
        DO 93 J = 1,N
          DS = DS + AJINV(J,I)*X(J)
   93   CONTINUE
        SP = SP + DS*F(I)
        SS = SS + X(I)*X(I)
        F(I) = DS
   92 CONTINUE
      DMULT = 1.0D0
      IF(DABS(SP)-0.1*SS.GE.0) GO TO 95
      DMULT = 8.0D-1
   95 PJ = DMULT/SS
      PA = DMULT/ (DMULT*SP+ (1.-DMULT)*SS)
      K = 0
      DO 96 I = 1,N
        SP = PJ*W(NW+I)
        SS = PA*W(MW+I)
        DO 97 J = 1,N
          K = K + 1
          W(K) = W(K) + SP*X(J)
          AJINV(I,J) = AJINV(I,J) + SS*F(J)
   97   CONTINUE
   96 CONTINUE
      GO TO 38

      END
      BLOCK DATA NS11CD
C     .. Common blocks ..
      COMMON /NS11BD/LP,LPD,IERR
      INTEGER IERR,LP,LPD
C     ..
C     .. Save statement ..
      SAVE /NS11BD/
C     ..
C     .. Data statements ..
      DATA LP/6/,LPD/6/
C     ..
C     .. Executable Statements ..
      END
