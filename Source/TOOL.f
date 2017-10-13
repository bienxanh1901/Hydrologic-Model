C=================================================================
C SUBROUTINE INTERPOLATE WITH LAPLACE METHID
C=================================================================
      SUBROUTINE INTERP(Y, X, AX, AY, N)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N
      REAL(8), INTENT(IN) :: Y(1:N), X(1:N), AX
      REAL(8), INTENT(OUT) :: AY
      INTEGER :: I
      REAL(8) :: DX, DY

      AY = 0.0D0
      IF(AX.EQ.X(N)) THEN
        AY = Y(N)
        RETURN
      ENDIF

      DO I = 1, N-1
        IF(AX.EQ.X(I)) THEN
            AY = Y(I)
        ELSEIF(AX.GT.X(I).AND.AX.LT.X(I + 1)) THEN

            DX = X(I + 1) - X(I)
            DY = Y(I + 1) - Y(I)
            AY = (DY/DX)*(AX - X(I)) + Y(I)

        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE INTERP
C=================================================================
C
C=================================================================
