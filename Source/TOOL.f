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
C=================================================================
C SUBROUTINE SET DATE AND TIME OF EACH TIME STEP
C=================================================================
      SUBROUTINE SET_DATE_TIME(UHG_TYPE)
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER :: N
      REAL(8) :: T

      ALLOCATE(CURRENT_DATE(0:NTIME - 1))
      ALLOCATE(CURRENT_TIME(0:NTIME - 1))

      CURRENT_DATE(0) = TRIM(START_DATE)
      CURRENT_TIME(0) = TRIM(START_TIME)
      CURRENT_DATE(NTIME - 1) = TRIM(END_DATE)
      CURRENT_DATE(NTIME - 1) = TRIM(END_TIME)

      T = 0

      DO N = 1, NTIME - 2

        T = T + DT

      ENDDO
      RETURN
      END SUBROUTINE GET_UHG
C=================================================================
C
C=================================================================
