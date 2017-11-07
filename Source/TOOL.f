C=================================================================
C SUBROUTINE INTERPOLATE WITH LAPLACE METHOD
C=================================================================
      SUBROUTINE INTERP(X, Y, AX, AY, N)
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
      SUBROUTINE SET_DATE_TIME
      USE CONSTANTS
      IMPLICIT NONE

      !Date: ddmmyyyy
      !Time: hh:mm
      ALLOCATE(CDATE(0:NTIME - 1))
      ALLOCATE(CTIME(0:NTIME - 1))


      RETURN
      END SUBROUTINE SET_DATE_TIME
C=================================================================
C
C=================================================================
C=================================================================
C FUNCTION CHECK FILE
C=================================================================
      SUBROUTINE CHK_FILE(FILE_NAME)
      IMPLICIT NONE
      CHARACTER(*) :: FILE_NAME
      LOGICAL :: EX


      INQUIRE(FILE=TRIM(FILE_NAME), EXIST=EX)
      IF(.NOT.EX) THEN
        WRITE(*,*) "ERROR!!!"
        WRITE(*,'(3A)')"File ", TRIM(FILE_NAME), " does not exist in the current directory!!! "
        STOP
      ENDIF

      RETURN
      END SUBROUTINE CHK_FILE
C=================================================================
C
C=================================================================
