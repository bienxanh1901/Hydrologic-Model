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
      SUBROUTINE SET_DATE_TIME(UHG_TYPE)
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER :: N
      REAL(8) :: T
      CHARACTER(8) :: D
      CHARACTER(5) :: T
      INTEGER:: HOUR, MINUTE, DAY, MONTH, YEAR, MONTH1
      LOGICAL :: LEAP_YEAR

      !Date: ddmmyyyy
      !Time: hh:mm
      ALLOCATE(CDATE(0:NTIME - 1))
      ALLOCATE(CTIME(0:NTIME - 1))

      CDATE(0) = TRIM(START_DATE)
      CTIME(0) = TRIM(START_TIME)
      CDATE(NTIME - 1) = TRIM(END_DATE)
      CTIME(NTIME - 1) = TRIM(END_TIME)


      READ(START_TIME(1:2),'(I2)') HOUR
      READ(START_TIME(4:5),'(I2)') MINUTE
      READ(START_DATE(1:2),'(I2)') DAY
      READ(START_DATE(3:4),'(I2)') MONTH
      READ(START_DATE(5:8),'(I4)') YEAR

      DO N = 1, NTIME - 2

        T = ''
        D = ''
        HOUR = HOUR + INT(DT/3600)
        MINUTE = MINUTE + INT(MOD(DT,3600)/60)

        !Increase 1 hour
        IF(MINUTE.GT.60) THEN

            HOUR = HOUR + 1
            MINUTE = MINUTE - 60

        ENDIF

        !Increase one day
        IF(HOUR.GT.23) THEN

            DAY = DAY + 1
            HOUR = HOUR - 23

        ENDIF

        !Increase month and year
        MONTH1 = MONTH
        MCASE: SELECT CASE(MONTH1)
            CASE(1,3,5,7,8,10,12)
                IF(DAY.GT.31) THEN

                    DAY = 1
                    IF(MONTH1 = 12) THEN
                        MONTH  = 1
                        YEAR = YEAR + 1
                    ELSE
                        MONTH = MONTH + 1
                    ENDIF

                ENDIF
            CASE(4,6,9,11)
                IF(DAY.GT.30) THEN
                    DAY = 1
                    MONTH = MONTH + 1
                ENDIF
            CASE(2)
                LEAP_YEAR = (MOD(YEAR,400).EQ.0).OR.
     &                      (MOD(YEAR,4).EQ.0.AND.MOD(YEAR,100).NE.0)
                IF((LEAP_YEAR.AND.DAY.GT.29).OR.(.NOT.LEAP_YEAR.AND.DAY.GT.28)) THEN

                    DAY = 1
                    MONTH = MONTH + 1

                ENDIF
            CASE DEFAULT

                PRINT*, 'Invalid month!!!'
                STOP

        END SELECT MCASE

        ! Write date and time
        WRITE(T,'(I2.2,A1,I2.2)') HOUR, ':', MINUTE
        CTIME(N) = T

        WRITE(D,'(I2.2,I2.2,I4.4)') DAY, MONTH, YEAR
        CDATE(N) = D

      ENDDO

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
