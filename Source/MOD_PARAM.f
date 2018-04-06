C=================================================================
C MODULE CONTAINS VARIABLES OF BASIN
C=================================================================
      MODULE PARAM
      USE datetime_module
      USE GATE_MOD
      USE SUBBASIN_MOD
      USE REACH_MOD
      USE RESERVOIR_MOD
      USE SOURCE_MOD
      USE BASIN_MOD
      IMPLICIT NONE

      !Number of BASIN
      INTEGER :: NBASIN
      INTEGER :: SIMULATION_MODE
      INTEGER :: FORECASTING_DURATION
      TYPE(BASIN_TYPE), POINTER, DIMENSION(:) :: BASIN
      !Inout
      CHARACTER(100) :: INPUT_DIR, OUTPUT_DIR, ROOT_DIR
      END MODULE PARAM
C=================================================================
C
C=================================================================

C=================================================================
C SUBROUTINE SET DATE AND TIME OF EACH TIME STEP
C=================================================================
      SUBROUTINE SET_DATE_TIME(TSTART, TEND)
      USE PARAM
      USE CONSTANTS
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: TSTART, TEND
      INTEGER :: IERR, I

      !Date time format:'dd-mm-yyyy hh:mm'

      START_TIME = strptime(TSTART, '%d-%m-%Y %H:%M')
      IF(SIMULATION_MODE.EQ.VALIDATION_MODE) THEN

        END_TIME = strptime(TEND, '%d-%m-%Y %H:%M')
        IF(START_TIME.GT.END_TIME) CALL WRITE_ERRORS('TSTART greater than TEND!')

        DELTAT = END_TIME - START_TIME

        NTIME = INT(DELTAT%total_seconds()/DT) + 1

      ELSE

        END_TIME = START_TIME
        NTIME = FORECASTING_DURATION/INT(DT)*2

      ENDIF

      ALLOCATE(TIME_ARR(0:NTIME - 1), STAT=IERR)
      CALL ChkMemErr('TIME_ARR', IERR)

      TIME_ARR(0) = START_TIME
      DELTAT = timedelta(0, 0, 0, INT(DT), 0)

      DO I = 1, NTIME - 1

        TIME_ARR(I) = TIME_ARR(I - 1) + DELTAT

      ENDDO

      RETURN
      END SUBROUTINE SET_DATE_TIME
C=================================================================
C
C=================================================================
