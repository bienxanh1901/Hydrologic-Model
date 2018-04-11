C=================================================================
C MODULE TIME VARIABLES
C=================================================================
      MODULE TIME
      USE datetime_module
      IMPLICIT NONE

      !Number of observation times
      INTEGER :: NTIME, NFCT
      TYPE(DATETIME) :: START_TIME, END_TIME, CURRENT_TIME
      TYPE(timedelta) :: DELTAT
      TYPE(DATETIME), POINTER, DIMENSION(:) :: TIME_ARR
      !Time interval
      REAL(8) :: DT
      INTEGER :: CURRENT_IDX

      END MODULE TIME
C=================================================================
C
C=================================================================

C=================================================================
C SUBROUTINE SET DATE AND TIME OF EACH TIME STEP
C=================================================================
      SUBROUTINE SET_DATE_TIME(TSTART, TEND)
      USE COMMON_PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: TSTART, TEND
      INTEGER :: IERR, I

      !Date time format:'dd-mm-yyyy hh:mm'

      START_TIME = strptime(TSTART, '%d-%m-%Y %H:%M')
      CURRENT_TIME = START_TIME
      IF(SIMULATION_MODE.EQ.VALIDATION_MODE) THEN

        END_TIME = strptime(TEND, '%d-%m-%Y %H:%M')
        IF(START_TIME.GT.END_TIME) CALL WRITE_ERRORS('TSTART greater than TEND!')

        DELTAT = END_TIME - START_TIME

        NTIME = INT(DELTAT%total_seconds()/DT) + 1

        ALLOCATE(TIME_ARR(0:NTIME - 1), STAT=IERR)
        CALL ChkMemErr('TIME_ARR', IERR)

        TIME_ARR(0) = START_TIME
        DELTAT = timedelta(0, 0, 0, INT(DT), 0)

        DO I = 1, NTIME - 1

            TIME_ARR(I) = TIME_ARR(I - 1) + DELTAT

        ENDDO

      ELSE

        END_TIME = START_TIME + timedelta(FORECASTING_DURATION, 0, 0, 0, 0)
        NFCT = FORECASTING_DURATION/INT(DT)
        NTIME = NFCT*100
        DELTAT = timedelta(0, 0, 0, INT(DT), 0)

      ENDIF


      RETURN
      END SUBROUTINE SET_DATE_TIME
C=================================================================
C
C=================================================================