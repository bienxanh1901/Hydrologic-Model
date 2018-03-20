C=================================================================
C MODULE TIME VARIABLES
C=================================================================
      MODULE TIME
      USE datetime_module
      IMPLICIT NONE

      !Number of observation times
      INTEGER :: NTIME
      TYPE(DATETIME) :: START_DATE, END_DATE
      TYPE(DATETIME), POINTER, DIMENSION(:) :: TIME_ARR
      !Time interval
      REAL(8) :: DT

      END MODULE TIME
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE SET DATE AND TIME OF EACH TIME STEP
C=================================================================
      SUBROUTINE SET_DATE_TIME(TSTART, TEND)
      USE CONSTANTS
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: TSTART, TEND
      TYPE(DATETIME) :: TS, TE
      TYPE(timedelta) :: DTIME
      INTEGER :: IERR, I

      !Date time format:'dd-mm-yyyy hh:mm'
      TS = strptime(TSTART, '%d-%m-%Y %H:%M')
      TE = strptime(TEND, '%d-%m-%Y %H:%M')

      DTIME = TE - TS

      NTIME = INT(DTIME%total_seconds()/DT) + 1

      ALLOCATE(TIME_ARR(0:NTIME - 1), STAT=IERR)
      CALL ChkMemErr('TIME_ARR', IERR)

      TIME_ARR(0) = TS
      TIME_ARR(NTIME - 1) = TE
      DTIME = timedelta(0, 0, 0, INT(DT), 0)

      DO I = 1, NTIME - 2

        TIME_ARR(I) = TIME_ARR(I - 1) + DTIME

      ENDDO

      RETURN
      END SUBROUTINE SET_DATE_TIME
C=================================================================
C
C=================================================================
