C=================================================================
C MODULE TIME VARIABLES
C=================================================================
      MODULE TIME
      USE datetime_module
      IMPLICIT NONE

      !Number of observation times
      INTEGER :: NTIME
      TYPE(DATETIME) :: START_DATE, END_DATE
      !Time interval
      REAL(8) :: DT
      !Date and time table
      CHARACTER(8), ALLOCATABLE, DIMENSION(:) :: ARR_TIME

      END MODULE TIME
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE SET DATE AND TIME OF EACH TIME STEP
C=================================================================
      SUBROUTINE SET_DATE_TIME(TSTART, TEND)
      USE CONSTANTS
      USE datetime_module
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: TSTART, TEND

      !Date: ddmmyyyy
      !Time: hh:mm


      RETURN
      END SUBROUTINE SET_DATE_TIME
C=================================================================
C
C=================================================================
