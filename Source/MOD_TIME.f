C=================================================================
C MODULE TIME VARIABLES
C=================================================================
      MODULE TIME
      USE datetime_module
      IMPLICIT NONE

      !Number of observation times
      INTEGER :: NTIME
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

