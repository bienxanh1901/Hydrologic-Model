C=================================================================
C MODULE CONTAINS OBSERVATION VARIABLES
C=================================================================
      MODULE CONSTANTS
      IMPLICIT NONE

      !define for data type
      INTEGER :: CONSTANT_DATA = 1
      INTEGER :: MONTHLY_DATA = 2
      INTEGER :: TIME_SERIES_DATA = 3
      !define for discharge control type
      INTEGER :: DC_DOOR_TYPE = 1
      INTEGER :: DC_ELEVATION_TYPE = 2
      !Number of observation times
      INTEGER :: NTIME
      CHARACTER(9) :: START_DATE, END_DATE
      CHARACTER(5) :: START_TIME, END_TIME

      !Time interval
      REAL(8) :: DT
      !Date and time table
      CHARACTER(9), ALLOCATABLE, DIMENSION(:) :: CURRENT_DATE
      CHARACTER(5), ALLOCATABLE, DIMENSION(:) :: CURRENT_TIME

      END MODULE CONSTANTS
C=================================================================
C
C=================================================================


