C=================================================================
C MODULE CONTAINS OBSERVATION VARIABLES
C=================================================================
      MODULE CONSTANTS
      IMPLICIT NONE

      !define for data type
      INTEGER, PARAMETER :: CONSTANT_DATA = 1
      INTEGER, PARAMETER :: MONTHLY_DATA = 2
      INTEGER, PARAMETER :: TIME_SERIES_DATA = 3
      !define for reach routing method
      INTEGER, PARAMETER :: MUSKINGUM_METHOD = 1
      !define for discharge control type
      INTEGER, PARAMETER :: DC_DOOR_TYPE = 1
      INTEGER, PARAMETER :: DC_ELEVATION_TYPE = 2
      !define loss method
      INTEGER, PARAMETER :: SCS_CURVE_LOSS = 1
      !define transform method
      INTEGER, PARAMETER :: SCS_UHG_TYPE = 1
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


