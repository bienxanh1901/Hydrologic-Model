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

      !define log file
      INTEGER :: ULOG
      CHARACTER(7) :: FLOG = "RUN.LOG"
      END MODULE CONSTANTS
C=================================================================
C
C=================================================================


