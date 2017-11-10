C=================================================================
C MODULE CONTAINS OBSERVATION VARIABLES
C=================================================================
      MODULE CONSTANTS
      IMPLICIT NONE

      !definition for data type
      INTEGER, PARAMETER :: CONSTANT_DATA = 1
      INTEGER, PARAMETER :: MONTHLY_DATA = 2
      INTEGER, PARAMETER :: TIME_SERIES_DATA = 3
      !definition for reach routing method
      INTEGER, PARAMETER :: MUSKINGUM_METHOD = 1
      !definition for reservoir routing method
      INTEGER, PARAMETER :: OUTFLOW_STRUCTURE = 1
      !definition for reservoir routing curve
      INTEGER, PARAMETER :: ELEVATION_STORAGE = 1
      INTEGER, PARAMETER :: ELEVATION_AREA = 2
      !definition for discharge control type
      INTEGER, PARAMETER :: DC_DOOR_TYPE = 1
      INTEGER, PARAMETER :: DC_ELEVATION_TYPE = 2
      !definition loss method
      INTEGER, PARAMETER :: SCS_CURVE_LOSS = 1
      !definition transform method
      INTEGER, PARAMETER :: SCS_UHG_TYPE = 1

      !define log file
      INTEGER :: ULOG
      CHARACTER(7) :: FLOG = "RUN.LOG"
      END MODULE CONSTANTS
C=================================================================
C
C=================================================================


