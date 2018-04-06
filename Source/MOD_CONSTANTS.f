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
      INTEGER, PARAMETER :: SPECIFIED_RELEASE = 1
      INTEGER, PARAMETER :: OUTFLOW_STRUCTURE = 2
      !definition for reservoir routing curve
      INTEGER, PARAMETER :: ELEVATION_STORAGE = 1
      INTEGER, PARAMETER :: ELEVATION_AREA = 2
      !definition loss method
      INTEGER, PARAMETER :: SCS_CURVE_LOSS = 1
      !definition transform method
      INTEGER, PARAMETER :: SCS_UHG_TYPE = 1
      !definition for gate
      INTEGER, PARAMETER :: PRECIPITATION_GATE = 1
      INTEGER, PARAMETER :: DISCHARGE_GATE = 2
      INTEGER, PARAMETER :: ELEVATION_OBS = 3

      !definition of simulation type:
      INTEGER, PARAMETER :: VALIDATION_MODE = 1
      INTEGER, PARAMETER :: REAL_TIME_MODE = 2

      !definition of max iteration in REAL_TIME_MODE
      INTEGER, PARAMETER :: MAXITER = 100   ! FOR DT = 15MINUTE ==> OVER 1 DAYS
      !define log file
      INTEGER :: ULOG = 11
      CHARACTER(7) :: FLOG = "RUN.LOG"
      CHARACTER(1):: FILE_PATH
      END MODULE CONSTANTS
C=================================================================
C
C=================================================================


