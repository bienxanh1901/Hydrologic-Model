C=================================================================
C MODULE CONTAINS OBSERVATION VARIABLES
C=================================================================
      MODULE OBSERVATION
      IMPLICIT NONE

      !Number of observation times
      INTEGER :: NTIME

      !Time interval
      REAL(8) :: DT

      !Precipitation observation
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: XF

      END MODULE OBSERVATION
C=================================================================
C
C=================================================================
C=================================================================
C MODULE CONTAINS OUTPUT VARIABLES
C=================================================================
      MODULE CALC
      IMPLICIT NONE

      ! type of watershed calculation method
      CHARACTER(10) :: MODEL
      !in-out flow
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: QF, QDC, QIN, LOSS, EXCESS
      !resource/power plan state
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: V, ZH


      END MODULE CALC
C=================================================================
C
C=================================================================

