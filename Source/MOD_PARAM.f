C=================================================================
C MODULE CONTAINS VARIABLES OF BASIN
C=================================================================
      MODULE CALC_PARAM
      USE datetime_module
      USE GATE_MOD
      USE SUBBASIN_MOD
      USE REACH_MOD
      USE RESERVOIR_MOD
      USE SOURCE_MOD
      USE BASIN_MOD
      IMPLICIT NONE

      !Number of BASIN
      INTEGER :: NBASIN
      TYPE(BASIN_TYPE), POINTER, DIMENSION(:) :: BASIN
      END MODULE CALC_PARAM
C=================================================================
C
C=================================================================


