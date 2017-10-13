C=================================================================
C MODULE CONTAINS VARIABLES OF BASING
C=================================================================
      MODULE BASING
      IMPLICIT NONE
C STRUCT FOR NAM PARAMMETERS
      TYPE NAM_PARAM
        ! 9 parameters
        REAL(8) :: UMAX, SLMAX, CQOF, CKIF,
     &             CK1,TOF, TIF, TG, CKBF
        ! initial value for NAM model
        REAL(8) :: U40, L20, OF0, FQ0, BF0, EP
      END TYPE NAM_PARAM
C STRUCT FOR BASING CHARACTERISTICS
      TYPE BASING_CHAR
        !Characteristics
        REAL(8) :: AREA, LENGTH, SLOPE, CN, Q0
        !number of hydrological stations
        INTEGER :: NSTATS

      END TYPE BASING_CHAR


      !Number of basing
      INTEGER :: NBASING
      !Basing characteristic
      TYPE(BASING_CHAR), ALLOCATABLE, DIMENSION(:) :: BASE
      !variable for NAM model
      TYPE(NAM_PARAM), ALLOCATABLE, DIMENSION(:) :: NAMPRM

      END MODULE BASING
C=================================================================
C
C=================================================================
