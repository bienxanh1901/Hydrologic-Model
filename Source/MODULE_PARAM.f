C=================================================================
C MODULE CONTAINS VARIABLES OF BASING
C=================================================================
      MODULE PARAM
      IMPLICIT NONE

C STRUCT FOR BASING CHARACTERISTICS
      TYPE SUBBASING_TYPE

        CHARACTER(100) :: NAME
        !Characteristics
        REAL(8) :: AREA, LENGTH, SLOPE
        !Parameter for UHG - LOSS
        REAL(8) :: CN, IMPERVIOUS, TLAG
        !Parameter for NAM model
        REAL(8) :: UMAX, SLMAX, CQOF, CKIF, CK1, TOF, TIF, TG, CKBF
        REAL(8) :: U40, L20, OF0, FQ0, BF0, EP
        !Precipitation gate
        REAL(8), ALLOCATABLE, DIMENSION(:) :: PRECIP
        !Base flow
        INTEGER :: BF_TYPE
        REAL(8) :: BF_CONST, BF_MONTHLY(1:12)
        REAL(8), ALLOCATABLE, DIMENSION(:) :: BASEFLOW
        !Transform method
        INTEGER :: TF_METHOD
        !Loss method
        INTEGER :: LOSS_METHOD
        !Output
        REAL(8), ALLOCATABLE, DIMENSION(:) :: LOSS, EXCESS, DIRECT_FLOW, TOTAL_FLOW
        !Downstream
        CHARACTER(100) :: DOWNSTREAM

      END TYPE SUBBASING_TYPE

C STRUCT FOR REACH
      TYPE REACH_TYPE

        CHARACTER(100) :: NAME
        !Routing method
        INTEGER :: ROUTING_METHOD
        !Parameter for Muskingum method
        REAL(8) :: K, X
        !Output
        REAL(8), ALLOCATABLE, DIMENSION(:) :: INFLOW, OUTFLOW
        !Downstream
        CHARACTER(100) :: DOWNSTREAM

      END TYPE REACH_TYPE

C STRUCT FOR REACH
      TYPE RESERVOIR_TYPE

        CHARACTER(100) :: NAME
        !Routing method
        INTEGER :: ROUTING_METHOD
        !Parameter for reservoir structure method
        REAL(8) :: Z0, DOORW, DC_COEFF, ZBT
        !Storage-elevation relation
        INTEGER :: NSE
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: SE_CURVE
        !Discharge control
        INTEGER :: DC_CTRL, NDE
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: DCE_CURVE
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ORE_CURVE
        INTEGER, ALLOCATABLE, DIMENSION(:) :: NDOOR_OPEN
        !Output
        REAL(8), ALLOCATABLE, DIMENSION(:) :: INFLOW, OUTFLOW, V, Z
        !Downstream
        CHARACTER(100) :: DOWNSTREAM

      END TYPE RESERVOIR_TYPE

C STRUCT FOR SOURCE
      TYPE SOURCE_TYPE

        CHARACTER(100) :: NAME
        !Source type
        INTEGER :: SRC_TYPE
        !Data
        REAL(8), ALLOCATABLE, DIMENSION(:) :: SRC_DATA
        !Downstream
        CHARACTER(100) :: DOWNSTREAM

      END TYPE SOURCE_TYPE


      !Number of objects
      INTEGER :: NSUBBASING, NREACH, NRESERVOIR, NSOURCE
      !Object data
      TYPE(SUBBASING_TYPE), ALLOCATABLE, DIMENSION(:) :: SUBBASING
      TYPE(REACH_TYPE), ALLOCATABLE, DIMENSION(:) :: REACH
      TYPE(RESERVOIR_TYPE), ALLOCATABLE, DIMENSION(:) :: RESERVOIR
      TYPE(SOURCE_TYPE), ALLOCATABLE, DIMENSION(:) :: SOURCE


      END MODULE PARAM
C=================================================================
C
C=================================================================
