C=================================================================
C MODULE CONTAINS VARIABLES OF BASIN
C=================================================================
      MODULE PARAM
      USE datetime_module
      IMPLICIT NONE

C STRUCT FOR BASIN CHARACTERISTICS
      TYPE SUBBASIN_TYPE

        CHARACTER(100) :: NAME
        !Characteristics
        REAL(8) :: AREA, LENGTH, SLOPE
        !Parameter for UHG - LOSS
        REAL(8) :: CN, IMPERVIOUS, TLAG
        !Parameter for NAM model
        REAL(8) :: UMAX, SLMAX, CQOF, CKIF, CK1, TOF, TIF, TG, CKBF
        REAL(8) :: U40, L20, OF0, FQ0, BF0, EP
        !Precipitation gate
        TYPE(GATE_TYPE), POINTER :: PRECIP
        !Base flow
        INTEGER :: BASE_FLOW_TYPE
        REAL(8) :: BF_CONST, BF_MONTHLY(1:12)
        REAL(8), ALLOCATABLE, DIMENSION(:) :: BASE_FLOW
        !Transform method
        INTEGER :: TRANSFORM
        !Array for UHG
        INTEGER :: NUHG
        REAL(8), ALLOCATABLE, DIMENSION(:) :: U
        !Loss method
        INTEGER :: LOSSRATE
        !Output
        REAL(8), ALLOCATABLE, DIMENSION(:) :: LOSS, EXCESS, DIRECT_FLOW, TOTAL_FLOW
        !Downstream
        CHARACTER(100) :: DOWNSTREAM

      END TYPE SUBBASIN_TYPE

C STRUCT FOR REACH
      TYPE REACH_TYPE

        CHARACTER(100) :: NAME
        !Routing method
        INTEGER :: ROUTE
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
        INTEGER :: ROUTE
        !Parameter for reservoir structure method
        REAL(8) :: Z0, DOORW, DC_COEFF, ZBT

        !Storage method
        INTEGER :: ROUTING_CURVE
        !Storage-elevation relation
        INTEGER :: NSE
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: SE_CURVE
        !Discharge control
        INTEGER :: DC_CTRL, NDE
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ED_CURVE
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: EH_CURVE
        INTEGER, ALLOCATABLE, DIMENSION(:) :: NDOOR_OPEN
        !Turbin discharge
        INTEGER :: TB_TYPE
        REAL(8) :: CONSTANT_DATA
        TYPE(GATE_TYPE), POINTER :: TURBIN_DATA
        !Output
        REAL(8), ALLOCATABLE, DIMENSION(:) :: INFLOW, OUTFLOW, STORAGE, ELEVATION
        !Downstream
        CHARACTER(100) :: DOWNSTREAM

      END TYPE RESERVOIR_TYPE

C STRUCT FOR SOURCE
      TYPE SOURCE_TYPE

        CHARACTER(100) :: NAME
        !Source type
        INTEGER :: SRC_TYPE
        !Data
        REAL(8) :: CONST_DATA
        TYPE(GATE_TYPE), POINTER :: SRC_DATA
        !Downstream
        CHARACTER(100) :: DOWNSTREAM

      END TYPE SOURCE_TYPE

C STRUCT FOR  GATE
      TYPE GATE_TYPE

        !Gate name
        CHARACTER(100) :: NAME
        !Type of gate
        INTEGER :: GATETYPE
        TYPE(DATETIME) :: START_TIME, END_TIME
        INTEGER :: NDATA, INTERVAL
        !Data
        CHARACTER(100) :: GATE_FILE
        REAL(8), ALLOCATABLE, DIMENSION(:) :: GATE_DATA

      END TYPE GATE_TYPE

C---------------------------------------------------------------------------------------------------------
C---------------------------------------------------------------------------------------------------------
      TYPE BASIN_TYPE
        !Number of objects
        INTEGER :: NSUBBASIN, NREACH, NRESERVOIR, NSOURCE, NPRECIP, NGATE
        !Object data
        TYPE(SUBBASIN_TYPE), POINTER, DIMENSION(:) :: SUBBASIN
        TYPE(REACH_TYPE), POINTER, DIMENSION(:) :: REACH
        TYPE(RESERVOIR_TYPE), POINTER, DIMENSION(:) :: RESERVOIR
        TYPE(SOURCE_TYPE), POINTER, DIMENSION(:) :: SOURCE
        TYPE(GATE_TYPE), POINTER, DIMENSION(:) :: GATE

      END TYPE BASIN_TYPE

      !Number of BASIN
      INTEGER :: NBASIN
      TYPE(BASIN_TYPE), POINTER, DIMENSION(:) :: BASIN
      !Inout
      CHARACTER(100) :: INPUT_DIR, OUPUT_DIR, ROOT_DIR
      END MODULE PARAM
C=================================================================
C
C=================================================================
