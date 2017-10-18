C=================================================================
C MODULE CONTAINS VARIABLES OF FLOOD ROUTING
C=================================================================
      MODULE ROUTING
      IMPLICIT NONE
C STRUCT FOR RIVER FLOOD ROUTING
      TYPE RIVER_ROUTING
        !Connectivity
        INTEGER :: NSRC, NBASE, INP_FLAG
        !Index of source
        INTEGER, ALLOCATABLE, DIMENSION(:) :: SRC, BASE
        !Coefficient
        REAL(8) :: K, X
        !Inflow
        REAL(8), ALLOCATABLE, DIMENSION(:) ::  QINP

      END TYPE RIVER_ROUTING

C STRUCT FOR RESOURCE FLOOD ROUTING
      TYPE RESERVOIR_ROUTING
        !Connectivity
        INTEGER :: NSRC, NBASE, INP_FLAG, QTB_FLAG
        !Index of source
        INTEGER, ALLOCATABLE, DIMENSION(:) :: SRC, BASE
        !Characteristics
        INTEGER :: NVZ, NDC
        REAL(8) :: DOOR_W, DC_COEFF, Z0, ZBT
        !V~Z relation, discharge control, inflow
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: VZ
        REAL(8), ALLOCATABLE, DIMENSION(:) :: QINP, QTB
        !Flood control
        INTEGER :: CTRL_TYPE
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: DC_CTR

      END TYPE RESERVOIR_ROUTING

      !Control variable
      LOGICAL :: ISROUTING
      ! Model for river flood routing
      CHARACTER(100) :: RIVER_MDL
      !Number of positions
      INTEGER :: NRV, NRS, NSTOTAL
      !Type: 1-river, 2-resource
      INTEGER, ALLOCATABLE, DIMENSION(:) :: FRTYPE
      !River routing characteristics
      TYPE(RIVER_ROUTING), ALLOCATABLE, DIMENSION(:) :: RIVER
      !Resource routing characteristics
      TYPE(RESERVOIR_ROUTING), ALLOCATABLE, DIMENSION(:) :: RESERVOIR

      END MODULE ROUTING
C=================================================================
C
C=================================================================
