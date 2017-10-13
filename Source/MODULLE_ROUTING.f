C=================================================================
C MODULE CONTAINS VARIABLES OF FLOOD ROUTING
C=================================================================
      MODULE ROUTING
      IMPLICIT NONE
C STRUCT FOR RIVER FLOOD ROUTING
      TYPE RIVER_ROUTING
        !Connectivity
        INTEGER :: NSRC, NBASE, NINF
        !Index of source
        INTEGER, ALLOCATABLE, DIMENSION(:) :: SRC, BASE
        !Coefficient
        REAL(8) :: TX
        !Inflow
        REAL(8), ALLOCATABLE, DIMENSION(:,:) ::  QINF
        !Name of inflow file
        CHARACTER(100) :: INFLOWF

      END TYPE RIVER_ROUTING

C STRUCT FOR RESOURCE FLOOD ROUTING
      TYPE RESEVOIR_ROUTING
        !Connectivity
        INTEGER :: NSRC, NBASE, NINF
        !Index of source
        INTEGER, ALLOCATABLE, DIMENSION(:) :: SRC, BASE
        !Characteristics
        INTEGER :: NVZ, NDC
        REAL(8) :: DOOR_W, DOOR_H, DC_COEFF, QTB, Z0
        !V~Z relation, discharge control, inflow
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: VZ, DC_CTR, QINF
        !Name of inflow file
        CHARACTER(100) :: INFLOWF

      END TYPE RESEVOIR_ROUTING

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
      TYPE(RESEVOIR_ROUTING), ALLOCATABLE, DIMENSION(:) :: RESEVOIR

      END MODULE ROUTING
C=================================================================
C
C=================================================================
