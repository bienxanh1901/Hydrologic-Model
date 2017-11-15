C=================================================================
C RESERVOIR_ROUTING
C=================================================================
      SUBROUTINE RESERVOIR_ROUTING(BS, RES, ITER)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(REACH_TYPE), POINTER :: RCH
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      TYPE(SOURCE_TYPE), POINTER :: SRC
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      INTEGER, INTENT(IN) :: ITER
      INTEGER :: I


      RETURN
      END SUBROUTINE RESERVOIR_ROUTING
C=================================================================
C
C=================================================================
C=================================================================
C OUTFLOW_STRUCTURE
C=================================================================
      SUBROUTINE OUTFLOW_STRUCTURE(BS, RES, ITER)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(REACH_TYPE), POINTER :: RCH
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      TYPE(SOURCE_TYPE), POINTER :: SRC
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      INTEGER, INTENT(IN) :: ITER
      INTEGER :: I

      RETURN
      END SUBROUTINE OUTFLOW_STRUCTURE
C=================================================================
C
C=================================================================
      SUBROUTINE GET_RESERVOIR_INFLOW(BS, RES, ITER)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      TYPE(REACH_TYPE), POINTER :: RCH
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      TYPE(SOURCE_TYPE), POINTER :: SRC
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      INTEGER, INTENT(IN) :: ITER
      INTEGER :: I

      RES%INFLOW(ITER) = 0.0D0

      DO I = 1, BS%NSUBBASIN

        SBS => BS%SUBBASIN(I)
        IF(TRIM(SBS%DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

            RES%INFLOW(ITER) = RES%INFLOW(ITER) + SBS%TOTAL_FLOW(ITER)

        ENDIF

      ENDDO

      DO I = 1, BS%NSOURCE

        SRC => BS%SOURCE(I)
        IF(TRIM(SRC%DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

            RES%INFLOW(ITER) = RES%INFLOW(ITER) + SRC%SRC_DATA%GATE_DATA(ITER)

        ENDIF

      ENDDO

      DO I = 1, BS%NREACH

        RCH => BS%REACH(I)
        IF(TRIM(RCH%DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

            RES%INFLOW(ITER) = RES%INFLOW(ITER) + RCH%OUTFLOW(ITER)

        ENDIF

      ENDDO

      RETURN
      END SUBROUTINE GET_RESERVOIR_INFLOW
