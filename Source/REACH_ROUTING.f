C=================================================================
C REACH_ROUTING
C=================================================================
      SUBROUTINE REACH_ROUTING(BS, RCH, ITER)
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
      END SUBROUTINE REACH_ROUTING
C=================================================================
C
C=================================================================
C=================================================================
C MUSKINGUM_CALC
C=================================================================
      SUBROUTINE MUSKINGUM_CALC(BS, RCH, ITER)
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
      END SUBROUTINE MUSKINGUM_CALC
C=================================================================
C
C=================================================================
      SUBROUTINE GET_REACH_INFLOW(BS, RCH, ITER)
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

      RCH%INFLOW(ITER) = 0.0D0

      DO I = 1, BS%NSUBBASIN

        SBS => BS%SUBBASIN(I)
        IF(TRIM(SBS%DOWNSTREAM).EQ.TRIM(RCH%NAME)) THEN

            RCH%INFLOW(ITER) = RCH%INFLOW(ITER) + SBS%TOTAL_FLOW(ITER)

        ENDIF

      ENDDO

      DO I = 1, BS%NSOURCE

        SRC => BS%SOURCE(I)
        IF(TRIM(SRC%DOWNSTREAM).EQ.TRIM(RCH%NAME)) THEN

            RCH%INFLOW(ITER) = RCH%INFLOW(ITER) + SRC%SRC_DATA%GATE_DATA(ITER)

        ENDIF

      ENDDO

      DO I = 1, BS%NRESERVOIR

        RES => BS%RESERVOIR(I)
        IF(TRIM(RES%DOWNSTREAM).EQ.TRIM(RCH%NAME)) THEN

            RCH%INFLOW(ITER) = RCH%INFLOW(ITER) + RES%OUTFLOW(ITER)

        ENDIF

      ENDDO

      RETURN
      END SUBROUTINE GET_REACH_INFLOW

