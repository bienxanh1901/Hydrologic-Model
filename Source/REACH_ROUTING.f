C=================================================================
C REACH_ROUTING
C=================================================================
      SUBROUTINE REACH_ROUTING(RCH, ITER)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE MUSKINGUM_CALC(RCH, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(REACH_TYPE), POINTER :: RCH
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE MUSKINGUM_CALC
      END INTERFACE
      TYPE(REACH_TYPE), POINTER :: RCH
      INTEGER, INTENT(IN) :: ITER

      SELECT CASE(RCH%ROUTE)
        CASE(MUSKINGUM_METHOD)
            CALL MUSKINGUM_CALC(RCH, ITER)
        CASE DEFAULT
            WRITE(*,*) 'Error: Invalid type of Reach routing method!!!'
            STOP
      END SELECT

      RETURN
      END SUBROUTINE REACH_ROUTING
C=================================================================
C
C=================================================================
C=================================================================
C MUSKINGUM_CALC
C=================================================================
      SUBROUTINE MUSKINGUM_CALC(RCH, ITER)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(REACH_TYPE), POINTER :: RCH
      INTEGER, INTENT(IN) :: ITER
      INTEGER :: N, I
      REAL(8) :: QIT1, QIT2, DT1, QOT1, QOT2
      REAL(8) :: C0, C1, C2, C3

      DT1 = DT
      QIT1 = (RCH%INFLOW(ITER - 1) - RCH%LOSS_VALUE)*(1.0D0 - RCH%LOSS_RATIO)
      IF(QIT1.LT.0) QIT1 = 0.0D0
      QIT2 = (RCH%INFLOW(ITER) - RCH%LOSS_VALUE)*(1.0D0 - RCH%LOSS_RATIO)
      IF(QIT2.LT.0) QIT2 = 0.0D0
      QOT1 = RCH%OUTFLOW(ITER - 1)
      QOT2 = 0.0D0
*      N = INT(DT)
      N = 1
      DO I = 1, N

        C0 = 2.0D0*RCH%K*(1.0D0 - RCH%X) + DT1
        C1 = (DT1 - 2.0D0*RCH%K*RCH%X)/C0
        C2 = (DT1 + 2.0D0*RCH%K*RCH%X)/C0
        C3 = (2.0D0*RCH%K*(1.0D0 - RCH%X) - DT1)/C0
        QOT2 = C1*QIT2 + C2*QIT1 + C3*QOT1

      ENDDO

      RCH%OUTFLOW(ITER) = QOT2

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
      TYPE(REACH_TYPE), POINTER :: RCH2
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

      DO I = 1, BS%NREACH

        RCH2 => BS%REACH(I)
        IF(TRIM(RCH2%DOWNSTREAM).EQ.TRIM(RCH%NAME)) THEN

            RCH%INFLOW(ITER) = RCH%INFLOW(ITER) + RCH2%OUTFLOW(ITER)

        ENDIF

      ENDDO

      DO I = 1, BS%NRESERVOIR

        RES => BS%RESERVOIR(I)
        IF(TRIM(RES%DOWNSTREAM).EQ.TRIM(RCH%NAME)) THEN

            RCH%INFLOW(ITER) = RCH%INFLOW(ITER) + RES%OUTFLOW(ITER)

        ENDIF

      ENDDO

*C LOSS/GAIN
*
*      RCH%INFLOW(ITER) = (RCH%INFLOW(ITER) - RCH%LOSS_VALUE)*(1.0D0 - RCH%LOSS_RATIO)

      RETURN
      END SUBROUTINE GET_REACH_INFLOW

