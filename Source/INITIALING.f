      SUBROUTINE INITIALING_VARIABLES
      USE COMMON_PARAM
      USE CALC_PARAM
      USE TIME
      USE CONSTANTS
      IMPLICIT NONE
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(REACH_TYPE), POINTER :: RCH
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      TYPE(GATE_TYPE), POINTER :: GT
      INTEGER :: I, J, K, IERR
      REAL(8) :: QIN

      CALL WRITE_LOG('INITIALING VARIABLES!!!')

      CURRENT_IDX = 0

      DO I = 1, NBASIN

        BS => BASIN(I)


        DO J = 1, BS%NGATE

            GT => BS%GATE(J)
            IERR = GT%SET_CURRENT_DATA()

        ENDDO


        DO J = 1,BS%NSUBBASIN

            SBS => BS%SUBBASIN(J)
            SBS%BASE_FLOW(0) = SBS%GET_BASE_FLOW(START_TIME)
            SBS%TOTAL_FLOW(0) = SBS%BASE_FLOW(0)

        ENDDO


        DO K = BS%MAX_LEVEL, 0, -1

            DO J = 1,BS%NREACH

                RCH => BS%REACH(J)
                IF(RCH%LEVEL.NE.K) CYCLE
                CALL BS%GET_REACH_INFLOW(RCH)
                QIN = (RCH%INFLOW(0) - RCH%LOSS_VALUE)*(1.0D0 - RCH%LOSS_RATIO)
                IF(QIN.LT.0D0) QIN = 0.0D0
                RCH%OUTFLOW(0) = QIN

            ENDDO

            DO J = 1,BS%NRESERVOIR

                RES => BS%RESERVOIR(J)
                IF(RES%LEVEL.NE.K) CYCLE
                CALL BS%GET_RESERVOIR_INFLOW(RES)

                IF(RES%TB_TYPE.EQ.CONSTANT_DATA) THEN

                    RES%OUTFLOW = RES%TB_CONST_DATA

                ELSE

                    RES%OUTFLOW = RES%TURBIN_GATE%CURRENT_DATA

                ENDIF

                RES%ELEVATION(0) = RES%Z0
                CALL INTERP(RES%SE_CURVE(1,1:RES%NSE),
     &                      RES%SE_CURVE(2,1:RES%NSE),
     &                      RES%Z0, RES%STORAGE(0), RES%NSE)

            ENDDO

        ENDDO

      ENDDO

      CALL CREATE_OUTPUT_DIR
      IF(SIMULATION_MODE.EQ.REAL_TIME_MODE)CALL WRITE_OUTPUT

      RETURN
      END SUBROUTINE INITIALING_VARIABLES
