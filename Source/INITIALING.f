      SUBROUTINE INITIALING_VARIABLES
      USE PARAM
      USE TIME
      USE CONSTANTS
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE GET_BASE_FLOW(SBS, ITER)
        USE PARAM
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE GET_BASE_FLOW

        SUBROUTINE GET_UHG(SBS)
        USE CONSTANTS
        USE PARAM
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        END SUBROUTINE GET_UHG

        SUBROUTINE GET_RESERVOIR_INFLOW(BS, RES, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(BASIN_TYPE), POINTER :: BS
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE GET_RESERVOIR_INFLOW

        SUBROUTINE GET_REACH_INFLOW(BS, RCH, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(BASIN_TYPE), POINTER :: BS
        TYPE(REACH_TYPE), POINTER :: RCH
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE GET_REACH_INFLOW

      END INTERFACE
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(REACH_TYPE), POINTER :: RCH
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      INTEGER :: I, J, K

      DO I = 1, NBASIN

        BS => BASIN(I)

        DO J = 1,BS%NSUBBASIN

            SBS => BS%SUBBASIN(J)
            SBS%LOSS(0) = 0.0D0
            SBS%EXCESS(0) = 0.0D0
            SBS%DIRECT_FLOW(0) = 0.0D0
            SBS%PRECIP%GATE_DATA(0) = 0.0D0
            IF(SBS%TRANSFORM.EQ.SCS_UHG_TYPE) THEN

                CALL GET_UHG(SBS)

            ENDIF

            IF(SBS%LOSSRATE.EQ.SCS_CURVE_LOSS) THEN

                SBS%S =(25400.0D0 - 254.0D0*SBS%CN)/SBS%CN
                SBS%P = 0.0D0
                SBS%PE = 0.0D0

            ENDIF

            CALL GET_BASE_FLOW(SBS, 0)

            SBS%TOTAL_FLOW(0) = SBS%DIRECT_FLOW(0) + SBS%BASE_FLOW(0)

        ENDDO


        DO K = BS%MAX_LEVEL, 0, -1

            DO J = 1,BS%NREACH

                RCH => BS%REACH(J)
                IF(RCH%LEVEL.NE.K) CYCLE
                CALL GET_REACH_INFLOW(BS, RCH, 0)
                RCH%OUTFLOW(0) = RCH%INFLOW(0)

            ENDDO

            DO J = 1,BS%NRESERVOIR

                RES => BS%RESERVOIR(J)
                IF(RES%LEVEL.NE.K) CYCLE
                CALL GET_RESERVOIR_INFLOW(BS, RES, 0)

                IF(.NOT.ASSOCIATED(RES%TURBIN_GATE)) THEN

                    RES%OUTFLOW = RES%TB_CONST_DATA

                ELSE

                    RES%OUTFLOW = RES%TURBIN_GATE%GATE_DATA(0)

                ENDIF

                RES%ELEVATION(0) = RES%Z0
                CALL INTERP(RES%SE_CURVE(1,1:RES%NSE),
     &                      RES%SE_CURVE(2,1:RES%NSE),
     &                      RES%Z0, RES%STORAGE(0), RES%NSE)

            ENDDO

        ENDDO

      ENDDO

      RETURN
      END SUBROUTINE INITIALING_VARIABLES
