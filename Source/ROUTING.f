      SUBROUTINE ROUTING_CALC
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE REACH_ROUTING(RCH, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(REACH_TYPE), POINTER :: RCH
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE REACH_ROUTING

        SUBROUTINE RESERVOIR_ROUTING(RES, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE RESERVOIR_ROUTING

        SUBROUTINE GET_REACH_INFLOW(BS, RCH, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(BASIN_TYPE), POINTER :: BS
        TYPE(REACH_TYPE), POINTER :: RCH
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE GET_REACH_INFLOW

        SUBROUTINE GET_RESERVOIR_INFLOW(BS, RES, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(BASIN_TYPE), POINTER :: BS
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE GET_RESERVOIR_INFLOW
      END INTERFACE
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      TYPE(REACH_TYPE), POINTER :: RCH
      INTEGER :: I, J, N, K

      CALL WRITE_LOG('STARTING ROUTING!!!')

      DO I = 1, NBASIN

        BS => BASIN(I)
        DO N = 1, NTIME - 1
            DO K = BS%MAX_LEVEL, 0, -1

                RCHLOOP:DO J = 1,BS%NREACH

                    RCH => BS%REACH(J)
                    IF(RCH%LEVEL.NE.K) CYCLE

                    CALL GET_REACH_INFLOW(BS, RCH, N)

                    IF(RCH%INFLOW(N).LT.0) THEN  !!!HAIPT
                        RCH%OUTFLOW(N) = -1.0
                        CYCLE RCHLOOP
                    ELSEIF(RCH%INFLOW(N - 1).LT.0)THEN
                        RCH%OUTFLOW(N) = RCH%INFLOW(N)
                        CYCLE RCHLOOP
                    ENDIF

                    IF(RCH%ROUTE.EQ.0) THEN

                        RCH%OUTFLOW(N) = RCH%INFLOW(N)

                    ELSE

                        CALL REACH_ROUTING(RCH, N)

                    ENDIF

                ENDDO RCHLOOP

                RESLOOP: DO J = 1,BS%NRESERVOIR

                    RES => BS%RESERVOIR(J)
                    IF(RES%LEVEL.NE.K) CYCLE
                    CALL GET_RESERVOIR_INFLOW(BS, RES, N)

                    IF(RES%INFLOW(N).LT.0) THEN  !!!HAIPT
                        RES%OUTFLOW(N) = -1.0
                        CYCLE RESLOOP
                    ELSEIF(RES%INFLOW(N - 1).LT.0) THEN
                        RES%OUTFLOW(N) = -1.0
                        CYCLE RESLOOP
                    ENDIF

                    IF(RES%ROUTE.EQ.0) CYCLE
                    CALL RESERVOIR_ROUTING(RES, N)

                ENDDO RESLOOP

            ENDDO

        ENDDO

      ENDDO


      RETURN
      END SUBROUTINE ROUTING_CALC
