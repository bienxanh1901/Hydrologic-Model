      SUBROUTINE ROUTING_CALC
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE REACH_ROUTING(BS, RCH, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(REACH_TYPE), POINTER :: RCH
        TYPE(BASIN_TYPE), POINTER :: BS
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE REACH_ROUTING

        SUBROUTINE RESERVOIR_ROUTING(BS, RES, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(BASIN_TYPE), POINTER :: BS
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE RESERVOIR_ROUTING
      END INTERFACE
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      TYPE(REACH_TYPE), POINTER :: RCH
      INTEGER :: I, J, N, K

      DO I = 1, NBASIN

        BS => BASIN(I)
        DO N = 1, NTIME - 1
            DO K = BS%MAX_LEVEL, 0, -1

                DO J = 1,BS%NREACH

                    RCH => BS%REACH(J)
                    IF(RCH%LEVEL.NE.K) CYCLE
                    IF(RCH%ROUTE.EQ.0) CYCLE
                    CALL REACH_ROUTING(BS, RCH, N)

                ENDDO

                DO J = 1,BS%NRESERVOIR

                    RES => BS%RESERVOIR(J)
                    IF(RES%LEVEL.NE.K) CYCLE
                    IF(RES%ROUTE.EQ.0) CYCLE
                    CALL RESERVOIR_ROUTING(BS, RES, N)

                ENDDO

            ENDDO

        ENDDO

      ENDDO


      RETURN
      END SUBROUTINE ROUTING_CALC
