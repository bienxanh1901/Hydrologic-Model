      SUBROUTINE CALCULATING_RUNOFF
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE TRANSFORM_CALC(SBS, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE TRANSFORM_CALC

        SUBROUTINE GET_BASE_FLOW(SBS, ITER)
        USE PARAM
        USE CONSTANTS
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE GET_BASE_FLOW

        SUBROUTINE LOSS_CALC(SBS, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE LOSS_CALC
      END INTERFACE

      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      INTEGER :: I, J, N

      CALL WRITE_LOG('CALCULATING RUNOFF!!!')

      DO I = 1, NBASIN

        BS => BASIN(I)

        DO J = 1,BS%NSUBBASIN

            SBS => BS%SUBBASIN(J)

            DO N = 1, NTIME - 1

                !IGNORE IF don't have value at n-time
                IF(SBS%PRECIP%GATE_DATA(N) < 0.0D0) THEN
                    SBS%EXCESS(N) = -1.0D0
                    SBS%LOSS(N) = -1.0D0
                    SBS%DIRECT_FLOW(N) = -1.0D0
                    SBS%TOTAL_FLOW(N) = -1.0D0
                    CYCLE
                ENDIF

                IF(SBS%LOSSRATE.NE.0) THEN

                    CALL LOSS_CALC(SBS, N)

                ELSE

                    IF(ASSOCIATED(SBS%PRECIP))SBS%EXCESS(N) = SBS%PRECIP%GATE_DATA(N)

                ENDIF

                IF(SBS%BASE_FLOW_TYPE.NE.0)CALL GET_BASE_FLOW(SBS, N)

                IF(SBS%TRANSFORM.NE.0)CALL TRANSFORM_CALC(SBS, N)

                SBS%TOTAL_FLOW(N) = SBS%DIRECT_FLOW(N) + SBS%BASE_FLOW(N)

            ENDDO

        ENDDO

      ENDDO
      RETURN
      END SUBROUTINE CALCULATING_RUNOFF
