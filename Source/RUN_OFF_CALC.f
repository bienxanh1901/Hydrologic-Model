      SUBROUTINE CALCULATING_RUNOFF
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE SCS_CURVE_NUMBER(SBS, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE SCS_CURVE_NUMBER

        SUBROUTINE GET_BASE_FLOW(SBS, ITER)
        USE PARAM
        USE CONSTANTS
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE GET_BASE_FLOW
      END INTERFACE

      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      INTEGER :: I, J, N

      DO I = 1, NBASIN

        BS => BASIN(I)

        DO J = 1,BS%NSUBBASIN

            SBS => BS%SUBBASIN(J)

            DO N = 1, NTIME - 1

                CALL LOSS_CALC(SBS, N)

                CALL GET_BASE_FLOW(SBS, N)

                CALL TRANSFORM_CALC(SBS, N)

            ENDDO

        ENDDO

      ENDDO
      RETURN
      END SUBROUTINE CALCULATING_RUNOFF
