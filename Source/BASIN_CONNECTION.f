C=================================================================
C CHECK THE LEVEL OF OBJECTS TO FIND THE CONNECTION OF BASIN
C=================================================================
      SUBROUTINE BASIN_CONNECTION
      USE CALC_PARAM
      IMPLICIT NONE
      INTEGER :: I
      TYPE(BASIN_TYPE), POINTER :: BS

      CALL WRITE_LOG('===================================')
      CALL WRITE_LOG('=                                 =')
      CALL WRITE_LOG('=    FINDING BASIN CONNECTION     =')
      CALL WRITE_LOG('=                                 =')
      CALL WRITE_LOG('===================================')
      CALL WRITE_LOG('')
      DO I = 1, NBASIN

        BS => BASIN(I)
        CALL BS%SET_OBJECTS_LEVEL

      ENDDO
      CALL SLEEP(1)
      RETURN
      END SUBROUTINE BASIN_CONNECTION


