      SUBROUTINE ALLOCATING_VARIABLES
      USE CALC_PARAM
      USE TIME
      IMPLICIT NONE
      INTEGER :: I
      TYPE(BASIN_TYPE), POINTER :: BS

      CALL WRITE_LOG('===================================')
      CALL WRITE_LOG('=                                 =')
      CALL WRITE_LOG('=       ALLOCATING MEMMORY        =')
      CALL WRITE_LOG('=                                 =')
      CALL WRITE_LOG('===================================')
      CALL WRITE_LOG('')


      DO I = 1, NBASIN

        BS => BASIN(I)
        CALL BS%BSALLOCATING

      ENDDO
      CALL SLEEP(1)
      RETURN
      END SUBROUTINE ALLOCATING_VARIABLES
