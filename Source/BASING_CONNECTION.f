C=================================================================
C CHECK THE LEVEL OF OBJECTS TO FIND THE CONNECTION OF BASIN
C=================================================================
      SUBROUTINE BASIN_CONNECTION
      USE PARAM
      IMPLICIT NONE
      INTEGER :: I, J, K, FIND_LEVEL
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(REACH_TYPE), POINTER :: RCH

      DO I = 1, NBASIN

        BS => BASIN(I)

        DO J = 1, BS%NREACH

            RCH => BS%REACH(J)
            IF(TRIM(RCH%DOWNSTREAM).EQ."") THEN

                RCH%LEVEL = FIND_LEVEL(RCH%DOWNSTREAM)

            ENDIF

        ENDDO

      ENDDO



      RETURN
      END SUBROUTINE BASIN_CONNECTION
C=================================================================
C
C=================================================================
C=================================================================
C CHECK THE LEVEL OF OBJECTS TO FIND THE CONNECTION OF BASIN
C=================================================================
      RECURSIVE FUNCTION FIND_LEVEL(DOWNSTREAM) RESULT(LEVEL)
      USE PARAM
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: DOWNSTREAM
      INTEGER :: LEVEL
      INTEGER :: I, J, K
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      TYPE(REACH_TYPE), POINTER :: RCH


      IF(TRIM(RCH%DOWNSTREAM).EQ."") THEN

        RETURN

      ELSE

        LEVEL = LEVEL + 1
        DO J = 1, BS%NREACH

            RCH => BS%REACH(J)
            IF(TRIM(DOWNSTREAM).NE.TRIM(RCH%NAME)) THEN

                RCH%LEVER = 1
                BS%MAX_LEVEL = BS%MAX_LEVEL + 1

            ENDIF

        ENDDO



      RETURN
      END FUNCTION FIND_LEVEL
C=================================================================
C
C=================================================================
