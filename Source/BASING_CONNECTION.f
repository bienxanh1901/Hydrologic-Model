C=================================================================
C CHECK THE LEVEL OF OBJECTS TO FIND THE CONNECTION OF BASIN
C=================================================================
      SUBROUTINE BASIN_CONNECTION
      USE PARAM
      IMPLICIT NONE

      INTERFACE
        RECURSIVE FUNCTION FIND_LEVEL(BS, DOWNSTREAM) RESULT(LEVEL)
        USE PARAM
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN) :: DOWNSTREAM
        INTEGER :: LEVEL
        INTEGER :: I, J, K
        TYPE(BASIN_TYPE), POINTER :: BS
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        TYPE(REACH_TYPE), POINTER :: RCH
        END FUNCTION FIND_LEVEL
      END INTERFACE

      INTEGER :: I, J
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(REACH_TYPE), POINTER :: RCH
      TYPE(RESERVOIR_TYPE), POINTER :: RES

      DO I = 1, NBASIN

        BS => BASIN(I)
        BS%MAX_LEVEL = 0

        DO J = 1, BS%NREACH

            RCH => BS%REACH(J)

            RCH%LEVEL = RCH%LEVEL + FIND_LEVEL(BS, RCH%DOWNSTREAM)
            BS%MAX_LEVEL = MAX(BS%MAX_LEVEL, RCH%LEVEL)

        ENDDO

        DO J = 1, BS%NRESERVOIR

            RES => BS%RESERVOIR(J)

            RES%LEVEL = RES%LEVEL + FIND_LEVEL(BS, RES%DOWNSTREAM)
            BS%MAX_LEVEL = MAX(BS%MAX_LEVEL, RES%LEVEL)

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
      RECURSIVE FUNCTION FIND_LEVEL(BS, DOWNSTREAM) RESULT(LEVEL)
      USE PARAM
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: DOWNSTREAM
      INTEGER :: LEVEL, J
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      TYPE(REACH_TYPE), POINTER :: RCH


      IF(TRIM(DOWNSTREAM).EQ."") THEN

        LEVEL = 0
        RETURN

      ELSE

        LEVEL = 1

        DO J = 1, BS%NREACH

            RCH => BS%REACH(J)
            IF(TRIM(DOWNSTREAM).NE.TRIM(RCH%NAME)) THEN

                LEVEL = LEVEL + FIND_LEVEL(BS, RCH%DOWNSTREAM)
                RETURN

            ENDIF

        ENDDO

        DO J = 1, BS%NRESERVOIR

            RES => BS%RESERVOIR(J)
            IF(TRIM(DOWNSTREAM).NE.TRIM(RES%NAME)) THEN

                LEVEL = LEVEL + FIND_LEVEL(BS, RES%DOWNSTREAM)
                RETURN

            ENDIF

        ENDDO

      ENDIF

      RETURN
      END FUNCTION FIND_LEVEL
C=================================================================
C
C=================================================================
