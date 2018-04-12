! set level for all basin objects to obtain basin connectivity
        SUBROUTINE SET_OBJECTS_LEVEL(SEFT)

        CLASS(BASIN_TYPE), INTENT(INOUT) :: SEFT
        INTEGER :: J
        TYPE(REACH_TYPE), POINTER :: RCH
        TYPE(RESERVOIR_TYPE), POINTER :: RES

        SEFT%MAX_LEVEL = 0

        DO J = 1, SEFT%NREACH

            RCH => SEFT%REACH(J)

            RCH%LEVEL = RCH%LEVEL + SEFT%FIND_LEVEL(RCH%DOWNSTREAM)
            SEFT%MAX_LEVEL = MAX(SEFT%MAX_LEVEL, RCH%LEVEL)

        ENDDO

        DO J = 1, SEFT%NRESERVOIR

            RES => SEFT%RESERVOIR(J)

            RES%LEVEL = RES%LEVEL + SEFT%FIND_LEVEL(RES%DOWNSTREAM)
            SEFT%MAX_LEVEL = MAX(SEFT%MAX_LEVEL, RES%LEVEL)

        ENDDO

        RETURN
        END SUBROUTINE SET_OBJECTS_LEVEL


        !find level of an object
        RECURSIVE FUNCTION FIND_LEVEL(SEFT,DOWNSTREAM) RESULT(LEVEL)

        CLASS(BASIN_TYPE), INTENT(INOUT) :: SEFT
        CHARACTER(*), INTENT(IN) :: DOWNSTREAM
        INTEGER :: LEVEL, J
        TYPE(REACH_TYPE), POINTER :: RCH
        TYPE(RESERVOIR_TYPE), POINTER :: RES


        IF(TRIM(DOWNSTREAM).EQ."") THEN

            LEVEL = 0
            RETURN

        ELSE

            LEVEL = 1

            DO J = 1, SEFT%NREACH

                RCH => SEFT%REACH(J)
                IF(TRIM(DOWNSTREAM).EQ.TRIM(RCH%NAME)) THEN

                    LEVEL = LEVEL + SEFT%FIND_LEVEL(RCH%DOWNSTREAM)
                    RETURN

                ENDIF

            ENDDO

            DO J = 1, SEFT%NRESERVOIR

                RES => SEFT%RESERVOIR(J)
                IF(TRIM(DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

                    LEVEL = LEVEL + SEFT%FIND_LEVEL(RES%DOWNSTREAM)
                    RETURN

                ENDIF

            ENDDO

        ENDIF

        RETURN
        END FUNCTION FIND_LEVEL
