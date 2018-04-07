      SUBROUTINE GET_REACH_INFLOW(SEFT, RCH)
      IMPLICIT NONE
      CLASS(BASIN_TYPE), INTENT(INOUT) :: SEFT
      TYPE(REACH_TYPE), POINTER :: RCH
      INTEGER :: I

      RCH%INFLOW(CURRENT_IDX) = 0.0D0

      DO I = 1, SEFT%NSUBBASIN

        IF(TRIM(SEFT%SUBBASIN(I)%DOWNSTREAM).EQ.TRIM(RCH%NAME)) THEN

            RCH%INFLOW(CURRENT_IDX) = RCH%INFLOW(CURRENT_IDX) + SEFT%SUBBASIN(I)%TOTAL_FLOW(CURRENT_IDX)

        ENDIF

      ENDDO

      DO I = 1, SEFT%NSOURCE

        IF(TRIM(SEFT%SOURCE(I)%DOWNSTREAM).EQ.TRIM(RCH%NAME)) THEN

            RCH%INFLOW(CURRENT_IDX) = RCH%INFLOW(CURRENT_IDX) + SEFT%SOURCE(I)%SRC_DATA%CURRENT_DATA

        ENDIF

      ENDDO

      DO I = 1, SEFT%NREACH

        IF(TRIM(SEFT%REACH(I)%DOWNSTREAM).EQ.TRIM(RCH%NAME)) THEN

            RCH%INFLOW(CURRENT_IDX) = RCH%INFLOW(CURRENT_IDX) + SEFT%REACH(I)%OUTFLOW(CURRENT_IDX)

        ENDIF

      ENDDO

      DO I = 1, SEFT%NRESERVOIR

        IF(TRIM(SEFT%RESERVOIR(I)%DOWNSTREAM).EQ.TRIM(RCH%NAME)) THEN

            RCH%INFLOW(CURRENT_IDX) = RCH%INFLOW(CURRENT_IDX) + SEFT%RESERVOIR(I)%OUTFLOW(CURRENT_IDX)

        ENDIF

      ENDDO

      RETURN
      END SUBROUTINE GET_REACH_INFLOW
C=================================================================
C
C=================================================================
      SUBROUTINE GET_RESERVOIR_INFLOW(SEFT, RES)
      IMPLICIT NONE
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      CLASS(BASIN_TYPE), INTENT(INOUT) :: SEFT

      INTEGER :: I

      RES%INFLOW(CURRENT_IDX) = 0.0D0

      DO I = 1, SEFT%NSUBBASIN

        IF(TRIM(SEFT%SUBBASIN(I)%DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

            RES%INFLOW(CURRENT_IDX) = RES%INFLOW(CURRENT_IDX) + SEFT%SUBBASIN(I)%TOTAL_FLOW(CURRENT_IDX)

        ENDIF

      ENDDO

      DO I = 1, SEFT%NSOURCE

        IF(TRIM(SEFT%SOURCE(I)%DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

            RES%INFLOW(CURRENT_IDX) = RES%INFLOW(CURRENT_IDX) + SEFT%SOURCE(I)%SRC_DATA%CURRENT_DATA

        ENDIF

      ENDDO

      DO I = 1, SEFT%NREACH

        IF(TRIM(SEFT%REACH(I)%DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

            RES%INFLOW(CURRENT_IDX) = RES%INFLOW(CURRENT_IDX) + SEFT%REACH(I)%OUTFLOW(CURRENT_IDX)

        ENDIF

      ENDDO

      DO I = 1, SEFT%NRESERVOIR

        IF(TRIM(SEFT%RESERVOIR(I)%DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

            RES%INFLOW(CURRENT_IDX) = RES%INFLOW(CURRENT_IDX) + SEFT%RESERVOIR(I)%OUTFLOW(CURRENT_IDX)

        ENDIF

      ENDDO

      RETURN
      END SUBROUTINE GET_RESERVOIR_INFLOW
