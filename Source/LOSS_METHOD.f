C=================================================================
C SUBROUTINE LOSS METHOD
C=================================================================
      SUBROUTINE LOSS_CALC(SEFT)

      CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT
      IF(SEFT%NPRECIP_GATE.LE.0) RETURN
      SELECT CASE(SEFT%LOSSRATE)
        CASE(SCS_CURVE_LOSS)
            CALL SEFT%SCS_CURVE_NUMBER
        CASE DEFAULT
            WRITE(*,*) 'Error: Invalid type of loss method!!!'
            STOP
      END SELECT

      RETURN
      END SUBROUTINE LOSS_CALC
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE SCS LOSS METHOD
C=================================================================
      SUBROUTINE SCS_CURVE_NUMBER(SEFT)
      IMPLICIT NONE
      CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT
      REAL(8) :: PE2, IMP, PRECIP, IA, PT, PET

      PT = SEFT%P
      PET = SEFT%PE
      PE2 = 0.0D0
      IA = 0.2D0*SEFT%S

      IF(SIMULATION_MODE.EQ.REAL_TIME_MODE.AND. ACTIVE_MODE.EQ.PREDICT_CALC_MODE) THEN

        PT = SEFT%PFC
        PET = SEFT%PEFC

      ENDIF

      PRECIP = SEFT%AVGPRECIP(CURRENT_IDX)
      IMP = PRECIP*SEFT%IMPERVIOUS
      PT = PT + PRECIP - IMP
      IF(PT.GT.IA) PE2 = (PT - IA)*(PT - IA)/(PT + 0.8D0*SEFT%S)
      SEFT%EXCESS(CURRENT_IDX) = PE2 - PET + IMP
      SEFT%LOSS(CURRENT_IDX) = PRECIP - SEFT%EXCESS(CURRENT_IDX)

      SEFT%PFC = PT
      SEFT%PEFC = PE2

      IF(SIMULATION_MODE.EQ.VALIDATION_MODE.OR.(SIMULATION_MODE.EQ.REAL_TIME_MODE.AND.ACTIVE_MODE.EQ.EXACTLY_CALC_MODE)) THEN

        SEFT%P = PT
        SEFT%PE = PE2

      ENDIF

      RETURN
      END SUBROUTINE SCS_CURVE_NUMBER
