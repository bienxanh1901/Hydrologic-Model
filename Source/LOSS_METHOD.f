C=================================================================
C SUBROUTINE UNIT_HYDROGRAPH_METHOD
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
C SUBROUTINE UNIT_HYDROGRAPH_METHOD
C=================================================================
      SUBROUTINE SCS_CURVE_NUMBER(SEFT)
      IMPLICIT NONE
      CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT
      REAL(8) :: PE2, IMP, PRECIP, IA

      PE2 = 0.0D0
      IA = 0.2D0*SEFT%S
      PRECIP = SEFT%AVGPRECIP(CURRENT_IDX)
      IMP = PRECIP*SEFT%IMPERVIOUS
      SEFT%P = SEFT%P + PRECIP - IMP
      IF(SEFT%P.GT.IA) PE2 = (SEFT%P - IA)*(SEFT%P - IA)/(SEFT%P + 0.8D0*SEFT%S)
      SEFT%EXCESS(CURRENT_IDX) = PE2 - SEFT%PE + IMP
      SEFT%PE = PE2
      SEFT%LOSS(CURRENT_IDX) = PRECIP - SEFT%EXCESS(CURRENT_IDX)


      RETURN
      END SUBROUTINE SCS_CURVE_NUMBER
C=================================================================
C
C=================================================================
