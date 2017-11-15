C=================================================================
C SUBROUTINE UNIT_HYDROGRAPH_METHOD
C=================================================================
      SUBROUTINE LOSS_CALC(SBS, ITER)
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
      END INTERFACE
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      INTEGER, INTENT(IN) :: ITER

      SELECT CASE(SBS%LOSSRATE)
        CASE(SCS_CURVE_LOSS)
            CALL SCS_CURVE_NUMBER(SBS, ITER)
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
      SUBROUTINE SCS_CURVE_NUMBER(SBS, ITER)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      INTEGER, INTENT(IN) :: ITER
      REAL(8) :: PE2, IMP, PRECIP, IA

      IA = 0.2D0*SBS%S
      PRECIP = SBS%PRECIP%GATE_DATA(ITER)
      IMP = PRECIP*SBS%IMPERVIOUS
      SBS%P = SBS%P + PRECIP - IMP
      IF(SBS%P.GT.IA) PE2 = (SBS%P - IA)*(SBS%P - IA)/(SBS%P + 0.8D0*SBS%S)
      SBS%EXCESS(ITER) = PE2 - SBS%PE + IMP
      SBS%PE = PE2
      SBS%LOSS(ITER) = PRECIP - SBS%EXCESS(ITER)


      RETURN
      END SUBROUTINE SCS_CURVE_NUMBER
C=================================================================
C
C=================================================================
