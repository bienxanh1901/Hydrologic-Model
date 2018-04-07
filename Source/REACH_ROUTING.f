C=================================================================
C REACH_ROUTING
C=================================================================
      SUBROUTINE REACH_ROUTING(SEFT)
      IMPLICIT NONE
      CLASS(REACH_TYPE), INTENT(INOUT) :: SEFT

      SELECT CASE(SEFT%ROUTE)
        CASE(MUSKINGUM_METHOD)
            CALL SEFT%MUSKINGUM_CALC
        CASE DEFAULT
            WRITE(*,*) 'Error: Invalid type of Reach routing method!!!'
            STOP
      END SELECT

      RETURN
      END SUBROUTINE REACH_ROUTING
C=================================================================
C
C=================================================================
C=================================================================
C MUSKINGUM_CALC
C=================================================================
      SUBROUTINE MUSKINGUM_CALC(SEFT)
      IMPLICIT NONE
      CLASS(REACH_TYPE), INTENT(INOUT) :: SEFT
      INTEGER :: N, I
      REAL(8) :: QIT1, QIT2, DT1, QOT1, QOT2
      REAL(8) :: C0, C1, C2, C3

      DT1 = DT
      QIT1 = (SEFT%INFLOW(CURRENT_IDX - 1) - SEFT%LOSS_VALUE)*(1.0D0 - SEFT%LOSS_RATIO)
      IF(QIT1.LT.0) QIT1 = 0.0D0
      QIT2 = (SEFT%INFLOW(CURRENT_IDX) - SEFT%LOSS_VALUE)*(1.0D0 - SEFT%LOSS_RATIO)
      IF(QIT2.LT.0) QIT2 = 0.0D0
      QOT1 = SEFT%OUTFLOW(CURRENT_IDX - 1)
      QOT2 = 0.0D0
*      N = INT(DT)
      N = 1
      DO I = 1, N

        C0 = 2.0D0*SEFT%K*(1.0D0 - SEFT%X) + DT1
        C1 = (DT1 - 2.0D0*SEFT%K*SEFT%X)/C0
        C2 = (DT1 + 2.0D0*SEFT%K*SEFT%X)/C0
        C3 = (2.0D0*SEFT%K*(1.0D0 - SEFT%X) - DT1)/C0
        QOT2 = C1*QIT2 + C2*QIT1 + C3*QOT1

      ENDDO

      SEFT%OUTFLOW(CURRENT_IDX) = QOT2

      RETURN
      END SUBROUTINE MUSKINGUM_CALC
C=================================================================
C
C=================================================================

