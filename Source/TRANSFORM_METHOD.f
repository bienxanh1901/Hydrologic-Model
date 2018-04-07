C=================================================================
C TRANSFORM_CALC
C=================================================================
      SUBROUTINE TRANSFORM_CALC(SEFT)
      IMPLICIT NONE
      CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT

      SELECT CASE(SEFT%TRANSFORM)
        CASE(SCS_UHG_TYPE)
            CALL SEFT%SCS_UHG
        CASE DEFAULT
            WRITE(*,*) 'Error: Invalid type of transform method!!!'
            STOP
      END SELECT

      RETURN
      END SUBROUTINE TRANSFORM_CALC
C=================================================================
C
C=================================================================
C=================================================================
C SCS_UHG
C=================================================================
      SUBROUTINE SCS_UHG(SEFT)
      IMPLICIT NONE
      CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT
      INTEGER :: M, L, N1
      REAL(8) :: SUMP

      SUMP = 0.0D0
      N1 = MAX(1,CURRENT_IDX - SEFT%NUHG + 1)
      DO M = N1,CURRENT_IDX
        L = CURRENT_IDX - M + 1
        SUMP = SUMP + SEFT%EXCESS(M)*SEFT%U(L)/10.0D0

      ENDDO

      SEFT%DIRECT_FLOW(CURRENT_IDX) = SUMP

      RETURN
      END SUBROUTINE SCS_UHG
C=================================================================
C
C=================================================================
