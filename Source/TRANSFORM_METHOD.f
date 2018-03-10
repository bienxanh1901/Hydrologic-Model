C=================================================================
C TRANSFORM_CALC
C=================================================================
      SUBROUTINE TRANSFORM_CALC(SBS, ITER)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE SCS_UHG(SBS, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE SCS_UHG
      END INTERFACE
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      INTEGER, INTENT(IN) :: ITER

      SELECT CASE(SBS%TRANSFORM)
        CASE(SCS_UHG_TYPE)
            CALL SCS_UHG(SBS, ITER)
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
      SUBROUTINE SCS_UHG(SBS, ITER)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      INTEGER, INTENT(IN) :: ITER
      INTEGER :: M, L, N1
      REAL(8) :: SUMP

      SUMP = 0.0D0
      N1 = MAX(1,ITER - SBS%NUHG + 1)
      DO M = N1,ITER

        L = ITER - M + 1
        SUMP = SUMP + SBS%EXCESS(M)*SBS%U(L)/10.0D0

      ENDDO

      SBS%DIRECT_FLOW(ITER) = SUMP

      RETURN
      END SUBROUTINE SCS_UHG
C=================================================================
C
C=================================================================
