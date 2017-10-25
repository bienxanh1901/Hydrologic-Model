C=================================================================
C SUBROUTINE UNIT_HYDROGRAPH_METHOD
C=================================================================
      SUBROUTINE SCS_CURVE_NUMBER
      USE UNIT_HYDROGRAPH
      USE OBSERVATION
      USE BASING
      USE CALC
      IMPLICIT NONE
      INTEGER :: I, N
      REAL(8) :: S, P, PE1, PE2, IA, IMP, F


      DO I = 1, NBASING

        S = (25400.0D0 - 254.0D0*BASE(I)%CN)/BASE(I)%CN
        IA = 0.2D0*S
        P = 0.0D0
        PE1 = 0.0D0
        DO N = 1,NTIME

            IMP = XF(I,N)*BASE(I)%IMPERVIOUS
            P = P + XF(I,N) - IMP
            IF(P.GT.IA) PE2 = (P - IA)*(P - IA)/(P + 0.8D0*S)
            EXCESS(I,N) = PE2 - PE1 + IMP
            PE1 = PE2
            LOSS(I,N) = XF(I,N) - EXCESS(I,N)


        ENDDO

      ENDDO

      RETURN
      END SUBROUTINE SCS_CURVE_NUMBER
C=================================================================
C
C=================================================================
