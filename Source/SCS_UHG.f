C=================================================================
C SUBROUTINE UNIT_HYDROGRAPH_METHOD
C=================================================================
      SUBROUTINE UHG_CALC
      USE UNIT_HYDROGRAPH
      IMPLICIT NONE

      IF(UHG_TYPE.EQ.1) THEN !SCS UHG

        CALL SCS_UHG_CALC

      ENDIF

      RETURN
      END SUBROUTINE UHG_CALC
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE UNIT_HYDROGRAPH_METHOD
C=================================================================
      SUBROUTINE SCS_UHG_CALC
      USE UNIT_HYDROGRAPH
      USE OBSERVATION
      USE BASING
      USE CALC
      IMPLICIT NONE
      INTEGER :: I, J, K, NUF, L
      REAL(8) :: TP, TC, DELTAD, TL, UP
      REAL(8) :: TU1(1:NUHG), U1(1:NUHG)
      REAL(8), ALLOCATABLE, DIMENSION(:) :: TU, U


      DO I = 1, NBASING


        TC = (BASE(I)%LENGTH*1000.0D0)**0.8*(2540.0D0 - 22.86D0*BASE(I)%CN)**0.7D0/
     &       (14104.0D0*BASE(I)%CN**0.70D0*BASE(I)%SLOPE**0.50D0)

        TL = 3.0D0*TC/5.0D0
        DELTAD = 0.133D0*TC
        TP = 0.50D0*DELTAD + TL
        UP = 2.080D0*BASE(I)%AREA/TP

        DO J = 1,NUHG

            TU1(J) = TP*UHG_DATA(1,J)
            U1(J) = UP*UHG_DATA(2,J)

        ENDDO

        NUF = INT(TU1(NUHG)) + 1
        ALLOCATE(TU(1:NUF))
        ALLOCATE(U(1:NUF))
        TU = 0.0D0
        U = 0.0D0

        DO J = 1, NUF

            IF(J.GT.1) TU(J) = TU(J - 1) + 1.0D0

            CALL INTERP(U1, TU1, TU(J), U(J), NUHG)

        ENDDO

        DO J = 1,NTIME

            QF(I,J) = BASE(I)%Q0
            DO K = 1,J
                L = J - K + 1
                IF(L.LE.NUF)THEN
                    QF(I,J) = QF(I,J) + XF(I,K)*U(L)
                ENDIF
            ENDDO

        ENDDO

        DEALLOCATE(TU, U)

      ENDDO

      RETURN
      END SUBROUTINE SCS_UHG_CALC
C=================================================================
C
C=================================================================


