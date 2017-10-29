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
      INTEGER :: I, J, NUF, L, M, N
      REAL(8) :: TP, TC, TL, UP, DR
      REAL(8) :: TU1(1:NUHG + 1), U1(1:NUHG + 1)
      REAL(8), ALLOCATABLE, DIMENSION(:) :: TU, U

      open(20,file='uhg.dat')
      DO I = 1, NBASING


*        TC = (BASE(I)%LENGTH*1000.0D0)**0.8*(2540.0D0 - 22.86D0*BASE(I)%CN)**0.7D0/
*     &       (14104.0D0*BASE(I)%CN**0.70D0*BASE(I)%SLOPE**0.50D0)
*
*        TL = 3.0D0*TC/5.0D0
        TL = 8.0D0
        TP = 0.50D0*DT/3600.0D0 + TL
        UP = 2.080D0*BASE(I)%AREA/TP

        DO J = 1,NUHG + 1

            TU1(J) = TP*UHG_DATA(1,J - 1)
            U1(J) = UP*UHG_DATA(2,J - 1)

        ENDDO

        NUF = INT(TU1(NUHG + 1))
        ALLOCATE(TU(0:NUF))
        ALLOCATE(U(0:NUF))
        TU = 0.0D0
        U = 0.0D0

        DO J = 1, NUF

            TU(J) = TU(J - 1) + 1.0D0

            CALL INTERP(U1, TU1, TU(J), U(J), NUHG)

        ENDDO
        CLOSE(20)
        QF(I,0) = BASE(I)%Q0
        DO N = 1,NTIME - 1

            DR = 0.0D0
            DO M = 1,N
                L = N - M + 1
                IF(L.LE.NUF)THEN

                    DR = DR + EXCESS(I,M)*U(L)/10.0D0

                ENDIF
            ENDDO
             QF(I,N) = BASE(I)%Q0 + DR

        ENDDO

        DEALLOCATE(TU, U)

      ENDDO

      RETURN
      END SUBROUTINE SCS_UHG_CALC
C=================================================================
C
C=================================================================


