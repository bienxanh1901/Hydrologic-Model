C=================================================================
C SUBROUTINE UNIT_HYDROGRAPH_METHOD
C=================================================================
      SUBROUTINE UHG_CALC
      USE UNIT_HYDROGRAPH
      USE OBSERVATION
      USE CHARACTERISTIC
      USE OUTPUT
      IMPLICIT NONE
      INTEGER :: I, J, K, NUF, L
      REAL(8) :: TP, TC, TR, TL, UP
      REAL(8) :: TU1(1:NUINT), U1(1:NUINT)
      REAL(8), ALLOCATABLE, DIMENSION(:) :: TU, U


      DO I = 1, MF

        TP = (DLF(I)*1000.0D0)**0.8*(2540.0D0 - 22.86D0*CN)**0.7D0/
     &     (14104.0D0*CN**0.70D0*Y(I)**0.50D0)
        TC = 5.0D0*TP/3.0D0
        TR = 2.0D0*TP/9.0D0
        TL = 0.50D0*TR + 0.60D0*TC
        UP = 2.080D0*F(I)/TL

        DO J = 1,NUINT

            TU1(J) = TL*TU0(J)
            U1(J) = UP*U0(J)

        ENDDO

        NUF = INT(TU1(NUINT)) + 1
        ALLOCATE(TU(1:NUF))
        ALLOCATE(U(1:NUF))
        TU = 0.0D0
        U = 0.0D0

        DO J = 1, NUF

            IF(J.GT.1) TU(J) = TU(J - 1) + 1.0D0

            CALL INTERP(U1, TU1, TU(J), U(J), NUINT)

        ENDDO

        DO J = 1,NT

            QF(I,J) = Q0(I)
            DO K = 1,J
                L = J - K + 1
                IF(L.LE.NUF)THEN
                    QF(I,J) = QF(I,J) + ALPHA*XF(I,K)*U(L)/SDV
                ENDIF
            ENDDO

        ENDDO

        DEALLOCATE(TU, U)

      ENDDO

      RETURN
      END SUBROUTINE UHG_CALC
C=================================================================
C
C=================================================================

C=================================================================
C SUBROUTINE NAM_MODEL
C=================================================================
      SUBROUTINE NAM_MODEL_CALC
      USE NAM_MODEL
      USE CHARACTERISTIC
      USE OBSERVATION
      USE OUTPUT
      IMPLICIT NONE
      INTEGER :: I, J
      REAL(8) :: ET, E1, E2, U1, L1, L2, QIF, QOF, G, LT, PN, DL
      REAL(8) :: OF, IFQ, BF

      U1 = U40
      L2 = L20
      OF = OF0
      IFQ = FQ0
      BF = BF0
      L1 = 0.0D0
      E1 = 0.0D0
      E2 = 00D0
      PN = 0.0D0
      LT = 0.0D0
      DL = 0.0D0
      QIF = 0.0D0
      QOF = 0.0D0
      G = 0.0D0

      DO I = 1, MF
        DO J = 1,NT

            U1 = U1 + XF(I,J)

            IF(U1.GE.EP) THEN

                U1 = U1 - EP
                L1 = L2
                LT = L1/SLMAX(I)

                QIF = 0.0D0

                IF(U1.GT.UMAX(I)) THEN

                    PN = U1 - UMAX(I)
                    U1 = UMAX(I)

                    QOF = CQOF(I)*PN*(LT - TOF(I))/(1 - TOF(I))
                    IF(QOF.LT.0.0D0) QOF = 0.0D0

                    G = (PN - QOF)*(LT - TG(I))/(1 - TG(I))
                    IF(G.LT.0.0D0) G = 0.0D0

                    DL = DMAX1(PN - QOF - G, 0.0D0)
                    L2 = L1 + DL


                ELSE

                    PN = 0.0D0
                    QOF = 0.0D0
                    G = 0.0D0
                    L2 = L1

                ENDIF

            ELSE

                E1 = U1
                U1 = 0.0D0
                E2 = EP*L1/SLMAX(I)

                ET = EP - E1
                E1 = DMAX1(ET,E2)
                E1 = DMIN1(E1,L2)

                L1 = L2 - E1
                LT = L1/SLMAX(I)
                QIF = (1/CKIF(I))*E1*(LT - TIF(I))/(1 - TIF(I))

                PN = 0.0D0
                QOF = 0.0D0
                G = 0.0D0

                L2 = L1

            ENDIF

            OF = QOF*(1 - EXP(-DT/CK1(I))) + OF*EXP(-DT/CK1(I))
            IFQ = QIF*(1 - EXP(-DT/CK1(I))) + IFQ*EXP(-DT/CK1(I))
            BF = G*(1 - EXP(-DT/CKBF(I))) + BF*EXP(-DT/CKBF(I))
            QF(I,J) = (OF + IFQ + BF)*F(I)*1000/(3600*DT)

        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE NAM_MODEL_CALC
C=================================================================
C
C=================================================================

