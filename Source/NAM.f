C=================================================================
C SUBROUTINE NAM_MODEL
C=================================================================
      SUBROUTINE NAM_MODEL_CALC
      USE OBSERVATION
      USE BASING
      USE CALC
      IMPLICIT NONE
      INTEGER :: I, J
      REAL(8) :: ET, E1, E2, U1, L1, L2, QIF, QOF, G, LT, PN, DL
      REAL(8) :: OF, IFQ, BF

      DO I = 1, NBASING
        U1 = NAMPRM(I)%U40
        L2 = NAMPRM(I)%L20
        OF = NAMPRM(I)%OF0
        IFQ = NAMPRM(I)%FQ0
        BF = NAMPRM(I)%BF0
        L1 = 0.0D0
        E1 = 0.0D0
        E2 = 00D0
        PN = 0.0D0
        LT = 0.0D0
        DL = 0.0D0
        QIF = 0.0D0
        QOF = 0.0D0
        G = 0.0D0

        DO J = 1,NTIME

            U1 = U1 + XF(I,J)

            IF(U1.GE.NAMPRM(I)%EP) THEN

                U1 = U1 - NAMPRM(I)%EP
                L1 = L2
                LT = L1/NAMPRM(I)%SLMAX

                QIF = 0.0D0

                IF(U1.GT.NAMPRM(I)%UMAX) THEN

                    PN = U1 - NAMPRM(I)%UMAX
                    U1 = NAMPRM(I)%UMAX

                    QOF = NAMPRM(I)%CQOF*PN*(LT - NAMPRM(I)%TOF)/
     &                    (1 - NAMPRM(I)%TOF)
                    IF(QOF.LT.0.0D0) QOF = 0.0D0

                    G = (PN - QOF)*(LT - NAMPRM(I)%TG)/
     &                  (1 - NAMPRM(I)%TG)
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
                E2 = NAMPRM(I)%EP*L1/NAMPRM(I)%SLMAX

                ET = NAMPRM(I)%EP - E1
                E1 = DMAX1(ET,E2)
                E1 = DMIN1(E1,L2)

                L1 = L2 - E1
                LT = L1/NAMPRM(I)%SLMAX
                QIF = (1/NAMPRM(I)%CKIF)*E1*(LT - NAMPRM(I)%TIF)/
     &                (1 - NAMPRM(I)%TIF)

                PN = 0.0D0
                QOF = 0.0D0
                G = 0.0D0

                L2 = L1

            ENDIF

            OF = QOF*(1 - EXP(-DT/NAMPRM(I)%CK1)) +
     &           OF*EXP(-DT/NAMPRM(I)%CK1)
            IFQ = QIF*(1 - EXP(-DT/NAMPRM(I)%CK1)) +
     &            IFQ*EXP(-DT/NAMPRM(I)%CK1)
            BF = G*(1 - EXP(-DT/NAMPRM(I)%CKBF)) +
     &           BF*EXP(-DT/NAMPRM(I)%CKBF)
            QF(I,J) = (OF + IFQ + BF)*BASE(I)%AREA*1000/(3600*DT)

        ENDDO

      ENDDO

      RETURN
      END SUBROUTINE NAM_MODEL_CALC
C=================================================================
C
C=================================================================
