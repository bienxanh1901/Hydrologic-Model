C=================================================================
C SUBROUTINE FLOOD ROUTING
C=================================================================
      SUBROUTINE ROUTING_CALC
      USE ROUTING
      IMPLICIT NONE
      INTEGER :: I,J,K

      CALL INIT_DISCHARGE
      J = 0
      K = 0
      DO I = 1, NSTOTAL

        IF(FRTYPE(I).EQ.1) THEN

            J = J + 1

            CALL RIVER_ROUTING(I,J)

        ELSE IF(FRTYPE(I).EQ.2) THEN

            K = K + 1
            CALL RESERVOIR_ROUTING(I,K)

        ENDIF

      ENDDO


      RETURN
      END SUBROUTINE ROUTING_CALC
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE RIVER FLOOD ROUTING
C=================================================================
      SUBROUTINE RIVER_ROUTING(INX,IDX)
      USE ROUTING
      USE OBSERVATION
      USE CALC
      IMPLICIT NONE
      INTEGER :: INX, I, J, IDX
      REAL(8) :: C0, C1, C2, C3, QI1, QI2

      QIN(INX,0) = QDC(INX,0)
      QI1 = QIN(INX,0) + RIVER(IDX)%QINP(1)

      DO I = 1, NTIME - 1

        !Calculate in flow (source)
        QI2 = 0.0D0

        IF(RIVER(IDX)%NSRC.GT.0) THEN

            DO J = 1, RIVER(IDX)%NSRC

                QI2 = QI2 + QDC(RIVER(IDX)%SRC(J),I)

            ENDDO

        ENDIF

        IF(RIVER(IDX)%NBASE.GT.0) THEN

            DO J = 1, RIVER(IDX)%NBASE

                QI2 = QI2 + QF(RIVER(IDX)%BASE(J),I)

            ENDDO

        ENDIF

        QIN(INX,I) = QI2

        IF(RIVER(IDX)%INP_FLAG.GT.0) THEN

            QI2 = QI2 + RIVER(IDX)%QINP(I)

        ENDIF



        C0 = 2.0D0*RIVER(IDX)%K*(1.0D0 - RIVER(IDX)%X) + DT
        C1 = (DT - 2.0D0*RIVER(IDX)%K*RIVER(IDX)%X)/C0
        C2 = (DT + 2.0D0*RIVER(IDX)%K*RIVER(IDX)%X)/C0
        C3 = (2.0D0*RIVER(IDX)%K*(1.0D0 - RIVER(IDX)%X) - DT)/C0
        QDC(INX,I) = C1*QI2 + C2*QI1 + C3*QDC(INX,I - 1)
        QI1 = QI2

      ENDDO

      RETURN
      END SUBROUTINE RIVER_ROUTING
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE RESERVOIR FLOOD ROUTING
C=================================================================
      SUBROUTINE RESERVOIR_ROUTING(INX,IDX)
      USE ROUTING
      USE OBSERVATION
      USE CALC
      IMPLICIT NONE
      INTEGER :: INX, I, J,IDX, K
      REAL(8) :: AREA, DH, DV, HEIGHT
      REAL(8) :: QI1, QI2, QO1, QO2, ZI, VI

      QI1 = QIN(INX,0)
      QO1 = QDC(INX,0) + RESERVOIR(IDX)%QTB(0)
      IF(RESERVOIR(IDX)%INP_FLAG.GT.0) QI1 = QI1 + RESERVOIR(IDX)%QINP(0)
      ZH(IDX,0) = RESERVOIR(IDX)%Z0
      !Interpolate initial volume from height
      CALL INTERP(RESERVOIR(IDX)%VZ(1,1:RESERVOIR(IDX)%NVZ),
     &            RESERVOIR(IDX)%VZ(2,1:RESERVOIR(IDX)%NVZ),
     &            ZH(IDX,0), V(IDX,0), RESERVOIR(IDX)%NVZ)

      DO I = 1, NTIME - 1

        !Calculate in flow (source)
        QI2 = 0.0D0

        IF(RESERVOIR(IDX)%NSRC.GT.0) THEN

            DO J = 1, RESERVOIR(IDX)%NSRC

                QI2 = QI2 + QDC(RESERVOIR(IDX)%SRC(J),I)

            ENDDO

        ENDIF

        IF(RESERVOIR(IDX)%NBASE.GT.0) THEN

            DO J = 1, RESERVOIR(IDX)%NBASE

                QI2 = QI2 + QF(RESERVOIR(IDX)%BASE(J),I)

            ENDDO

        ENDIF

        QIN(INX,I) = QI2

        IF(RESERVOIR(IDX)%INP_FLAG.GT.0) THEN

            QI2 = QI2 + RESERVOIR(IDX)%QINP(I)

        ENDIF

        ZI = ZH(IDX,I - 1)
        VI = V(IDX,I - 1)
        DO K =1, 3600

            IF(RESERVOIR(IDX)%CTRL_TYPE.EQ.1) THEN

                CALL GET_DISCHARGE(ZI, IDX, QDC(INX, I))

            ELSE

                CALL GET_DOOR_AREA(ZI, IDX, AREA, HEIGHT)

                IF(AREA.GT.0.0D0)THEN

                    DH = ZI - RESERVOIR(IDX)%ZBT
                    IF(DH.GE.HEIGHT) THEN

                        QDC(INX, I) = RESERVOIR(IDX)%DC_COEFF*AREA*DSQRT(9.81D0*2.0D0*(DH - 0.5*HEIGHT))

                    ELSE

                        QDC(INX, I) = 0.44D0*AREA*DSQRT(9.81*2.0*DH)

                    ENDIF

                ENDIF

            ENDIF
            QO2 = QDC(INX, I) + RESERVOIR(IDX)%QTB(I)
            DV = (0.5D0*((QI2 + QI1) - (QO2 + QO1)))*DT/3600.0d0

            VI = VI + DV


            CALL INTERP(RESERVOIR(IDX)%VZ(2,1:RESERVOIR(IDX)%NVZ),
     &            RESERVOIR(IDX)%VZ(1,1:RESERVOIR(IDX)%NVZ),
     &            VI, ZI, RESERVOIR(IDX)%NVZ)
        ENDDO

        V(IDX,I) = VI
        ZH(IDX,I) = ZI
        QI1 = QI2
        QO1 = QO2

      ENDDO

      RETURN
      END SUBROUTINE RESERVOIR_ROUTING
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE INITIAL THE DISCHARGE
C=================================================================
      SUBROUTINE INIT_DISCHARGE
      USE BASING
      USE ROUTING
      USE OBSERVATION
      USE CALC
      IMPLICIT NONE
      INTEGER :: I, J, K, L

      K = 0
      L = 0
      DO I = 1, NSTOTAL

        IF(FRTYPE(I).EQ.1) THEN

            K = K + 1

            IF(RIVER(K)%NSRC.GT.0) THEN

                DO J = 1, RIVER(K)%NSRC

                    QDC(I,0) = QDC(I,0) + QDC(RIVER(K)%SRC(J), 0)

                ENDDO

            ENDIF

            IF(RIVER(K)%NBASE.GT.0) THEN

                DO J = 1, RIVER(K)%NBASE

                    QDC(I,0) = QDC(I,0) + BASE(RIVER(K)%BASE(J))%Q0

                ENDDO

            ENDIF

*            IF(RIVER(K)%INP_FLAG.GT.0) THEN
*
*                QDC(I,0) = QDC(I,0) + RIVER(K)%QINP(1)
*
*            ENDIF

        ELSE IF(FRTYPE(I).EQ.2) THEN

            L = L + 1
            QDC(I,0) = 0.0D0
            IF(RESERVOIR(L)%NBASE.GT.0) THEN

                DO J = 1, RESERVOIR(L)%NBASE

                    QIN(I,0) = QIN(I,0) + QF(RESERVOIR(L)%BASE(J),0)

                ENDDO

            ENDIF
*            QDC(I,0) = RESERVOIR(L)%QTB(1)

*            IF(RESERVOIR(L)%NSRC.GT.0) THEN
*
*                DO J = 1, RESERVOIR(L)%NSRC
*
*                    QDC(I,0) = QDC(I,0) + QDC(RESERVOIR(L)%SRC(J), 0)
*
*                ENDDO
*
*            ENDIF
*
*            IF(RESERVOIR(L)%NBASE.GT.0) THEN
*
*                DO J = 1, RESERVOIR(L)%NBASE
*
*                    QDC(I,0) = QDC(I,0) + BASE(RESERVOIR(L)%BASE(J))%Q0
*
*                ENDDO
*
*            ENDIF
*
*
*            IF(RESERVOIR(L)%NINF.GT.0) THEN
*
*                DO J = 1, RESERVOIR(L)%NINF
*
*                    QDC(I,0) = QDC(I,0) + RESERVOIR(L)%QINP(J)
*
*                ENDDO
*
*            ENDIF

        ENDIF

      ENDDO

      RETURN
      END SUBROUTINE INIT_DISCHARGE
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE INITIAL THE DISCHARGE
C=================================================================
      SUBROUTINE GET_DOOR_AREA(ZI, I, AREA, HEIGHT)
      USE ROUTING
      USE CALC
      IMPLICIT NONE
      INTEGER :: I, J
      REAL(8) :: ZI, AREA, HEIGHT

      IF(ZI.LT.RESERVOIR(I)%DC_CTR(1,1)) THEN

        AREA = 0.0D0
        RETURN

      ELSE IF(ZI.GE.RESERVOIR(I)%DC_CTR(RESERVOIR(I)%NDC,1)) THEN

        J = RESERVOIR(I)%NDC
        AREA = RESERVOIR(I)%DC_CTR(J,2)*RESERVOIR(I)%DC_CTR(J,3)*RESERVOIR(I)%DOOR_W
        HEIGHT = RESERVOIR(I)%DC_CTR(J,3)
        RETURN

      ELSE

        DO J = 2,RESERVOIR(I)%NDC
            IF(ZI.GE.RESERVOIR(I)%DC_CTR(J - 1,1).AND.
     &         ZI.LT.RESERVOIR(I)%DC_CTR(J,1)) THEN

                AREA = RESERVOIR(I)%DC_CTR(J,2)*RESERVOIR(I)%DC_CTR(J,3)*RESERVOIR(I)%DOOR_W
                HEIGHT = RESERVOIR(I)%DC_CTR(J,3)
                RETURN

            ENDIF
        ENDDO

      ENDIF



      RETURN
      END SUBROUTINE GET_DOOR_AREA
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE INITIAL THE DISCHARGE
C=================================================================
      SUBROUTINE GET_DISCHARGE(ZI, I, Q)
      USE ROUTING
      USE CALC
      IMPLICIT NONE
      INTEGER :: I, J
      REAL(8) :: ZI, Q

      IF(ZI.LT.RESERVOIR(I)%DC_CTR(1,1)) THEN

        Q = 0.0D0
        RETURN

      ELSE IF(ZI.GE.RESERVOIR(I)%DC_CTR(RESERVOIR(I)%NDC,1)) THEN

        J = RESERVOIR(I)%NDC
        Q = RESERVOIR(I)%DC_CTR(J,2)
        RETURN

      ELSE

*        DO J = 2,RESERVOIR(I)%NDC
*            IF(ZI.GE.RESERVOIR(I)%DC_CTR(J - 1,1).AND.
*     &         ZI.LT.RESERVOIR(I)%DC_CTR(J,1)) THEN
*
*                Q = RESERVOIR(I)%DC_CTR(J,2)
*                RETURN
*
*            ENDIF
*        ENDDO
            CALL INTERP(RESERVOIR(I)%DC_CTR(1:RESERVOIR(I)%NDC,2),
     &            RESERVOIR(I)%DC_CTR(1:RESERVOIR(I)%NDC,1),
     &            ZI, Q, RESERVOIR(I)%NDC)
      ENDIF



      RETURN
      END SUBROUTINE GET_DISCHARGE
C=================================================================
C
C=================================================================
