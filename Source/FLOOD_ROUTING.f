C=================================================================
C SUBROUTINE FLOOD ROUTING
C=================================================================
      SUBROUTINE FLOOD_ROUTING_CALC
      USE ROUTING
      IMPLICIT NONE
      INTEGER :: I,J,K

      CALL INIT_DISCHARGE
      J = 0
      K = 0
      DO I = 1, NSTOTAL

        IF(FRTYPE(I).EQ.1) THEN

            J = J + 1

            CALL RIVER_FLOOD_ROUTING_CALC(I,J)

        ELSE IF(FRTYPE(I).EQ.2) THEN

            K = K + 1
            CALL RESOURCE_FLOOD_ROUTING_CALC(I,K)

        ENDIF

      ENDDO


      RETURN
      END SUBROUTINE FLOOD_ROUTING_CALC
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE RIVER FLOOD ROUTING
C=================================================================
      SUBROUTINE RIVER_FLOOD_ROUTING_CALC(INX,IDX)
      USE ROUTING
      USE OBSERVATION
      USE CALC
      IMPLICIT NONE
      INTEGER :: INX, I, J, IDX
      REAL(8) :: C0, C1, C2, C3

      QIN(INX,0) = QDC(INX,0)

      DO I = 1, NTIME

        !Calculate in flow (source)
        QIN(INX,I) = 0.0D0

        IF(RIVER(IDX)%NSRC.GT.0) THEN

            DO J = 1, RIVER(IDX)%NSRC

                QIN(INX,I) = QIN(INX,I) + QDC(RIVER(IDX)%SRC(J),I)

            ENDDO

        ENDIF

        IF(RIVER(IDX)%NBASE.GT.0) THEN

            DO J = 1, RIVER(IDX)%NBASE

                QIN(INX,I) = QIN(INX,I) + QF(RIVER(IDX)%BASE(J),I)

            ENDDO

        ENDIF

        IF(RIVER(IDX)%NINF.GT.0) THEN

            DO J = 1, RIVER(IDX)%NINF

                QIN(INX,I) = QIN(INX,I) + RIVER(IDX)%QINF(J,I)

            ENDDO

        ENDIF

        C0 = RIVER(IDX)%TX - 0.25D0*RIVER(IDX)%TX + 0.5D0*DT
        C1 = (- 0.25D0*RIVER(IDX)%TX + 0.5D0*DT)/C0
        C2 = (0.25D0*RIVER(IDX)%TX + 0.5D0*DT)/C0
        C3 = (RIVER(IDX)%TX - 0.25D0*RIVER(IDX)%TX - 0.5D0*DT)/C0
        QDC(INX,I) = C1*QIN(INX,I) + C2*QIN(INX,I-1) + C3*QDC(INX,I - 1)

      ENDDO

      RETURN
      END SUBROUTINE RIVER_FLOOD_ROUTING_CALC
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE RESOURCE FLOOD ROUTING
C=================================================================
      SUBROUTINE RESOURCE_FLOOD_ROUTING_CALC(INX,IDX)
      USE ROUTING
      USE OBSERVATION
      USE CALC
      IMPLICIT NONE
      INTEGER :: INX, I, J,IDX
      REAL(8) :: QDCTMP, AREA, DH, DV, HEIGHT

      QIN(INX,0) = QDC(INX,0)

      ZH(IDX,0) = RESERVOIR(IDX)%Z0
      !Interpolate initial volume from height
      CALL INTERP(RESERVOIR(IDX)%VZ(1,1:RESERVOIR(IDX)%NVZ),
     &            RESERVOIR(IDX)%VZ(2,1:RESERVOIR(IDX)%NVZ),
     &            ZH(IDX,0), V(IDX,0), RESERVOIR(IDX)%NVZ)

      DO I = 1, NTIME

        !Calculate in flow (source)
        QIN(INX,I) = 0.0D0

        IF(RESERVOIR(IDX)%NSRC.GT.0) THEN

            DO J = 1, RESERVOIR(IDX)%NSRC

                QIN(INX,I) = QIN(INX,I) + QDC(RESERVOIR(IDX)%SRC(J),I)

            ENDDO

        ENDIF

        IF(RESERVOIR(IDX)%NBASE.GT.0) THEN

            DO J = 1, RESERVOIR(IDX)%NBASE

                QIN(INX,I) = QIN(INX,I) + QF(RESERVOIR(IDX)%BASE(J),I)

            ENDDO

        ENDIF

        IF(RESERVOIR(IDX)%NINF.GT.0) THEN

            DO J = 1, RESERVOIR(IDX)%NINF

                QIN(INX,I) = QIN(INX,I) + RESERVOIR(IDX)%QINF(J,I)

            ENDDO

        ENDIF


        QDC(INX, I) = RESERVOIR(IDX)%QTB
        CALL GET_DOOR_AREA(ZH(IDX,I - 1), IDX, AREA, HEIGHT)

        IF(AREA.GT.0.0D0)THEN

            DH = ZH(IDX,I - 1) - RESERVOIR(IDX)%ZMAX
            IF(DH.GE.HEIGHT) THEN

                QDCTMP = RESERVOIR(IDX)%DC_COEFF*AREA*DSQRT(9.81D0*2.0D0*(DH - 0.5*HEIGHT))

            ELSE

                QDCTMP = 0.44D0*AREA*DSQRT(9.81*2.0*DH)

            ENDIF

            QDC(INX, I) = QDC(INX, I) + QDCTMP

        ENDIF

        DV = (0.5D0*((QIN(INX,I) + QIN(INX,I - 1)) - (QDC(INX, I) - QDC(INX, I - 1))))*DT

        V(IDX,I) = V(IDX,I - 1) + DV


        CALL INTERP(RESERVOIR(IDX)%VZ(2,1:RESERVOIR(IDX)%NVZ),
     &            RESERVOIR(IDX)%VZ(1,1:RESERVOIR(IDX)%NVZ),
     &            V(IDX,I), ZH(IDX,I), RESERVOIR(IDX)%NVZ)

      ENDDO

      RETURN
      END SUBROUTINE RESOURCE_FLOOD_ROUTING_CALC
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

            IF(RIVER(K)%NINF.GT.0) THEN

                DO J = 1, RIVER(K)%NINF

                    QDC(I,0) = QDC(I,0) + RIVER(K)%QINF(J,0)

                ENDDO

            ENDIF

        ELSE IF(FRTYPE(I).EQ.2) THEN

            L = L + 1

            QDC(I,0) = RESERVOIR(L)%QTB

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
*                    QDC(I,0) = QDC(I,0) + RESERVOIR(L)%QINF(J)
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
