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
      REAL(8) :: QDCTMP, AREA, DH, DV

      QIN(INX,0) = QDC(INX,0)

      ZH(IDX,0) = RESEVOIR(IDX)%Z0
      !Interpolate initial volume from height
      CALL INTERP(RESEVOIR(IDX)%VZ(1,1:RESEVOIR(IDX)%NVZ),
     &            RESEVOIR(IDX)%VZ(2,1:RESEVOIR(IDX)%NVZ),
     &            ZH(IDX,0), V(IDX,0), RESEVOIR(IDX)%NVZ)

      DO I = 1, NTIME

        !Calculate in flow (source)
        QIN(INX,I) = 0.0D0

        IF(RESEVOIR(IDX)%NSRC.GT.0) THEN

            DO J = 1, RESEVOIR(IDX)%NSRC

                QIN(INX,I) = QIN(INX,I) + QDC(RESEVOIR(IDX)%SRC(J),I)

            ENDDO

        ENDIF

        IF(RESEVOIR(IDX)%NBASE.GT.0) THEN

            DO J = 1, RESEVOIR(IDX)%NBASE

                QIN(INX,I) = QIN(INX,I) + QF(RESEVOIR(IDX)%BASE(J),I)

            ENDDO

        ENDIF

        IF(RESEVOIR(IDX)%NINF.GT.0) THEN

            DO J = 1, RESEVOIR(IDX)%NINF

                QIN(INX,I) = QIN(INX,I) + RESEVOIR(IDX)%QINF(J,I)

            ENDDO

        ENDIF


        QDC(INX, I) = RESEVOIR(IDX)%QTB
        CALL GET_DOOR_AREA(ZH(IDX,I - 1), IDX, AREA)

*        IF(ZH(IDX,I - 1).GE.RESEVOIR(IDX)%ZMAX)THEN
*
*            DH = ZH(IDX,I - 1) - RESEVOIR(IDX)%ZMAX
*            IF(DH.GE.RESEVOIR(IDX)%DOOR_H) THEN
*
*                DOOR_AREA = RESEVOIR(IDX)%DC_COEFF*RESEVOIR(IDX)%NDOOR*
*     &                      RESEVOIR(IDX)%DOOR_H*RESEVOIR(IDX)%DOOR_W
*                QDCTMP = DOOR_AREA*DSQRT(9.81*2.0*DH - 0.5*RESEVOIR(IDX)%DOOR_H)
*
*            ELSE
*
*                DOOR_AREA = 0.44*RESEVOIR(IDX)%NDOOR*
*     &                      DH*RESEVOIR(IDX)%DOOR_W
*                QDCTMP = DOOR_AREA*DSQRT(9.81*2.0*DH)
*
*            ENDIF
*
*            QDC(INX, I) = QDC(INX, I) + QDCTMP
*
*        ENDIF

        DV = (0.5*((QIN(INX,I) + QIN(INX,I - 1)) - (QDC(INX, I) - QDC(INX, I - 1))))*DT

        V(IDX,I) = V(IDX,I - 1) + DV


        CALL INTERP(RESEVOIR(IDX)%VZ(2,1:RESEVOIR(IDX)%NVZ),
     &            RESEVOIR(IDX)%VZ(1,1:RESEVOIR(IDX)%NVZ),
     &            V(IDX,I), ZH(IDX,I), RESEVOIR(IDX)%NVZ)

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

            QDC(I,0) = RESEVOIR(L)%QTB

*            IF(RESEVOIR(L)%NSRC.GT.0) THEN
*
*                DO J = 1, RESEVOIR(L)%NSRC
*
*                    QDC(I,0) = QDC(I,0) + QDC(RESEVOIR(L)%SRC(J), 0)
*
*                ENDDO
*
*            ENDIF
*
*            IF(RESEVOIR(L)%NBASE.GT.0) THEN
*
*                DO J = 1, RESEVOIR(L)%NBASE
*
*                    QDC(I,0) = QDC(I,0) + BASE(RESEVOIR(L)%BASE(J))%Q0
*
*                ENDDO
*
*            ENDIF
*
*
*            IF(RESEVOIR(L)%NINF.GT.0) THEN
*
*                DO J = 1, RESEVOIR(L)%NINF
*
*                    QDC(I,0) = QDC(I,0) + RESEVOIR(L)%QINF(J)
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
      SUBROUTINE GET_DOOR_AREA(ZI, I, AREA)
      USE ROUTING
      USE CALC
      IMPLICIT NONE
      INTEGER :: I, J, K, L
      REAL(8) :: ZI, AREA

      IF(ZI.LT.RESEVOIR(I)%DC_CTR(1,1)) THEN

        AREA = 0.0D0
        RETURN

      ELSE IF(ZI.GE.RESEVOIR(I)%DC_CTR(RESEVOIR(I)%NDC,1)) THEN

        J = RESEVOIR(I)%NDC
        AREA = RESEVOIR(I)%DC_CTR(J,2)*RESEVOIR(I)%DC_CTR(J,3)*RESEVOIR(I)%DOOR_W
        RETURN

      ELSE

        DO J = 2,RESEVOIR(I)%NDC
            IF(ZI.GE.RESEVOIR(I)%DC_CTR(J - 1,1).AND.
     &         ZI.LT.RESEVOIR(I)%DC_CTR(J,1)) THEN

                AREA = RESEVOIR(I)%DC_CTR(J,2)*RESEVOIR(I)%DC_CTR(J,3)*RESEVOIR(I)%DOOR_W
                RETURN

            ENDIF
        ENDDO

      ENDIF



      RETURN
      END SUBROUTINE GET_DOOR_AREA
C=================================================================
C
C=================================================================
