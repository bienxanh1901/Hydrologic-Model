C=================================================================
C RESERVOIR_ROUTING
C=================================================================
      SUBROUTINE RESERVOIR_ROUTING(SEFT)
      IMPLICIT NONE
      CLASS(RESERVOIR_TYPE), INTENT(INOUT) :: SEFT

      CALL SEFT%RESERVOIR_ROUTING_METHOD

      RETURN
      END SUBROUTINE RESERVOIR_ROUTING
C=================================================================
C
C=================================================================
C=================================================================
C SPECIFIED_RELEASE
C=================================================================
      SUBROUTINE RESERVOIR_ROUTING_METHOD(SEFT)
      IMPLICIT NONE
      CLASS(RESERVOIR_TYPE), INTENT(INOUT) :: SEFT
      INTEGER :: N, I
      REAL(8) :: QIT1, QIT2, DT1, QOT1, QOT2
      REAL(8) :: ZT, VT, DV

      DT1 = DT

      QIT1 = SEFT%INFLOW(CURRENT_IDX - 1)
      QIT2 = SEFT%INFLOW(CURRENT_IDX)
      QOT1 = SEFT%OUTFLOW(CURRENT_IDX - 1)
      ZT = SEFT%ELEVATION(CURRENT_IDX - 1)
      VT = SEFT%STORAGE(CURRENT_IDX - 1)

      N = INT(DT)
      N = 1
      QOT2 = 0.0D0
      DO I = 1,N

        CALL SEFT%SPILLWAY_DISCHARGE(ZT, QOT2)
        IF(SEFT%TB_TYPE.EQ.CONSTANT_DATA) THEN

            QOT2 = QOT2 + SEFT%TB_CONST_DATA

        ELSE

            QOT2 = QOT2 + SEFT%TURBIN_GATE%CURRENT_DATA

        ENDIF

        DV = (0.5D0*((QIT2 + QIT1) - (QOT2 + QOT1)))*DT1
        VT = VT + DV
        CALL INTERP(SEFT%SE_CURVE(2,1:SEFT%NSE),
     &              SEFT%SE_CURVE(1,1:SEFT%NSE),
     &              VT, ZT, SEFT%NSE)


      ENDDO

      SEFT%OUTFLOW(CURRENT_IDX) = QOT2
      SEFT%ELEVATION(CURRENT_IDX) = ZT
      SEFT%STORAGE(CURRENT_IDX) = VT

      RETURN
      END SUBROUTINE RESERVOIR_ROUTING_METHOD
C=================================================================
C
C=================================================================
C=================================================================
C SPILLWAY_DISCHARGE
C=================================================================
      SUBROUTINE SPILLWAY_DISCHARGE(SEFT, Z, Q)
      IMPLICIT NONE
      CLASS(RESERVOIR_TYPE), INTENT(INOUT) :: SEFT
      REAL(8), INTENT(IN) :: Z
      REAL(8), INTENT(OUT):: Q
      REAL(8) :: S, H, DH

      IF(SEFT%ROUTE.EQ.SPECIFIED_RELEASE) THEN

        IF(Z.LT.SEFT%ED_CURVE(1,1)) THEN

            Q = 0.0D0
            RETURN

        ELSE IF(Z.GE.SEFT%ED_CURVE(1,SEFT%NED)) THEN

            Q = SEFT%ED_CURVE(2,SEFT%NED)
            RETURN

        ELSE

            CALL INTERP(SEFT%ED_CURVE(1,1:SEFT%NED),
     &              SEFT%ED_CURVE(2,1:SEFT%NED), Z, Q, SEFT%NSE)

        ENDIF

      ELSE IF(SEFT%ROUTE.EQ.OUTFLOW_STRUCTURE) THEN

        IF(Z.LT.SEFT%EH_CURVE(1,1)) THEN

            Q = 0.0D0
            RETURN

        ELSE

            CALL SEFT%GET_DOOR_AREA(Z, S, H)
            DH = Z - SEFT%ZSW
            IF(DH.GE.H) THEN

                Q = SEFT%DC_COEFF*S*DSQRT(9.81D0*2.0D0*(DH - 0.5*H))

            ELSE

                Q = 0.44D0*S*DSQRT(9.81*2.0*DH)

            ENDIF

        ENDIF

      ENDIF

      RETURN
      END SUBROUTINE SPILLWAY_DISCHARGE

C=================================================================
C GET_DOOR_AREA
C=================================================================
      SUBROUTINE GET_DOOR_AREA(SEFT, Z, S, H)
      IMPLICIT NONE
      CLASS(RESERVOIR_TYPE), INTENT(INOUT) :: SEFT
      REAL(8), INTENT(IN) :: Z
      REAL(8), INTENT(OUT) :: S, H
      INTEGER :: J

      IF(Z.LT.SEFT%EH_CURVE(1,1)) THEN

        S = 0.0D0
        H = 0.0D0
        RETURN

      ELSE IF(Z.GE.SEFT%EH_CURVE(1, SEFT%NED)) THEN


        H = SEFT%EH_CURVE(2, SEFT%NED)
        S = H*SEFT%NDOOR(SEFT%NED)*SEFT%DOORW
        RETURN

      ELSE

        DO J = 2,SEFT%NED
            IF(Z.GE.SEFT%EH_CURVE(1, J-1).AND.
     &         Z.LT.SEFT%EH_CURVE(1,J)) THEN

                H = SEFT%EH_CURVE(2,J)
                S = H*SEFT%NDOOR(J)*SEFT%DOORW

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
C SPECIFIED_RELEASE
C=================================================================
      SUBROUTINE CORRECT_DATA_CURRENT_TIME(SEFT)
      IMPLICIT NONE
      CLASS(RESERVOIR_TYPE), INTENT(INOUT) :: SEFT
      REAL(8) :: QIT1, QIT2, DT1, QOT1, QOT2
      REAL(8) :: ZT1, ZT2, VT1, VT2, DV

      DT1 = DT

      QIT1 = SEFT%INFLOW(CURRENT_IDX - 1)
      QOT1 = SEFT%OUTFLOW(CURRENT_IDX - 1)
      ZT1 = SEFT%ELEVATION(CURRENT_IDX - 1)
      ZT2 = SEFT%ZOBS%CURRENT_DATA
      VT1 = SEFT%STORAGE(CURRENT_IDX - 1)

      CALL INTERP(SEFT%SE_CURVE(1,1:SEFT%NSE),
     &            SEFT%SE_CURVE(2,1:SEFT%NSE),
     &            ZT2, VT2, SEFT%NSE)

      DV = VT2 - VT1
      QOT2 = 0.0D0

      CALL SEFT%SPILLWAY_DISCHARGE(ZT2, QOT2)
      IF(SEFT%TB_TYPE.EQ.CONSTANT_DATA) THEN

        QOT2 = QOT2 + SEFT%TB_CONST_DATA

      ELSE

        QOT2 = QOT2 + SEFT%TURBIN_GATE%CURRENT_DATA

      ENDIF

      QIT2 = 2.0D0*DV/DT1 + (QOT2 + QOT1) - QIT1

      SEFT%OUTFLOW(CURRENT_IDX) = QOT2
      SEFT%ELEVATION(CURRENT_IDX) = ZT2
      SEFT%STORAGE(CURRENT_IDX) = VT2
      SEFT%INFLOW(CURRENT_IDX) = QIT2

      RETURN
      END SUBROUTINE CORRECT_DATA_CURRENT_TIME
