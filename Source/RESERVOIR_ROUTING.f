C=================================================================
C RESERVOIR_ROUTING
C=================================================================
      SUBROUTINE RESERVOIR_ROUTING(RES, ITER)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE RESERVOIR_ROUTING_METHOD(RES, ITER)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        INTEGER, INTENT(IN) :: ITER
        END SUBROUTINE RESERVOIR_ROUTING_METHOD
      END INTERFACE
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      INTEGER, INTENT(IN) :: ITER

      CALL RESERVOIR_ROUTING_METHOD(RES, ITER)


      RETURN
      END SUBROUTINE RESERVOIR_ROUTING
C=================================================================
C
C=================================================================
C=================================================================
C SPECIFIED_RELEASE
C=================================================================
      SUBROUTINE RESERVOIR_ROUTING_METHOD(RES, ITER)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE

        SUBROUTINE SPILLWAY_DISCHARGE(RES, Z, Q)
        USE PARAM
        USE CONSTANTS
        USE TIME
        IMPLICIT NONE
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        REAL(8), INTENT(IN) :: Z
        REAL(8), INTENT(OUT):: Q
        END SUBROUTINE SPILLWAY_DISCHARGE

      END INTERFACE
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      INTEGER, INTENT(IN) :: ITER
      INTEGER :: N, I
      REAL(8) :: QIT1, QIT2, DT1, QOT1, QOT2
      REAL(8) :: ZT, VT, DV

      IF(RES%INFLOW(ITER).LT.0.0D0)THEN
        RES%OUTFLOW(ITER) = -1.0D0
        RES%ELEVATION(ITER) = -1.0D0
        RES%STORAGE(ITER) = -1.0D0
        RETURN

      ELSEIF(RES%INFLOW(ITER - 1).LT.0.0D0) THEN
        IF(.NOT.ASSOCIATED(RES%Z_OBS)) THEN
            RES%ELEVATION(ITER) = RES%Z_OBS%GATE_DATA(ITER)
            CALL SPILLWAY_DISCHARGE(RES, ZT, QOT2)
        ELSE

            RES%ELEVATION(ITER) = RES%Z0

        ENDIF

        IF(.NOT.ASSOCIATED(RES%TURBIN_GATE)) THEN

            QOT2 = QOT2 + RES%TB_CONST_DATA

        ELSE

            QOT2 = QOT2 + RES%TURBIN_GATE%GATE_DATA(ITER)

        ENDIF

        RES%OUTFLOW(ITER) = QOT2

        CALL INTERP(RES%SE_CURVE(1,1:RES%NSE),
     &              RES%SE_CURVE(2,1:RES%NSE),
     &              RES%ELEVATION(ITER), RES%STORAGE(ITER), RES%NSE)
        RETURN
      ENDIF

      DT1 = 1.0D0/DT

      QIT1 = RES%INFLOW(ITER - 1)
      QIT2 = RES%INFLOW(ITER)
      QOT1 = RES%OUTFLOW(ITER - 1)
      ZT = RES%ELEVATION(ITER - 1)
      VT = RES%STORAGE(ITER - 1)

      N = INT(DT)

      DO I = 1,N

        CALL SPILLWAY_DISCHARGE(RES, ZT, QOT2)
        IF(.NOT.ASSOCIATED(RES%TURBIN_GATE)) THEN

            QOT2 = QOT2 + RES%TB_CONST_DATA

        ELSE

            QOT2 = QOT2 + RES%TURBIN_GATE%GATE_DATA(ITER)

        ENDIF

        DV = (0.5D0*((QIT2 + QIT1) - (QOT2 + QOT1)))*DT1
        VT = VT + DV
        CALL INTERP(RES%SE_CURVE(2,1:RES%NSE),
     &              RES%SE_CURVE(1,1:RES%NSE),
     &              VT, ZT, RES%NSE)


      ENDDO

      RES%OUTFLOW(ITER) = QOT2
      RES%ELEVATION(ITER) = ZT
      RES%STORAGE(ITER) = VT

      RETURN
      END SUBROUTINE RESERVOIR_ROUTING_METHOD
C=================================================================
C
C=================================================================
C=================================================================
C SPILLWAY_DISCHARGE
C=================================================================
      SUBROUTINE SPILLWAY_DISCHARGE(RES, Z, Q)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE GET_DOOR_AREA(RES, Z, S, H)
        USE PARAM
        IMPLICIT NONE
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        REAL(8), INTENT(IN) :: Z
        REAL(8), INTENT(OUT) :: S, H
        END SUBROUTINE GET_DOOR_AREA
      END INTERFACE
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      REAL(8), INTENT(IN) :: Z
      REAL(8), INTENT(OUT):: Q
      REAL(8) :: S, H, DH

      IF(RES%ROUTE.EQ.SPECIFIED_RELEASE) THEN

        IF(Z.LT.RES%ED_CURVE(1,1)) THEN

            Q = 0.0D0
            RETURN

        ELSE IF(Z.GE.RES%ED_CURVE(1,RES%NED)) THEN

            Q = RES%ED_CURVE(2,RES%NED)
            RETURN

        ELSE

            CALL INTERP(RES%ED_CURVE(1,1:RES%NED),
     &              RES%ED_CURVE(2,1:RES%NED), Z, Q, RES%NSE)

        ENDIF

      ELSE IF(RES%ROUTE.EQ.OUTFLOW_STRUCTURE) THEN

        IF(Z.LT.RES%EH_CURVE(1,1)) THEN

            Q = 0.0D0
            RETURN

        ELSE

            CALL GET_DOOR_AREA(RES, Z, S, H)
            DH = Z - RES%ZSW
            IF(DH.GE.H) THEN

                Q = RES%DC_COEFF*S*DSQRT(9.81D0*2.0D0*(DH - 0.5*H))

            ELSE

                Q = 0.44D0*S*DSQRT(9.81*2.0*DH)

            ENDIF

        ENDIF

      ENDIF

      RETURN
      END SUBROUTINE SPILLWAY_DISCHARGE
C=================================================================
C
C=================================================================
      SUBROUTINE GET_RESERVOIR_INFLOW(BS, RES, ITER)
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      TYPE(REACH_TYPE), POINTER :: RCH
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      TYPE(SOURCE_TYPE), POINTER :: SRC
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      INTEGER, INTENT(IN) :: ITER
      TYPE(RESERVOIR_TYPE), POINTER :: RES2
      INTEGER :: I

      RES%INFLOW(ITER) = 0.0D0

      DO I = 1, BS%NSUBBASIN

        SBS => BS%SUBBASIN(I)
        IF(TRIM(SBS%DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

            RES%INFLOW(ITER) = RES%INFLOW(ITER) + SBS%TOTAL_FLOW(ITER)

        ENDIF

      ENDDO

      DO I = 1, BS%NSOURCE

        SRC => BS%SOURCE(I)
        IF(TRIM(SRC%DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

            RES%INFLOW(ITER) = RES%INFLOW(ITER) + SRC%SRC_DATA%GATE_DATA(ITER)

        ENDIF

      ENDDO

      DO I = 1, BS%NREACH

        RCH => BS%REACH(I)
        IF(TRIM(RCH%DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

            IF(RCH%OUTFLOW(ITER) < 0.0D0) THEN !!!HAIPT
                RES%INFLOW(ITER) = -1.0D0
                RETURN
            ENDIF

            RES%INFLOW(ITER) = RES%INFLOW(ITER) + RCH%OUTFLOW(ITER)

        ENDIF

      ENDDO

      DO I = 1, BS%NRESERVOIR

        RES2 => BS%RESERVOIR(I)
        IF(TRIM(RES2%DOWNSTREAM).EQ.TRIM(RES%NAME)) THEN

            RES%INFLOW(ITER) = RES%INFLOW(ITER) + RES2%OUTFLOW(ITER)

        ENDIF

      ENDDO

      RETURN
      END SUBROUTINE GET_RESERVOIR_INFLOW
C=================================================================
C GET_DOOR_AREA
C=================================================================
      SUBROUTINE GET_DOOR_AREA(RES, Z, S, H)
      USE PARAM
      IMPLICIT NONE
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      REAL(8), INTENT(IN) :: Z
      REAL(8), INTENT(OUT) :: S, H
      INTEGER :: J

      IF(Z.LT.RES%EH_CURVE(1,1)) THEN

        S = 0.0D0
        H = 0.0D0
        RETURN

      ELSE IF(Z.GE.RES%EH_CURVE(1, RES%NED)) THEN


        H = RES%EH_CURVE(2, RES%NED)
        S = H*RES%NDOOR(RES%NED)*RES%DOORW
        RETURN

      ELSE

        DO J = 2,RES%NED
            IF(Z.GE.RES%EH_CURVE(1, J-1).AND.
     &         Z.LT.RES%EH_CURVE(1,J)) THEN

                H = RES%EH_CURVE(2,J)
                S = H*RES%NDOOR(J)*RES%DOORW

                RETURN

            ENDIF
        ENDDO

      ENDIF



      RETURN
      END SUBROUTINE GET_DOOR_AREA
C=================================================================
C
C=================================================================
