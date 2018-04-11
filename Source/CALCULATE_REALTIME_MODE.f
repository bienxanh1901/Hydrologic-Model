      SUBROUTINE RUN_REAL_TIME_MODE
      USE TIME
      USE CALC_PARAM
      USE COMMON_PARAM
      IMPLICIT NONE
      INTEGER :: I
      INTEGER,DIMENSION(8) :: TVAL
      TYPE(datetime) :: CRRTIME
      TYPE(timedelta):: DES

      CALL WRITE_LOG('STARTING CALCULATION!!!')

      DO

        ACTIVE_MODE = EXACTLY_CALC_MODE
        CURRENT_IDX = CURRENT_IDX + 1
        CURRENT_TIME = CURRENT_TIME + DELTAT
        print*, CURRENT_IDX

        !Sleep for waiting data
        SLEEPLOOP: DO

            CALL DATE_AND_TIME(VALUES=TVAL)
            CRRTIME = datetime(TVAL(1),TVAL(2),TVAL(3),TVAL(5),TVAL(6),TVAL(7))
            IF(CURRENT_TIME.LE.CRRTIME) EXIT SLEEPLOOP
            DES = CURRENT_TIME - CRRTIME
            CALL SLEEP(DES%getSeconds())

        ENDDO SLEEPLOOP

        !Read data at current time
        CALL UPDATE_GATE_DATA

        !Calculating exactly mode
        DO I = 1,NBASIN

            CALL BASIN(I)%CALCULATING_RUNOFF

            CALL BASIN(I)%ROUTING_CALC
        ENDDO

        !Calculating predict mode
        ACTIVE_MODE = PREDICT_CALC_MODE
        CALL FORECASTING

        !Write result
        CALL WRITE_OUTPUT


        !TEST
        IF(CURRENT_IDX.EQ.24) STOP

      ENDDO




      END SUBROUTINE RUN_REAL_TIME_MODE