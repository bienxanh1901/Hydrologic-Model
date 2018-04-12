      SUBROUTINE RUN_REAL_TIME_MODE
      USE TIME
      USE CALC_PARAM
      USE COMMON_PARAM
      IMPLICIT NONE
      INTEGER :: I
      INTEGER,DIMENSION(8) :: TVAL
      TYPE(datetime) :: CRRTIME
      TYPE(timedelta):: DES
      CHARACTER(100) :: LOGCMD


      DO

        ACTIVE_MODE = EXACTLY_CALC_MODE
        CURRENT_IDX = CURRENT_IDX + 1
        CURRENT_TIME = CURRENT_TIME + DELTAT
        WRITE(LOGCMD,'(A8,A)')"TIME: ", TRIM(CURRENT_TIME%strftime('%d-%m-%Y %H:%M'))
        CALL WRITE_LOG(TRIM(LOGCMD))
        CALL WRITE_LOG("---===")

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


        !Reset all array
        IF(CURRENT_IDX.EQ.NTIME - 2*NFCT) CALL RESET_MEMORIES
        !TEST
        IF(CURRENT_IDX.EQ.48) STOP

      ENDDO




      END SUBROUTINE RUN_REAL_TIME_MODE
