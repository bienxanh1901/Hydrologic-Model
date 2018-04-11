C=================================================================
C WRITE_OUTPUT
C=================================================================
      SUBROUTINE CREATE_OUTPUT_DIR
      USE COMMON_PARAM
      USE CALC_PARAM
      USE TIME
      USE CONSTANTS
      IMPLICIT NONE
      TYPE(BASIN_TYPE), POINTER :: BS
      INTEGER :: I
      CHARACTER(100) :: COMMAND

      CALL WRITE_LOG('CREATING OUTPUT DIRECTORIES!!!')

      !CALL get_environment_variable("PATH", FILE_PATH)
      FILE_PATH="\"
      COMMAND = 'mkdir '//TRIM(OUTPUT_DIR)
      CALL SYSTEM(TRIM(COMMAND))

      DO I = 1,NBASIN

        BS => BASIN(I)
        COMMAND = TRIM(OUTPUT_DIR)//TRIM(FILE_PATH)//TRIM(BS%NAME)
        CALL SYSTEM('mkdir '//TRIM(COMMAND))

      ENDDO

      RETURN
      END SUBROUTINE CREATE_OUTPUT_DIR
C=================================================================
C
C=================================================================
C=================================================================
C WRITE_OUTPUT
C=================================================================
      SUBROUTINE WRITE_OUTPUT
      USE COMMON_PARAM
      USE CALC_PARAM
      USE TIME
      USE CONSTANTS
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE WRITE_BASIN(BS)
        USE CALC_PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(BASIN_TYPE), POINTER :: BS
        END SUBROUTINE WRITE_BASIN
      END INTERFACE
      TYPE(BASIN_TYPE), POINTER :: BS
      INTEGER :: I

      DO I = 1,NBASIN

        BS => BASIN(I)
        CALL WRITE_BASIN(BS)

      ENDDO

      RETURN
      END SUBROUTINE WRITE_OUTPUT
C=================================================================
C
C=================================================================
C=================================================================
C WRITE_BASIN
C=================================================================
      SUBROUTINE WRITE_BASIN(BS)
      USE CALC_PARAM
      USE TIME
      USE CONSTANTS
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE WRITE_SUBASIN(SBS)
        USE CALC_PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        END SUBROUTINE WRITE_SUBASIN

        SUBROUTINE WRITE_REACH(RCH)
        USE CALC_PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(REACH_TYPE), POINTER :: RCH
        END SUBROUTINE WRITE_REACH

        SUBROUTINE WRITE_RESERVOIR(RES)
        USE CALC_PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        END SUBROUTINE WRITE_RESERVOIR

        SUBROUTINE WRITE_SUBASIN_FC(SBS)
        USE CALC_PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        END SUBROUTINE WRITE_SUBASIN_FC

        SUBROUTINE WRITE_REACH_FC(RCH)
        USE CALC_PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(REACH_TYPE), POINTER :: RCH
        END SUBROUTINE WRITE_REACH_FC

        SUBROUTINE WRITE_RESERVOIR_FC(RES)
        USE CALC_PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        END SUBROUTINE WRITE_RESERVOIR_FC
      END INTERFACE
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      TYPE(REACH_TYPE), POINTER :: RCH
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      INTEGER :: I
      CHARACTER(100) :: COMMAND

      DO I = 1,BS%NSUBBASIN

        SBS => BS%SUBBASIN(I)
        COMMAND = TRIM(OUTPUT_DIR)//TRIM(FILE_PATH)//TRIM(BS%NAME)//TRIM(FILE_PATH)//TRIM(SBS%NAME)//".csv"

        IF(SIMULATION_MODE.EQ.VALIDATION_MODE) THEN
            OPEN(UNIT=FUOUT1, FILE=TRIM(COMMAND), STATUS='REPLACE')
            CALL WRITE_SUBASIN(SBS)
        ELSE
            IF(CURRENT_TIME.EQ.START_TIME) THEN
                OPEN(UNIT=FUOUT1, FILE=TRIM(COMMAND), STATUS='REPLACE')
            ELSE
                OPEN(UNIT=FUOUT1, FILE=TRIM(COMMAND), ACCESS='APPEND')
                COMMAND = TRIM(OUTPUT_DIR)//TRIM(FILE_PATH)//TRIM(BS%NAME)//TRIM(FILE_PATH)//TRIM(SBS%NAME)//"_FC.csv"
                OPEN(UNIT=FUOUT2, FILE=TRIM(COMMAND), STATUS='REPLACE')
            ENDIF
            CALL WRITE_SUBASIN_FC(SBS)
        ENDIF

      ENDDO

      DO I = 1,BS%NREACH

        RCH => BS%REACH(I)
        COMMAND =TRIM(OUTPUT_DIR)//TRIM(FILE_PATH)//TRIM(BS%NAME)//TRIM(FILE_PATH)//TRIM(RCH%NAME)//".csv"

        IF(SIMULATION_MODE.EQ.VALIDATION_MODE) THEN
            OPEN(UNIT=FUOUT1, FILE=TRIM(COMMAND), STATUS='REPLACE')
            CALL WRITE_REACH(RCH)
        ELSE
            IF(CURRENT_TIME.EQ.START_TIME) THEN
                OPEN(UNIT=FUOUT1, FILE=TRIM(COMMAND), STATUS='REPLACE')
            ELSE
                OPEN(UNIT=FUOUT1, FILE=TRIM(COMMAND), ACCESS='APPEND')
                COMMAND =TRIM(OUTPUT_DIR)//TRIM(FILE_PATH)//TRIM(BS%NAME)//TRIM(FILE_PATH)//TRIM(RCH%NAME)//"_FC.csv"
                OPEN(UNIT=FUOUT2, FILE=TRIM(COMMAND), STATUS='REPLACE')
            ENDIF
            CALL WRITE_REACH_FC(RCH)
        ENDIF


      ENDDO

      DO I = 1,BS%NRESERVOIR

        RES => BS%RESERVOIR(I)
        COMMAND =TRIM(OUTPUT_DIR)//TRIM(FILE_PATH)//TRIM(BS%NAME)//TRIM(FILE_PATH)//TRIM(RES%NAME)//".csv"

        IF(SIMULATION_MODE.EQ.VALIDATION_MODE) THEN
            OPEN(UNIT=FUOUT1, FILE=TRIM(COMMAND), STATUS='REPLACE')
            CALL WRITE_RESERVOIR(RES)
        ELSE
            IF(CURRENT_TIME.EQ.START_TIME) THEN
                OPEN(UNIT=FUOUT1, FILE=TRIM(COMMAND), STATUS='REPLACE')
            ELSE
                OPEN(UNIT=FUOUT1, FILE=TRIM(COMMAND), ACCESS='APPEND')
                COMMAND =TRIM(OUTPUT_DIR)//TRIM(FILE_PATH)//TRIM(BS%NAME)//TRIM(FILE_PATH)//TRIM(RES%NAME)//"_FC.csv"
                OPEN(UNIT=FUOUT2, FILE=TRIM(COMMAND), STATUS='REPLACE')
            ENDIF
            CALL WRITE_RESERVOIR_FC(RES)
        ENDIF


      ENDDO

      RETURN
      END SUBROUTINE WRITE_BASIN
C=================================================================
C
C=================================================================
C=================================================================
C
C=================================================================
      SUBROUTINE WRITE_SUBASIN(SBS)
      USE COMMON_PARAM
      USE CALC_PARAM
      USE TIME
      USE CONSTANTS
      USE datetime_module
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      INTEGER :: N
      CHARACTER(16) :: DTWRT1, DTWRT2

      WRITE(FUOUT1,21) 'DAY','TIME', 'PRECIP(mm)','LOSS(mm)','EXCESS(mm)',
     &                'DIRECT FLOW(m3/s)', 'BASE FLOW(m3/s)', 'TOTAL FLOW(m3/s)'

      DO N = 0,NTIME - 1

        DTWRT1 = TIME_ARR(N)%strftime('%m/%d/%y')
        DTWRT2 = TIME_ARR(N)%strftime('%H:%M')
        WRITE(FUOUT1,22) '"'//TRIM(DTWRT1)//'"',TRIM(DTWRT2),SBS%AVGPRECIP(N), SBS%LOSS(N),
     &                  SBS%EXCESS(N), SBS%DIRECT_FLOW(N),
     &                  SBS%BASE_FLOW(N), SBS%TOTAL_FLOW(N)

      ENDDO

      CLOSE(FUOUT1)

      RETURN
21    FORMAT(8(A,','))
22    FORMAT(2(A,','),6(F15.8,','))
      END SUBROUTINE WRITE_SUBASIN
C=================================================================
C
C=================================================================
C=================================================================
C
C=================================================================
      SUBROUTINE WRITE_SUBASIN_FC(SBS)
      USE COMMON_PARAM
      USE CALC_PARAM
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      TYPE(datetime) :: TIMEP
      INTEGER :: N
      CHARACTER(16) :: DTWRT1, DTWRT2

      !Write Calculation result
      IF(CURRENT_TIME.EQ.START_TIME) THEN
        WRITE(FUOUT1,21) 'DAY','TIME', 'PRECIP(mm)','LOSS(mm)','EXCESS(mm)',
     &                  'DIRECT FLOW(m3/s)', 'BASE FLOW(m3/s)', 'TOTAL FLOW(m3/s)'
      ENDIF
      N = CURRENT_IDX
      DTWRT1 = CURRENT_TIME%strftime('%m/%d/%y')
      DTWRT2 = CURRENT_TIME%strftime('%H:%M')
      WRITE(FUOUT1,22) '"'//TRIM(DTWRT1)//'"',TRIM(DTWRT2),SBS%AVGPRECIP(N), SBS%LOSS(N),
     &                  SBS%EXCESS(N), SBS%DIRECT_FLOW(N),
     &                  SBS%BASE_FLOW(N), SBS%TOTAL_FLOW(N)


      CLOSE(FUOUT1)
      IF(CURRENT_TIME.EQ.START_TIME) RETURN

      !Write forecasting result
      WRITE(FUOUT2,21) 'DAY','TIME', 'PRECIP(mm)','LOSS(mm)','EXCESS(mm)',
     &                'DIRECT FLOW(m3/s)', 'BASE FLOW(m3/s)', 'TOTAL FLOW(m3/s)'
      TIMEP = CURRENT_TIME
      DO N = CURRENT_IDX + 1,CURRENT_IDX + NFCT
        TIMEP = TIMEP + DELTAT
        DTWRT1 = TIMEP%strftime('%m/%d/%y')
        DTWRT2 = TIMEP%strftime('%H:%M')
        WRITE(FUOUT2,22) '"'//TRIM(DTWRT1)//'"',TRIM(DTWRT2),SBS%AVGPRECIP(N), SBS%LOSS(N),
     &                  SBS%EXCESS(N), SBS%DIRECT_FLOW(N),
     &                  SBS%BASE_FLOW(N), SBS%TOTAL_FLOW(N)

      ENDDO

      CLOSE(FUOUT2)

      RETURN
21    FORMAT(8(A,','))
22    FORMAT(2(A,','),6(F15.8,','))
      END SUBROUTINE WRITE_SUBASIN_FC
C=================================================================
C
C=================================================================

C=================================================================
C
C=================================================================
      SUBROUTINE WRITE_REACH(RCH)
      USE CALC_PARAM
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(REACH_TYPE), POINTER :: RCH
      INTEGER :: N
      CHARACTER(16) :: DTWRT1, DTWRT2

      WRITE(FUOUT1,21) 'DAY','TIME', 'INFLOW (m3/s)', 'OUTFLOW (m3/s)'

      DO N = 0,NTIME - 1

        DTWRT1 = TIME_ARR(N)%strftime('%m/%d/%y')
        DTWRT2 = TIME_ARR(N)%strftime('%H:%M')
        WRITE(FUOUT1,22) TRIM(DTWRT1),TRIM(DTWRT2),RCH%INFLOW(N), RCH%OUTFLOW(N)

      ENDDO

      CLOSE(FUOUT1)

      RETURN
21    FORMAT(4(A,','))
22    FORMAT(2(A,','),2(F15.8,','))
      END SUBROUTINE WRITE_REACH
C=================================================================
C
C=================================================================

C=================================================================
C
C=================================================================
      SUBROUTINE WRITE_REACH_FC(RCH)
      USE CALC_PARAM
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(REACH_TYPE), POINTER :: RCH
      TYPE(datetime) :: TIMEP
      INTEGER :: N
      CHARACTER(16) :: DTWRT1, DTWRT2

      !Write Calculation result
      IF(CURRENT_TIME.EQ.START_TIME) THEN
        WRITE(FUOUT1,21) 'DAY','TIME', 'INFLOW (m3/s)', 'OUTFLOW (m3/s)'
      ENDIF
      N = CURRENT_IDX
      DTWRT1 = CURRENT_TIME%strftime('%m/%d/%y')
      DTWRT2 = CURRENT_TIME%strftime('%H:%M')
      WRITE(FUOUT1,22) TRIM(DTWRT1),TRIM(DTWRT2),RCH%INFLOW(N), RCH%OUTFLOW(N)

      CLOSE(FUOUT1)
      IF(CURRENT_TIME.EQ.START_TIME) RETURN

      !Write forecasting result
      WRITE(FUOUT2,21) 'DAY','TIME', 'INFLOW (m3/s)', 'OUTFLOW (m3/s)'
      TIMEP = CURRENT_TIME
      DO N = CURRENT_IDX + 1,CURRENT_IDX + NFCT
        TIMEP = TIMEP + DELTAT
        DTWRT1 = TIMEP%strftime('%m/%d/%y')
        DTWRT2 = TIMEP%strftime('%H:%M')
        WRITE(FUOUT2,22) TRIM(DTWRT1),TRIM(DTWRT2),RCH%INFLOW(N), RCH%OUTFLOW(N)

      ENDDO

      CLOSE(FUOUT2)

      RETURN
21    FORMAT(4(A,','))
22    FORMAT(2(A,','),2(F15.8,','))
      END SUBROUTINE WRITE_REACH_FC
C=================================================================
C
C=================================================================
C=================================================================
C WRITE_RESERVOIR
C=================================================================
      SUBROUTINE WRITE_RESERVOIR(RES)
      USE CALC_PARAM
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(RESERVOIR_TYPE), POINTER :: RES

      INTEGER :: N
      CHARACTER(16) :: DTWRT1, DTWRT2

      !Write Calculation result
      WRITE(FUOUT1,21) 'DAY','TIME', 'INFLOW(m3/s)',
     &                'STORAGE(1000m3)', 'ELEVATION(m)', 'OUTFLOW(m3/s)'

      DO N = 0,NTIME - 1

        DTWRT1 = TIME_ARR(N)%strftime('%m/%d/%y')
        DTWRT2 = TIME_ARR(N)%strftime('%H:%M')
        WRITE(FUOUT1,22) TRIM(DTWRT1),TRIM(DTWRT2),RES%INFLOW(N), RES%STORAGE(N)/1000.0D0,
     &                  RES%ELEVATION(N), RES%OUTFLOW(N)

      ENDDO

      CLOSE(FUOUT1)

      RETURN
21    FORMAT(6(A,','))
22    FORMAT(2(A,','),4(F15.8,','))
      END SUBROUTINE WRITE_RESERVOIR
C=================================================================
C
C=================================================================
C=================================================================
C WRITE_RESERVOIR
C=================================================================
      SUBROUTINE WRITE_RESERVOIR_FC(RES)
      USE CALC_PARAM
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      TYPE(datetime) :: TIMEP
      INTEGER :: N
      CHARACTER(16) :: DTWRT1, DTWRT2

      IF(CURRENT_TIME.EQ.START_TIME) THEN
        WRITE(FUOUT1,21) 'DAY','TIME', 'INFLOW(m3/s)',
     &                  'STORAGE(1000m3)', 'ELEVATION(m)', 'OUTFLOW(m3/s)'
      ENDIF
      N = CURRENT_IDX
      DTWRT1 = CURRENT_TIME%strftime('%m/%d/%y')
      DTWRT2 = CURRENT_TIME%strftime('%H:%M')
      WRITE(FUOUT1,22) TRIM(DTWRT1),TRIM(DTWRT2),RES%INFLOW(N), RES%STORAGE(N)/1000.0D0,
     &                  RES%ELEVATION(N), RES%OUTFLOW(N)

      CLOSE(FUOUT1)
      IF(CURRENT_TIME.EQ.START_TIME) RETURN

      !Write forecasting result
      WRITE(FUOUT2,21) 'DAY','TIME', 'INFLOW(m3/s)',
     &                  'STORAGE(1000m3)', 'ELEVATION(m)', 'OUTFLOW(m3/s)'
      TIMEP = CURRENT_TIME
      DO N = CURRENT_IDX + 1,CURRENT_IDX + NFCT
        TIMEP = TIMEP + DELTAT
        DTWRT1 = TIMEP%strftime('%m/%d/%y')
        DTWRT2 = TIMEP%strftime('%H:%M')
        WRITE(FUOUT2,22) TRIM(DTWRT1),TRIM(DTWRT2),RES%INFLOW(N), RES%STORAGE(N)/1000.0D0,
     &                  RES%ELEVATION(N), RES%OUTFLOW(N)

      ENDDO

      CLOSE(FUOUT2)

      RETURN
21    FORMAT(6(A,','))
22    FORMAT(2(A,','),4(F15.8,','))
      END SUBROUTINE WRITE_RESERVOIR_FC
C=================================================================
C
C=================================================================
      SUBROUTINE WRITE_LOG(TRACE_LOG)
      USE CONSTANTS
      IMPLICIT NONE
      CHARACTER(*) :: TRACE_LOG

      WRITE(*,*) TRIM(TRACE_LOG)
      WRITE(ULOG,*) TRIM(TRACE_LOG)

      WRITE(*,*)
      WRITE(ULOG,*)


      RETURN
      END SUBROUTINE WRITE_LOG
C=================================================================
C
C=================================================================
      SUBROUTINE WRITE_ERRORS(ERROR_LOG)
      USE CONSTANTS
      IMPLICIT NONE
      CHARACTER(*) :: ERROR_LOG

      WRITE(*,*) '      ERROR!!!'
      WRITE(ULOG,*) '       ERROR!!!'
      WRITE(*,*) '      ', TRIM(ERROR_LOG)
      WRITE(ULOG,*) '       ',TRIM(ERROR_LOG)

      WRITE(*,*)
      WRITE(ULOG,*)

      CLOSE(ULOG)
      STOP


      RETURN
      END SUBROUTINE WRITE_ERRORS
