C=================================================================
C WRITE_OUTPUT
C=================================================================
      SUBROUTINE WRITE_OUTPUT
      USE PARAM
      USE TIME
      USE CONSTANTS
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE WRITE_BASIN(BS)
        USE PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(BASIN_TYPE), POINTER :: BS
        END SUBROUTINE WRITE_BASIN
      END INTERFACE
      TYPE(BASIN_TYPE), POINTER :: BS
      INTEGER :: I
      CHARACTER(100) :: COMMAND

      !CALL get_environment_variable("PATH", FILE_PATH)
      FILE_PATH="\"
      COMMAND = 'mkdir '//TRIM(OUTPUT_DIR)
      CALL SYSTEM(TRIM(COMMAND))

      DO I = 1,NBASIN

        BS => BASIN(I)
        COMMAND = 'mkdir '//TRIM(OUTPUT_DIR)//TRIM(FILE_PATH)//TRIM(BS%NAME)
        CALL SYSTEM(TRIM(COMMAND))

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
      USE PARAM
      USE TIME
      USE CONSTANTS
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE WRITE_SUBASING(SBS,FUNIT)
        USE PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        INTEGER, INTENT(IN) :: FUNIT
        END SUBROUTINE WRITE_SUBASING

        SUBROUTINE WRITE_REACH(RCH,FUNIT)
        USE PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(REACH_TYPE), POINTER :: RCH
        INTEGER, INTENT(IN) :: FUNIT
        END SUBROUTINE WRITE_REACH

        SUBROUTINE WRITE_RESERVOIR(RES,FUNIT)
        USE PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        INTEGER, INTENT(IN) :: FUNIT
        END SUBROUTINE WRITE_RESERVOIR
      END INTERFACE
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      TYPE(REACH_TYPE), POINTER :: RCH
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      INTEGER :: I, FUNIT
      CHARACTER(100) :: COMMAND

      FUNIT = 20
      DO I = 1,BS%NSUBBASIN

        SBS => BS%SUBBASIN(I)
        COMMAND = TRIM(OUTPUT_DIR)//TRIM(FILE_PATH)//TRIM(BS%NAME)//TRIM(FILE_PATH)//TRIM(SBS%NAME)//".csv"
        OPEN(UNIT=FUNIT, FILE=TRIM(COMMAND), STATUS='REPLACE')
        CALL WRITE_SUBASING(SBS,FUNIT)

      ENDDO

      DO I = 1,BS%NREACH

        RCH => BS%REACH(I)
        COMMAND =TRIM(OUTPUT_DIR)//TRIM(FILE_PATH)//TRIM(BS%NAME)//TRIM(FILE_PATH)//TRIM(RCH%NAME)//".csv"
        OPEN(UNIT=FUNIT, FILE=TRIM(COMMAND), STATUS='REPLACE')
        CALL WRITE_REACH(RCH,FUNIT)

      ENDDO

      DO I = 1,BS%NRESERVOIR

        RES => BS%RESERVOIR(I)
        COMMAND =TRIM(OUTPUT_DIR)//TRIM(FILE_PATH)//TRIM(BS%NAME)//TRIM(FILE_PATH)//TRIM(RES%NAME)//".csv"
        OPEN(UNIT=FUNIT, FILE=TRIM(COMMAND), STATUS='REPLACE')
        CALL WRITE_RESERVOIR(RES,FUNIT)

      ENDDO

      RETURN
      END SUBROUTINE WRITE_BASIN
C=================================================================
C
C=================================================================
C=================================================================
C WRITE_SUBASING
C=================================================================
      SUBROUTINE WRITE_SUBASING(SBS,FUNIT)
      USE PARAM
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      INTEGER, INTENT(IN) :: FUNIT
      INTEGER :: N
      CHARACTER(16) :: DTWRT1, DTWRT2

      WRITE(FUNIT,21) 'DAY','TIME', 'PRECIP(mm)','LOSS(mm)','EXCESS(mm)',
     &                'DIRECT FLOW(m3/s)', 'BASE FLOW(m3/s)', 'TOTAL FLOW(m3/s)'

      DO N = 0,NTIME - 1

        DTWRT1 = TIME_ARR(N)%strftime('%m/%d/%y')
        DTWRT2 = TIME_ARR(N)%strftime('%H:%M')
        WRITE(FUNIT,22) TRIM(DTWRT1),TRIM(DTWRT2),SBS%PRECIP%GATE_DATA(N), SBS%LOSS(N),
     &                  SBS%EXCESS(N), SBS%DIRECT_FLOW(N),
     &                  SBS%BASE_FLOW(N), SBS%TOTAL_FLOW(N)

      ENDDO

      CLOSE(FUNIT)

      RETURN
21    FORMAT(8(A,','))
22    FORMAT(2(A,','),6(F15.8,','))
      END SUBROUTINE WRITE_SUBASING
C=================================================================
C
C=================================================================
C=================================================================
C WRITE_REACH
C=================================================================
      SUBROUTINE WRITE_REACH(RCH,FUNIT)
      USE PARAM
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(REACH_TYPE), POINTER :: RCH
      INTEGER, INTENT(IN) :: FUNIT
      INTEGER :: N
      CHARACTER(16) :: DTWRT1, DTWRT2

      WRITE(FUNIT,21) 'DAY','TIME', 'INFLOW (m3/s)', 'OUTFLOW (m3/s)'

      DO N = 0,NTIME - 1

        DTWRT1 = TIME_ARR(N)%strftime('%m/%d/%y')
        DTWRT2 = TIME_ARR(N)%strftime('%H:%M')
        WRITE(FUNIT,22) TRIM(DTWRT1),TRIM(DTWRT2),RCH%INFLOW(N), RCH%OUTFLOW(N)

      ENDDO

      CLOSE(FUNIT)

      RETURN
21    FORMAT(4(A,','))
22    FORMAT(2(A,','),2(F15.8,','))
      END SUBROUTINE WRITE_REACH
C=================================================================
C
C=================================================================
C=================================================================
C WRITE_RESERVOIR
C=================================================================
      SUBROUTINE WRITE_RESERVOIR(RES,FUNIT)
      USE PARAM
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      INTEGER, INTENT(IN) :: FUNIT
      INTEGER :: N
      CHARACTER(16) :: DTWRT1, DTWRT2

      WRITE(FUNIT,21) 'DAY','TIME', 'INFLOW(m3/s)',
     &                'STORAGE(1000m3)', 'ELEVATION(m)', 'OUTFLOW(m3/s)'

      DO N = 0,NTIME - 1

        DTWRT1 = TIME_ARR(N)%strftime('%m/%d/%y')
        DTWRT2 = TIME_ARR(N)%strftime('%H:%M')
        WRITE(FUNIT,22) TRIM(DTWRT1),TRIM(DTWRT2),RES%INFLOW(N), RES%STORAGE(N)/1000.0D0,
     &                  RES%ELEVATION(N), RES%OUTFLOW(N)

      ENDDO

      CLOSE(FUNIT)

      RETURN
21    FORMAT(6(A,','))
22    FORMAT(2(A,','),4(F15.8,','))
      END SUBROUTINE WRITE_RESERVOIR
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
