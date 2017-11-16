C=================================================================
C WRITE_OUTPUT
C=================================================================
      SUBROUTINE WRITE_OUTPUT
      USE PARAM
      USE TIME
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

      COMMAND = 'mkdir -p '//TRIM(OUTPUT_DIR)
      CALL SYSTEM(TRIM(COMMAND))
      CALL SYSTEM('cd '//TRIM(OUTPUT_DIR))

      DO I = 1,NBASIN

        BS => BASIN(I)
        COMMAND = 'mkdir -p '//TRIM(BS%NAME)
        CALL SYSTEM(TRIM(COMMAND))
        CALL SYSTEM('cd '//TRIM(BS%NAME))

        CALL WRITE_BASIN(BS)

        CALL SYSTEM('cd ..')


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
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE WRITE_SUBASING(SBS)
        USE PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(SUBBASIN_TYPE), POINTER :: SBS
        END SUBROUTINE WRITE_SUBASING

        SUBROUTINE WRITE_REACH(RCH)
        USE PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(REACH_TYPE), POINTER :: RCH
        END SUBROUTINE WRITE_REACH

        SUBROUTINE WRITE_RESERVOIR(RES)
        USE PARAM
        USE TIME
        IMPLICIT NONE
        TYPE(RESERVOIR_TYPE), POINTER :: RES
        END SUBROUTINE WRITE_RESERVOIR
      END INTERFACE
      TYPE(BASIN_TYPE), POINTER :: BS
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      TYPE(REACH_TYPE), POINTER :: RCH
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      INTEGER :: I


      DO I = 1,BS%NSUBBASIN

        SBS => BS%SUBBASIN(I)
        CALL WRITE_SUBASING(SBS)

      ENDDO

      DO I = 1,BS%NREACH

        RCH => BS%REACH(I)
        CALL WRITE_REACH(RCH)

      ENDDO

      DO I = 1,BS%NRESERVOIR

        RES => BS%RESERVOIR(I)
        CALL WRITE_RESERVOIR(RES)

      ENDDO

      RETURN
      END SUBROUTINE WRITE_BASIN
C=================================================================
C
C=================================================================
C=================================================================
C WRITE_SUBASING
C=================================================================
      SUBROUTINE WRITE_SUBASING(SBS)
      USE PARAM
      USE TIME
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(SUBBASIN_TYPE), POINTER :: SBS
      INTEGER :: N, FUNIT

      FUNIT = 20
      OPEN(UNIT=FUNIT, FILE=TRIM(SBS%NAME)//'.csv', STATUS='REPLACE')
      WRITE(FUNIT,21) 'TIME', 'PRECIP(mm)','LOSS(mm)','EXCESS(mm)',
     &                'DIRECT FLOW(m3/s)', 'BASE FLOW(m3/s)', 'TOTAL FLOW(m3/s)'
      DO N = 0,NTIME - 1

        WRITE(FUNIT,22) N,SBS%PRECIP%GATE_DATA(N), SBS%LOSS(N),
     &                  SBS%EXCESS(N), SBS%DIRECT_FLOW(N),
     &                  SBS%BASE_FLOW(N), SBS%TOTAL_FLOW(N)

      ENDDO

      CLOSE(FUNIT)

      RETURN
21    FORMAT(7(A,','))
22    FORMAT(I5,',',6(F15.8,','))
      END SUBROUTINE WRITE_SUBASING
C=================================================================
C
C=================================================================
C=================================================================
C WRITE_REACH
C=================================================================
      SUBROUTINE WRITE_REACH(RCH)
      USE PARAM
      USE TIME
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(REACH_TYPE), POINTER :: RCH
      INTEGER :: N, FUNIT

      FUNIT = 20
      OPEN(UNIT=FUNIT, FILE=TRIM(RCH%NAME)//'.csv', STATUS='REPLACE')
      WRITE(FUNIT,21) 'TIME', 'INFLOW (m3/s)', 'OUTFLOW (m3/s)'
      DO N = 0,NTIME - 1

        WRITE(FUNIT,22) N,RCH%INFLOW(N), RCH%OUTFLOW(N)

      ENDDO

      CLOSE(FUNIT)

      RETURN
21    FORMAT(3(A,','))
22    FORMAT(I5,',',2(F15.8,','))
      END SUBROUTINE WRITE_REACH
C=================================================================
C
C=================================================================
C=================================================================
C WRITE_RESERVOIR
C=================================================================
      SUBROUTINE WRITE_RESERVOIR(RES)
      USE PARAM
      USE TIME
      IMPLICIT NONE
      INTERFACE
      END INTERFACE
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      INTEGER :: N, FUNIT

      FUNIT = 20
      OPEN(UNIT=FUNIT, FILE=TRIM(RES%NAME)//'.csv', STATUS='REPLACE')
      WRITE(FUNIT,21) 'TIME', 'INFLOW(m3/s)',
     &                'STORAGE(m3)', 'ELEVATION(m)', 'OUTFLOW(m3/s)'
      DO N = 0,NTIME - 1

        WRITE(FUNIT,22) N,RES%INFLOW(N), RES%STORAGE(N),
     &                  RES%ELEVATION(N), RES%OUTFLOW(N)

      ENDDO

      CLOSE(FUNIT)

      RETURN
21    FORMAT(5(A,','))
22    FORMAT(I5,',',4(F15.8,','))
      END SUBROUTINE WRITE_RESERVOIR
C=================================================================
C
C=================================================================
