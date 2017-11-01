C=================================================================
C SUBROUTINE READ INPUT
C=================================================================
      SUBROUTINE READ_INPUT
      USE PARAM
      USE CONSTANTS
      USE UNIT_HYDROGRAPH
      IMPLICIT NONE
      INTEGER :: FUNIT
      CHARACTER(30) :: F1
      LOGICAL :: EX
      NAMELIST /BASING/ NSUBBASING, NSOURCE, NREACH, NRESERVOIR,
     &                  NTIME, DT, START_DATE, START_TIME, END_DATE, END_TIME


C Open input file
      F1 = 'Input.dat'
      INQUIRE(FILE=TRIM(F1), EXIST=EX)
      IF(.NOT.EX) THEN
        WRITE(*,*) "ERROR!!!"
        WRITE(*,'(3A)')"File ", TRIM(F1), " does not exist in the current directory!!! "
        STOP
      ENDIF

      FUNIT = 10
      OPEN(UNIT=FUNIT, FILE=TRIM(F1),STATUS='OLD')

C Read name list BASING
      NSUBBASING = 0
      NSOURCE = 0
      NREACH = 0
      NRESERVOIR = 0
      NTIME = 0
      DT = 0.0D0
      START_DATE = ""
      START_TIME = ""
      END_DATE = ""
      END_TIME = ""
      READ(FUNIT,BASING,ERR=99)
      CLOSE(FUNIT)

C Read sub basing
      IF(NSUBBASING.GT.0) CALL READ_SUB_BASING

C Read source
      IF(NSOURCE.GT.0) CALL READ_SOURCE

C Read reach
      IF(NREACH.GT.0) CALL READ_REACH

C Read sub basing
      IF(NRESERVOIR.GT.0) CALL READ_RESERVOIR

C Set date and time
      CALL SET_DATE_TIME

      RETURN
99    WRITE(*,*) 'ERROR WHILE READING FILE: ', TRIM(F1)
      STOP
      END SUBROUTINE READ_INPUT
C=================================================================
C
C=================================================================

C=================================================================
C READ SUB BASING INPUT
C=================================================================
      SUBROUTINE READ_SUB_BASING
      USE PARAM
      USE CONSTANTS
      USE UNIT_HYDROGRAPH
      IMPLICIT NONE
      INTEGER :: FUNIT
      CHARACTER(30) :: F1
      LOGICAL :: EX
      NAMELIST /SBNL/ NSUBBASING, NSOURCE, NREACH, NRESERVOIR,
     &                  NTIME, DT, START_DATE, START_TIME, END_DATE, END_TIME



      ALLOCATER(SUBBASING(1:NSUBBASING))


      RETURN
      END SUBROUTINE READ_INPUT_CHAR
C=================================================================
C
C=================================================================
C=================================================================
C READ SOURCE INPUT
C=================================================================
      SUBROUTINE READ_SOURCE
      IMPLICIT NONE

      RETURN
      END SUBROUTINE READ_INPUT_CHAR
C=================================================================
C
C=================================================================
C=================================================================
C READ REACH INPUT
C=================================================================
      SUBROUTINE READ_REACH
      IMPLICIT NONE

      RETURN
      END SUBROUTINE READ_INPUT_CHAR
C=================================================================
C
C=================================================================
C=================================================================
C READ RESERVOIR INPUT
C=================================================================
      SUBROUTINE READ_RESERVOIR
      IMPLICIT NONE

      RETURN
      END SUBROUTINE READ_INPUT_CHAR
C=================================================================
C
C=================================================================
