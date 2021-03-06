      PROGRAM HYDRAULIC_MODELLING
      USE PARAM
      USE CONSTANTS
      IMPLICIT NONE

C Open log file
      OPEN(UNIT=ULOG, FILE=TRIM(FLOG),STATUS='REPLACE')

C Introduction
      CALL GETCWD(ROOT_DIR)
      CALL WRITE_LOG('WORKING DIRECTORY: ')
      CALL WRITE_LOG('  '//TRIM(ROOT_DIR))

C Read input parameters
      CALL READING_INPUT

C Find the connection of basing
      CALL BASIN_CONNECTION

C Allocate necessary variables
      CALL ALLOCATING_VARIABLES

C Initial variables
      CALL INITIALING_VARIABLES

C Starting calculate
      CALL CALCULATING_RUNOFF

      CALL ROUTING_CALC

C Write result
      CALL WRITE_OUTPUT

      CALL WRITE_LOG('END OF CALCULATION!!!')
      CLOSE(ULOG)

      WRITE(*,*) 'PRESS ANY KEY TO STOP!!!'
      READ(*,*)
      END PROGRAM HYDRAULIC_MODELLING

