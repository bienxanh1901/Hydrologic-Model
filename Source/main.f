      PROGRAM FLOOD_MODELING
      USE PARAM
      USE CONSTANTS
      IMPLICIT NONE

C Introduction
!      CALL GETCWD(ROOT_DIR)
C Read input parameters
      WRITE(*,*) 'READING INPUT DATA!!!'
      CALL READING_INPUT
C Find the connection of basing
      WRITE(*,*) 'FINDING BASIN CONNECTION!!!'
      CALL BASIN_CONNECTION
C Allocate necessary variables
      WRITE(*,*) 'ALLOCATING MEMMORY!!!'
      CALL ALLOCATING_VARIABLES
C Initial variables
      WRITE(*,*) 'INITIALING VARIABLES!!!'
      CALL INITIALING_VARIABLES

C Starting calculate
      CALL CALCULATING_RUNOFF

      CALL ROUTING_CALC

C Write result
      CALL WRITE_OUTPUT

      WRITE(*,*) 'END OF CALCULATION!!!'
      STOP!
      END PROGRAM FLOOD_MODELING

