      PROGRAM FLOOD_MODELING
      USE PARAM
      USE CONSTANTS
      IMPLICIT NONE
      REAL(8) :: T
      INTEGER :: I

*C Read input parameters
*      WRITE(*,*) 'READ INPUT DATA!!!'
*      CALL READ_INPUT
*      CALL BASIN_CONNECTION
*
*C Initial variables at start time
*C Start Main loop
*      T = 0.0D0
*      I = 0
*      MAIN_LOOP:DO
*
*        T = T + DT/3600.0D0
*        I = I + 1
*
*C Loss model
*
*        CALL LOSS_CALCULATION(I)
*C Flood calculation
*
*        CALL TRANSFORM_ CALCULATION(I)
*
*C Flood routing
*
*        CALL ROUTING_CALCULATION(I)
*
*C Write output
*
*        CALL WRITE_OUTPUT(T, I)
*
*        !Exit main loop
*        IF(I.EQ.NTIME - 1) EXIT MAIN_LOOP
*
*      ENDDO MAIN_LOOP


      WRITE(*,*) 'END OF CALCULATION!!!'
      END PROGRAM FLOOD_MODELING

