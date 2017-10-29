      PROGRAM FLOOD_MODELING
      USE OBSERVATION
      USE CALC
      USE BASING
      USE ROUTING
      IMPLICIT NONE

C Read input parameters
      WRITE(*,*) 'READ INPUT DATA!!!'
      CALL READ_INPUT
C Loss model
      WRITE(*,*) 'CALCULATING LOSS'
      ALLOCATE(LOSS(1:NBASING, 0:NTIME - 1))
      ALLOCATE(EXCESS(1:NBASING, 0:NTIME - 1))
      LOSS = 0.0D0
      EXCESS = 0.0D0
      CALL SCS_CURVE_NUMBER
C Flood calculation
      WRITE(*,*) 'CALCULATING FLOW!!!'
      ALLOCATE(QF(1:NBASING, 0:NTIME - 1))
      IF(MODEL.EQ.'UHG') THEN

        CALL UHG_CALC

      ELSE IF (MODEL.EQ.'NAM') THEN

        CALL NAM_MODEL_CALC

      ENDIF

C Flood routing
      IF(ISROUTING) THEN
        WRITE(*,*) 'FLOOD ROUTING!!!'
        ALLOCATE(QDC(1:NSTOTAL, 0:NTIME - 1))
        ALLOCATE(QIN(1:NSTOTAL, 0:NTIME - 1))
        IF(NRS.GT.0) THEN
            ALLOCATE(ZH(1:NRS, 0:NTIME - 1))
            ALLOCATE(V(1:NRS, 0:NTIME - 1))
            ZH = 0.0D0
            V = 0.0D0
        ENDIF
        QDC = 0.0D0
        QIN = 0.0D0
        CALL ROUTING_CALC

      ENDIF
C Write output
      WRITE(*,*) 'WRITE OUTPUT DATA!!!'
      CALL WRITE_OUTPUT

      END PROGRAM FLOOD_MODELING

