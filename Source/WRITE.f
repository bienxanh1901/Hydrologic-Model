C=================================================================
C SUBROUTINE WRITE OUTPUT
C=================================================================
      SUBROUTINE WRITE_OUTPUT
      USE CALC
      USE BASING
      USE OBSERVATION
      USE ROUTING
      IMPLICIT NONE
      CHARACTER(30) :: FILE_NAME
      INTEGER :: I,J, K, L

      FILE_NAME = 'OUTPUT_FLOOD_CALCULATION.DAT'
      OPEN(UNIT = 10, FILE=TRIM(FILE_NAME), STATUS='REPLACE')

      DO J = 1,NTIME
        WRITE(10,*) REAL(J - 1,8)*DT/3600.0D0,(XF(I,J)*1000.0D0,QF(I,J), I=1,NBASING)
      ENDDO

      CLOSE(10)

      FILE_NAME = 'OUTPUT_FLOOD_ROUTING.DAT'
      OPEN(UNIT = 10, FILE=TRIM(FILE_NAME), STATUS='REPLACE')
      K = 0
      L = 0
      DO J = 1,NSTOTAL

        IF(FRTYPE(J).EQ.1) THEN

            L = L + 1
            WRITE(10,'(A,I1,A)') "POSITION_",J, " : RIVER"
            WRITE(10,*) (RIVER(L)%QINP(I),I=1,NTIME)
            WRITE(10,*) (QIN(J,I),I=1,NTIME)
            WRITE(10,*) (QDC(J,I),I=1,NTIME)

        ELSE

            K = K + 1
            WRITE(10,'(A,I1,A)') "POSITION_",J, " : RESERVOIR"
            WRITE(10,*) (RESERVOIR(K)%QINP(I),I=1,NTIME)
            WRITE(10,*) (QIN(J,I),I=1,NTIME)
            WRITE(10,*) (RESERVOIR(K)%QTB(I),I=1,NTIME)
            WRITE(10,*) (QDC(J,I),I=1,NTIME)
            WRITE(10,*) (V(K,I)/1000000.0D0,I=1,NTIME)
            WRITE(10,*) (ZH(K,I),I=1,NTIME)

        ENDIF

      ENDDO

      CLOSE(10)



      RETURN
      END SUBROUTINE WRITE_OUTPUT
C=================================================================
C
C=================================================================
