C=================================================================
C SUBROUTINE WRITE OUTPUT
C=================================================================
      SUBROUTINE WRITE_OUTPUT
      USE CALC
      USE BASING
      USE OBSERVATION
      IMPLICIT NONE
      CHARACTER(30) :: FILE_NAME
      INTEGER :: I,J

      FILE_NAME = 'OUTPUT_FILE1.DAT'
      OPEN(UNIT = 10, FILE=TRIM(FILE_NAME), STATUS='REPLACE')

      DO J = 1,NTIME
        WRITE(10,*) REAL(J - 1,8)*DT/3600.0D0,(XF(I,J),QF(I,J), I=1,NBASING)
      ENDDO

      CLOSE(10)


      RETURN
      END SUBROUTINE WRITE_OUTPUT
C=================================================================
C
C=================================================================
