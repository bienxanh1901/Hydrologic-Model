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
      INTEGER :: I,J

      FILE_NAME = 'OUTPUT1.DAT'
      OPEN(UNIT = 10, FILE=TRIM(FILE_NAME), STATUS='REPLACE')

      DO J = 1,NTIME
        WRITE(10,*) REAL(J - 1,8)*DT/3600.0D0,(XF(I,J),QF(I,J), I=1,NBASING)
      ENDDO

      CLOSE(10)

      FILE_NAME = 'OUTPUT2.DAT'
      OPEN(UNIT = 10, FILE=TRIM(FILE_NAME), STATUS='REPLACE')

      DO J = 1,NTIME

        WRITE(10,*) REAL(J - 1,8)*DT/3600.0D0,(QIN(I,J),QDC(I,J), I=1,NSTOTAL)

      ENDDO

      CLOSE(10)


      FILE_NAME = 'OUTPUT3.DAT'
      OPEN(UNIT = 10, FILE=TRIM(FILE_NAME), STATUS='REPLACE')

      DO J = 1,NTIME

        WRITE(10,*) REAL(J - 1,8)*DT/3600.0D0,(V(I,J)/1000000.0D0,ZH(I,J), I=1,NRS)

      ENDDO

      CLOSE(10)

      RETURN
      END SUBROUTINE WRITE_OUTPUT
C=================================================================
C
C=================================================================
