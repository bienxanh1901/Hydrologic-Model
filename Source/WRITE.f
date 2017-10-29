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
      CHARACTER(2) :: IWR


      DO I = 1,NBASING

        WRITE(IWR,'(I2.2)') I
        FILE_NAME = 'OUTPUT_BASING_'//IWR//'.DAT'
        OPEN(UNIT = 10, FILE=TRIM(FILE_NAME), STATUS='REPLACE')

        DO J = 0,NTIME - 1

            WRITE(10,*) REAL(J,8)*DT/3600.0D0, XF(I,J), LOSS(I,J), EXCESS(I,J),
     &                  BASE(I)%Q0, QF(I,J) - BASE(I)%Q0, QF(I,J)

        ENDDO

        CLOSE(10)

      ENDDO



      K = 0
      L = 0
      DO I = 1,NSTOTAL

        IF(FRTYPE(I).EQ.1) THEN

            L = L + 1
            WRITE(IWR,'(I2.2)') L
            FILE_NAME = 'OUTPUT_REACH_'//IWR//'.DAT'
            OPEN(UNIT = 10, FILE=TRIM(FILE_NAME), STATUS='REPLACE')

            DO J = 0,NTIME - 1

                WRITE(10,*) REAL(J,8)*DT/3600.0D0, QIN(I,J),QDC(I,J)

            ENDDO

            CLOSE(10)

        ELSE

            K = K + 1
            WRITE(IWR,'(I2.2)') K
            FILE_NAME = 'OUTPUT_RESERVOIR_'//IWR//'.DAT'
            OPEN(UNIT = 10, FILE=TRIM(FILE_NAME), STATUS='REPLACE')

            DO J = 0, NTIME - 1

                WRITE(10,*) QIN(I,J),QDC(I,J),V(K,J)/1000000.0D0, ZH(K,J)
            ENDDO

            CLOSE(10)
        ENDIF

      ENDDO





      RETURN
      END SUBROUTINE WRITE_OUTPUT
C=================================================================
C
C=================================================================
