C=================================================================
C CHECK FILE
C=================================================================
      SUBROUTINE CHK_FILE(FILE_NAME)
      USE CONSTANTS
      IMPLICIT NONE
      CHARACTER(*) :: FILE_NAME
      LOGICAL :: EX


      INQUIRE(FILE=TRIM(FILE_NAME), EXIST=EX)
      IF(.NOT.EX) THEN
        WRITE(*,*) "ERROR!!!"
        WRITE(*,'(3A)')"File ", TRIM(FILE_NAME), " does not exist in the current directory!!! "
        WRITE(ULOG,*) "ERROR!!!"
        WRITE(ULOG,'(3A)')"File ", TRIM(FILE_NAME), " does not exist in the current directory!!! "
        CLOSE(ULOG)
        STOP 'Stop program'
      ENDIF

      RETURN
      END SUBROUTINE CHK_FILE
C=================================================================
C
C=================================================================
C=================================================================
C CHECK MEMORY ALLOCATION
C=================================================================
      SUBROUTINE ChkMemErr(VarName,IZERO)
      USE CONSTANTS
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: VarName
      INTEGER IZERO

      IF (IZERO==0) RETURN
      WRITE(*,*) 'ERROR!!!'
      WRITE(*,'(2A)') 'Memory allocation failed for ', TRIM(VarName)
      WRITE(ULOG,*) 'ERROR!!!'
      WRITE(ULOG,'(2A)') 'Memory allocation failed for ', TRIM(VarName)
      CLOSE(ULOG)
      STOP 'Stop program'

      RETURN
      END SUBROUTINE ChkMemErr
C=================================================================
C
C=================================================================
