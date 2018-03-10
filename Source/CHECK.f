C=================================================================
C CHECK FILE
C=================================================================
      SUBROUTINE CHK_FILE(FILE_NAME)
      USE CONSTANTS
      IMPLICIT NONE
      CHARACTER(*) :: FILE_NAME
      LOGICAL :: EX

      INQUIRE(FILE=TRIM(FILE_NAME), EXIST=EX)

      IF(.NOT.EX) CALL WRITE_ERRORS('File '//TRIM(FILE_NAME)//' does not exist in the current directory!!! ')

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

      IF (IZERO.NE.0) CALL WRITE_ERRORS('Memory allocation failed for variables'//TRIM(VarName))

      RETURN
      END SUBROUTINE ChkMemErr
C=================================================================
C
C=================================================================
