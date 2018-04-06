      MODULE RESERVOIR_MOD
      USE datetime_module
      USE GATE_MOD
      USE CONSTANTS
      IMPLICIT NONE

      PUBLIC :: RESERVOIR_TYPE
      PUBLIC :: RESERVOIR_PTR

      TYPE RESERVOIR_TYPE

        CHARACTER(100) :: NAME
        !Level
        INTEGER :: LEVEL = 0
        !Routing method
        INTEGER :: ROUTE
        !Parameter for reservoir structure method
        REAL(8) :: Z0, DOORW, DC_COEFF, ZSW

        !Storage method
        INTEGER :: ROUTING_CURVE
        !Storage-elevation relation
        INTEGER :: NSE
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: SE_CURVE
        !Discharge control
        INTEGER :: NED
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: ED_CURVE
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: EH_CURVE
        INTEGER, ALLOCATABLE, DIMENSION(:) :: NDOOR
        !Turbin discharge
        INTEGER :: TB_TYPE
        REAL(8) :: TB_CONST_DATA
        TYPE(GATE_TYPE), POINTER :: TURBIN_GATE
        !Output
        REAL(8), ALLOCATABLE, DIMENSION(:) :: INFLOW, OUTFLOW, STORAGE, ELEVATION
        !Downstream
        CHARACTER(100) :: DOWNSTREAM

        CONTAINS

        PROCEDURE,PASS(SEFT), PUBLIC :: SET_ROUTING_CURVE
        PROCEDURE,PASS(SEFT), PUBLIC :: SET_ROUTING_PARAM
        PROCEDURE,PASS(SEFT), PUBLIC :: SET_TURBIN_PARAM
        PROCEDURE,PASS(SEFT), PUBLIC :: RESALLOCATING


      END TYPE RESERVOIR_TYPE

      TYPE RESERVOIR_PTR
        !POINTER
        TYPE(RESERVOIR_TYPE), POINTER :: RESEVOIR
      END TYPE RESERVOIR_PTR

      INTERFACE RESERVOIR_TYPE
        MODULE PROCEDURE :: RESERVOIR_TYPE_CONSTRUCTOR
      END INTERFACE RESERVOIR_TYPE

      CONTAINS

        !Constructor
        PURE ELEMENTAL TYPE(RESERVOIR_TYPE) FUNCTION RESERVOIR_TYPE_CONSTRUCTOR(NAME, DOWNSTREAM, Z0)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN) :: NAME, DOWNSTREAM
        REAL(8), INTENT(IN) :: Z0


        RESERVOIR_TYPE_CONSTRUCTOR%NAME = TRIM(NAME)
        RESERVOIR_TYPE_CONSTRUCTOR%DOWNSTREAM = TRIM(DOWNSTREAM)
        RESERVOIR_TYPE_CONSTRUCTOR%Z0 = Z0

        END FUNCTION RESERVOIR_TYPE_CONSTRUCTOR

        !Set relation curve
        !At present the curve is storage - elevation curve
        SUBROUTINE SET_ROUTING_CURVE(SEFT, CURVE_FILE)

            CLASS(RESERVOIR_TYPE), INTENT(INOUT) :: SEFT
            CHARACTER(*), INTENT(IN) :: CURVE_FILE
            INTEGER :: FU, IERR, J

            FU = 10
            !Read storage - elevation curve
            CALL CHK_FILE(TRIM(CURVE_FILE))
            OPEN(UNIT=FU, FILE=TRIM(CURVE_FILE), STATUS='OLD')

            READ(FU,*) SEFT%NSE

            ALLOCATE(SEFT%SE_CURVE(1:2,1:SEFT%NSE), STAT=IERR)
            CALL ChkMemErr('STORAGE-ELEVATION-CURVE', IERR)

            DO J = 1, SEFT%NSE

                READ(FU,*) SEFT%SE_CURVE(1:2,J)

            ENDDO

            SEFT%SE_CURVE(2,:) = SEFT%SE_CURVE(2,:)*1000.0D0

            CLOSE(FU)

        END SUBROUTINE SET_ROUTING_CURVE

        ! set parameter for routing
        SUBROUTINE SET_ROUTING_PARAM(SEFT, ROUTE, ROUTE_FILE, DOORW, DC_COEFF, ZSW)

            CLASS(RESERVOIR_TYPE), INTENT(INOUT) :: SEFT
            INTEGER, INTENT(IN) :: ROUTE
            REAL(8), INTENT(IN) :: DOORW, DC_COEFF, ZSW
            CHARACTER(*), INTENT(IN) :: ROUTE_FILE
            INTEGER :: FU, IERR, J

            FU = 10
            SEFT%ROUTE = ROUTE
            CALL CHK_FILE(TRIM(ROUTE_FILE))
            OPEN(UNIT=FU, FILE=TRIM(ROUTE_FILE), STATUS='OLD')

            READ(FU,*) SEFT%NED

            IF(ROUTE.EQ.OUTFLOW_STRUCTURE) THEN

                SEFT%DOORW = DOORW
                SEFT%DC_COEFF = DC_COEFF
                SEFT%ZSW = ZSW

                ALLOCATE(SEFT%NDOOR(1:SEFT%NED), STAT=IERR)
                CALL ChkMemErr('NDOOR_OPEN', IERR)
                ALLOCATE(SEFT%EH_CURVE(1:2,1:SEFT%NED), STAT=IERR)
                CALL ChkMemErr('EH_CURVE', IERR)

                DO J = 1, SEFT%NED

                    READ(FU,*) SEFT%EH_CURVE(1:2,J), SEFT%NDOOR(J)

                ENDDO

            ELSE IF(ROUTE.EQ.SPECIFIED_RELEASE) THEN

                ALLOCATE(SEFT%ED_CURVE(1:2,1:SEFT%NED),STAT=IERR)
                CALL ChkMemErr('ED_CURVE', IERR)

                DO J = 1, SEFT%NED

                    READ(FU,*) SEFT%ED_CURVE(1:2,J)

                ENDDO

            ENDIF

            CLOSE(FU)

        END SUBROUTINE SET_ROUTING_PARAM


        ! set parameter for turbin outflow
        SUBROUTINE SET_TURBIN_PARAM(SEFT, TTYPE, CONST_DATA, TGATE, GATEARR, NGARR)

            CLASS(RESERVOIR_TYPE), INTENT(INOUT) :: SEFT
            INTEGER, INTENT(IN) :: TTYPE, NGARR
            REAL(8), INTENT(IN) :: CONST_DATA
            CHARACTER(*), INTENT(IN) :: TGATE
            TYPE(GATE_TYPE), POINTER, DIMENSION(:), INTENT(IN) :: GATEARR
            INTEGER :: J

            SEFT%TB_TYPE = TTYPE
            IF(TTYPE.EQ.CONSTANT_DATA) THEN

                SEFT%TB_CONST_DATA = CONST_DATA

            ELSE

                SEFT%TURBIN_GATE => NULL()
                DO J = 1, NGARR

                    IF(TRIM(GATEARR(J)%NAME).EQ.TRIM(TGATE)) SEFT%TURBIN_GATE => GATEARR(J)

                ENDDO

                IF(.NOT.ASSOCIATED(SEFT%TURBIN_GATE)) CALL WRITE_ERRORS('Undefined source gate named '//TRIM(TGATE))

            ENDIF

        END SUBROUTINE SET_TURBIN_PARAM


        SUBROUTINE RESALLOCATING(SEFT)

        CLASS(RESERVOIR_TYPE), INTENT(INOUT) :: SEFT
        INTEGER :: IERR

        ALLOCATE(SEFT%INFLOW(0:NTIME - 1),STAT = IERR)
        CALL ChkMemErr('RESERVOIR INFLOW', IERR)
        ALLOCATE(SEFT%OUTFLOW(0:NTIME - 1),STAT = IERR)
        CALL ChkMemErr('RESERVOIR OUTFLOW', IERR)
        ALLOCATE(SEFT%STORAGE(0:NTIME - 1),STAT = IERR)
        CALL ChkMemErr('RESERVOIR STORAGE', IERR)
        ALLOCATE(SEFT%ELEVATION(0:NTIME - 1),STAT = IERR)
        CALL ChkMemErr('RESERVOIR ELEVATION', IERR)
        SEFT%INFLOW = 0.0D0
        SEFT%OUTFLOW = 0.0D0
        SEFT%STORAGE = 0.0D0
        SEFT%ELEVATION = 0.0D0


        END SUBROUTINE RESALLOCATING

      END MODULE RESERVOIR_MOD


      MODULE RESERVOIR_PTR_LIST_MOD
      USE RESERVOIR_MOD

#define LIST_DATA RESERVOIR_PTR
#include "linkedlist.f90"
#undef LIST_DATA

      END MODULE RESERVOIR_PTR_LIST_MOD
