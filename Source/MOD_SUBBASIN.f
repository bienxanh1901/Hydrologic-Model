      MODULE SUBBASIN_MOD
      USE datetime_module
      USE COMMON_PARAM
      USE GATE_MOD
      USE CONSTANTS
      USE UHG_MOD
      USE TIME

      IMPLICIT NONE

      PUBLIC :: SUBBASIN_TYPE
      PUBLIC :: SUBBASIN_PTR


      TYPE SUBBASIN_TYPE

        CHARACTER(100) :: NAME
        !Characteristics
        REAL(8) :: AREA, LENGTH, SLOPE
        !Parameter for UHG - LOSS
        REAL(8) :: CN, IMPERVIOUS, TLAG
        !Parameter for NAM model
        REAL(8) :: UMAX, SLMAX, CQOF, CKIF, CK1, TOF, TIF, TG, CKBF
        REAL(8) :: U40, L20, OF0, FQ0, BF0, EP
        !Precipitation gate
        INTEGER :: NPRECIP_GATE
        TYPE(GATE_PTR), ALLOCATABLE, DIMENSION(:) :: PRECIP
        REAL(8), ALLOCATABLE, DIMENSION(:) :: THESEN_COEFF
        !Base flow
        INTEGER :: BASE_FLOW_TYPE
        REAL(8) :: BF_CONST, BF_MONTHLY(1:12)
        REAL(8), ALLOCATABLE, DIMENSION(:) :: BASE_FLOW
        !Transform method
        INTEGER :: TRANSFORM
        !Array for UHG
        INTEGER :: NUHG
        REAL(8), ALLOCATABLE, DIMENSION(:) :: U
        !Loss method
        INTEGER :: LOSSRATE
        !Potential maximum retention (S)
        !Accumulate precipitation excess (PE)
        !Accumulate rainfall depth (P)
        REAL(8) :: S, P, PE, PFC, PEFC
        !Output
        REAL(8), ALLOCATABLE, DIMENSION(:) :: LOSS, EXCESS, DIRECT_FLOW, TOTAL_FLOW, AVGPRECIP
        !Downstream
        CHARACTER(100) :: DOWNSTREAM

        CONTAINS

        PROCEDURE,PASS(SEFT), PUBLIC :: SET_LOSS_RATE_PARAM
        PROCEDURE,PASS(SEFT), PUBLIC :: SET_TRANSFORM_PARAM
        PROCEDURE,PASS(SEFT), PUBLIC :: SET_PRECIPITATION_PARAM
        PROCEDURE,PASS(SEFT), PUBLIC :: AVERAGED_PRECIP
        PROCEDURE,PASS(SEFT), PUBLIC :: SET_UHG
        PROCEDURE,PASS(SEFT), PUBLIC :: GET_BASE_FLOW
        PROCEDURE,PASS(SEFT), PUBLIC :: SBSALLOCATING
        PROCEDURE,PASS(SEFT), PUBLIC :: LOSS_CALC
        PROCEDURE,PASS(SEFT), PUBLIC :: TRANSFORM_CALC
        PROCEDURE,PASS(SEFT), PRIVATE :: SCS_CURVE_NUMBER
        PROCEDURE,PASS(SEFT), PRIVATE :: SET_SCS_UHG
        PROCEDURE,PASS(SEFT), PRIVATE :: SCS_UHG



      END TYPE SUBBASIN_TYPE

      TYPE SUBBASIN_PTR
        !POINTER
        TYPE(SUBBASIN_TYPE), POINTER :: SUBBASIN
      END TYPE SUBBASIN_PTR

      INTERFACE SUBBASIN_TYPE
        MODULE PROCEDURE :: SUBBASIN_TYPE_CONSTRUCTOR
      END INTERFACE SUBBASIN_TYPE

      CONTAINS

        !Constructor
        TYPE(SUBBASIN_TYPE) FUNCTION SUBBASIN_TYPE_CONSTRUCTOR(NAME, DOWNSTREAM, BASE_FLOW_TYPE, AREA, BF_CONST, BF_MONTHLY)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN) :: NAME, DOWNSTREAM
        INTEGER, INTENT(IN) :: BASE_FLOW_TYPE
        REAL(8), INTENT(IN) :: AREA, BF_CONST, BF_MONTHLY(1:12)

        SUBBASIN_TYPE_CONSTRUCTOR%NAME = TRIM(NAME)
        SUBBASIN_TYPE_CONSTRUCTOR%DOWNSTREAM = TRIM(DOWNSTREAM)
        SUBBASIN_TYPE_CONSTRUCTOR%BASE_FLOW_TYPE = BASE_FLOW_TYPE
        SUBBASIN_TYPE_CONSTRUCTOR%AREA = AREA

        IF(BASE_FLOW_TYPE.EQ.CONSTANT_DATA) THEN

            SUBBASIN_TYPE_CONSTRUCTOR%BF_CONST = BF_CONST

        ELSEIF(BASE_FLOW_TYPE.EQ.MONTHLY_DATA) THEN

            SUBBASIN_TYPE_CONSTRUCTOR%BF_MONTHLY = BF_MONTHLY

        ENDIF

        END FUNCTION SUBBASIN_TYPE_CONSTRUCTOR

        !Set parameter for LOSS method
        PURE ELEMENTAL SUBROUTINE SET_LOSS_RATE_PARAM(SEFT, METHOD, IMPERVIOUS)

            CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT
            INTEGER, INTENT(IN) :: METHOD
            REAL(8), INTENT(IN) :: IMPERVIOUS

            SEFT%LOSSRATE = METHOD
            IF(METHOD.EQ.SCS_CURVE_LOSS) THEN

                SEFT%IMPERVIOUS = IMPERVIOUS/100.0D0
                SEFT%S =(25400.0D0 - 254.0D0*SEFT%CN)/SEFT%CN
                SEFT%P = 0.0D0
                SEFT%PE = 0.0D0

            ENDIF


        END SUBROUTINE SET_LOSS_RATE_PARAM


        !Set parameter for transform method
        SUBROUTINE SET_TRANSFORM_PARAM(SEFT, METHOD, CN, TLAG, LENGTH, SLOPE)

            CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT
            INTEGER, INTENT(IN) :: METHOD
            REAL(8), INTENT(IN) :: CN, TLAG, LENGTH, SLOPE

            SEFT%TRANSFORM = METHOD
            SEFT%CN = CN
            SEFT%TLAG = TLAG
            SEFT%LENGTH = LENGTH
            SEFT%SLOPE = SLOPE

            CALL SEFT%SET_UHG

        END SUBROUTINE SET_TRANSFORM_PARAM


        !Set parameter precipitation gate
        SUBROUTINE SET_PRECIPITATION_PARAM(SEFT, NGATE, GATENAME, GATEARR, NGARR)

            CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT
            INTEGER, INTENT(IN) :: NGATE, NGARR
            CHARACTER(*), INTENT(IN) :: GATENAME
            TYPE(GATE_TYPE), POINTER, DIMENSION(:):: GATEARR
            INTEGER :: J, K, FU
            CHARACTER(50) :: TMP

            FU = 10
            SEFT%NPRECIP_GATE = NGATE
            IF(NGATE.EQ.1) THEN

                ALLOCATE(SEFT%PRECIP(1))
                ALLOCATE(SEFT%THESEN_COEFF(1))
                SEFT%THESEN_COEFF(1) = 1.0d0

                DO J = 1, NGARR

                    IF(TRIM(GATEARR(J)%NAME).EQ.TRIM(GATENAME)) SEFT%PRECIP(1)%GATE => GATEARR(J)

                ENDDO

                IF(.NOT.ASSOCIATED(SEFT%PRECIP(1)%GATE)) CALL WRITE_ERRORS('Undefined Precipitation gate named '//TRIM(GATENAME))

            ELSE

                !Read gate and Thesen coefficient
                ALLOCATE(SEFT%PRECIP(1:NGATE))
                ALLOCATE(SEFT%THESEN_COEFF(1:NGATE))

                OPEN(UNIT=FU, FILE=TRIM(GATENAME), STATUS='OLD')
                READ(FU,*) TMP

                GATELOOP: DO K = 1, NGATE

                    READ(FU,*) TMP, SEFT%THESEN_COEFF(K)

                    DO J = 1, NGARR

                        IF(TRIM(GATEARR(J)%NAME).EQ.TRIM(TMP)) THEN
                            SEFT%PRECIP(K)%GATE => GATEARR(J)
                            CYCLE GATELOOP
                        ENDIF

                    ENDDO

                    IF(.NOT.ASSOCIATED(SEFT%PRECIP(K)%GATE)) CALL WRITE_ERRORS('Undefined Precipitation gate named '//TRIM(TMP))

                ENDDO GATELOOP

                CLOSE(FU)

            ENDIF

        END SUBROUTINE SET_PRECIPITATION_PARAM

        ! Calculate average precipitation
        SUBROUTINE AVERAGED_PRECIP(SEFT)

            CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT
            INTEGER :: I

            SEFT%AVGPRECIP(CURRENT_IDX) = 0.0D0
            IF(SEFT%NPRECIP_GATE.EQ.0) RETURN
            DO I = 1, SEFT%NPRECIP_GATE
                SEFT%AVGPRECIP(CURRENT_IDX) = SEFT%AVGPRECIP(CURRENT_IDX) + SEFT%PRECIP(I)%GATE%CURRENT_DATA*SEFT%THESEN_COEFF(I)
            ENDDO

        END SUBROUTINE AVERAGED_PRECIP

        ! set type of unit hydrological graph
        SUBROUTINE SET_UHG(SEFT)

        CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT

        SELECT CASE (SEFT%TRANSFORM)
        CASE(SCS_UHG_TYPE)
            CALL SEFT%SET_SCS_UHG
        CASE DEFAULT
            WRITE(*,*) 'Error: Invalid type of unit hydrograph!!!'
            STOP
        END SELECT

        RETURN
        END SUBROUTINE SET_UHG

        ! set SCS UHG
        SUBROUTINE SET_SCS_UHG(SEFT)

        CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT
        REAL(8) :: UHG_DATA(1:2,1:N_SCS)
        REAL(8) :: TP, UP, T, TC
        INTEGER :: I

        IF(SEFT%TLAG.EQ.0.0) THEN

            !IF(SEFT%LENGTH.EQ.0.OR.SEFT%SLOPE.EQ.0)

            TC = (SEFT%LENGTH*1000.0D0)**0.8*(2540.0D0 - 22.86D0*SEFT%CN)**0.7D0/
     &           (14104.0D0*SEFT%CN**0.70D0*SEFT%SLOPE**0.50D0)

            SEFT%TLAG = 3.0D0*TC/5.0D0


        ENDIF

        TP = 0.50D0*DT/3600.0D0 + SEFT%TLAG
        UP = 2.080D0*SEFT%AREA/TP

        DO I = 1,N_SCS

            UHG_DATA(1,I) = TP*SCS_X(I)
            UHG_DATA(2,I) = UP*SCS_Y(I)

        ENDDO

        SEFT%NUHG = INT(UHG_DATA(1,N_SCS)/(DT/3600.0D0))

        ALLOCATE(SEFT%U(0:SEFT%NUHG))
        SEFT%U = 0.0D0
        T = 0.0D0
        DO I = 1, SEFT%NUHG

            T = T + DT/3600.0D0
            CALL INTERP(UHG_DATA(1,1:N_SCS), UHG_DATA(2,1:N_SCS), T, SEFT%U(I), N_SCS)

        ENDDO
        RETURN
        END SUBROUTINE SET_SCS_UHG


        ! get base flow at current calculation step
        REAL(8) FUNCTION GET_BASE_FLOW(SEFT, CURRTIME)

        CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT
        TYPE(DATETIME), INTENT(IN) :: CURRTIME
        INTEGER :: MONTH

        GET_BASE_FLOW = 0.0D0
        IF(SEFT%BASE_FLOW_TYPE.LE.0) RETURN
        IF(SEFT%BASE_FLOW_TYPE.EQ.CONSTANT_DATA) THEN

            GET_BASE_FLOW = SEFT%BF_CONST

        ELSEIF(SEFT%BASE_FLOW_TYPE.EQ.MONTHLY_DATA) THEN

            MONTH = CURRTIME%getMonth()
            GET_BASE_FLOW = SEFT%BF_MONTHLY(MONTH)

        ENDIF

        RETURN
        END FUNCTION GET_BASE_FLOW


        SUBROUTINE SBSALLOCATING(SEFT)

        CLASS(SUBBASIN_TYPE), INTENT(INOUT) :: SEFT
        INTEGER :: IERR

        ALLOCATE(SEFT%LOSS(0:NTIME - 1),STAT = IERR)
        CALL ChkMemErr('SUB-BASIN LOSS', IERR)
        ALLOCATE(SEFT%EXCESS(0:NTIME - 1),STAT = IERR)
        CALL ChkMemErr('SUB-BASIN EXCESS', IERR)
        ALLOCATE(SEFT%DIRECT_FLOW(0:NTIME - 1),STAT = IERR)
        CALL ChkMemErr('SUB-BASIN DIRECT_FLOW', IERR)
        ALLOCATE(SEFT%TOTAL_FLOW(0:NTIME - 1),STAT = IERR)
        CALL ChkMemErr('SUB-BASIN TOTAL_FLOW', IERR)
        ALLOCATE(SEFT%BASE_FLOW(0:NTIME - 1),STAT = IERR)
        CALL ChkMemErr('SUB-BASIN BASE_FLOW', IERR)
        ALLOCATE(SEFT%AVGPRECIP(0:NTIME - 1),STAT = IERR)
        CALL ChkMemErr('SUB-BASIN BASE_FLOW', IERR)

        SEFT%LOSS = 0.0D0
        SEFT%EXCESS = 0.0D0
        SEFT%DIRECT_FLOW = 0.0D0
        SEFT%BASE_FLOW = 0.0D0
        SEFT%TOTAL_FLOW = 0.0D0
        SEFT%AVGPRECIP = 0.0D0

        END SUBROUTINE SBSALLOCATING

        INCLUDE "LOSS_METHOD.f"
        INCLUDE "TRANSFORM_METHOD.f"
      END MODULE SUBBASIN_MOD



      MODULE SUBBASIN_PTR_LIST_MOD
      USE SUBBASIN_MOD

#define LIST_DATA SUBBASIN_PTR
#include "linkedlist.f90"
#undef LIST_DATA

      END MODULE SUBBASIN_PTR_LIST_MOD
