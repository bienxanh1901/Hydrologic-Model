C=================================================================
C SUBROUTINE READ INPUT
C=================================================================
      SUBROUTINE READ_INPUT
      USE PARAM
      USE CONSTANTS
      USE UNIT_HYDROGRAPH
      IMPLICIT NONE
      INTEGER :: FUNIT
      CHARACTER(30) :: F1
      NAMELIST /BASING/ NSUBBASING, NSOURCE, NREACH, NRESERVOIR,
     &                  NTIME, DT, START_DATE, START_TIME, END_DATE, END_TIME


C Open input file
      FUNIT = 10
      F1 = 'Input.dat'
      CALL CHK_FILE(TRIM(F1))
      OPEN(UNIT=FUNIT, FILE=TRIM(F1),STATUS='OLD')

C Read name list BASING
      NSUBBASING = 0
      NSOURCE = 0
      NREACH = 0
      NRESERVOIR = 0
      NTIME = 0
      DT = 0.0D0
      START_DATE = ""
      START_TIME = ""
      END_DATE = ""
      END_TIME = ""
      READ(FUNIT,BASING,ERR=99)

      IF(TRIM(START_DATE).EQ.'') THEN

        WRITE(*,*) 'Please set start date with format 'DDMMYYYY' and restart '
        STOP

      ENDIF

      IF(TRIM(START_TIME).EQ.'') THEN

        WRITE(*,*) 'Please set start date with format 'HH:MM' and restart '
        STOP

      ENDIF
C Read sub basing
      IF(NSUBBASING.GT.0) CALL READ_SUB_BASING(FUNIT)

C Read source
      IF(NSOURCE.GT.0) CALL READ_SOURCE(FUNIT)

C Read reach
      IF(NREACH.GT.0) CALL READ_REACH(FUNIT)

C Read sub basing
      IF(NRESERVOIR.GT.0) CALL READ_RESERVOIR(FUNIT)

C Set date and time
      CALL SET_DATE_TIME

      RETURN
99    WRITE(*,*) 'ERROR WHILE READING FILE: ', TRIM(F1)
      STOP
      END SUBROUTINE READ_INPUT
C=================================================================
C
C=================================================================

C=================================================================
C READ SUB BASING INPUT
C=================================================================
      SUBROUTINE READ_SUB_BASING(FUNIT)
      USE PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      INTEGER :: TF_METHOD, BF_TYPE, LOSS_METHOD, I, J, FU
      REAL(8) :: AREA, CN, IMPERVIOUS, TLAG
      REAL(8) :: BF_CONST, BF_MONTHLY(1:12)
      CHARACTER(100) :: NAME, DOWNSTREAM, PRECIPF
      CHARACTER(3) :: ICH

      NAMELIST /SBG1/ NAME, DOWNSTREAM, PRECIPF, LOSS_METHOD, TF_METHOD, AREA, CN, IMPERVIOUS, TLAG
      NAMELIST /SBG2/ BF_TYPE, BF_CONST, BF_MONTHLY

      ALLOCATER(SUBBASING(1:NSUBBASING))
      FU = 10

      DO I = 1, NSUBBASING

        !Initial values
        WRITE(ICH,'(I3.3)') I
        NAME = "SUB_BASING_"//ICH
        DOWNSTREAM = ""
        PRECIPF = ""
        BF_TYPE = CONSTANT_DATA
        LOSS_METHOD = 0
        TF_METHOD = SCS_UHG_TYPE
        AREA = 0.0D0
        CN = 0.0D0
        IMPERVIOUS = 0.0D0
        TLAG = 0.0D0
        BF_CONST = 0.0D0
        BF_MONTHLY = 0.0D0

        !Read sub-basing  ith
        READ(FUNIT,SBG1)
        READ(FUNIT,SBG2)

        !Stop if the file contains precipitation value is not set.
        IF(TRIM(PRECIPF).EQ."") THEN

            WRITE(*,*) "Please set the file name for precipitation data PRECIPF!!"
            STOP

        ENDIF

        !Basing characteristic
        SUBBASING(I)%NAME = TRIM(NAME)
        SUBBASING(I)%DOWNSTREAM = TRIM(DOWNSTREAM)
        SUBBASING(I)%BF_TYPE = BF_TYPE
        SUBBASING(I)%AREA = AREA
        IF(BF_TYPE.EQ.CONSTANT_DATA) SUBBASING(I)%BF_CONST = BF_CONST
        ELSE IF(BF_TYPE.EQ.MONTHLY_DATA) SUBBASING(I)%BF_MONTHLY = BF_MONTHLY

        !Loss method
        SUBBASING(I)%LOSS_METHOD = LOSS_METHOD
        IF(LOSS_METHOD.EQ.SCS_CURVE_LOSS) SUBBASING(I)%IMPERVIOUS = IMPERVIOUS

        !Transform method
        SUBBASING(I)%TF_METHOD = TF_METHOD
        SUBBASING(I)%CN = CN
        SUBBASING(I)%TLAG = TLAG
        IF(SUBBASING(I)%TLAG.GE.1) CALL GET_UHG(SUBBASING(I))

        !Read precipitation
        ALLOCATE(SUBBASING(I)%PRECIP(0:NTIME - 1))

        CALL CHK_FILE(TRIM(PRECIPF))
        OPEN(UNIT=FU, FILE=TRIM(PRECIPF), STATUS='OLD')

        DO J = 0, NTIME - 1
            READ(FU,*) SUBBASING(I)%PRECIP(J)
        ENDDO

        CLOSE(FU)

        !Allocate variables
        ALLOCATE(SUBBASING(I)%LOSS(0:NTIME - 1))
        ALLOCATE(SUBBASING(I)%EXCESS(0:NTIME - 1))
        ALLOCATE(SUBBASING(I)%BASE_FLOW(0:NTIME - 1))
        ALLOCATE(SUBBASING(I)%DIRECT_FLOW(0:NTIME - 1))
        ALLOCATE(SUBBASING(I)%TOTAL_FLOW(0:NTIME - 1))
        SUBBASING(I)%LOSS = 0.0D0
        SUBBASING(I)%EXCESS = 0.0D0
        SUBBASING(I)%BASE_FLOW = 0.0D0
        SUBBASING(I)%DIRECT_FLOW = 0.0D0
        SUBBASING(I)%TOTAL_FLOW = 0.0D0


      ENDDO


      RETURN
      END SUBROUTINE READ_SUB_BASING
C=================================================================
C
C=================================================================
C=================================================================
C READ SOURCE INPUT
C=================================================================
      SUBROUTINE READ_SOURCE(FUNIT)
      USE PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      INTEGER :: SRC_TYPE, I, J, FU
      REAL(8) :: SRC_CONST
      CHARACTER(100) :: DOWNSTREAM, SRCF, NAME
      CHARACTER(3) :: ICH

      NAMELIST /SRCI/ SRC_TYPE, SRC_CONST, DOWNSTREAM, SRCF, NAME

      ALLOCATE(SOURCE(1:NSOURCE))
      FU = 10

      DO I = 1, NSOURCE

        !Initial values
        WRITE(ICH,'(I3.3)') I
        NAME = "SOURCE_"//ICH
        DOWNSTREAM = ""
        SRCF = ""
        SRC_TYPE = CONSTANT_DATA
        SRC_CONST = 0.0D0

        !Read source ith
        READ(FUNIT,SRCI)

        IF(SRC_TYPE.EQ.TIME_SERIES_DATA.AND.TRIM(SRCF).EQ."") THEN

            WRITE(*,*) "Please set the file name for source time data SRCF!!"
            STOP

        ENDIF

        SOURCE(I)%NAME = TRIM(NAME)
        SOURCE(I)%DOWNSTREAM = TRIM(DOWNSTREAM)
        SOURCE(I)%SRC_TYPE = SRC_TYPE


        !Source data
        ALLOCATE(SOURCE(I)%SRC_DATA(0:NTIME - 1))
        IF(SRC_TYPE.EQ.CONSTANT_DATA) THEN

            SOURCE(I)%SRC_DATA = SRC_CONST

        ELSE IF(SRC_TYPE.EQ.TIME_SERIES_DATA) THEN

            CALL CHK_FILE(TRIM(SRCF))
            OPEN(UNIT=FU, FILE=TRIM(SRCF), STATUS='OLD')
            DO J = 0, NTIME - 1
                READ(FU,*) SOURCE(I)%SRC_DATA(J)
            ENDDO

            CLOSE(FU)

        ENDIF

      ENDDO

      RETURN
      END SUBROUTINE READ_SOURCE
C=================================================================
C
C=================================================================
C=================================================================
C READ REACH INPUT
C=================================================================
      SUBROUTINE READ_REACH(FUNIT)
      USE PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      INTEGER :: ROUTING_METHOD, I
      REAL(8) :: K, X
      CHARACTER(100) :: DOWNSTREAM, NAME
      CHARACTER(3) :: ICH

      NAMELIST /REACHNL/ NAME, DOWNSTREAM, K, X, ROUTING_METHOD

      ALLOCATE(REACH(1:NREACH))

      DO I = 1, NREACH

        !Initial values
        WRITE(ICH,'(I3.3)') I
        NAME = "REACH_"//ICH
        DOWNSTREAM = ""
        ROUTING_METHOD = MUSKINGUM_METHOD

        READ(FUNIT, REACHNL)

        REACH(I)%NAME = TRIM(NAME)
        REACH(I)%DOWNSTREAM = TRIM(DOWNSTREAM)
        REACH(I)%ROUTING_METHOD = ROUTING_METHOD
        IF(ROUTING_METHOD.EQ.MUSKINGUM_METHOD) THEN

            REACH(I)%K = K
            REACH(I)%X = X

        ENDIF

        !Allocate variable
        ALLOCATE(REACH(I)%INFLOW(0:NTIME - 1))
        ALLOCATE(REACH(I)%OUTFLOW(0:NTIME - 1))
        REACH(I)%INFLOW = 0.0D0
        REACH(I)%OUTFLOW = 0.0D0

      ENDDO

      RETURN
      END SUBROUTINE READ_REACH
C=================================================================
C
C=================================================================
C=================================================================
C READ RESERVOIR INPUT
C=================================================================
      SUBROUTINE READ_RESERVOIR(FUNIT)
      USE PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      INTEGER :: ROUTING_METHOD, DC_CTRL, I, J, FU
      REAL(8) :: Z0, DOORW, DC_COEFF, ZBT
      CHARACTER(100) :: DOWNSTREAM, NAME, SEFN, DCFN
      CHARACTER(3) :: ICH

      NAMELIST /RES1/ NAME, DOWNSTREAM, ROUTING_METHOD
      NAMELIST /RES2/ Z0, DOORW, DC_COEFF, ZBT, SEFN
      NAMELIST /RES3/ DC_CTRL, DCFN


      ALLOCATE(RESERVOIR(1:NRESERVOIR))
      FU = 10

      DO I = 1, NRESERVOIR

        !Initial values
        WRITE(ICH,'(I3.3)') I
        NAME = "RESERVOIR_"//ICH
        DOWNSTREAM = ""
        SEFN = ""
        DCFN = ""
        ROUTING_METHOD = 0
        DC_CTRL = 0
        Z0 = 0.0D0
        DOORW = 0.0D0
        DC_COEFF = 0.0D0
        ZBT = 0.0D0


        READ(FUNIT, RES1)
        READ(FUNIT, RES2)
        READ(FUNIT, RES3)

        IF(TRIM(SEFN).EQ."") THEN

            WRITE(*,*) "Please set the file name for STORAGE - ELEVATION CURVE SEFN!!"
            STOP

        ENDIF

        IF(TRIM(DCFN).EQ."") THEN

            WRITE(*,*) "Please set the file name for discharge control data DCFN!!"
            STOP

        ENDIF

        RESERVOIR(I)%NAME = TRIM(NAME)
        RESERVOIR(I)%DOWNSTREAM = TRIM(DOWNSTREAM)
        RESERVOIR(I)%ROUTING_METHOD = ROUTING_METHOD

        RESERVOIR(I)%Z0 = Z0
        RESERVOIR(I)%DOORW = DOORW
        RESERVOIR(I)%DC_COEFF = DC_COEFF
        RESERVOIR(I)%ZBT = ZBT

        !Read storage - elevation curve
        CALL CHK_FILE(TRIM(SEFN))
        OPEN(UNIT=FU, FILE=TRIM(SEFN), STATUS='OLD')

        READ(FU,*) RESERVOIR(I)%NSE

        ALLOCATE(RESERVOIR(I)%SE_CURVE(1:RESERVOIR(I)%NSE))

        DO J = 1, RESERVOIR(I)%NSE

            READ(FU,*) RESERVOIR(I)%SE_CURVE(J)

        ENDDO

        CLOSE(FU)

        !Read discharge - elevation curve
        RESERVOIR(I)%DC_CTRL = DC_CTRL

        CALL CHK_FILE(TRIM(DCFN))
        OPEN(UNIT=FU, FILE=TRIM(DCFN), STATUS='OLD')

        READ(FU,*) RESERVOIR(I)%NDE

        IF(DC_CTRL.EQ.DC_DOOR_TYPE) THEN

            ALLOCATE(RESERVOIR(I)%NDOOR_OPEN(1:RESERVOIR(I)%NDE))
            ALLOCATE(RESERVOIR(I)%ORE_CURVE(1:2,1:RESERVOIR(I)%NDE))

            DO J = 1, RESERVOIR(I)%NDE

                READ(FU,*) RESERVOIR(I)%ORE_CURVE(1:2,J), RESERVOIR(I)%NDOOR_OPEN(J)

            ENDDO

        ELSE IF(DC_CTRL.EQ.DC_ELEVATION_TYPE) THEN

            ALLOCATE(RESERVOIR(I)%DCE_CURVE(1:2,1:RESERVOIR(I)%NDE))

            DO J = 1, RESERVOIR(I)%NDE

                READ(FU,*) RESERVOIR(I)%DCE_CURVE(1:2,J)

            ENDDO

        ENDIF

        CLOSE(FU)

        !Allocate variable
        ALLOCATE(RESERVOIR(I)%INFLOW(0:NTIME - 1))
        ALLOCATE(RESERVOIR(I)%OUTFLOW(0:NTIME - 1))
        ALLOCATE(RESERVOIR(I)%STORAGE(0:NTIME - 1))
        ALLOCATE(RESERVOIR(I)%STORAGE(0:NTIME - 1))
        RESERVOIR(I)%INFLOW = 0.0D0
        RESERVOIR(I)%OUTFLOW = 0.0D0
        RESERVOIR(I)%STORAGE = 0.0D0
        RESERVOIR(I)%ELEVATION = 0.0D0

      ENDDO

      RETURN
      END SUBROUTINE READ_RESERVOIR
C=================================================================
C
C=================================================================
