C=================================================================
C SUBROUTINE READ INPUT
C=================================================================
      SUBROUTINE READING_INPUT
      USE COMMON_PARAM
      USE CALC_PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTEGER :: FUNIT
      CHARACTER(5) :: F1
      CHARACTER(16) :: TSTART, TEND
      NAMELIST /INP/ NBASIN, DT, SIMULATION_MODE, FORECASTING_DURATION
      NAMELIST /CTRL/ TSTART, TEND
      NAMELIST /IODIR/ INPUT_DIR, OUTPUT_DIR


      CALL WRITE_LOG('READING INPUT DATA!!!')
C Open input file

      FUNIT = 30
      F1 = 'Input'
      CALL CHK_FILE(TRIM(F1))
      OPEN(UNIT=FUNIT, FILE=TRIM(F1),STATUS='OLD')

C Read name list common
      NBASIN = 0
      NTIME = 0
      DT = 0.0D0
      TSTART = ''
      TEND = ''
      INPUT_DIR = ''
      OUTPUT_DIR = 'OUTPUT'
      SIMULATION_MODE = VALIDATION_MODE
      FORECASTING_DURATION = 6 ! 6 HOURS

      READ(FUNIT,INP, ERR=99)
      READ(FUNIT,CTRL, ERR=99)
      READ(FUNIT,IODIR,ERR=99)
      CLOSE(FUNIT)

C Check parameter
      IF(SIMULATION_MODE.NE.VALIDATION_MODE.AND.
     &   SIMULATION_MODE.NE.REAL_TIME_MODE) THEN

        CALL WRITE_LOG('    WARNING!!: SIMULATION MODE is automatically set by 1')
        SIMULATION_MODE = VALIDATION_MODE

      ENDIF

      IF(SIMULATION_MODE.NE.REAL_TIME_MODE.AND.
     &      FORECASTING_DURATION.LE.0) THEN

         CALL WRITE_LOG('    WARNING!!: FORECASTING_DURATION MODE is automatically set by 6 (Hours)')
         FORECASTING_DURATION = 6 ! 6 HOURS

      ENDIF

      FORECASTING_DURATION = FORECASTING_DURATION*3600

      IF(NBASIN.EQ.0) CALL WRITE_ERRORS('Number of BASIN(NBASIN) is zero or has not been set!')

      IF(DT.EQ.0.0D0) CALL WRITE_ERRORS('Time interval(DT) is zero or has not been set!')

      IF(TRIM(TSTART).EQ.'') CALL WRITE_ERRORS('Please set start time(TSTART) with format ''DD-MM-YYYY HH:MM'' and restart application')

      IF(SIMULATION_MODE.EQ.VALIDATION_MODE.AND.TRIM(TEND).EQ.'')
     & CALL WRITE_ERRORS('Please set end time(TEND) with format ''DD-MM-YYYY HH:MM'' and restart the application')

      IF(TRIM(INPUT_DIR).EQ.'') CALL WRITE_ERRORS('Please set input directory(INPUT_DIR) and restart the application')

C Set date and time
      CALL SET_DATE_TIME(TSTART, TEND)

C Read BASIN
      CALL READ_BASIN

      RETURN
99    CALL WRITE_ERRORS('Problem occurred while reading file: '//TRIM(F1))
      END SUBROUTINE READING_INPUT
C=================================================================
C
C=================================================================
C=================================================================
C
C=================================================================
      SUBROUTINE READ_BASIN
      USE COMMON_PARAM
      USE CALC_PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER :: FUNIT, IERR, I
      INTEGER :: NSUBBASIN, NSOURCE, NREACH, NRESERVOIR, NGATE
      CHARACTER(2) :: ICH
      CHARACTER(100) :: F1, NAME
      TYPE(BASIN_TYPE), POINTER :: BS

      NAMELIST /BSNL/ NSUBBASIN, NSOURCE, NREACH, NRESERVOIR, NGATE, NAME


C Allocate array
      ALLOCATE(BASIN(1:NBASIN),STAT=IERR)
      CALL ChkMemErr('BASIN', IERR)

      DO I = 1, NBASIN
        BS => BASIN(I)

C Open input file
        WRITE(ICH,'(I2.2)') I
        CALL WRITE_LOG('  READING BASIN '//TRIM(ICH))
        FUNIT = 30
        F1 = TRIM(INPUT_DIR)//'/Basin_'//ICH
        CALL CHK_FILE(TRIM(F1))
        OPEN(UNIT=FUNIT, FILE=TRIM(F1),STATUS='OLD')

C Read name list BASIN
        NSUBBASIN = 0
        NSOURCE = 0
        NREACH = 0
        NRESERVOIR = 0
        NGATE = 0
        NAME='BASIN_'//ICH
        READ(FUNIT,BSNL,ERR=99)
        BS = BASIN_TYPE_CONSTRUCTOR(TRIM(NAME), NSUBBASIN, NSOURCE, NGATE, NREACH, NRESERVOIR)

C Read gate
        IF(NGATE.GT.0) CALL READ_GATE(FUNIT, BASIN(I))

C Read sub basin
        IF(NSUBBASIN.GT.0) CALL READ_SUB_BASIN(FUNIT, BASIN(I))

C Read source
        IF(NSOURCE.GT.0) CALL READ_SOURCE(FUNIT, BASIN(I))

C Read reach
        IF(NREACH.GT.0) CALL READ_REACH(FUNIT, BASIN(I))

C Read reservoir
        IF(NRESERVOIR.GT.0) CALL READ_RESERVOIR(FUNIT, BASIN(I))

      ENDDO
      CLOSE(FUNIT)
      RETURN
99    CALL WRITE_ERRORS('Problem occurred while reading file: '//TRIM(F1))
      END SUBROUTINE READ_BASIN
C=================================================================
C
C=================================================================
C=================================================================
C
C=================================================================
      SUBROUTINE READ_GATE(FUNIT, BS)
      USE COMMON_PARAM
      USE CALC_PARAM
      USE CONSTANTS
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      TYPE(BASIN_TYPE) :: BS
      INTEGER :: GATETYPE, I, IERR
      INTEGER :: INTERVAL
      CHARACTER(100) :: NAME, DATAFILE
      CHARACTER(3) :: ICH
      CHARACTER(16) :: TSTART, TEND
      TYPE(GATE_TYPE), POINTER :: GT

      NAMELIST /GTNL/ NAME, GATETYPE, TSTART, TEND, DATAFILE, INTERVAL

      ALLOCATE(BS%GATE(1:BS%NGATE), STAT=IERR)
      CALL ChkMemErr('GATE', IERR)

      DO I = 1, BS%NGATE
        GT => BS%GATE(I)

        !Initial values
        WRITE(ICH,'(I3.3)') I
        CALL WRITE_LOG('    READING GATE '//TRIM(ICH))
        NAME = "GATE_"//ICH
        TSTART = ""
        TEND = ""
        DATAFILE = ""
        GATETYPE = 0
        INTERVAL = 0


        READ(FUNIT,GTNL, ERR=99)

C Check parameter
        IF(TRIM(DATAFILE).EQ.'') CALL WRITE_ERRORS('Please set data DATAFILE and restart the application')
        DATAFILE = TRIM(INPUT_DIR)//'/'//TRIM(DATAFILE)
        CALL CHK_FILE(TRIM(DATAFILE))

        IF(SIMULATION_MODE.EQ.VALIDATION_MODE) THEN

            IF(TRIM(TSTART).EQ.'') CALL WRITE_ERRORS('Please set TSTART with format ''DD-MM-YYYY HH:MM'' and restart application')

            IF(TRIM(TEND).EQ.'') CALL WRITE_ERRORS('Please set TEND with format ''DD-MM-YYYY HH:MM'' and restart the application')

            IF(INTERVAL.EQ.0) CALL WRITE_ERRORS('Time INTERVAL is zero or has not been set!')

        ENDIF

        GT = GATE_TYPE_CONSTRUCTOR(NAME, GATETYPE, TSTART, TEND, DATAFILE, INTERVAL)

        IF(SIMULATION_MODE.EQ.VALIDATION_MODE) THEN

            CALL GT%READ_ALL_DATA()

        ENDIF

      ENDDO

      RETURN
99    CALL WRITE_ERRORS('Problem occurred while reading GATE DATA: '//TRIM(ICH))
      STOP
      END SUBROUTINE READ_GATE
C=================================================================
C
C=================================================================
C=================================================================
C READ SUB BASIN INPUT
C=================================================================
      SUBROUTINE READ_SUB_BASIN(FUNIT, BS)
      USE COMMON_PARAM
      USE CALC_PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      TYPE(BASIN_TYPE) :: BS
      INTEGER :: LOSSRATE, BASE_FLOW_TYPE, TRANSFORM, I, IERR, NPRECIP_GATE
      REAL(8) :: AREA, CN, IMPERVIOUS, TLAG, LENGTH, SLOPE
      REAL(8) :: BF_CONST, BF_MONTHLY(1:12)
      CHARACTER(100) :: NAME, DOWNSTREAM, PRECIP_GATE
      CHARACTER(3)   :: ICH
      TYPE(SUBBASIN_TYPE), POINTER :: SBS

      NAMELIST /SBSNL/ NAME, DOWNSTREAM, PRECIP_GATE, LOSSRATE, TRANSFORM,
     &                 AREA, CN, IMPERVIOUS, TLAG, LENGTH, SLOPE,
     &                 BASE_FLOW_TYPE, BF_CONST, BF_MONTHLY, NPRECIP_GATE

      ALLOCATE(BS%SUBBASIN(1:BS%NSUBBASIN), STAT=IERR)
      CALL ChkMemErr('SUB BASIN', IERR)

      BSLOOP: DO I = 1, BS%NSUBBASIN
        SBS => BS%SUBBASIN(I)
        !Initial values
        WRITE(ICH,'(I3.3)') I
        CALL WRITE_LOG('    READING SUB_BASIN '//TRIM(ICH))
        NAME = "SUB_BASIN_"//ICH
        DOWNSTREAM = ""
        PRECIP_GATE = ""
        BASE_FLOW_TYPE = 0
        LOSSRATE = 0
        TRANSFORM = 0
        AREA = 0.0D0
        LENGTH = 0.0D0
        SLOPE = 0.0D0
        CN = 0.0D0
        IMPERVIOUS = 0.0D0
        TLAG = 0.0D0
        BF_CONST = 0.0D0
        BF_MONTHLY = 0.0D0
        NPRECIP_GATE = 0

        READ(FUNIT,SBSNL,ERR=99)

C Check parameter
        IF(TRIM(DOWNSTREAM).EQ.'') CALL WRITE_LOG('    WARNING!!: No DOWNSTREAM for sub basin '//TRIM(NAME))

        SBS = SUBBASIN_TYPE_CONSTRUCTOR(NAME, DOWNSTREAM, BASE_FLOW_TYPE, AREA, BF_CONST, BF_MONTHLY(1:12))
        CALL SBS%SET_TRANSFORM_PARAM(TRANSFORM, CN, TLAG, LENGTH, SLOPE)
        CALL SBS%SET_LOSS_RATE_PARAM(LOSSRATE, IMPERVIOUS)


        IF(NPRECIP_GATE.LE.0) THEN

            CALL WRITE_LOG('    WARNING!!: No Precipitation gate is set for sub_basin '//TRIM(NAME))
            CYCLE BSLOOP

        ELSE IF(TRIM(PRECIP_GATE).EQ.'') THEN

                CALL WRITE_ERRORS('Please set PRECIP_GATE for basin '//ICH)

        ENDIF

        CALL SBS%SET_PRECIPITATION_PARAM(NPRECIP_GATE, PRECIP_GATE, BS%GATE, BS%NGATE)

      ENDDO BSLOOP

      RETURN
99    CALL WRITE_ERRORS('Problem occurred while reading SUB-BASIN DATA: '//TRIM(ICH))
      END SUBROUTINE READ_SUB_BASIN
C=================================================================
C
C=================================================================
C=================================================================
C READ SOURCE INPUT
C=================================================================
      SUBROUTINE READ_SOURCE(FUNIT, BS)
      USE COMMON_PARAM
      USE CALC_PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      TYPE(BASIN_TYPE) :: BS
      INTEGER :: SRC_TYPE, I, IERR
      REAL(8) :: CONST_DATA
      CHARACTER(100) :: DOWNSTREAM, NAME, SRC_GATE
      CHARACTER(3) :: ICH
      TYPE(SOURCE_TYPE), POINTER :: SRC

      NAMELIST /SRCNL/ SRC_TYPE, CONST_DATA, DOWNSTREAM, NAME, SRC_GATE

      ALLOCATE(BS%SOURCE(1:BS%NSOURCE), STAT=IERR)
      CALL ChkMemErr('SOURCE', IERR)

      DO I = 1, BS%NSOURCE
        SRC => BS%SOURCE(I)
        !Initial values
        WRITE(ICH,'(I3.3)') I
        CALL WRITE_LOG('    READING SOURCE '//TRIM(ICH))
        NAME = "SOURCE_"//ICH
        DOWNSTREAM = ""
        SRC_GATE = ""
        SRC_TYPE = CONSTANT_DATA
        CONST_DATA = 0.0D0

        !Read source ith
        READ(FUNIT,SRCNL, ERR=99)

        IF(SRC_TYPE.EQ.TIME_SERIES_DATA.AND.TRIM(SRC_GATE).EQ."") THEN

            CALL WRITE_ERRORS('Please set the gate name for source '//TRIM(NAME))

        ENDIF

        IF(TRIM(DOWNSTREAM).EQ.'') CALL WRITE_LOG('    WARNING!!: No DOWNSTREAM for source '//TRIM(NAME))

        SRC = SOURCE_TYPE_CONSTRUCTOR(NAME, DOWNSTREAM)
        CALL SRC%SET_DATA_PARAM(SRC_TYPE,CONST_DATA, SRC_GATE, BS%GATE, BS%NGATE)

      ENDDO

      RETURN
99    CALL WRITE_ERRORS('Problem occurred while reading SOURCE DATA: '//TRIM(ICH))
      END SUBROUTINE READ_SOURCE
C=================================================================
C
C=================================================================
C=================================================================
C READ REACH INPUT
C=================================================================
      SUBROUTINE READ_REACH(FUNIT, BS)
      USE COMMON_PARAM
      USE CALC_PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      TYPE(BASIN_TYPE) :: BS
      INTEGER :: ROUTE, I, IERR
      REAL(8) :: K, X, LOSS_VALUE, LOSS_RATIO
      CHARACTER(100) :: DOWNSTREAM, NAME
      CHARACTER(3) :: ICH
      TYPE(REACH_TYPE), POINTER :: RCH

      NAMELIST /REACHNL/ NAME, DOWNSTREAM, K, X, ROUTE, LOSS_RATIO, LOSS_VALUE

      ALLOCATE(BS%REACH(1:BS%NREACH), STAT=IERR)
      CALL ChkMemErr('REACH', IERR)

      DO I = 1, BS%NREACH

        RCH => BS%REACH(I)
        !Initial values
        WRITE(ICH,'(I3.3)') I
        CALL WRITE_LOG('    READING REACH '//TRIM(ICH))
        NAME = "REACH_"//ICH
        DOWNSTREAM = ""
        ROUTE = 0
        K = 0.0D0
        X = 0.0D0
        LOSS_VALUE = 0.0D0
        LOSS_RATIO = 0.0D0
        READ(FUNIT, REACHNL, ERR=99)
        IF(TRIM(DOWNSTREAM).EQ.'') CALL WRITE_LOG('    WARNING!!: No DOWNSTREAM for reach '//TRIM(NAME))
        RCH = REACH_TYPE_CONSTRUCTOR(NAME, DOWNSTREAM)
        CALL RCH%SET_ROUTE_PARAM(ROUTE, LOSS_RATIO, LOSS_VALUE, K, X)
      ENDDO

      RETURN
99    CALL WRITE_ERRORS('Problem occurred while reading REACH DATA: '//TRIM(ICH))
      END SUBROUTINE READ_REACH
C=================================================================
C
C=================================================================
C=================================================================
C READ RESERVOIR INPUT
C=================================================================
      SUBROUTINE READ_RESERVOIR(FUNIT, BS)
      USE COMMON_PARAM
      USE CALC_PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      TYPE(BASIN_TYPE) :: BS
      INTEGER :: ROUTE, I, IERR, ROUTING_CURVE, TB_TYPE
      REAL(8) :: Z0, DOORW, DC_COEFF, ZSW, TB_CONST_DATA
      CHARACTER(100) :: DOWNSTREAM, NAME, RTCFN, DCFN, TURBIN_GATE, F1
      CHARACTER(3) :: ICH
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      NAMELIST /RESNL/ NAME, DOWNSTREAM, ROUTE, Z0, ROUTING_CURVE, RTCFN,
     &                 DOORW, DC_COEFF, ZSW, DCFN, TB_TYPE,
     &                 TB_CONST_DATA, TURBIN_GATE


      ALLOCATE(BS%RESERVOIR(1:BS%NRESERVOIR), STAT=IERR)
      CALL ChkMemErr('RESERVOIR', IERR)

      DO I = 1, BS%NRESERVOIR

        RES => BS%RESERVOIR(I)

        !Initial values
        WRITE(ICH,'(I3.3)') I
        CALL WRITE_LOG('    READING RESERVOIR '//TRIM(ICH))
        NAME = "RESERVOIR_"//ICH
        DOWNSTREAM = ""
        ROUTE = 0
        Z0 = 0.0D0
        ROUTING_CURVE = 0
        RTCFN = ""
        DOORW = 0.0D0
        DC_COEFF = 0.0D0
        ZSW = 0.0D0
        DCFN = ""
        TB_TYPE = 0
        TB_CONST_DATA = 0.0D0
        TURBIN_GATE = ""


        READ(FUNIT, RESNL,ERR=99)

        IF(TRIM(DOWNSTREAM).EQ.'') CALL WRITE_LOG('    WARNING!!: No DOWNSTREAM for reservoir '//TRIM(NAME))

        RES = RESERVOIR_TYPE_CONSTRUCTOR(NAME, DOWNSTREAM, Z0)

        F1 = TRIM(INPUT_DIR)//'/'//TRIM(RTCFN)
        CALL RES%SET_ROUTING_CURVE(F1)

        !Read discharge - elevation curve

        F1 = TRIM(INPUT_DIR)//'/'//TRIM(DCFN)
        CALL RES%SET_ROUTING_PARAM(ROUTE, F1, DOORW, DC_COEFF, ZSW)


        IF(TB_TYPE.EQ.CONSTANT_DATA) THEN

        ELSE

            IF(TRIM(TURBIN_GATE).EQ.'') CALL WRITE_ERRORS('Please set the gate name turbin gate of reservoir '//TRIM(NAME))

        ENDIF

        CALL RES%SET_TURBIN_PARAM(TB_TYPE, TB_CONST_DATA, TURBIN_GATE, BS%GATE, BS%NGATE)

      ENDDO

      RETURN
99    CALL WRITE_ERRORS('Problem occurred while reading RESERVOIR DATA: '//TRIM(ICH))
      END SUBROUTINE READ_RESERVOIR
C=================================================================
C
C=================================================================
