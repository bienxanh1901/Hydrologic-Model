C=================================================================
C SUBROUTINE READ INPUT
C=================================================================
      SUBROUTINE READING_INPUT
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTEGER :: FUNIT
      CHARACTER(5) :: F1
      CHARACTER(16) :: TSTART, TEND
      NAMELIST /INP/ NBASIN, DT
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

      READ(FUNIT,INP, ERR=99)
      READ(FUNIT,CTRL, ERR=99)
      READ(FUNIT,IODIR,ERR=99)
      CLOSE(FUNIT)

C Check parameter
      IF(NBASIN.EQ.0) CALL WRITE_ERRORS('Number of BASIN(NBASIN) is zero or has not been set!')

      IF(DT.EQ.0.0D0) CALL WRITE_ERRORS('Time interval(DT) is zero or has not been set!')

      IF(TRIM(TSTART).EQ.'') CALL WRITE_ERRORS('Please set start time(TSTART) with format ''DD-MM-YYYY HH:MM'' and restart application')

      IF(TRIM(TEND).EQ.'') CALL WRITE_ERRORS('Please set end time(TEND) with format ''DD-MM-YYYY HH:MM'' and restart the application')

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
      USE PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER :: FUNIT, IERR, I
      INTEGER :: NSUBBASIN, NSOURCE, NREACH, NRESERVOIR, NGATE
      CHARACTER(2) :: ICH
      CHARACTER(100) :: F1, NAME

      NAMELIST /BSNL/ NSUBBASIN, NSOURCE, NREACH, NRESERVOIR, NGATE, NAME


C Allocate array
      ALLOCATE(BASIN(1:NBASIN),STAT=IERR)
      CALL ChkMemErr('BASIN', IERR)

      DO I = 1, NBASIN


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
        BASIN(I)%NAME = TRIM(NAME)
        BASIN(I)%NSUBBASIN = NSUBBASIN
        BASIN(I)%NSOURCE = NSOURCE
        BASIN(I)%NREACH = NREACH
        BASIN(I)%NRESERVOIR = NRESERVOIR
        BASIN(I)%NGATE = NGATE

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
      USE PARAM
      USE CONSTANTS
      USE TIME
      USE datetime_module
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      TYPE(BASIN_TYPE) :: BS
      INTEGER :: GATETYPE, I, J, FU, IERR
      INTEGER :: INTERVAL
      CHARACTER(100) :: NAME, DATAFILE, F1
      CHARACTER(3) :: ICH
      CHARACTER(16) :: TSTART, TEND
      TYPE(GATE_TYPE), POINTER :: GT
      TYPE(timedelta) :: DTIME

      NAMELIST /GTNL/ NAME, GATETYPE, TSTART, TEND, DATAFILE, INTERVAL

      ALLOCATE(BS%GATE(1:BS%NGATE), STAT=IERR)
      CALL ChkMemErr('GATE', IERR)
      FU = 10

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
        IF(TRIM(TSTART).EQ.'') CALL WRITE_ERRORS('Please set start time(TSTART) with format ''DD-MM-YYYY HH:MM'' and restart application')

        IF(TRIM(TEND).EQ.'') CALL WRITE_ERRORS('Please set end time(TEND) with format ''DD-MM-YYYY HH:MM'' and restart the application')

        IF(TRIM(TEND).EQ.'') CALL WRITE_ERRORS('Please set data file name(DATAFILE) and restart the application')

        IF(INTERVAL.EQ.0) CALL WRITE_ERRORS('Time INTERVAL is zero or has not been set!')

C
        GT%NAME = TRIM(NAME)
        GT%TS = strptime(TRIM(TSTART), '%d-%m-%Y %H:%M')
        GT%TE = strptime(TRIM(TEND), '%d-%m-%Y %H:%M')
        DTIME = GT%TE - GT%TS

        GT%NDATA = INT(DTIME%total_seconds()/INTERVAL) + 1
        GT%GATETYPE = GATETYPE
        GT%DT = INTERVAL

        F1 = TRIM(INPUT_DIR)//'/'//TRIM(DATAFILE)
        CALL CHK_FILE(TRIM(F1))
        OPEN(UNIT=FU, FILE=TRIM(F1), STATUS='OLD')

        ALLOCATE(GT%GATE_DATA(0:GT%NDATA - 1), STAT=IERR)
        CALL ChkMemErr('GATE_DATA', IERR)

        DO J = 0, GT%NDATA - 1

            READ(FU,*) GT%GATE_DATA(J)

        ENDDO

        CLOSE(FU)

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
      USE PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      TYPE(BASIN_TYPE) :: BS
      INTEGER :: LOSSRATE, BASE_FLOW_TYPE, TRANSFORM, I, J, FU, IERR
      REAL(8) :: AREA, CN, IMPERVIOUS, TLAG, LENGTH, SLOPE
      REAL(8) :: BF_CONST, BF_MONTHLY(1:12)
      CHARACTER(100) :: NAME, DOWNSTREAM, PRECIP_GATE
      CHARACTER(3) :: ICH
      TYPE(SUBBASIN_TYPE), POINTER :: SBS

      NAMELIST /SBSNL/ NAME, DOWNSTREAM, PRECIP_GATE, LOSSRATE, TRANSFORM,
     &                AREA, CN, IMPERVIOUS, TLAG, LENGTH, SLOPE, BASE_FLOW_TYPE, BF_CONST, BF_MONTHLY

      ALLOCATE(BS%SUBBASIN(1:BS%NSUBBASIN), STAT=IERR)
      CALL ChkMemErr('SUB BASIN', IERR)
      FU = 10

      DO I = 1, BS%NSUBBASIN
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

        READ(FUNIT,SBSNL,ERR=99)

C Check parameter
        IF(TRIM(DOWNSTREAM).EQ.'') CALL WRITE_LOG('    WARNING!!: No DOWNSTREAM for sub basin '//TRIM(NAME))


C
        SBS%NAME = TRIM(NAME)
        SBS%DOWNSTREAM = TRIM(DOWNSTREAM)
        SBS%BASE_FLOW_TYPE = BASE_FLOW_TYPE
        SBS%AREA = AREA
        IF(BASE_FLOW_TYPE.EQ.CONSTANT_DATA) THEN

            SBS%BF_CONST = BF_CONST

        ELSEIF(BASE_FLOW_TYPE.EQ.MONTHLY_DATA) THEN

            SBS%BF_MONTHLY = BF_MONTHLY

        ENDIF

        !Loss method
        SBS%LOSSRATE = LOSSRATE
        IF(LOSSRATE.EQ.SCS_CURVE_LOSS) SBS%IMPERVIOUS = IMPERVIOUS/100.0D0

        !Transform method
        SBS%TRANSFORM = TRANSFORM
        SBS%CN = CN
        SBS%TLAG = TLAG
        SBS%LENGTH = LENGTH
        SBS%SLOPE = SLOPE

        SBS%PRECIP => NULL()

        IF(TRIM(PRECIP_GATE).EQ.'') THEN

            CALL WRITE_LOG('    WARNING!!: No Precipitation gate is set for sub_basin '//TRIM(NAME))
            CYCLE

        ENDIF

        DO J = 1, BS%NGATE

            IF(TRIM(BS%GATE(I)%NAME).EQ.TRIM(PRECIP_GATE)) SBS%PRECIP => BS%GATE(I)

        ENDDO

      ENDDO


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
      USE PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      TYPE(BASIN_TYPE) :: BS
      INTEGER :: SRC_TYPE, I, J, IERR
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

        SRC%NAME = TRIM(NAME)
        SRC%DOWNSTREAM = TRIM(DOWNSTREAM)
        SRC%SRC_TYPE = SRC_TYPE

        !Source data
        IF(SRC_TYPE.EQ.CONSTANT_DATA) THEN

            SRC%CONST_DATA = CONST_DATA

        ELSE

            DO J = 1, BS%NGATE

                IF(TRIM(BS%GATE(J)%NAME).EQ.TRIM(SRC_GATE)) SRC%SRC_DATA => BS%GATE(J)

            ENDDO

        ENDIF

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
      USE PARAM
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

        RCH%NAME = TRIM(NAME)
        RCH%DOWNSTREAM = TRIM(DOWNSTREAM)
        RCH%ROUTE = ROUTE
        RCH%LOSS_RATIO = LOSS_RATIO
        RCH%LOSS_VALUE = LOSS_VALUE
        IF(ROUTE.EQ.MUSKINGUM_METHOD) THEN

            RCH%K = K*3600.0D0
            RCH%X = X

        ENDIF

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
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: FUNIT
      TYPE(BASIN_TYPE) :: BS
      INTEGER :: ROUTE, I, J, FU, IERR, ROUTING_CURVE, TB_TYPE
      REAL(8) :: Z0, DOORW, DC_COEFF, ZSW, TB_CONST_DATA
      CHARACTER(100) :: DOWNSTREAM, NAME, RTCFN, DCFN, TURBIN_GATE, F1
      CHARACTER(3) :: ICH
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      NAMELIST /RESNL/ NAME, DOWNSTREAM, ROUTE, Z0, ROUTING_CURVE, RTCFN,
     &                 DOORW, DC_COEFF, ZSW, DCFN, TB_TYPE,
     &                 TB_CONST_DATA, TURBIN_GATE


      ALLOCATE(BS%RESERVOIR(1:BS%NRESERVOIR), STAT=IERR)
      CALL ChkMemErr('RESERVOIR', IERR)
      FU = 10

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

        RES%NAME = TRIM(NAME)
        RES%DOWNSTREAM = TRIM(DOWNSTREAM)
        RES%ROUTE = ROUTE
        RES%Z0 = Z0

        !Read storage - elevation curve
        F1 = TRIM(INPUT_DIR)//'/'//TRIM(RTCFN)
        CALL CHK_FILE(TRIM(F1))
        OPEN(UNIT=FU, FILE=TRIM(F1), STATUS='OLD')

        READ(FU,*) RES%NSE
        ALLOCATE(RES%SE_CURVE(1:2,1:RES%NSE), STAT=IERR)
        CALL ChkMemErr('STORAGE-ELEVATION-CURVE', IERR)

        DO J = 1, RES%NSE

            READ(FU,*) RES%SE_CURVE(1:2,J)

        ENDDO

        RES%SE_CURVE(2,:) = RES%SE_CURVE(2,:)*1000.0D0

        CLOSE(FU)

        !Read discharge - elevation curve

        F1 = TRIM(INPUT_DIR)//'/'//TRIM(DCFN)
        CALL CHK_FILE(TRIM(F1))
        OPEN(UNIT=FU, FILE=TRIM(F1), STATUS='OLD')

        READ(FU,*) RES%NED

        IF(ROUTE.EQ.OUTFLOW_STRUCTURE) THEN

            RES%DOORW = DOORW
            RES%DC_COEFF = DC_COEFF
            RES%ZSW = ZSW

            ALLOCATE(RES%NDOOR(1:RES%NED), STAT=IERR)
            CALL ChkMemErr('NDOOR_OPEN', IERR)
            ALLOCATE(RES%EH_CURVE(1:2,1:RES%NED), STAT=IERR)
            CALL ChkMemErr('EH_CURVE', IERR)


            DO J = 1, RES%NED

                READ(FU,*) RES%EH_CURVE(1:2,J), RES%NDOOR(J)

            ENDDO

        ELSE IF(ROUTE.EQ.SPECIFIED_RELEASE) THEN

            ALLOCATE(RES%ED_CURVE(1:2,1:RES%NED),STAT=IERR)
            CALL ChkMemErr('ED_CURVE', IERR)

            DO J = 1, RES%NED

                READ(FU,*) RES%ED_CURVE(1:2,J)

            ENDDO

        ENDIF

        CLOSE(FU)

        !Turbin data
        RES%TB_TYPE = TB_TYPE
        IF(TB_TYPE.EQ.CONSTANT_DATA) THEN

            RES%TB_CONST_DATA = TB_CONST_DATA

        ELSE

            RES%TURBIN_GATE => NULL()
            IF(TRIM(TURBIN_GATE).EQ.'') CALL WRITE_ERRORS('Please set the gate name turbin gate of reservoir '//TRIM(NAME))
            DO J = 1, BS%NGATE

                IF(TRIM(BS%GATE(J)%NAME).EQ.TRIM(TURBIN_GATE)) RES%TURBIN_GATE => BS%GATE(J)

            ENDDO

        ENDIF



      ENDDO

      RETURN
99    CALL WRITE_ERRORS('Problem occurred while reading RESERVOIR DATA: '//TRIM(ICH))
      END SUBROUTINE READ_RESERVOIR
C=================================================================
C
C=================================================================
