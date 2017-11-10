C=================================================================
C SUBROUTINE READ INPUT
C=================================================================
      SUBROUTINE READ_INPUT
      USE PARAM
      USE CONSTANTS
      USE TIME
      IMPLICIT NONE
      INTEGER :: FUNIT
      CHARACTER(100) :: F1
      CHARACTER(16) :: TSTART, TEND
      NAMELIST /INP/ NBASIN, NTIME, DT
      NAMELIST /CTRL/ TSTART, TEND
      NAMELIST /IODIR/ INPUT_DIR, OUPUT_DIR


C Open log file
      OPEN(UNIT=ULOG, FILE=TRIM(FLOG),STATUS='REPLACE')
C Open input file
      CALL GETCWD(ROOT_DIR)
      FUNIT = 10
      F1 = 'Input.dat'
      CALL CHK_FILE(TRIM(F1))
      OPEN(UNIT=FUNIT, FILE=TRIM(F1),STATUS='OLD')

C Read name list common
      NBASIN = 0
      NTIME = 0
      DT = 0.0D0
      TSTART = ""
      TEND = ""
      INPUT_DIR = ""
      OUPUT_DIR = ""

      READ(FUNIT,INP, ERR=99)
      READ(FUNIT,CTRL, ERR=99)
      READ(FUNIT,IODIR, ERR=99)
      CLOSE(FUNIT)
      !Check parameter
      IF(NBASIN.EQ.0) THEN

        WRITE(*,*) 'ERROR!!!'
        WRITE(*,*) 'Number of BASIN is zero or has not been set'
        STOP 'Stop program'

      ENDIF
      !Check time control
*      IF(TRIM(TSTART).EQ.'') THEN
*
*        WRITE(*,*) 'ERROR!!!'
*        WRITE(*,*) 'Please set start time with format ''DD-MM-YYYY HH:MM'' and restart application'
*        STOP 'Stop program'
*
*      ENDIF
*
*      IF(TRIM(TEND).EQ.'') THEN
*
*        WRITE(*,*) 'ERROR!!!'
*        WRITE(*,*) 'Please set end time with format ''DD-MM-YYYY HH:MM'' and restart the application'
*        STOP 'Stop program'
*
*      ENDIF
C Read BASIN
      CALL READ_BASIN

C Set date and time
      CALL SET_DATE_TIME(TSTART, TEND)

      RETURN
99    WRITE(*,*) 'ERROR WHILE READING FILE: ', TRIM(F1)
      CLOSE(FUNIT)
      STOP
      END SUBROUTINE READ_INPUT
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE READ INPUT
C=================================================================
      SUBROUTINE READ_BASIN
      USE PARAM
      USE CONSTANTS
      IMPLICIT NONE
      INTEGER :: FUNIT, IERR, I
      INTEGER :: NSUBBASIN, NSOURCE, NREACH, NRESERVOIR, NGATE
      CHARACTER(2) :: ICH
      CHARACTER(100) :: F1

      NAMELIST /BSNL/ NSUBBASIN, NSOURCE, NREACH, NRESERVOIR, NGATE

C Allocate array
      ALLOCATE(BASIN(1:NBASIN),STAT=IERR)
      CALL ChkMemErr('BASIN', IERR)

      DO I = 1, NBASIN
C Open input file
        WRITE(ICH,'(I2.2)') I
        FUNIT = 10
        F1 = TRIM(INPUT_DIR)//'/Basin_'//ICH
        CALL CHK_FILE(TRIM(F1))
        OPEN(UNIT=FUNIT, FILE=TRIM(F1),STATUS='OLD')

C Read name list BASIN
        NSUBBASIN = 0
        NSOURCE = 0
        NREACH = 0
        NRESERVOIR = 0
        NGATE = 0
        READ(FUNIT,BSNL,ERR=99)
        BASIN(I)%NSUBBASIN = NSUBBASIN
        BASIN(I)%NSOURCE = NSOURCE
        BASIN(I)%NREACH = NREACH
        BASIN(I)%NRESERVOIR = NRESERVOIR
        BASIN(I)%NGATE = NGATE

C Read gate
        IF(NGATE.GT.0) CALL READ_GATE(FUNIT, BASIN(I))

C Read sub BASIN
        IF(NSUBBASIN.GT.0) CALL READ_SUB_BASIN(FUNIT, BASIN(I))

C Read source
        IF(NSOURCE.GT.0) CALL READ_SOURCE(FUNIT, BASIN(I))

C Read reach
        IF(NREACH.GT.0) CALL READ_REACH(FUNIT, BASIN(I))

C Read sub reservoir
        IF(NRESERVOIR.GT.0) CALL READ_RESERVOIR(FUNIT, BASIN(I))

      ENDDO
      CLOSE(FUNIT)
      RETURN
99    WRITE(*,*) 'ERROR WHILE READING FILE: ', TRIM(F1)
      CLOSE(FUNIT)
      STOP
      END SUBROUTINE READ_BASIN
C=================================================================
C
C=================================================================
C=================================================================
C READ SUB BASIN INPUT
C=================================================================
      SUBROUTINE READ_GATE(FUNIT, BS)
      USE PARAM
      USE CONSTANTS
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

      NAMELIST /GATENL/ NAME, GATETYPE, TSTART, TEND, DATAFILE, INTERVAL

      ALLOCATE(BS%GATE(1:BS%NGATE), STAT=IERR)
      CALL ChkMemErr('GATE', IERR)
      FU = 10

      DO I = 1, BS%NGATE
        GT => BS%GATE(I)

        !Initial values
        WRITE(ICH,'(I3.3)') I
        NAME = "GATE_"//ICH
        TSTART = ""
        TEND = ""
        DATAFILE = ""
        GATETYPE = 0
        INTERVAL = 0


        !Read sub-BASIN  ith
        READ(FUNIT,GATENL,ERR=99)

        !BASIN characteristic
        GT%NAME = TRIM(NAME)
        GT%START_TIME = strptime(TRIM(TSTART), '%d-%m-%Y %H:%M')
        GT%END_TIME = strptime(TRIM(TEND), '%d-%m-%Y %H:%M')
        GT%GATETYPE = GATETYPE
        GT%INTERVAL = INTERVAL

        F1 = TRIM(INPUT_DIR)//'/'//TRIM(DATAFILE)
        CALL CHK_FILE(TRIM(F1))
        OPEN(UNIT=FU, FILE=TRIM(F1), STATUS='OLD')

        READ(FU,*) GT%NDATA

        ALLOCATE(GT%GATE_DATA(1:GT%NDATA), STAT=IERR)
        CALL ChkMemErr('GATE_DATA', IERR)

        DO J = 1, GT%NDATA

            READ(FU,*) GT%GATE_DATA(J)

        ENDDO

        CLOSE(FU)

      ENDDO


      RETURN
99    WRITE(*,*) 'ERROR WHILE READING GATE DATA: '
      CLOSE(FUNIT)
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
      REAL(8) :: AREA, CN, IMPERVIOUS, TLAG
      REAL(8) :: BF_CONST, BF_MONTHLY(1:12)
      CHARACTER(100) :: NAME, DOWNSTREAM, PRECIP_GATE
      CHARACTER(3) :: ICH
      TYPE(SUBBASIN_TYPE), POINTER :: SBS

      NAMELIST /SBG1/ NAME, DOWNSTREAM, PRECIP_GATE, LOSSRATE, TRANSFORM,
     &                AREA, CN, IMPERVIOUS, TLAG
      NAMELIST /SBG2/ BASE_FLOW_TYPE, BF_CONST, BF_MONTHLY

      ALLOCATE(BS%SUBBASIN(1:BS%NSUBBASIN), STAT=IERR)
      CALL ChkMemErr('SUB BASIN', IERR)
      FU = 10

      DO I = 1, BS%NSUBBASIN
        SBS => BS%SUBBASIN(I)
        !Initial values
        WRITE(ICH,'(I3.3)') I
        NAME = "SUB_BASIN_"//ICH
        DOWNSTREAM = ""
        PRECIP_GATE = ""
        BASE_FLOW_TYPE = CONSTANT_DATA
        LOSSRATE = 0
        TRANSFORM = SCS_UHG_TYPE
        AREA = 0.0D0
        CN = 0.0D0
        IMPERVIOUS = 0.0D0
        TLAG = 0.0D0
        BF_CONST = 0.0D0
        BF_MONTHLY = 0.0D0

        !Read sub-BASIN  ith
        READ(FUNIT,SBG1,ERR=99)
        READ(FUNIT,SBG2,ERR=99)

        !Stop if the file contains precipitation value is not set.
*        IF(TRIM(PRECIPF).EQ."") THEN
*
*            WRITE(*,*) "Please set the file name for precipitation data PRECIPF!!"
*            STOP
*
*        ENDIF

        !BASIN characteristic
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
        IF(LOSSRATE.EQ.SCS_CURVE_LOSS) SBS%IMPERVIOUS = IMPERVIOUS

        !Transform method
        SBS%TRANSFORM = TRANSFORM
        SBS%CN = CN
        SBS%TLAG = TLAG
        IF(SBS%TLAG.GE.1) CALL GET_UHG(BS%SUBBASIN(I))

        SBS%PRECIP => NULL()
        DO J = 1, BS%NGATE

            IF(TRIM(BS%GATE(I)%NAME).EQ.TRIM(PRECIP_GATE)) SBS%PRECIP => BS%GATE(I)

        ENDDO

      ENDDO


      RETURN
99    WRITE(*,*) 'ERROR WHILE READING SUB BASIN DATA'
      CLOSE(FUNIT)
      STOP
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
        NAME = "SOURCE_"//ICH
        DOWNSTREAM = ""
        SRC_GATE = ""
        SRC_TYPE = CONSTANT_DATA
        CONST_DATA = 0.0D0

        !Read source ith
        READ(FUNIT,SRCNL, ERR=99)

        IF(SRC_TYPE.EQ.TIME_SERIES_DATA.AND.TRIM(SRC_GATE).EQ."") THEN

            WRITE(*,*) "Please set the gate name for source time data SRC_GATE!!"
            STOP

        ENDIF

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
99    WRITE(*,*) 'ERROR WHILE READING SOURCE DATA'
      CLOSE(FUNIT)
      STOP
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
      REAL(8) :: K, X
      CHARACTER(100) :: DOWNSTREAM, NAME
      CHARACTER(3) :: ICH
      TYPE(REACH_TYPE), POINTER :: RCH

      NAMELIST /REACHNL/ NAME, DOWNSTREAM, K, X, ROUTE

      ALLOCATE(BS%REACH(1:BS%NREACH), STAT=IERR)
      CALL ChkMemErr('REACH', IERR)

      DO I = 1, BS%NREACH

        RCH => BS%REACH(I)
        !Initial values
        WRITE(ICH,'(I3.3)') I
        NAME = "REACH_"//ICH
        DOWNSTREAM = ""
        ROUTE = MUSKINGUM_METHOD
        K = 2
        X = 0.25

        READ(FUNIT, REACHNL, ERR=99)

        RCH%NAME = TRIM(NAME)
        RCH%DOWNSTREAM = TRIM(DOWNSTREAM)
        RCH%ROUTE = ROUTE
        IF(ROUTE.EQ.MUSKINGUM_METHOD) THEN

            RCH%K = K
            RCH%X = X

        ENDIF

      ENDDO

      RETURN
99    WRITE(*,*) 'ERROR WHILE READING REACH DATA'
      CLOSE(FUNIT)
      STOP
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
      INTEGER :: ROUTE, DC_CTRL, I, J, FU, IERR, ROUTING_CURVE, TB_TYPE
      REAL(8) :: Z0, DOORW, DC_COEFF, ZBT, TB_CONST_DATA
      CHARACTER(100) :: DOWNSTREAM, NAME, RTCFN, DCFN, TURBIN_GATE, F1
      CHARACTER(3) :: ICH
      TYPE(RESERVOIR_TYPE), POINTER :: RES
      NAMELIST /RES1/ NAME, DOWNSTREAM, ROUTE, Z0
      NAMELIST /RES2/ ROUTING_CURVE, RTCFN
      NAMELIST /RES3/ DC_CTRL, DOORW, DC_COEFF, ZBT, DCFN
      NAMELIST /RES4/ TB_TYPE, TB_CONST_DATA, TURBIN_GATE


      ALLOCATE(BS%RESERVOIR(1:BS%NRESERVOIR), STAT=IERR)
      CALL ChkMemErr('RESERVOIR', IERR)
      FU = 10

      DO I = 1, BS%NRESERVOIR

        RES => BS%RESERVOIR(I)

        !Initial values
        WRITE(ICH,'(I3.3)') I
        NAME = "RESERVOIR_"//ICH
        DOWNSTREAM = ""
        ROUTE = OUTFLOW_STRUCTURE
        Z0 = 0.0D0
        ROUTING_CURVE = ELEVATION_STORAGE
        RTCFN = ""
        DC_CTRL = DC_ELEVATION_TYPE
        DOORW = 0.0D0
        DC_COEFF = 0.0D0
        ZBT = 0.0D0
        DCFN = ""
        TB_TYPE = CONSTANT_DATA
        TB_CONST_DATA = 0.0D0
        TURBIN_GATE = ""


        READ(FUNIT, RES1, ERR=99)
        READ(FUNIT, RES2, ERR=99)
        READ(FUNIT, RES3, ERR=99)
        READ(FUNIT, RES4, ERR=99)


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

        CLOSE(FU)

        !Read discharge - elevation curve
        RES%DC_CTRL = DC_CTRL

        F1 = TRIM(INPUT_DIR)//'/'//TRIM(DCFN)
        CALL CHK_FILE(TRIM(F1))
        OPEN(UNIT=FU, FILE=TRIM(F1), STATUS='OLD')

        READ(FU,*) RES%NDE

        IF(DC_CTRL.EQ.DC_DOOR_TYPE) THEN

            RES%DOORW = DOORW
            RES%DC_COEFF = DC_COEFF
            RES%ZBT = ZBT

            ALLOCATE(RES%NDOOR_OPEN(1:RES%NDE), STAT=IERR)
            CALL ChkMemErr('NDOOR_OPEN', IERR)
            ALLOCATE(RES%EH_CURVE(1:2,1:RES%NDE), STAT=IERR)
            CALL ChkMemErr('EH_CURVE', IERR)


            DO J = 1, RES%NDE

                READ(FU,*) RES%EH_CURVE(1:2,J), RES%NDOOR_OPEN(J)

            ENDDO

        ELSE IF(DC_CTRL.EQ.DC_ELEVATION_TYPE) THEN

            ALLOCATE(RES%ED_CURVE(1:2,1:RES%NDE),STAT=IERR)
            CALL ChkMemErr('ED_CURVE', IERR)

            DO J = 1, RES%NDE

                READ(FU,*) RES%ED_CURVE(1:2,J)

            ENDDO

        ENDIF

        CLOSE(FU)

        !Turbin data
        RES%TB_TYPE = TB_TYPE
        IF(TB_TYPE.EQ.CONSTANT_DATA) THEN

            RES%TB_CONST_DATA = TB_CONST_DATA

        ELSE

            DO J = 1, BS%NGATE

                IF(TRIM(BS%GATE(J)%NAME).EQ.TRIM(TURBIN_GATE)) RES%TURBIN_GATE => BS%GATE(J)

            ENDDO

        ENDIF



      ENDDO

      RETURN
99    WRITE(*,*) 'ERROR WHILE READING RESERVOIR DATA'
      CLOSE(FUNIT)
      STOP
      END SUBROUTINE READ_RESERVOIR
C=================================================================
C
C=================================================================
