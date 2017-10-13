C=================================================================
C SUBROUTINE READ INPUT
C=================================================================
      SUBROUTINE READ_INPUT
      USE CALC, ONLY: MODEL
      USE BASING, ONLY: NBASING
      USE OBSERVATION, ONLY: NTIME, DT
      USE UNIT_HYDROGRAPH
      USE ROUTING
      IMPLICIT NONE
      INTEGER :: FUNIT
      CHARACTER(30) :: F1, OBSF, CHAF, FRTF
      LOGICAL :: EX
      NAMELIST /OBSER/ NBASING, NTIME, DT, OBSF
      NAMELIST /FLCALC/ MODEL, UHG_TYPE, CHAF
      NAMELIST /FLRT/ ISROUTING, RIVER_MDL, FRTF

C Open input file
      F1 = 'Input.dat'
      INQUIRE(FILE=TRIM(F1), EXIST=EX)
      IF(.NOT.EX) THEN
        WRITE(*,*) "ERROR!!!"
        WRITE(*,'(3A)')"File ", TRIM(F1),
     &            "does not exist in the current directory!!! "
        STOP
      ENDIF

      FUNIT = 10
      OPEN(UNIT=FUNIT, FILE=TRIM(F1),STATUS='OLD')

C Read name list OBSER
      NBASING = 0
      NTIME = 0
      DT = 0.0D0
      OBSF = 'OBSFILE'
      READ(FUNIT,OBSER,ERR=99)

      !Check value
c      IF(NBASING.LE.0) CALL ARLET_INVALID_VALUE('NBASING')
c      IF(NTIME.LE.0) CALL ARLET_INVALID_VALUE('NTIME')
c      IF(DT.LE.0.0D0) CALL ARLET_INVALID_VALUE('DT')
      OBSF = TRIM(OBSF)
C Read name list FLCALC
      MODEL = ''
      UHG_TYPE = 0
      CHAF = 'CHARFILE'
      READ(FUNIT,FLCALC,ERR=99)

      !Check value
c     IF(MODEL.EQ.'') CALL ARLET_INVALID_VALUE('MODEL')
c     IF(MODEL.EQ.'UHG') THEN
c       IF(UHG_TYPE.LE.0.OR.UHG_TYPE.GT.1) THEN
c            WRITE(*,*) 'ERROR!'
c            WRITE(*,*) 'UHG_TYPE = 1 : SCS HYDROGRAPH'
c            WRITE(*,*) 'PLEASE CHOOSE AND RERUN'
c            GOTO 100
c        ENDIF
c      ENDIF
      CHAF = TRIM(CHAF)
C Read name list FLRT
      ISROUTING=.FALSE.
      RIVER_MDL = 'Muskingum'
      FRTF = 'RTFILE'
      READ(FUNIT,FLRT,ERR=99)
      CALL READ_INPUT_CHAR(CHAF)
      CALL READ_INPUT_OBSER(OBSF)
      IF(ISROUTING) CALL READ_INPUT_RTNG(FRTF)


      RETURN
99    WRITE(*,*) 'ERROR WHILE READING FILE: ', TRIM(F1)
C100   STOP
      END SUBROUTINE READ_INPUT
C=================================================================
C
C=================================================================

C=================================================================
C SUBROUTINE READ INPUT F1
C=================================================================
      SUBROUTINE READ_INPUT_CHAR(FILE_NAME)
      USE CALC
      USE OBSERVATION
      USE BASING
      USE UNIT_HYDROGRAPH
      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: FILE_NAME
      INTEGER :: FUNIT
      LOGICAL :: EX
C CHECK AND OPEN INPUT FILE
      INQUIRE(FILE=TRIM(FILE_NAME), EXIST=EX)
      IF(.NOT.EX) THEN
        WRITE(*,*) "ERROR!!!"
        WRITE(*,'(3A)')"File ", TRIM(FILE_NAME),
     &            "does not exist in the current directory!!! "
        STOP
      ENDIF

      FUNIT = 10
      OPEN(UNIT=FUNIT, FILE=TRIM(FILE_NAME),STATUS='OLD')

C Allocate array
      ALLOCATE(BASE(1:NBASING))

      IF(MODEL.EQ.'UHG') THEN

C READ PARAMETER FOR UNIT HYDROGRAPH METHOD
        CALL GET_UHG
        CALL READ_DATA_FOR_UHG(FUNIT)

      ELSE IF(MODEL.EQ.'NAM') THEN

C READ PARAMETER FOR NAM MODEL
        ALLOCATE(NAMPRM(1:NBASING))

        CALL READ_DATA_FOR_NAM(FUNIT)

      ENDIF

      CLOSE(FUNIT)
      RETURN
      END SUBROUTINE READ_INPUT_CHAR
C=================================================================
C
C=================================================================

C=================================================================
C SUBROUTINE READ INPUT F1
C=================================================================
      SUBROUTINE READ_INPUT_OBSER(FILE_NAME)
      USE OBSERVATION
      USE BASING
      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: FILE_NAME
      INTEGER :: I, J, K, FUNIT, NSTATS_TOTAL, CNT
      CHARACTER(100) :: CTMP
      LOGICAL :: EX
      REAL(8), ALLOCATABLE, DIMENSION(:) :: HX
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: XTMP
C CHECK AND OPEN INPUT FILE
      INQUIRE(FILE=TRIM(FILE_NAME), EXIST=EX)
      IF(.NOT.EX) THEN
        WRITE(*,*) "ERROR!!!"
        WRITE(*,'(3A)')"File ", TRIM(FILE_NAME),
     &            "does not exist in the current directory!!! "
        STOP
      ENDIF

      FUNIT = 10
      OPEN(UNIT=FUNIT, FILE=TRIM(FILE_NAME),STATUS='OLD')

      ALLOCATE(XF(1:NBASING,1:NTIME))
      NSTATS_TOTAL = 0

      DO I=1,NBASING

        NSTATS_TOTAL = NSTATS_TOTAL + BASE(I)%NSTATS

      ENDDO

      ALLOCATE(HX(1:NSTATS_TOTAL))
      ALLOCATE(XTMP(1:NSTATS_TOTAL, 1:NTIME))

C READ PRECIPITATION COEFFICIENT OF EACH STATION
      READ(FUNIT,*) CTMP
      READ(FUNIT,*)(HX(I),I=1,NSTATS_TOTAL)

C READ PRECIPITATION OF EACH STATION INTIME
      READ(FUNIT,*) CTMP
      DO I = 1, NTIME

        READ(FUNIT,*) (XTMP(J,I), J = 1, NSTATS_TOTAL)

      ENDDO

      CLOSE(FUNIT)

C CALCULATION AVERAGE PRECIPITATION

      XF = 0.0D0

      DO I=1,NTIME
        K = 1
        CNT = 0
        STLOOP:DO J=1,NSTATS_TOTAL
            CNT = CNT + 1
            XF(K,I) = XF(K,I) + HX(J)*XTMP(J,I)/1000.0D0
            IF(CNT.EQ.BASE(K)%NSTATS)THEN
                K = K + 1
                CNT = 0
            ENDIF

        ENDDO STLOOP

      ENDDO

      DEALLOCATE(HX,XTMP)
      RETURN
      END SUBROUTINE READ_INPUT_OBSER
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE READ INPUT F1
C=================================================================
      SUBROUTINE READ_INPUT_RTNG(FILE_NAME)
      USE ROUTING
      IMPLICIT NONE

      CHARACTER(*), INTENT(IN) :: FILE_NAME
      CHARACTER(100) :: CTMP
      INTEGER :: FUNIT, I, J, K
      LOGICAL :: EX
C CHECK AND OPEN INPUT FILE
      INQUIRE(FILE=TRIM(FILE_NAME), EXIST=EX)
      IF(.NOT.EX) THEN
        WRITE(*,*) "ERROR!!!"
        WRITE(*,'(3A)')"File ", TRIM(FILE_NAME),
     &            "does not exist in the current directory!!! "
        STOP
      ENDIF

      FUNIT = 10
      OPEN(UNIT=FUNIT, FILE=TRIM(FILE_NAME),STATUS='OLD')
      READ(FUNIT,*) CTMP
      READ(FUNIT,*)NRV, NRS
      NSTOTAL = NRV + NRS
      ALLOCATE(FRTYPE(1:NSTOTAL))
      IF(NRV.GT.0) ALLOCATE(RIVER(1:NRV))
      IF(NRS.GT.0) ALLOCATE(RESEVOIR(1:NRS))

      !Read comments
      DO I = 1,10
        READ(FUNIT,*) CTMP
      ENDDO

      !Read data
      J = 0
      K = 0
      DO I = 1, NSTOTAL

        READ(FUNIT,*) CTMP
        READ(FUNIT,*) FRTYPE(I)

        IF(FRTYPE(I).EQ.1) THEN
            J = J + 1
            CALL READ_DATA_FOR_RIVER(FUNIT, J)

        ELSE IF(FRTYPE(I).EQ.2) THEN
            K = K + 1
            CALL READ_DATA_FOR_RESEVOIR(FUNIT, K)

        ENDIF

      ENDDO

      CLOSE(FUNIT)
      RETURN
      END SUBROUTINE READ_INPUT_RTNG
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE READ DATA INPUT FOR UHG
C=================================================================
      SUBROUTINE READ_DATA_FOR_UHG(FUNIT)
      USE BASING
      IMPLICIT NONE
      CHARACTER(100) :: CTMP
      INTEGER :: I, FUNIT

      READ(FUNIT,*) CTMP
      DO I = 1, NBASING

        READ(FUNIT,*) CTMP
        READ(FUNIT,*) BASE(I)%AREA, BASE(I)%LENGTH, BASE(I)%SLOPE,
     &                BASE(I)%Q0, BASE(I)%CN,  BASE(I)%NSTATS

      ENDDO

      RETURN
      END SUBROUTINE READ_DATA_FOR_UHG
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE READ DATA INPUT FOR NAM
C=================================================================
      SUBROUTINE READ_DATA_FOR_NAM(FUNIT)
      USE BASING
      IMPLICIT NONE
      CHARACTER(100) :: CTMP
      INTEGER :: I, FUNIT

      READ(FUNIT,*) CTMP
      READ(FUNIT,*) CTMP
      DO I = 1, NBASING

        READ(FUNIT,*) CTMP
        READ(FUNIT,*) BASE(I)%AREA, NAMPRM(I)%U40, NAMPRM(I)%L20,
     &                NAMPRM(I)%EP, NAMPRM(I)%OF0, NAMPRM(I)%BF0,
     &                BASE(I)%NSTATS

        READ(FUNIT,*) NAMPRM(I)%UMAX, NAMPRM(I)%SLMAX, NAMPRM(I)%CQOF,
     &                NAMPRM(I)%CKIF, NAMPRM(I)%CK1, NAMPRM(I)%TOF,
     &                NAMPRM(I)%TIF, NAMPRM(I)%TG, NAMPRM(I)%CKBF
      ENDDO

      RETURN
      END SUBROUTINE READ_DATA_FOR_NAM
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE READ DATA INPUT FOR RIVER FLOOD ROUTING
C=================================================================
      SUBROUTINE READ_DATA_FOR_RIVER(FUNIT,I)
      USE ROUTING
      USE OBSERVATION
      IMPLICIT NONE
      INTEGER :: J, K, FUNIT, FINF, I

      READ(FUNIT,*) RIVER(I)%NSRC, RIVER(I)%NBASE,
     &              RESEVOIR(I)%NINF, RIVER(I)%TX

      IF(RIVER(I)%NSRC.GT.0) THEN

        ALLOCATE(RIVER(I)%SRC(1:RIVER(I)%NSRC))
        READ(FUNIT,*)(RIVER(I)%SRC(J), J = 1, RIVER(I)%NSRC)

      ENDIF

      IF(RIVER(I)%NBASE.GT.0) THEN

        ALLOCATE(RIVER(I)%BASE(1:RIVER(I)%NBASE))
        READ(FUNIT,*)(RIVER(I)%BASE(J), J = 1, RIVER(I)%NBASE)

      ENDIF

      IF(RIVER(I)%NINF.GT.0) THEN

        ALLOCATE(RIVER(I)%QINF(1:RIVER(I)%NINF,1:NTIME))
        READ(FUNIT,*) RIVER(I)%INFLOWF
        FINF = 11
        OPEN(FINF, FILE=TRIM(RIVER(I)%INFLOWF),STATUS='OLD')
        DO K = 1,NTIME
            READ(FINF,*) (RIVER(I)%QINF(J,K), J = 1, RIVER(I)%NINF)
        ENDDO
        CLOSE(FINF)

      ENDIF

      RETURN
      END SUBROUTINE READ_DATA_FOR_RIVER
C=================================================================
C
C=================================================================
C=================================================================
C SUBROUTINE READ DATA INPUT FOR RIVER FLOOD ROUTING
C=================================================================
      SUBROUTINE READ_DATA_FOR_RESEVOIR(FUNIT, I)
      USE ROUTING
      USE OBSERVATION
      IMPLICIT NONE
      INTEGER :: J, K, FUNIT, FINF, I

      !Read basic param
      READ(FUNIT,*) RESEVOIR(I)%NSRC, RESEVOIR(I)%NBASE,
     &              RESEVOIR(I)%NINF, RESEVOIR(I)%NVZ,
     &              RESEVOIR(I)%NDC, RESEVOIR(I)%DOOR_W,
     &              RESEVOIR(I)%DOOR_H, RESEVOIR(I)%DC_COEFF,
     &              RESEVOIR(I)%QTB, RESEVOIR(I)%Z0

      !Read source from other routing sources
      IF(RESEVOIR(I)%NSRC.GT.0) THEN

        ALLOCATE(RESEVOIR(I)%SRC(1:RESEVOIR(I)%NSRC))
        READ(FUNIT,*)(RESEVOIR(I)%SRC(J), J = 1, RESEVOIR(I)%NSRC)

      ENDIF

      !Read source from UHG or NAM calculation
      IF(RESEVOIR(I)%NBASE.GT.0) THEN
        ALLOCATE(RESEVOIR(I)%BASE(1:RESEVOIR(I)%NBASE))
        READ(FUNIT,*) (RESEVOIR(I)%BASE(J), J = 1, RESEVOIR(I)%NBASE)
      ENDIF

      !Read source from user input file
      IF(RESEVOIR(I)%NINF.GT.0) THEN

        ALLOCATE(RESEVOIR(I)%QINF(1:RESEVOIR(I)%NINF,1:NTIME))
        READ(FUNIT,*) RESEVOIR(I)%INFLOWF
        FINF = 11
        OPEN(FINF, FILE=TRIM(RESEVOIR(I)%INFLOWF),STATUS='OLD')
        DO K = 1,NTIME
            READ(FINF,*) (RESEVOIR(I)%QINF(J,K), J = 1, RESEVOIR(I)%NINF)
        ENDDO
        CLOSE(FINF)

      ENDIF

      !Read Z~V relation of RESEVOIR
      ALLOCATE(RESEVOIR(I)%VZ(1:2,1:RESEVOIR(I)%NVZ))
      READ(FUNIT,*) (RESEVOIR(I)%VZ(1,J), J=1,RESEVOIR(I)%NVZ)
      READ(FUNIT,*) (RESEVOIR(I)%VZ(2,J), J=1,RESEVOIR(I)%NVZ)
      RESEVOIR(I)%VZ(1,:) = RESEVOIR(I)%VZ(1,:)*1000000.0D0

      !Read discharge control Z - Ndoor - door height
      ALLOCATE(RESEVOIR(I)%DC_CTR(1:RESEVOIR(I)%NDC, 1:3))
      READ(FUNIT,*) (RESEVOIR(I)%DC_CTR(J,1),J = 1,RESEVOIR(I)%NDC)
      READ(FUNIT,*) (RESEVOIR(I)%DC_CTR(J,2),J = 1,RESEVOIR(I)%NDC)
      READ(FUNIT,*) (RESEVOIR(I)%DC_CTR(J,3),J = 1,RESEVOIR(I)%NDC)


      RETURN
      END SUBROUTINE READ_DATA_FOR_RESEVOIR
C=================================================================
C
C=================================================================
