C=================================================================
C MODULE CONTAINS VARIABLES FOR UNIT HYDROGRAPH
C=================================================================
      MODULE UNIT_HYDROGRAPH
      IMPLICIT NONE

      !Dimensionless Unit Hydrograph
      INTEGER :: NUINT
      REAL(8), ALLOCATABLE, DIMENSION(:) :: TU0,U0

      !Coefficients
      REAL(8) :: CN, ALPHA, SDV

      END MODULE UNIT_HYDROGRAPH
C=================================================================
C
C=================================================================


C=================================================================
C MODULE CONTAINS VARIABLES NAM MODEL
C=================================================================
      MODULE NAM_MODEL
      IMPLICIT NONE

      !Night parameters of NAM model
      REAL(8), ALLOCATABLE, DIMENSION(:) :: UMAX, SLMAX, CQOF, CKIF,
     &                                      CK1,TOF, TIF, TG, CKBF

      !resource basing parameters
      REAL(8) :: U40, L20, OF0, FQ0, BF0, EP


      END MODULE NAM_MODEL
C=================================================================
C
C=================================================================


C=================================================================
C MODULE CONTAINS VARIABLES OF RIVERS/POOLS CHARACTERISTIC
C=================================================================
      MODULE CHARACTERISTIC
      IMPLICIT NONE

      !basing characteristic
      REAL(8), ALLOCATABLE, DIMENSION(:) :: DLF, Y, F, Q0, TX

C
C Pool characteristic
C
      REAL(8), ALLOCATABLE, DIMENSION(:) :: B, ZN, HS, SCX,
     &                                      AMAX, QTB, ZBD,
     &                                      HBT, ZDL, HGC
      !Z~V relation graph of pool
      INTEGER, ALLOCATABLE, DIMENSION(:) :: NVJ
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: VZJ, ZVJ

      END MODULE CHARACTERISTIC
C=================================================================
C
C=================================================================


C=================================================================
C MODULE CONTAINS OBSERVATION VARIABLES
C=================================================================
      MODULE OBSERVATION
      IMPLICIT NONE

      INTEGER :: MF, MS, NT, NX, KMP
      INTEGER, ALLOCATABLE, DIMENSION(:) :: NOI, NF, KL, KTD, KTRA
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: JT, JF

      !Time interval
      REAL(8) :: DT, DT1

      !Precipitation observation
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: HSX, X,XF

      !Flow observation
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: T
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: QTD

      END MODULE OBSERVATION


C=================================================================
C
C=================================================================


C=================================================================
C MODULE CONTAINS OUTPUT VARIABLES
C=================================================================
      MODULE OUTPUT
      IMPLICIT NONE

      !in-out flow
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: QF, QR
      !resource/power plan state
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: V, ZH


      END MODULE OUTPUT
C=================================================================
C
C=================================================================


C=================================================================
C MODULE CONTAINS INPUT/OUTPUT FILENAME
C=================================================================
      MODULE FILENAME
      IMPLICIT NONE

      CHARACTER(100) :: F1, F2
      CHARACTER(100) :: F3, F4, F5, F6


      END MODULE FILENAME
C=================================================================
C
C=================================================================


C=================================================================
C SUBROUTINE INTERP
C=================================================================
      SUBROUTINE INTERP(Y, X, AX, AY, N)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N
      REAL(8), INTENT(IN) :: Y(1:N), X(1:N), AX
      REAL(8), INTENT(OUT) :: AY
      INTEGER :: I
      REAL(8) :: DX, DY

      AY = 0.0D0
      IF(AX.EQ.X(N)) THEN
        AY = Y(N)
        RETURN
      ENDIF

      DO I = 1, N-1
        IF(AX.EQ.X(I)) THEN
            AY = Y(I)
        ELSEIF(AX.GT.X(I).AND.AX.LT.X(I + 1)) THEN

            DX = X(I + 1) - X(I)
            DY = Y(I + 1) - Y(I)
            AY = (DY/DX)*(AX - X(I)) + Y(I)

        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE INTERP
