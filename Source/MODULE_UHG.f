C=================================================================
C MODULE CONTAINS VARIABLES FOR UNIT HYDROGRAPH
C=================================================================
      MODULE UNIT_HYDROGRAPH
      IMPLICIT NONE



C Variable definition
      INTEGER :: NUHG
      REAL(8), ALLOCATABLE, DIMENSION(:,:) :: UHG_DATA

      END MODULE UNIT_HYDROGRAPH
C     Subroutine definition
C=================================================================
C Subroutine get unit hydrograph
C=================================================================
      SUBROUTINE GET_UHG(UHG_TYPE)
      USE CONSTANTS
      USE UNIT_HYDROGRAPH
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: UHG_TYPE

      SELECT CASE (UHG_TYPE)
        CASE(SCS_UHG_TYPE)
            CALL GET_SCS_UHG
        CASE DEFAULT
            WRITE(*,*) 'Error: Invalid type of unit hydrograph!!!'
            STOP
      END SELECT

      RETURN
      END SUBROUTINE GET_UHG
C=================================================================
C
C=================================================================
C=================================================================
C Subroutine get SCS unit hydrograph
C=================================================================
      SUBROUTINE GET_SCS_UHG
      USE UNIT_HYDROGRAPH
      IMPLICIT NONE

      NUHG = 32
      ALLOCATE(UHG_DATA(1:2, 0:32))
C This dimensionless unit hydrograph was developed by Victor Mockus(1957)
      !T/TP
      UHG_DATA(1,0:32)= (/0.0D0, 0.1D0, 0.2D0, 0.3D0, 0.4D0, 0.5D0,
     &                    0.6D0, 0.7D0, 0.8D0, 0.9D0, 1.0D0, 1.1D0,
     &                    1.2D0, 1.3D0, 1.4D0, 1.5D0, 1.6D0, 1.7D0,
     &                    1.8D0, 1.9D0, 2.0D0, 2.2D0, 2.4D0, 2.6D0,
     &                    2.8D0, 3.0D0, 3.2D0, 3.4D0, 3.6D0, 3.8D0,
     &                    4.0D0, 4.5D0, 5.0D0/)

      UHG_DATA(2,0:32)= (/0.0D0, 0.03D0, 0.1D0, 0.19D0, 0.31D0, 0.47D0,
     &                    0.66D0, 0.82D0, 0.93D0, 0.99D0, 1.0D0, 0.99D0,
     &                    0.93D0, 0.86D0, 0.78D0, 0.68D0, 0.56D0, 0.46D0,
     &                    0.39D0, 0.33D0, 0.28D0, 0.207D0, 0.147D0, 0.107D0,
     &                    0.077D0, 0.055D0, 0.04D0, 0.029D0, 0.021D0,
     &                    0.015D0, 0.011D0, 0.005D0, 0.0D0/)


      RETURN
      END SUBROUTINE GET_SCS_UHG

C=================================================================
C
C=================================================================




