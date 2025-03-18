!=============================================================================80
!
! Compute primitive variable gradient using Green's theorem for viscous fluxes.
!
!=============================================================================80

module scheme_gradient_Green

  implicit none
  private

  public :: gradient_Green_sub

contains

!=========================== GRADIENT_GREEN_SUB ==============================80
!
!> Compute primitive variable gradient using Green's theorem for viscous fluxes.
!
! TODO: this is simplest implementation
!
!         * --------- * --------- * --------- *
!         |           |           |           |
!         |            ...........            |
!         |I-1        :I          :I+1        |
!         * --------- * --- o --- * --------- *
!         |           :     I+1/2 :           |
!         |            ...........            |
!         |           |           |           |
!         * --------- * --------- * --------- *
!
!      --- control volume
!      ... auxiliary volume
!
!   div(U)_x = sum (Um * Sm'_x) / Omega'
!
!     U: flow variables, velocity / temperature etc.
!     Sm'_x: x-component auxiliary volume face vector
!     Omega': volume of auxiliary
!
!   1. curvilinear coordinate
!   2. x direction = i direction = xi direction
!       geometry       mesh         curvilinear
!   3. uniform grid
!
!   Sm'_x on j, k direction = 0    =>
!     div(U)_x = (U(I+1) * dJ*dK - U(I) * dJ*dK) / (dI * dJ * dK)
!              = (U(I+1) - U(I)) / dI
!   Sm'_y on i, k direction = 0    =>
!     div(U)_y = (U(J+1/2) * dI*dK - U(J-1/2) * dI*dK) / (dI * dJ * dK)
!              = ((U(I+1,J+1) + U(I+1,J) + U(I,J+1) + U(I,J))/4 -
!                 (U(I+1,J) + U(I+1,J-1) + U(I,J) + U(I,J-1))/4) / dJ
!              = (U(I+1,J+1) - U(I+1,J) + U(I,J+1) - U(I,J)) / (2 * dJ)
!   Sm'_z on i, k direction = 0    =>
!     div(U)_z = (U(I+1/2,J+1/2) * dJ - U(I-1/2,J-1/2) * dJ) / (dI * dJ)
!
!=============================================================================80

  subroutine gradient_Green_sub()
  continue
  end subroutine gradient_Green_sub

end module scheme_gradient_Green
