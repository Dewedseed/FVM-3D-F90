!=============================================================================80
!
!> Weighted Essentially Non-Oscillatory (WENO) scheme
!
!=============================================================================80

module scheme_WENO

  use kind_parameter, only : dp, i8
  use global_devlog,  only : devlog, LogLevel

  implicit none
  private

  public :: reconstruct_WENO_sub

contains

!=============================================================================80
!
!> Reconstruct using weighted Essentially Non-Oscillatory (WENO) scheme
!
! FDM: reconstruct fluxs
! FVM: reconstruct conservative variables
!
!=============================================================================80

  subroutine reconstruct_WENO_sub(nface, neq, dghost, qg, qr, wind)
  !...Declare input/output variables
    integer(i8), intent(in) :: nface
    integer,     intent(in) :: neq !< number of equations
    integer(i8), intent(in) :: dghost
    real(dp),    intent(inout) :: qg(:,:) !< conservative variables (ghost mesh)
    real(dp),    intent(inout) :: qr(:,:) !< reconstruct variables (real mesh)
    integer,     intent(in) :: wind !< -1: left, 1: right
  !...Declare local variable
    integer(i8) :: stencil(2)
    integer     :: ieq
    integer(i8) :: iface, ig, ig_left, ig_right
  continue
  !...Set stencil
    ! real-mesh:                     * --- * --- * --- *
    ! real-index:                    1     2     3     4
    ! face-index:                 1     2     3     4     5
    ! ghost-mesh:  + ... + ... + ... * --- * --- * --- * ... + ... + ... +
    ! p-index:     1     2     3     4     5     6     7     8     9     10
    ! stencil-Q^L  =========================
    ! stencil-Q^R        =========================
    !
    select case (wind)
      case (-1)
        ! stencil(L) = (i-2, i-1, i, i+1, i+2)
        stencil = (/ -2, 2 /)
      case (1)
        ! stencil(R) = (i-1, i, i+1, i+2, i+3)
        stencil = (/ -1, 3 /)
      case default
        call devlog%print(where="scheme_WENO::reconstruct_WENO_sub", &
                          message="Invalid wind direction",&
                          level=LogLevel%error)
    end select
  !...Loop over faces
    do iface = 1, nface
      ig = iface + dghost - 1
      ig_left  = ig + stencil(1)
      ig_right = ig + stencil(2)
    !...Reconstruct conservative variables
      do ieq = 1, neq
        qr(iface, ieq) = WENO_JS(qg(ig_left:ig_right, ieq))
      end do
    end do
  end subroutine reconstruct_WENO_sub

  !> Reconstruct variables using WENO scheme
  !    ur = sum(omega_k * q_k), k = 1, 2, 3
  !  ENO 3-order Variables
  !    q_1 = 1/3 * u(i-2) - 7/6 * u(i-1) + 11/6 * u(i)
  !    q_2 = -1/6 * u(i-1) + 5/6 * u(i) + 1/3 * u(i+1)
  !    q_3 = 1/3 * u(i) + 5/6 * u(i+1) - 1/6 * u(i+2)
  !  Jiang and Shu Weights
  !    omega_k = alpha_k / (alpha_1 + alpha_2 + alpha_3)
  !    alpha_k = c_k / (IS_k + epsilon)^2, c = (0.1, 0.6, 0.3)
  !  Local smoothness indicators
  !    IS_1 = 13/12 * (u(i-2) - 2*u(i-1) + u(i))**2 + 1/4 * (u(i-2) - 4*u(i-1) + 3*u(i))**2
  !    IS_2 = 13/12 * (u(i-1) - 2*u(i) + u(i+1))**2 + 1/4 * (u(i-1) - u(i+1))**2
  !    IS_3 = 13/12 * (u(i) - 2*u(i+1) + u(i+2))**2 + 1/4 * (3*u(i) - 4*u(i+1) + u(i+2))**2
  real(dp) function WENO_JS(u) result(ur)
  !...Declare input/output variables
    real(dp), intent(in) :: u(:) !< varialbes stencil
  !...Declare local variables
    real(dp) :: q0, q1, q2 !< ENO 3-order variables
    real(dp) :: omega_0, omega_1, omega_2 !< weights
    real(dp) :: alpha_0, alpha_1, alpha_2, alpha_sum
    real(dp) :: IS_0, IS_1, IS_2 !< local smoothness indicators
    real(dp) :: c_0 = 0.1_dp
    real(dp) :: c_1 = 0.6_dp
    real(dp) :: c_2 = 0.3_dp
    real(dp) :: eps = 1.0e-6_dp
  continue
  !...Compute ENO 3-order variables
    q0 =  1.0_dp/3.0_dp * u(1) - 7.0_dp/6.0_dp * u(2) + 11.0_dp/6.0_dp * u(3)
    q1 = -1.0_dp/6.0_dp * u(2) + 5.0_dp/6.0_dp * u(3) + 1.0_dp/3.0_dp  * u(4)
    q2 =  1.0_dp/3.0_dp * u(3) + 5.0_dp/6.0_dp * u(4) - 1.0_dp/6.0_dp  * u(5)
  !...Compute local smoothness indicators
    IS_0 = 13.0_dp/12.0_dp * (u(1) - 2.0_dp * u(2) +          u(3))**2 + &
             1.0_dp/4.0_dp * (u(1) - 4.0_dp * u(2) + 3.0_dp * u(3))**2
    IS_1 = 13.0_dp/12.0_dp * (u(2) - 2.0_dp * u(3) +          u(4))**2 + &
             1.0_dp/4.0_dp * (u(2)                 +          u(4))**2
    IS_2 = 13.0_dp/12.0_dp * (         u(3) - 2.0_dp * u(4) + u(5))**2 + &
             1.0_dp/4.0_dp * (3.0_dp * u(3) - 4.0_dp * u(4) + u(5))**2
  !...Compute weights
    alpha_0 = c_0 / (IS_0 + eps)**2
    alpha_1 = c_1 / (IS_1 + eps)**2
    alpha_2 = c_2 / (IS_2 + eps)**2
    alpha_sum = alpha_0 + alpha_1 + alpha_2
    omega_0 = alpha_0 / alpha_sum
    omega_1 = alpha_1 / alpha_sum
    omega_2 = alpha_2 / alpha_sum
  !...Compute reconstructed value
    ur = omega_0 * q0 + omega_1 * q1 + omega_2 * q2
  end function WENO_JS

end module scheme_WENO

