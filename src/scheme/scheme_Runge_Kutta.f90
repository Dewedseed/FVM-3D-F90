!=============================================================================80
!
!> Runge Kutta scheme
!
!=============================================================================80

module scheme_Runge_Kutta

  use kind_parameter, only : dp

  use global_devlog, only : devlog, LogLevel

  implicit none

!...Define scope
  private

  public  :: Runge_Kutta_sub

!...Declare local variables

contains

!================================== Runge_Kutta ==============================80
!
!>  3-Stage Runge-Kutta scheme
!
!=============================================================================80

  subroutine Runge_Kutta_sub(m, dt, u0, um, rm)

  !...Declare input/output variables
    integer,  intent(in) :: m   !< m-stage Runge-Kutta
    real(dp), intent(in) :: dt  !< real time step

    real(dp), dimension(:,:,:,:), pointer :: u0 !< 0-stage conserved quantity
    real(dp), dimension(:,:,:,:), pointer :: um !< m-stage conserved quantity
    real(dp), dimension(:,:,:,:), pointer :: rm !< m-stage residual

  !...Declare local variables
    real(dp), dimension(3), parameter :: alpha_0 = (/     1.0,     0.0,     0.0 /)
    real(dp), dimension(3), parameter :: alpha_1 = (/     1.0,     0.0,     1.0 /)
    real(dp), dimension(3), parameter :: alpha_2 = (/ 3.0/4.0, 1.0/4.0, 1.0/4.0 /)
    real(dp), dimension(3), parameter :: alpha_3 = (/ 1.0/3.0, 2.0/3.0, 2.0/3.0 /)

  continue

    select case (m)
      case (0)
        u0 = um
      case (1)
        um = u0 + dt * rm
      case (2)
        um = alpha_2(1) * u0 + alpha_2(2) * um + alpha_2(3) * dt * rm
      case (3)
        um = alpha_3(1) * u0 + alpha_3(2) * um + alpha_3(3) * dt * rm
      case default
        call devlog%print(where="scheme_Runge_Kutta::Runge_Kutta_sub", &
                          message="Exceeding Runge-Kutta scheme stage!", &
                          level=LogLevel%error)
    end select

  end subroutine Runge_Kutta_sub

end module scheme_Runge_Kutta
