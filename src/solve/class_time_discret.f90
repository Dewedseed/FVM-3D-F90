!=============================================================================80
!
!> Temporal discretization variables and methods.
!
!=============================================================================80

module class_time_discret

  use kind_parameter, only : dp, len_short
  use kind_parameter, only : double

  use global_class, only : error, terminal, mpi
  use global_class, only : string

  use global_devlog, only : devlog, LogLevel

  use global_type_defs, only : TypeDefs

  implicit none

!...Define scope
  private

  public  :: ClassTimeDiscret

!...Declare local variables
  type :: ClassTimeDiscret
    integer,  private :: scheme   = 0 !< time discretization scheme
    integer,  private :: total_RK_stage = 3 !< m-stage Runge-Kutta
    character(len_short), private :: timestep_type = ""
              !< time step type:
              !! (1) "fixed"  fixed time step;
              !! (2) "set"    set as variable;
              !! (3) "CFL"    based on CFL.
    real(dp), private :: dt_fix   = 0.0_dp !< time step
  contains
    procedure, public, pass :: input       => input_sub
    procedure, public, pass :: check_input => check_input_sub
    procedure, public, pass :: timestep    => timestep_fun
    procedure, public, pass :: solve       => solve_sub
    procedure, private, pass :: Runge_Kutta => Runge_Kutta_sub
    final :: delete_ClassTimeDiscret
  end type ClassTimeDiscret

contains

  !> Constructor of class ClassTimeDiscret
  function new_ClassTimeDiscret() result(this)
    type(ClassTimeDiscret) :: this
  continue
  end function new_ClassTimeDiscret

  !> Destructor of class ClassTimeDiscret
  subroutine delete_ClassTimeDiscret(this)
    type(ClassTimeDiscret), intent(inout) :: this
  continue
  end subroutine delete_ClassTimeDiscret

  !> Input time discretization information
  subroutine input_sub(this, vname, vvalue)
  !...Declare input/output variables
    class(ClassTimeDiscret), intent(inout) :: this
    character(len=*),        intent(in)    :: vname  !< variable name
    character(len=*),        intent(in)    :: vvalue !< variable value
  continue
    select case (trim(vname))
      case ("time_scheme")
        this%scheme = TypeDefs%SchemeIndex(trim(vvalue))
      case ("timestep_set")
        this%timestep_type = trim(vvalue)
      case ("fix_timestep")
        this%dt_fix = string%to_num(vvalue, double, error%iread)
        if ( error%read("fix_timestep") ) call mpi%stop(error%what())
      case default
        call devlog%print(where="ClassTimeDiscret::input_sub", &
                          message="Invalid argument: " // trim(vname), &
                          level=LogLevel%error)
    end select
  end subroutine input_sub

  !> Check input
  subroutine check_input_sub(this)
  !...Declare input/output variables
    class(ClassTimeDiscret), intent(inout) :: this
  continue
    call terminal%print_no_wrap("- Check time discretization parameters...")
    if ( this%scheme == TypeDefs%Scheme%NULL ) then
      call mpi%stop("Invalid time discretization scheme.")
    end if
    if ( this%dt_fix < -1.0E-6 ) then
      call mpi%stop("Invalid time step.")
    end if
    call terminal%print("Done.")
    select case (trim(this%timestep_type))
      case ("fixed")
        continue
      case ("set")
        call devlog%print(where="ClassTimeDiscret::check_input_sub", &
                          message="Not implemented yet.", &
                          level=LogLevel%error)
      case ("CFL")
        call devlog%print(where="ClassTimeDiscret::check_input_sub", &
                          message="Not implemented yet.", &
                          level=LogLevel%error)
      case default
        call mpi%stop("Invalid timestep type.")
    end select
  end subroutine check_input_sub

  !> compute time step
  function timestep_fun(this) result(timestep)
  !...Declare input/output variables
    class(ClassTimeDiscret), intent(inout) :: this
    real(dp)                               :: timestep
  continue
    select case (trim(this%timestep_type))
      case ("fixed")
        timestep = this%dt_fix
      case ("set")
      case ("CFL")
    end select
  end function timestep_fun

  !> Solve equation
  subroutine solve_sub(this, geometry, flowfield, spaceDiscr, iteration)
    use class_geometry,      only : ClassGeometry
    use class_flow_field,    only : ClassFlowField
    use class_space_discret, only : ClassSpaceDiscret
    use class_iteration,     only : ClassIteration
  !...Declare input/output variables
    class(ClassTimeDiscret), intent(inout) :: this
    type(ClassGeometry),     intent(inout) :: geometry
    type(ClassFlowField),    intent(inout) :: flowfield
    type(ClassSpaceDiscret), intent(inout) :: spaceDiscr
    type(ClassIteration),    intent(inout) :: iteration
  !...Declare local variables
    integer :: iblk
    real(dp) :: dt
  continue
  !...Compute time step and real time
    dt                 = this%timestep()
    iteration%time     = iteration%time + dt
    call flowfield%set_realtime(iteration%time)
  !...Select time discretization scheme
    if ( this%scheme == TypeDefs%Scheme%Runge_Kutta ) then
      call this%Runge_Kutta(dt, geometry, flowfield, spaceDiscr)
    else if ( this%scheme == TypeDefs%Scheme%BDF1 ) then
      ! call this%BDF1()
    end if
  !...Compute total residual
    call iteration%reset_residual()
    do iblk = 1, geometry%nblocks()
      call iteration%compute_residual(flowfield%block(iblk)%get_rhs())
    end do
    call iteration%update()
  end subroutine solve_sub

  !> Runge-Kutta time integration
  subroutine Runge_Kutta_sub(this, dt, geometry, flowfield, spaceDiscr)
    use class_geometry,      only : ClassGeometry
    use class_flow_field,    only : ClassFlowField
    use class_space_discret, only : ClassSpaceDiscret
    use numerical_scheme,    only : TimeScheme
  !...Declare input/output variables
    class(ClassTimeDiscret), intent(inout) :: this
    real(dp),                intent(inout) :: dt
    type(ClassGeometry),     intent(inout) :: geometry
    type(ClassFlowField),    intent(inout) :: flowfield
    type(ClassSpaceDiscret), intent(inout) :: spaceDiscr
  !...Declare local variables
    integer :: iRK !< Runge-Kutta stage
    integer :: iblk !< index of block
  continue
    call flowfield%reset_q0()
  !...Runge-Kutta Loop
    do iRK = 1, this%total_RK_stage
      call devlog%print('RK stage ' // string%from(iRK))
      call flowfield%reset_rhs()
    !...Inviscid flux
      call spaceDiscr%inviscid_flux(geometry, flowfield)
    !...Viscous flux (Navier-Stokes)
      call spaceDiscr%viscous_flux(geometry, flowfield)
    !...Source Term
      ! call source%update(geometry, flowfield)
    !...Temporal discretization
      do iblk = 1, geometry%nblocks()
        call devlog%print('Time Stepping (Runge-Kutta) Block ' // string%from(iblk))
        call TimeScheme%Runge_Kutta(iRK, dt, &
                                    flowfield%block(iblk)%get_q0(), &
                                    flowfield%block(iblk)%get_q(), &
                                    flowfield%block(iblk)%get_rhs())
      end do
    !...Update primitive variables
      call flowfield%update()
    end do
  end subroutine Runge_Kutta_sub

end module class_time_discret
