!=============================================================================80
!
!> Fluid properties class
!!
!!  (1) conserved variables (rho, rho*u, rho*v, rho*w, rho*E)
!!  (2) primitive variables (rho, u, v, w, E)
!!  (3) reference variables (rho, u, v, w, p or T)_ref
!!  (4) dependent variables (p, T, c, gamma, cp, cv)
!!  (5) Transport variables (D, mu, k)
!!  (6) Thermal variables (e, h, s)
!!  (7) Turbulent Variable (k, epsilon, omega)
!
!=============================================================================80

module class_flow_field

  use kind_parameter,     only : i2, i4, i8, dp, len_short
  use container_linklist, only : linklist

  use defs_block_data,    only : ClassBlockData
  use global_type_defs,   only : TypeDefs
  use defs_fluid_props,   only : Reference_Variable

  use global_class,   only : error, mpi, terminal

  use global_class,  only : string
  use global_devlog, only : devlog, LogLevel

  implicit none

!...Define scope
  private

  public :: ClassFlowField

!...Declare Class
  type :: ClassFlowField
  !...Public variables
    integer,  public :: eqn_type = 0 !< equation type (0: Euler, 1: NS)
    integer,  public :: gas_type = 0 !< gas type
    real(dp), public :: gamma    = 1.40_dp !< ratio of specific heats
    type(Reference_Variable), pointer, public :: p_ref(:) => null()
      !< primitive reference variables
    real(dp), public :: realtime = 0.0_dp !< time
    integer,  public :: nblocks  = 0 !< number of blocks
    type(ClassBlockData), pointer, public :: block(:) => null()
  !...Private variables 1
    integer, private :: ncon_var = 0 !< number of conserved variables = equations
    logical, private :: small_memory = .false.
      !< if memory capacity is small, save data in blockdata-q array only;
      !! change variables type (conservative or primitive) in blockdata-q array;
      !! otherwise, save conservative variables in blockdata-q array and
      !! save primitive variables in blockdata-p array
    logical, private :: is_conservative  = .false.
      !< conservative or not (primitive)
      !! use for <small_memory> to check variable type
    logical, private :: is_dimensionless = .false. !< dimensionless or not
    type(linklist), private :: realtime_list !< save realtime list for cgns file
    !...Private variables 2
    integer(i2), private :: iDensity    = 0
    integer(i2), private :: iMomentum_x = 0
    integer(i2), private :: iMomentum_y = 0
    integer(i2), private :: iMomentum_z = 0
    integer(i2), private :: iEnergy     = 0
    integer(i2), private :: iTurb_Kinte = 0 !< turbulent kinetic energy
    integer(i2), private :: iTurb_Dissp = 0 !< turbulent dissipation
  contains
    procedure :: input           => input_sub
    procedure :: check_input     => check_input_sub
    procedure :: allocate_memory => allocate_memory_sub
    procedure :: variable_type   => variable_type_fun
    procedure :: initial         => initial_sub
    procedure :: to_conservative => to_conservative_sub
    procedure :: nconserved_var  => nconserved_var_fun
    procedure :: dimensionless   => dimensionless_sub
    procedure :: set_realtime    => set_realtime_sub
    procedure :: update          => update_primitive_sub
    procedure :: reset_q0        => update_q0_sub
    procedure :: reset_rhs       => reset_rhs_sub

    procedure, private :: convert_q_to_p     => convert_q_to_p_sub
    procedure, private :: convert_q_to_p_line => convert_q_to_p_line_sub
    procedure, private :: convert_q_to_p_face => convert_q_to_p_face_sub
    procedure, private :: convert_q_to_p_blk => convert_q_to_p_blk_sub
    procedure, private :: convert_p_to_q_blk => convert_p_to_q_blk_sub
    generic :: convert => convert_q_to_p, &
                          convert_q_to_p_line, &
                          convert_q_to_p_face, &
                          convert_q_to_p_blk, &
                          convert_p_to_q_blk
    final :: delete_ClassFlowField
  end type ClassFlowField

contains

  !> Destructor of class ClassFlowField
  subroutine delete_ClassFlowField(this)
  !...Deallocating pointer attribute
    type(ClassFlowField), intent(inout) :: this
  continue
    if (associated(this%block)) deallocate(this%block)
    if (associated(this%p_ref)) deallocate(this%p_ref)
  end subroutine delete_ClassFlowField

  !> Input fluid properties
  subroutine input_sub(this, vname, vvalue)
  !...Declare input/output variables
    class(ClassFlowField), intent(inout) :: this
    character(len=*),      intent(in)    :: vname  !< variable name
    character(len=*),      intent(in)    :: vvalue !< variable value
  continue
    select case (trim(vname))
      case ("equation")
        this%eqn_type = TypeDefs%EquationIndex(trim(vvalue))
      case ("gasModel")
        this%gas_type = TypeDefs%GasIndex(trim(vvalue))
      case default
        call devlog%print(where="ClassFlowField::input_sub", &
                          message="Unknown variable name: "//trim(vname), &
                          level=LogLevel%error)
    end select
  end subroutine input_sub

  !> Check fluid properties
  subroutine check_input_sub(this)
  !...Declare input/output variables
    class(ClassFlowField), intent(inout) :: this
  continue
    call terminal%print_no_wrap("- Check flowfield parameters...")
    if ( this%eqn_type == TypeDefs%Equation%NULL ) then
        call mpi%stop("Invalid equation type.")
    end if
    if ( this%gas_type == TypeDefs%Gas%NULL ) then
        call mpi%stop("Invalid gas model.")
    end if
    call terminal%print("Done.")
  end subroutine check_input_sub

  !> Allocate memory for fluid properties
  subroutine allocate_memory_sub(this, system, geometry)
    use class_equation, only : ClassPhysicalSystem
    use class_geometry, only : ClassGeometry
  !...Declare input/output variables
    class(ClassFlowField),     intent(inout) :: this
    type(ClassPhysicalSystem), intent(inout) :: system
    type(ClassGeometry),       intent(inout) :: geometry
  !...Local variables
    integer(i4) :: ib
  continue
    call terminal%print(">> Allocate memory for flowfield...")
  !...Allocate memory for blocks
    this%nblocks = geometry%nblocks()
    allocate(this%block(geometry%nblocks()), stat=error%ialloc)
    if (error%allocate("Blocks Data")) call mpi%stop(error%what())
    do ib = 1, this%nblocks
      call this%block(ib)%link_geom(geometry%block(ib))
    end do
  !...Get variable indices
    this%ncon_var    = system%size()
    if (this%ncon_var == 0) call mpi%stop("No physical system created.")
    this%iDensity    = system%eqn_index('mass')
    this%iMomentum_x = system%eqn_index('momentumX')
    this%iMomentum_y = system%eqn_index('momentumY')
    this%iMomentum_z = system%eqn_index('momentumZ')
    this%iEnergy     = system%eqn_index('energy')
    this%iTurb_Kinte = system%eqn_index('turbKinetic')
    this%iTurb_Dissp = system%eqn_index('turbDissp')
  !...Allocate memory for variables
    do ib = 1, this%nblocks
      call this%block(ib)%allocate_memory(this%ncon_var, error)
      if (error%occur()) call mpi%stop(error%what())
    end do
  end subroutine allocate_memory_sub

  !> Return number of conserved variables
  integer function nconserved_var_fun(this) result(ncon_var)
    class(ClassFlowField), intent(inout) :: this
  continue
    ncon_var = this%ncon_var
  end function nconserved_var_fun

  !> Return variable type
  character(len_short) function variable_type_fun(this) result(var_type)
  !...Declare input/output variables
    class(ClassFlowField), intent(inout) :: this
  continue
    if ( this%is_conservative ) then
      var_type = "conservative variables"
    else
      var_type = "primitive variables"
    end if
  end function variable_type_fun

  !> Initial condition
  subroutine initial_sub(this)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    class(ClassFlowField), intent(inout) :: this
  !...Declare local variables
    type(Primitive_Variable) :: p_set
    real(dp)    :: region(2,3)
    integer(i4) :: ib
    real(dp)    :: x_mid, y_mid
  continue
    call terminal%print()
    call terminal%print(">> Initialize flowfield...")
  ! TODO: This is for Riemann-2D problem
    do ib = 1, this%nblocks
      x_mid = 0.8_dp
      y_mid = 0.8_dp
    !...Region 1
      region = reshape( (/ x_mid, 1.0_dp, y_mid, 1.0_dp, 0.0_dp, 1.0_dp /), (/ 2, 3 /) )
      p_set%rho   = 1.5_dp
      p_set%u     = 0.0_dp
      p_set%v     = 0.0_dp
      p_set%w     = 0.0_dp
      p_set%press = 1.5_dp
      call this%block(ib)%init_value(p_set, region)
    !...Region 2
      region = reshape( (/ 0.0_dp, x_mid, y_mid, 1.0_dp, 0.0_dp, 1.0_dp /), (/ 2, 3 /) )
      p_set%rho   = 0.5323_dp
      p_set%u     = 1.206_dp
      p_set%v     = 0.0_dp
      p_set%w     = 0.0_dp
      p_set%press = 0.3_dp
      call this%block(ib)%init_value(p_set, region)
    !...Region 3
      region = reshape( (/ 0.0_dp, x_mid, 0.0_dp, y_mid, 0.0_dp, 1.0_dp /), (/ 2, 3 /) )
      p_set%rho   = 0.138_dp
      p_set%u     = 1.206_dp
      p_set%v     = 1.206_dp
      p_set%w     = 0.0_dp
      p_set%press = 0.029_dp
      call this%block(ib)%init_value(p_set, region)
    !...Region 4
      region = reshape( (/ x_mid, 1.0_dp, 0.0_dp, y_mid, 0.0_dp, 1.0_dp /), (/ 2, 3 /) )
      p_set%rho   = 0.5323_dp
      p_set%u     = 0.0_dp
      p_set%v     = 1.206_dp
      p_set%w     = 0.0_dp
      p_set%press = 0.3_dp
      call this%block(ib)%init_value(p_set, region)
    end do
  !...Update conservative variables
    do ib = 1, this%nblocks
      call this%convert(this%block(ib)%get_qp(), &
                        this%block(ib)%get_q(), &
                        this%ncon_var)
    end do
  !...Update ghost cell regions
    do ib = 1, this%nblocks
      call this%block(ib)%update_ghost_cell()
    end do
  !...End
    call terminal%print()
  end subroutine initial_sub

  !> Convert primitive variables to conservative variables
  subroutine to_conservative_sub(this)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    class(ClassFlowField), intent(inout) :: this
  !...Declare local variables
    type(Primitive_Variable), pointer :: qp_new(:,:,:) !< new primitive variable
    real(dp), pointer :: u(:,:,:)
    real(dp), pointer :: v(:,:,:)
    real(dp), pointer :: w(:,:,:)
    real(dp), pointer :: p(:,:,:) !< pressure
    real(dp), pointer :: rho(:,:,:)
    real(dp), pointer :: xmom(:,:,:)
    real(dp), pointer :: ymom(:,:,:)
    real(dp), pointer :: zmom(:,:,:)
    real(dp), pointer :: ener(:,:,:)
    integer(i4) :: ib
    integer(i8) :: i, j, k
    real(dp)    :: gamma
  continue
  ! TODO: ideal gas only
    if ( this%gas_type == TypeDefs%Gas%Calorically_Perfect ) then
      gamma = this%gamma
    else
      gamma = 1.4_dp
    end if
    do ib = 1, this%nblocks
    !...Data buffer
      qp_new => this%block(ib)%get_qp()
      u    => qp_new%u
      v    => qp_new%v
      w    => qp_new%w
      p    => qp_new%press
    !...Compute conservative variables
      rho  => this%block(ib)%get(this%iDensity)
      xmom => this%block(ib)%get(this%iMomentum_x)
      ymom => this%block(ib)%get(this%iMomentum_y)
      zmom => this%block(ib)%get(this%iMomentum_z)
      ener => this%block(ib)%get(this%iEnergy)
    !...Compute conservative variables
      rho  = qp_new%rho
      xmom = rho * u
      ymom = rho * v
      zmom = rho * w
      ener = p/(gamma - 1.0_dp) + 0.5_dp * rho * (u**2 + v**2 + w**2)
    end do
  end subroutine to_conservative_sub

  !> Dimensionless variables
  subroutine dimensionless_sub(this)
  !...Declare input/output variables
    class(ClassFlowField), intent(inout) :: this
  !...Declare local variables
  continue
    call terminal%print(">> Flowfield dimensionless...")
  end subroutine dimensionless_sub

  !> Set real time
  subroutine set_realtime_sub(this, realtime)
  !...Declare input/output variables
    class(ClassFlowField), intent(inout) :: this
    real(dp),              intent(in)    :: realtime
  continue
    call devlog%print("Flowfield real time " // string%from(realtime))
    this%realtime = realtime
    error%ialloc = this%realtime_list%push_back(this%realtime)
    if (error%allocate("Realtime")) call mpi%stop(error%what())
  end subroutine set_realtime_sub

  !> update primitive variables
  subroutine update_primitive_sub(this)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    class(ClassFlowField), intent(inout) :: this
  !...Declare local variables
    integer :: iblk
  continue
  !...Get primitive and conservative variables
    do iblk = 1, this%nblocks
      call this%convert(this%block(iblk)%get_q(), this%block(iblk)%get_qp())
      call this%block(iblk)%update_ghost_cell()
    end do
  end subroutine update_primitive_sub

  !> Convert primitive variables to conservative variables
  !   Primitive variables:    rho, u, v, w, p
  !   Conservative variables: rho, rho*u, rho*v, rho*w, rho*E
  !   rho*E = p/(gamma - 1.0_dp) + 0.5_dp * rho * (u**2 + v**2 + w**2)
  !
  !    E = e + e_k
  !    e = c_v T                  gamma = h / e
  !    h = c_p T             =>   gamma * e = e + p / rho
  !    h = e + p / rho            rho * e = p / (gamma - 1.0_dp)
  !    gamma = c_p / c_v
  !
  subroutine convert_p_to_q_blk_sub(this, p, q, neqn)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    class(ClassFlowField),    intent(inout) :: this
    type(Primitive_Variable), intent(inout) :: p(:,:,:)
    real(dp),                 intent(inout) :: q(:,:,:,:)
    integer,                  intent(in)    :: neqn
  !...Declare local variable
    integer :: ieqn
  continue
    do ieqn = 1, neqn
      if ( ieqn == this%iDensity ) then
        q(:,:,:,ieqn) = p%rho
      else if ( ieqn == this%iMomentum_x ) then
        q(:,:,:,ieqn) = p%rho * p%u
      else if ( ieqn == this%iMomentum_y ) then
        q(:,:,:,ieqn) = p%rho * p%v
      else if ( ieqn == this%iMomentum_z ) then
        q(:,:,:,ieqn) = p%rho * p%w
      else if ( ieqn == this%iEnergy ) then
        q(:,:,:,ieqn) = p%press / (this%gamma - 1.0_dp) + &
                        0.5_dp * p%rho * (p%u**2 + p%v**2 + p%w**2)
      end if
    end do
  end subroutine convert_p_to_q_blk_sub

  !> Convert conservative variables to primitive variables (block)
  !     Primitive variables:    rho, u, v, w, p
  !     Conservative variables: rho, rho*u, rho*v, rho*w, rho*E
  !     p = (gamma - 1.0_dp) * (rho*E - 0.5_dp * rho * (u**2 + v**2 + w**2))
  subroutine convert_q_to_p_blk_sub(this, q, p)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    class(ClassFlowField),    intent(inout) :: this
    real(dp),                 intent(inout) :: q(:,:,:,:)
    type(Primitive_Variable), intent(inout) :: p(:,:,:)
  continue
    p%rho   = q(:,:,:,this%iDensity)
    p%u     = q(:,:,:,this%iMomentum_x) / p%rho
    p%v     = q(:,:,:,this%iMomentum_y) / p%rho
    p%w     = q(:,:,:,this%iMomentum_z) / p%rho
    p%press = (this%gamma - 1.0_dp) * (q(:,:,:,this%iEnergy) - &
                0.5_dp * p%rho * (p%u**2 + p%v**2 + p%w**2))
  end subroutine convert_q_to_p_blk_sub

  !> Convert conservative variables to primitive variables (face)
  subroutine convert_q_to_p_face_sub(this, q, p)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    class(ClassFlowField),    intent(inout) :: this
    real(dp),                 intent(inout) :: q(:,:,:)
    type(Primitive_Variable), intent(inout) :: p(:,:)
  continue
    p%rho   = q(:,:,this%iDensity)
    p%u     = q(:,:,this%iMomentum_x) / p%rho
    p%v     = q(:,:,this%iMomentum_y) / p%rho
    p%w     = q(:,:,this%iMomentum_z) / p%rho
    p%press = (this%gamma - 1.0_dp) * (q(:,:,this%iEnergy) - &
                0.5_dp * p%rho * (p%u**2 + p%v**2 + p%w**2))
  end subroutine convert_q_to_p_face_sub

  !> Convert primitive variables to conservative variables (line)
  subroutine convert_q_to_p_line_sub(this, q, p)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    class(ClassFlowField),    intent(inout) :: this
    real(dp),                 intent(inout) :: q(:,:)
    type(Primitive_Variable), intent(inout) :: p(:)
  continue
    p%rho   = q(:,this%iDensity)
    p%u     = q(:,this%iMomentum_x) / p%rho
    p%v     = q(:,this%iMomentum_y) / p%rho
    p%w     = q(:,this%iMomentum_z) / p%rho
    p%press = (this%gamma - 1.0_dp) * (q(:,this%iEnergy) - &
                0.5_dp * p%rho * (p%u**2 + p%v**2 + p%w**2))
  end subroutine convert_q_to_p_line_sub

  !> Convert primitive variables to conservative variables (point)
  subroutine convert_q_to_p_sub(this, q, p)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    class(ClassFlowField),    intent(inout) :: this
    real(dp),                 intent(inout) :: q(:)
    type(Primitive_Variable), intent(inout) :: p
  continue
    p%rho = q(this%iDensity)
    p%u   = q(this%iMomentum_x) / p%rho
    p%v   = q(this%iMomentum_y) / p%rho
    p%w   = q(this%iMomentum_z) / p%rho
    p%press = (this%gamma - 1.0_dp) * (q(this%iEnergy) - &
        0.5_dp * p%rho * (p%u**2 + p%v**2 + p%w**2))
  end subroutine convert_q_to_p_sub

  !> Update initial conservative variables
  subroutine update_q0_sub(this)
  !...Declare input/output variables
    class(ClassFlowField), intent(inout) :: this
  !...Declare local variable
    integer :: iblk
    real(dp), pointer :: q0(:,:,:,:)
    real(dp), pointer :: q(:,:,:,:)
  continue
    do iblk = 1, this%nblocks
      q0 => this%block(iblk)%get_q0()
      q  => this%block(iblk)%get_q()
      q0 = q
    end do
  end subroutine update_q0_sub

  !> Reset residual before each time step
  subroutine reset_rhs_sub(this)
  !...Declare input/output variables
    class(ClassFlowField), intent(inout) :: this
  !...Declare local variables
    real(dp), pointer :: rhs(:,:,:,:)
    integer :: iblk
  continue
    do iblk = 1, this%nblocks
      rhs => this%block(iblk)%get_rhs()
      rhs = 0.0_dp
    end do
  end subroutine reset_rhs_sub

end module class_flow_field
