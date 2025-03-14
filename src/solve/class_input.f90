!=============================================================================80
!
!> Get input information: argument, file...
!
!=============================================================================80

module class_input

  use kind_parameter, only : len_short

  use global_class, only : ClassFile
  use global_class, only : error, mpi, terminal

  implicit none

!...Define scope
  private

  public  :: ClassInput

!...Declare local variables
  type :: ClassInput
    type(ClassFile),       private :: casefile
  contains
    procedure, public, pass   :: parse_arguments => parse_arguments_sub
    procedure, public, pass   :: read_casefile   => read_casefile_sub
    procedure, public, nopass :: check_casefile  => check_casefile_sub
    final :: delete_ClassInput  !< Destructor
  end type ClassInput

  !> Constructor of class ClassInput
  interface ClassInput
    module procedure new_ClassInput
  end interface

contains

  !> Constructor of class ClassInput
  function new_ClassInput() result(this)
  !...Declare input/output variables
    type(ClassInput) :: this
  continue
    call this%casefile%new("")
  end function new_ClassInput

  !> Destructor of class ClassInput
  subroutine delete_ClassInput(this)
  !...Deallocating pointer attribute
    type(ClassInput), intent(inout) :: this
  continue
    if ( this%casefile%is_open() ) call this%casefile%close()
  end subroutine delete_ClassInput

  !> Get and parse argument command
  subroutine parse_arguments_sub(this, solver, devlog)
  !...Use module
    use global_class,  only : ClassArgParse
    use class_solver,  only : ClassSolver
    use global_devlog, only : ClassDevLog
  !...Declare input/output variables
    class(ClassInput), intent(inout) :: this
    type(ClassSolver), intent(inout) :: solver
    type(ClassDevLog), intent(inout) :: devlog
  !...Declare local variables
    type(ClassArgParse) :: parser
  continue
    call terminal%print(">> Getting command arguments...")
  !...Initialize Argument Parser
    parser = ClassArgParse()
  !...Get command arguments
    error%ialloc = parser%get_arguments()
    if ( error%allocate("command arguments") ) call mpi%stop(error%what())
    if ( parser%nargs() == 0 ) call mpi%stop("No command arguments.")
    call terminal%print(parser%print())
  !...Parse command arguments
    call this%casefile%new(parser%parse_args("--input"))
    call solver%case%input("path", this%casefile%get_path())
  !...Check command arguments
    error = parser%check()
    if ( error%occur() ) call mpi%stop(error%what())
  !...Set devlog
    devlog = ClassDevLog(open=mpi%is_host(), path=this%casefile%get_path())
  !...End get arguments
    call terminal%print()
    call terminal%flush()
  end subroutine parse_arguments_sub

  !> Read casefile
  subroutine read_casefile_sub(this, solver, ouput)
    use global_class,     only : ClassNamelist
    use class_solver,     only : ClassSolver
    use class_output,     only : ClassOutput
  !...Declare input/output variables
    class(ClassInput), intent(inout) :: this
    type(ClassSolver), intent(inout) :: solver
    type(ClassOutput), intent(inout) :: ouput
  !...Declare local variables
    type(ClassNamelist)  :: nml
    integer              :: ivar
    character(len_short) :: vars(6)
  continue
    call terminal%print(">> Read case input file...")
    call terminal%print("Case File: " // this%casefile%name())
    call terminal%print()
  !...Open casefile
    call this%casefile%open(status='old', action='read', error=error)
    if (error%occur()) call mpi%stop(error%what())
  !...Read namelist
    nml = ClassNamelist("General")
    call nml%read(this%casefile, error)
    if ( error%occur() ) call mpi%stop(error%what())
    call terminal%print(nml%info())
    call solver%case%input("name",          nml%get("name"))
    call solver%case%input(    "dimension", nml%get("dimension"))
    call solver%geometry%input("dimension", nml%get("dimension"))
    call ouput%input(          "dimension", nml%get("dimension"))
    call solver%case%input("dimensionless", nml%get("dimensionless"))
    call solver%flowfield%input("equation", nml%get("equation"))
    call solver%flowfield%input("gasModel", nml%get("gasModel"))
    call nml%check(error)
    if ( error%occur() ) call mpi%stop(error%what())

    nml = ClassNamelist("Geometry")
    call nml%read(this%casefile, error)
    if ( error%occur() ) call mpi%stop(error%what())
    call terminal%print(nml%info())
    call solver%geometry%input("type",      nml%get("type"))
    call solver%geometry%input("gridfile",  nml%get("gridfile"))
    call solver%geometry%input("geoType",   nml%get("geoType"))
    call solver%geometry%input("gridType",  nml%get("gridType"))
    call solver%geometry%input("geoSize",   nml%get("geoSize"))
    call solver%geometry%input("gridSize",  nml%get("gridSize"))
    call solver%geometry%input("writeGrid", nml%get("writeGrid"))
    call nml%check(error)
    if ( error%occur() ) call mpi%stop(error%what())

    nml = ClassNamelist("Inviscid_Flux")
    call nml%read(this%casefile, error)
    if ( error%occur() ) call mpi%stop(error%what())
    call terminal%print(nml%info())
    call solver%spaceDiscr%input("reconstruct",   nml%get("reconstruct"))
    call solver%spaceDiscr%input("kappa",         nml%get("kappa"))
    call solver%spaceDiscr%input("limiter",       nml%get("limiter"))
    call solver%spaceDiscr%input("limiter_coeff", nml%get("limiter_coeff"))
    call solver%spaceDiscr%input("flux_split",    nml%get("flux_split"))
    call solver%spaceDiscr%input("entropy_fix",   nml%get("entropy_fix"))
    call nml%check(error)
    if ( error%occur() ) call mpi%stop(error%what())

    nml = ClassNamelist("Time_Stepping")
    call nml%read(this%casefile, error)
    if ( error%occur() ) call mpi%stop(error%what())
    call terminal%print(nml%info())
    call solver%timeDiscr%input("time_scheme",  nml%get("time_scheme"))
    call solver%timeDiscr%input("timestep_set", nml%get("timestep_set"))
    call solver%timeDiscr%input("fix_timestep", nml%get("fix_timestep"))
    call solver%iteration%input("total_time",   nml%get("total_time"))
    call ouput%input(           "output_time",  nml%get("output_time"))
    call solver%iteration%input("absolute_residual", nml%get("absolute_residual"))
    call solver%iteration%input("relative_residual", nml%get("relative_residual"))
    call nml%check(error)
    if ( error%occur() ) call mpi%stop(error%what())

    nml = ClassNamelist("Output")
    call nml%read(this%casefile, error)
    if ( error%occur() ) call mpi%stop(error%what())
    call terminal%print(nml%info())
    call ouput%input("format",      nml%get("format"))
    call ouput%input("solnfile",    nml%get("solnfile"))
    call ouput%input("Density",     nml%get("Density"))
    call ouput%input("VelocityX",   nml%get("VelocityX"))
    call ouput%input("VelocityY",   nml%get("VelocityY"))
    call ouput%input("VelocityZ",   nml%get("VelocityZ"))
    call ouput%input("Pressure",    nml%get("Pressure"))
    call ouput%input("Temperature", nml%get("Temperature"))
    call nml%check(error)
    if ( error%occur() ) call mpi%stop(error%what())

    !...Close casefile
    call this%casefile%close()
    call terminal%print()

  end subroutine read_casefile_sub

  !> Check casefile choice
  subroutine check_casefile_sub(solver, output)
    use class_solver, only : ClassSolver
    use class_output, only : ClassOutput
  !...Declare input/output variables
    type(ClassSolver), intent(inout) :: solver
    type(ClassOutput), intent(inout) :: output
  !...Declare local variables
  continue
    call terminal%print(">> Check casefile input parameters...")
    call solver%case%check_input()
    call solver%geometry%check_input()
    call solver%flowfield%check_input()
    call solver%spaceDiscr%check_input()
    call solver%timeDiscr%check_input()
    call solver%iteration%check_input()
    call output%check_input()
    call terminal%print()
  end subroutine check_casefile_sub

end module class_input
