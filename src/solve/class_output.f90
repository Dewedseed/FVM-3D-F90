!=============================================================================80
!
!> Description
!
!=============================================================================80

module class_output

  use kind_parameter,     only : dp, i8, len_short, double
  use container_linklist, only : linklist
  use global_type_defs,    only : TypeDefs

  use global_class, only : terminal, error, mpi
  use global_class, only : string
  use global_class, only : ClassFile

  use interface_cgns_write, only : ClassCGNSFile

  use global_devlog, only : devlog, LogLevel

  implicit none

!...Define scope
  private

  public  :: ClassOutput

!...Declare class
  type :: ClassTimeRecord
    character(len=10),  private :: time_start
    character(len=10),  private :: time_end
    character(len=8),   private :: data_start
    character(len=8),   private :: data_end
    integer,            private :: clock_start
    integer,            private :: clock_end
    integer,            private :: clock_rate
  contains
    procedure, public, pass :: start_time => start_time_fun
    procedure, public, pass :: end_time   => end_time_fun
    procedure, public, pass :: total_time => total_time_fun
  end type ClassTimeRecord

  type :: ClassOutput
    integer,                 public  :: filetype   = 0 !< 0: cgns, 1: tecplot
    integer,                 public  :: idimension = 0
    real(dp),                public  :: dtime      = 0 !< output time
    real(dp),                private :: time(1000) = 0 !< output time
    integer,                 private :: nsoln      = 0 !< index of solution
    integer,                 private :: nvars      = 0 !< number of output variables
    type(linklist),          private :: varlist
    integer,                 private, allocatable :: varindex(:)
    character(len_short),    private, allocatable :: varname(:)
    type(ClassFile),         private :: postfile
    type(ClassFile),         private :: restartfile
    type(ClassFile),         private :: logfile     !< log file
    type(ClassTimeRecord),   private :: record
    type(ClassCGNSFile),     private :: cgnsfile
    logical,                 private, allocatable :: resOut(:) !< map output residual
  contains
    procedure, public,  pass :: input       => input_sub
    procedure, public,  pass :: check_input => check_input_sub
    procedure, public,  pass :: start_time  => start_time_sub
    procedure, public,  pass :: end_time    => end_time_sub
    procedure, public,  pass :: initial     => initial_sub
    procedure, public,  pass :: flowfield   => write_flowfield_sub
    procedure, public,  pass :: finish      => finish_sub
    procedure, public,  pass :: residual    => residual_sub
    procedure, private, pass :: set_data    => set_data_sub
    procedure, private, pass :: get_data    => get_data_fun
    final :: delete_ClassOutput
  end type ClassOutput

  !> Constructor of class ClassOutput
  interface ClassOutput
    module procedure new_ClassOutput
  end interface ClassOutput

contains

  !> Constructor of class ClassOutput
  function new_ClassOutput() result(this)
  !...Declare input/output variables
    type(ClassOutput) :: this
  continue
  end function new_ClassOutput

  !> Destructor of class ClassOutput
  subroutine delete_ClassOutput(this)
  !...Declare input/output variables
    type(ClassOutput), intent(inout) :: this
  continue
    if (allocated(this%varindex)) deallocate(this%varindex)
    if (allocated(this%varname))   deallocate(this%varname)
  end subroutine delete_ClassOutput

  !> Input arguments
  subroutine input_sub(this, vname, vvalue)
  !...Declare input/output variables
    class(ClassOutput), intent(inout) :: this
    character(len=*),   intent(in)    :: vname  !< variable name
    character(len=*),   intent(in)    :: vvalue !< variable value
  !...Declare local variables
    logical :: flag_out
  continue
    if ( TypeDefs%VariableIndex(trim(vname)) == TypeDefs%Variable%NULL ) then
      select case (trim(vname))
        case ("format")
          this%filetype = TypeDefs%FileIndex(vvalue)
        case ("solnfile")
          call this%postfile%new(trim(vvalue))
        case ("dimension")
          this%idimension = TypeDefs%DimensionIndex(vvalue)
        case ("output_time")
          this%dtime = string%to_num(vvalue, double, error%iread)
          if ( error%read("output_time") ) call mpi%stop(error%what())
        case default
          call devlog%print(where="ClassOutput::input_sub", &
                            message="Invalid argument: " // trim(vname), &
                            level=LogLevel%error)
      end select
    else
      flag_out = string%to_logical(vvalue, error%iread)
      if ( flag_out ) then
        this%nvars = this%nvars + 1
        error%ialloc = this%varlist%push_back(TypeDefs%VariableIndex(vname))
        if (error%allocate("Output Variables List")) call mpi%stop(error%what())
      end if
    end if
  end subroutine input_sub

  !> Check input arguments
  subroutine check_input_sub(this)
  !...Declare input/output variables
    class(ClassOutput), intent(inout) :: this
  continue
    call terminal%print_no_wrap("- Check output parameters...")
    if ( this%filetype == TypeDefs%File%NULL ) then
      call mpi%stop("Invalid solution file format.")
    end if
    if ( this%dtime < -1.0E-6 ) then
      call mpi%stop("Invalid output time.")
    end if
    if ( this%nvars == 0 ) then
      call mpi%stop("No output variables.")
    end if
    call terminal%print("Done.")
  end subroutine check_input_sub

  !> Initialize output file
  subroutine initial_sub(this, case, mesh)
    use class_case,      only : ClassCase
    use class_geometry,  only : ClassGeometry
    use defs_block_geom, only : ClassGeomBlock
  !...Declare input/output variables
    class(ClassOutput),  intent(inout) :: this
    type(ClassCase),     intent(in)    :: case
    type(ClassGeometry), intent(inout) :: mesh
  !...Declare local variables
    type(ClassGeomBlock), pointer :: blk
    integer :: iblock, ivar
  continue
    call terminal%print(">> Initialize output file...")
  !...Initialize output file
    call this%postfile%new(trim(case%path) // "/" // trim(case%name) // ".cgns")
    call terminal%print("Solution file:  " // this%postfile%name())
  !...Create cgns file and write mesh
      this%cgnsfile = ClassCGNSFile(this%postfile%name())
    call terminal%print("- Open CGNS file")
      call this%cgnsfile%open(error)
      if (error%occur()) call mpi%stop(error%what())
    call terminal%print("- Create Base Node")
      call this%cgnsfile%add_base(mesh%idimension, mesh%nblocks(), error)
      if (error%occur()) call mpi%stop(error%what())
    call terminal%print("- Create Zone Node")
      do iblock = 1, mesh%nblocks()
        blk => mesh%block(iblock)
        call this%cgnsfile%add_zone(iblock, blk%nvert(), error)
        if (error%occur()) call mpi%stop(error%what())
      end do
    call terminal%print("- Create GridCoordinates Node")
      do iblock = 1, mesh%nblocks()
        blk => mesh%block(iblock)
        call this%cgnsfile%add_coord(iblock, blk%coord(), error)
        if (error%occur()) call mpi%stop(error%what())
      end do
  !...Get number of output variables
    call this%set_data()
    call terminal%print("- Number of variables " // string%from(this%nvars))
      do ivar = 1, this%nvars
        call terminal%print("  " // string%from(ivar) // ": " &
                                 // trim(this%varname(ivar)))
      end do
  !...Initialize restart file
    call this%restartfile%new(trim(case%path) // "/" // trim(case%name) // ".rst")
    call terminal%print("Restart file: " // this%restartfile%name())
  !...Initialize log file
    call this%logfile%new(trim(case%path) // "/" // trim(case%name) // ".log")
    call terminal%print("Log file: " // this%logfile%name())
  !...End
    call terminal%print()
  end subroutine initial_sub

  !> Write flowfield
  subroutine write_flowfield_sub(this, flowfield, timestep)
    use class_flow_field, only : ClassFlowField
  !...Declare input/output variables
    class(ClassOutput),    intent(inout) :: this
    type(ClassFlowField),  intent(in)    :: flowfield
    integer(i8),           intent(in),   optional :: timestep
  !...Declare local variables
    integer               :: iblock
    real(dp), allocatable :: vdata(:,:,:,:)
    real(dp), save        :: time_out = 0.0_dp
    real(dp), parameter   :: time_toler = 1.0E-9_dp !< time tolerance
  continue
    if ( present(timestep) ) then
      if (abs(flowfield%realtime - time_out) > time_toler) return
      call terminal%print()
      call terminal%print("-- Output flowfield (" // &
                          "TimeStep: " // string%from(timestep) // ", " // &
                          "RealTime: " // string%from(flowfield%realtime, '(ES14.8)') // ")")
      call terminal%print()
    else
      call terminal%print(">> Output initial flowfield...")
    end if
  !...Write flowfield to CGNS file
    do iblock = 1, flowfield%nblocks
      vdata = this%get_data(flowfield%block(iblock))
      call this%cgnsfile%add_soln(iblock, this%nsoln, this%nvars, &
                                  this%varname, vdata, error)
    end do
  !...End
    this%nsoln = this%nsoln + 1
    this%time(this%nsoln) = time_out
    time_out = time_out + this%dtime
    call terminal%print()
  end subroutine write_flowfield_sub

  !> Finish output file
  subroutine finish_sub(this)
  !...Declare input/output variables
    class(ClassOutput), intent(inout) :: this
  !...Declare local variables
    integer :: ivar
  continue
  !...Add time interation data
    call this%cgnsfile%add_iter(this%nsoln, this%time(1:this%nsoln), error)
    call this%cgnsfile%add_iter(0, this%nsoln, error)
  !...Close solution file
    call terminal%print(">> Close solution file...")
    call terminal%print("- Name: " // this%postfile%name())
    call this%cgnsfile%close(error)
    if (error%occur()) call mpi%stop(error%what())
    call terminal%print("- Number of variables: " // string%from(this%nvars))
    do ivar = 1, this%nvars
      call terminal%print("     " // string%from(ivar) // ": " // trim(this%varname(ivar)))
    end do
    call terminal%print("- Number of solutions: " // string%from(this%nsoln))
    call terminal%print()
  !...Check restart file
    call terminal%print(">> Check restart file...")
    call terminal%print()
  end subroutine finish_sub

  !> Start time
  subroutine start_time_sub(this)
  !...Declare input/output variables
    class(ClassOutput), intent(inout) :: this
  continue
    call date_and_time(this%record%data_start, this%record%time_start)
    call system_clock(this%record%clock_start, this%record%clock_rate)

    call terminal%print()
    call terminal%print("START TIME: " // this%record%start_time())
    call terminal%print()

    call devlog%print()
    call devlog%print("START SIMULATION: " // this%record%start_time())
    call devlog%print()
  end subroutine start_time_sub

  !> End time
  subroutine end_time_sub(this)
  !...Declare input/output variables
    class(ClassOutput), intent(inout) :: this
  continue
    call date_and_time(this%record%data_end, this%record%time_end)
    call system_clock(this%record%clock_end)

    call terminal%print()
    call terminal%print("START: " // this%record%start_time())
    call terminal%print("END:   " // this%record%end_time())
    call terminal%print()
    call terminal%print("Total simulation time(s): " // &
                         string%from(this%record%total_time(), '(ES14.8)'))
    call terminal%print()

    call devlog%print()
    call devlog%print("END SIMULATION: " // this%record%end_time())
    call devlog%print()
  end subroutine end_time_sub

  !> Start time
  function start_time_fun(this) result(time)
  !...Declare input/output variables
    class(ClassTimeRecord), intent(inout) :: this
    character(len=:),         allocatable :: time
  continue
    time = this%data_start      //  " " // &
           this%time_start(1:2) //  ":" // &
           this%time_start(3:4) //  ":" // &
           this%time_start(5:6)
  end function start_time_fun

  !> End time
  function end_time_fun(this) result(time)
  !...Declare input/output variables
    class(ClassTimeRecord), intent(inout) :: this
    character(len=:),         allocatable :: time
  continue
    time = this%data_end      //  " " // &
           this%time_end(1:2) //  ":" // &
           this%time_end(3:4) //  ":" // &
           this%time_end(5:6)
  end function end_time_fun

  !> Total time
  function total_time_fun(this) result(time)
  !...Declare input/output variables
    class(ClassTimeRecord), intent(inout) :: this
    real(dp),                 allocatable :: time
  continue
    time = dble(this%clock_end - this%clock_start) / dble(this%clock_rate)
  end function total_time_fun

  !> Set output data variables
  subroutine set_data_sub(this)
    use global_type_defs,  only : TypeDefs
  !...Declare input/output variables
    class(ClassOutput), intent(inout) :: this
  !...Declare local variables
    integer :: ivar
    class(*), pointer :: curr__
  continue
  !...Check output variables numbers
    if ( this%nvars /= this%varlist%size() ) then
      call devlog%print(where="ClassOutput::set_data_sub", &
                        message="Inconsistent number of output variables", &
                        level=LogLevel%error)
    end if
  !...Allocate variables
    allocate(this%varindex(this%nvars), stat=error%ialloc)
    if (error%allocate("Output Variable Index")) call mpi%stop(error%what())
    allocate(this%varname(this%nvars), stat=error%ialloc)
    if (error%allocate("Output Variable Names")) call mpi%stop(error%what())
  !...Get data from linklist of variables
    ivar = 0
    do while (this%varlist%cycle())
      curr__ => this%varlist%current()
      select type (curr__)
        type is (integer)
          ivar = ivar + 1
          this%varindex(ivar) = curr__
        class default
          call devlog%print(where="ClassOutput::set_data_sub", &
                            message="Unexpected type of variable index", &
                            level=LogLevel%error)
      end select
    end do
  !...Set output variables
    do ivar = 1, this%nvars
      this%varname(ivar) = TypeDefs%VariableName(this%varindex(ivar))
    end do
  end subroutine set_data_sub

  !> Get output data
  function get_data_fun(this, blk) result(vdata)
    use defs_block_data,  only : ClassBlockData
    use defs_block_geom,  only : ClassGeomBlock
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    class(ClassOutput),   intent(inout) :: this
    type(ClassBlockData), intent(inout) :: blk
    real(dp),             allocatable   :: vdata(:,:,:,:)
  !...Declare local variables
    type(ClassGeomBlock),     pointer :: mesh
    type(Primitive_Variable), pointer :: qp(:,:,:)
    integer :: ivar
  continue
  !...Get mesh and primitive variables
    mesh => blk%get_geom()
    qp   => blk%get_qp()
  !...Allocate data
    allocate(vdata(mesh%nvert(1), mesh%nvert(2), &
             mesh%nvert(3), this%nvars), stat=error%ialloc)
    if (error%allocate("Output Data")) call mpi%stop(error%what())
  !...Set data
    do ivar = 1, this%nvars
      if ( this%varindex(ivar) == TypeDefs%Variable%Density ) then
        vdata(:,:,:,ivar) = qp(:,:,:)%rho
      else if ( this%varindex(ivar) == TypeDefs%Variable%Velocity_X ) then
        vdata(:,:,:,ivar) = qp(:,:,:)%u
      else if ( this%varindex(ivar) == TypeDefs%Variable%Velocity_Y ) then
        vdata(:,:,:,ivar) = qp(:,:,:)%v
      else if ( this%varindex(ivar) == TypeDefs%Variable%Velocity_Z ) then
        vdata(:,:,:,ivar) = qp(:,:,:)%w
      else if ( this%varindex(ivar) == TypeDefs%Variable%StaticPressure ) then
        vdata(:,:,:,ivar) = qp(:,:,:)%press
      end if
    end do
  end function get_data_fun

  !> Print residual
  subroutine residual_sub(this, iter)
    use class_iteration, only : ClassIteration
  !...Declare input/output variables
    class(ClassOutput),   intent(inout) :: this
    type(ClassIteration), intent(in)    :: iter
  !...Declare local variables
    character(len_short) :: res_out
    integer :: i
  continue
    call devlog%print()
    call devlog%print("Print residual step " // string%from(iter%step))
    call devlog%print()
  !...Printe header
    if ( iter%step == 0 ) then
    !...Allocate memory
      allocate(this%resOut(iter%nres), stat=error%ialloc)
      if ( error%allocate("Residuals Map") ) call mpi%stop(error%what())
      this%resOut = .true.
      do i = 1, iter%nres
        if ( this%idimension == 2 ) then
          if ( iter%res_name(i) == "R_momz" ) this%resOut(i) = .false.
        end if
      end do
    !...Open log file
      ! call this%logfile%open(status="replace", action="write", error=error)
      ! if ( error%occur() ) call mpi%stop(error%what())
    !...Print header
      call terminal%print_no_wrap("Iter", '(1x,A10)')
      do i = 1, iter%nres
        if ( this%resOut(i) ) then
          res_out = iter%res_name(i)
          if ( i == 1 ) then
            call terminal%print_no_wrap(res_out, '(1x,A14)')
          else
            call terminal%print_no_wrap(res_out, '(1x,A18)')
          end if
        end if
      end do
      call terminal%print()
    end if
  !...Print residual
    call terminal%print_no_wrap(string%from(iter%step), '(1x,A10)')
    do i = 1, iter%nres
      if ( this%resOut(i) ) then
        res_out = string%from(iter%res_abs(i), '(es18.10)')
        call terminal%print_no_wrap(res_out, '(1x,A18)')
      end if
    end do
    call terminal%print()
  end subroutine residual_sub

end module class_output
