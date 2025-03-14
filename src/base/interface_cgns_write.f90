!=============================================================================80
!
!> Interface module for IO of CGNS Format meshfile.
!
! User Guide:
!   https://cgns.github.io/cgns-modern.github.io/doc/userguide.html
! Library Interface:
!   https://cgns.github.io/cgns-modern.github.io/standard/MLL/CGNS_MLL.html#standardmll
! File Node Library:
!   https://cgns.github.io/cgns-modern.github.io/standard/FMM/nodes.html#
! Type Define:
!   https://cgns.github.io/CGNS_docs_current/midlevel/general.html#typedefs
! CFL3D Implement:
!   https://github.com/nasa/CFL3D/blob/master/source/tools/cgns_to_cfl3dinput.F
!
!=============================================================================80

module interface_cgns_write

  use kind_parameter, only : len_short, len_string, sp, dp, i4, i8

  use global_error, only : ClassError

  use cgns

  implicit none

!...Define scope
  private

  public  :: ClassCGNSFile

!...Declare local variables
  integer, parameter :: len_cgns = 32

!...Declare class
  type :: ClassCGNSZone
    character(len=:),  allocatable, public :: name
    integer(cgsize_t), allocatable, public :: isize(:,:)
    integer,             public :: index_zone = 0
  end type ClassCGNSZone

  type :: ClassCGNSFile
    character(len=:), allocatable, private :: file_name
    integer, private :: index_file = 0
    integer, private :: ier        = 0
    integer, private :: index_base = 0
    integer, private :: icelldim   = 0
    integer, private :: iphysdim   = 0
    integer, private :: nbase_zone = 0
    type(ClassCGNSZone), pointer, private :: zones(:) => null()
  contains
  !...Public procedures
    procedure, public, pass :: open      => open_file_sub
    procedure, public, pass :: close     => close_file_sub
    procedure, public, pass :: add_base  => add_base_sub
    procedure, public, pass :: add_zone  => add_zone_sub
    procedure, public, pass :: add_coord => add_coord_sub
    procedure, public, pass :: add_soln  => add_soln_sub

    procedure, private :: add_base_iter => add_base_iter_sub
    procedure, private :: add_zone_iter => add_zone_iter_sub
    generic :: add_iter => add_base_iter, add_zone_iter
  !...Private procedures
    procedure, private, pass :: occur     => occur_error_fun
    final :: delete_ClassCGNSFile
  end type ClassCGNSFile

  !> Constructor of class ClassCGNSFile
  interface ClassCGNSFile
    module procedure new_ClassCGNSFile
  end interface ClassCGNSFile

contains

  !> Constructor of class ClassCGNSFile
  function new_ClassCGNSFile(file_name) result(this)
    character(len=*), intent(in) :: file_name
    type(ClassCGNSFile) :: this
  continue
    this%file_name = trim(file_name)
  end function new_ClassCGNSFile

  !> Destructor of class ClassCGNSFile
  subroutine delete_ClassCGNSFile(this)
    type(ClassCGNSFile), intent(inout) :: this
  continue
    if ( associated(this%zones) ) deallocate(this%zones)
  end subroutine delete_ClassCGNSFile

!============================== Private Procedures ===========================80
!
! occur_error_fun: if (this%occur(error)) return
!
!=============================================================================80

  !> Get error message
  logical function occur_error_fun(this, error) result(is_error)
    class(ClassCGNSFile), intent(inout) :: this
    type(ClassError),     intent(inout) :: error
    character(len_string) :: error_message = ""
  continue
    if (this%ier .ne. CG_OK) then
      call cg_get_error_f(error_message)
      call error%set(occur=.true., info=error_message)
      return
    end if
  end function occur_error_fun

!============================== Public Procedures ===========================80
!
! (1) Open CGNS file
! (2) add mesh
! (3) add result
!
! File Node Structure:
!
!  Base
!    Zone
!      GridCoordinates
!
!=============================================================================80

  !> Open CGNS file
  subroutine open_file_sub(this, error)
    class(ClassCGNSFile), intent(inout) :: this
    type(ClassError),     intent(inout) :: error
  continue
    call cg_open_f(this%file_name, CG_MODE_WRITE, this%index_file, this%ier)
    if (this%occur(error)) return
  end subroutine open_file_sub

  !> Close CGNS file
  subroutine close_file_sub(this, error)
    class(ClassCGNSFile), intent(inout) :: this
    type(ClassError),     intent(inout) :: error
  continue
    call cg_close_f(this%index_file, this%ier)
    if (this%occur(error)) return
  end subroutine close_file_sub

  !> Add Base Node
  subroutine add_base_sub(this, idimension, nblocks, error)
    class(ClassCGNSFile), intent(inout) :: this
    integer,              intent(in)    :: idimension
    integer,              intent(in)    :: nblocks
    type(ClassError),     intent(inout) :: error
  !...Declare local variables
    character(len_cgns) :: basename
  continue
  !...Create base
    basename = "Base"
    this%icelldim  = idimension
    this%iphysdim  = idimension
    call cg_base_write_f(this%index_file, basename, &
                         this%icelldim,   this%iphysdim, &
                         this%index_base, this%ier)
    if (this%occur(error)) return
  !...Create zones of bases
    this%nbase_zone = nblocks
    allocate(this%zones(nblocks), stat=error%ialloc)
    if ( error%allocate("CGNS Zones") ) return
  end subroutine add_base_sub

  !> Add Zone Node
  subroutine add_zone_sub(this, iblk, iszie, error)
    use interface_stdlib_string, only : ClassStringMethod
  !...Declare input/output variables
    class(ClassCGNSFile), intent(inout) :: this
    integer,              intent(in)    :: iblk
    integer(i8),          intent(in)    :: iszie(:)
    type(ClassError),     intent(inout) :: error
  !...Declare local variables
    type(ClassStringMethod) :: string
    integer                 :: idim
  continue
    this%zones(iblk)%name = "Zone " // string%from(iblk)
    allocate(this%zones(iblk)%isize(this%icelldim,3), stat=error%ialloc)
    if ( error%allocate("CGNS Zone Size") ) return
    do idim = 1, this%icelldim
      ! vertex size
      this%zones(iblk)%isize(idim,1) = iszie(idim)
      ! cell size
      this%zones(iblk)%isize(idim,2) = iszie(idim) - 1
      ! boundary vertex size (always zero for structured grids)
      this%zones(iblk)%isize(idim,3) = 0
    end do
  !...Create zone
    call cg_zone_write_f(this%index_file, &
                         this%index_base, &
                         this%zones(iblk)%name, &
                         this%zones(iblk)%isize, &
                         Structured, &
                         this%zones(iblk)%index_zone, &
                         this%ier)
    if (this%occur(error)) return
  end subroutine add_zone_sub

  !> Add GridCoordinates Node
  subroutine add_coord_sub(this, iblk, coord, error)
    class(ClassCGNSFile), intent(inout) :: this
    integer,              intent(in)    :: iblk
    real(dp),             intent(in)    :: coord(:,:,:,:)
    type(ClassError),     intent(inout) :: error
  !...Declare local variables
    integer           :: idim
    integer           :: index_coord
    character(len=11) :: coordname(3)
  continue
    coordname(1) = 'CoordinateX'
    coordname(2) = 'CoordinateY'
    coordname(3) = 'CoordinateZ'

    if ( this%icelldim == 2 ) then
      do idim = 1, this%icelldim
        call cg_coord_write_f(this%index_file, &
                              this%index_base, &
                              this%zones(iblk)%index_zone, &
                              RealDouble, &
                              coordname(idim), &
                              coord(:,:,1,idim), &
                              index_coord, &
                              this%ier)
        if (this%occur(error)) return
      end do
      ! write
    else if ( this%icelldim == 3 ) then
      do idim = 1, this%icelldim
        call cg_coord_write_f(this%index_file, &
                              this%index_base, &
                              this%zones(iblk)%index_zone, &
                              RealDouble, &
                              coordname(idim), &
                              coord(:,:,:,idim), &
                              index_coord, &
                              this%ier)
        if (this%occur(error)) return
      end do
    end if
  end subroutine add_coord_sub

  !> Add Solution Node
  subroutine add_soln_sub(this, iblk, isoln, nvar, vname, vdata, error)
    use interface_stdlib_string, only : ClassStringMethod
  !...Declare input/output variables
    class(ClassCGNSFile), intent(inout) :: this
    integer,              intent(in)    :: iblk
    integer,              intent(in)    :: isoln
    integer,              intent(in)    :: nvar !< number of variables
    character(len=*),     intent(in)    :: vname(:) !< variable names
    real(dp),             intent(in)    :: vdata(:,:,:,:) !< variable data
    type(ClassError),     intent(inout) :: error
  !...Declare local variables
    character(len_cgns), allocatable :: solnname
    type(ClassStringMethod)          :: string
    integer                          :: index_flow
    integer                          :: index_field
    integer                          :: ivar  !< index of variable
  continue
  !...Create new solution node
    solnname = "FlowSolution" // string%from(isoln)
    call cg_sol_write_f(this%index_file, &
                        this%index_base, &
                        this%zones(iblk)%index_zone, &
                        solnname, &
                        Vertex, &
                        index_flow, &
                        this%ier)
    if (this%occur(error)) return
  !...write flow solution (user must use SIDS-standard names here)
    if ( this%icelldim == 2 ) then ! 2D
      do ivar = 1, nvar
        call cg_field_write_f(this%index_file, &
                              this%index_base, &
                              this%zones(iblk)%index_zone, &
                              index_flow, &
                              RealDouble, &
                              trim(adjustl(vname(ivar))), &
                              vdata(:,:,1,ivar), &
                              index_field, &
                              this%ier)
      end do
    else if ( this%icelldim == 3 ) then ! 3D
      do ivar = 1, nvar
        call cg_field_write_f(this%index_file, &
                              this%index_base, &
                              this%zones(iblk)%index_zone, &
                              index_flow, &
                              RealDouble, &
                              trim(adjustl(vname(ivar))), &
                              vdata(:,:,:,ivar), &
                              index_field, &
                              this%ier)
        if (this%occur(error)) return
      end do
    end if
  end subroutine add_soln_sub

  !> Add BaseIterativeData (Real time)
  !
  !   +-CGNSLibraryVersion
  !   +-Base
  !   | +-TimeIterValues
  !   |   +-TimeValues
  !   +-Zone 1
  !     +-ZoneType
  !     +-GridCoordinates
  !     | +-CoordinateX
  !     | +-CoordinateY
  !     | +-CoordinateY
  !     +-FlowSolution0
  !     +-ZoneIterativeData
  !       +-FlowSolutionPointers
  !
  subroutine add_base_iter_sub(this, nsolns, time, error)
    class(ClassCGNSFile), intent(inout) :: this
    integer,              intent(in)    :: nsolns !< number of solutions
    real(dp),             intent(in)    :: time(:) !< time values
    type(ClassError),     intent(inout) :: error
  !...Declare local variables
    integer :: index_iter !< index of iterative data
    integer :: data_dim !< data dimension
    integer(cgsize_t) :: dim_vec  !< dimension vector (data_dim)
  continue
  !...Add simulation type
    call cg_simulation_type_write_f(this%index_file, &
                                    this%index_base, &
                                    TimeAccurate, &
                                    this%ier)
    if (this%occur(error)) return
  !...Create new BaseIterativeData
    call cg_biter_write_f(this%index_file, &
                          this%index_base, &
                          'TimeIterValues', &
                          nsolns, &
                          this%ier)
    if (this%occur(error)) return
  !...Go to BaseIterativeData level
    index_iter = 1
    call cg_goto_f(this%index_file, &
                   this%index_base, &
                   this%ier,        &
                   'BaseIterativeData_t', &
                   index_iter, &
                   'end')
    if (this%occur(error)) return
  !...Write TimeValues
    data_dim = 1
    dim_vec  = size(time)
    ! https://stackoverflow.com/questions/69686755/fortran-error-type-mismatch-between-actual-argument-at-1-and-actual-argument
    call cg_array_write_f('TimeValues', &
                            RealDouble, &
                            data_dim, &
                            dim_vec, &
                            time, &
                            this%ier)
    if (this%occur(error)) return
  end subroutine add_base_iter_sub

  !> Add ZoneIterativeData (Solution time map)
  subroutine add_zone_iter_sub(this, iblk, nsolns, error)
    use interface_stdlib_string, only : ClassStringMethod
  !...Declare input/output variables
    class(ClassCGNSFile), intent(inout) :: this
    integer,              intent(in)    :: iblk !< block index (0 for all)
    integer,              intent(in)    :: nsolns !< number of solutions
    type(ClassError),     intent(inout) :: error
  !...Declare local variable
    type(ClassStringMethod) :: string
    character(len_cgns)     :: solnname(nsolns)
    integer :: index_iter !< index of iterative data
    integer :: data_dim = 2 !< data dimension
    integer(cgsize_t) :: dim_vec(2)   !< dimension vector (data_dim)
    integer :: ib, bb, be
  continue
    if (iblk == 0) then
      bb = 1
      be = this%nbase_zone
    else
      bb = iblk
      be = iblk
    end if
    do ib = bb, be
    !...Create new ZoneIterativeData
      call cg_ziter_write_f(this%index_file, &
                            this%index_base, &
                            this%zones(ib)%index_zone, &
                            'ZoneIterativeData', &
                            this%ier)
      if (this%occur(error)) return
    !...Go to ZoneIterativeData level
      index_iter = 1
      call cg_goto_f(this%index_file, &
                    this%index_base, &
                    this%ier,        &
                    'Zone_t', &
                    this%zones(ib)%index_zone, &
                    'ZoneIterativeData_t', &
                    index_iter, &
                    'end')
      if (this%occur(error)) return
    !...Add (solution, time) map
      getSolnName : block
        integer :: isoln
        do isoln = 1, nsolns
          solnname(isoln) = "FlowSolution" // string%from(isoln - 1)
        end do
      end block getSolnName
      dim_vec(1) = len_cgns
      dim_vec(2) = nsolns
      call cg_array_write_f('FlowSolutionPointers', &
                            Character, &
                            data_dim, &
                            dim_vec, &
                            solnname, &
                            this%ier)
      if (this%occur(error)) return
    end do
  end subroutine add_zone_iter_sub

end module interface_cgns_write
