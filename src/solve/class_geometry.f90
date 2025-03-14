!=============================================================================80
!
!> Class of geometry information
!
!  Uniform grid
!
!    ^ dir_y
!    |
!    |    6  *******************************
!            *    |    |    |    |    |    *
!         5  *----|----|----|----|----|----*
!            *    |    |    |    |    |    *
!         4  *----|----|----|----|----|----*
!            *    |    |    |    |    |    *
!         3  *----|----|----|----|----|----*
!            *    |    |    |    |    |    *
!         2  *----|----|----|----|----|----*
!            *    |    |    |    |    |    *
!     j   1  *******************************
!            1    2    3    4    5    6    7
!
!            i                           ---> dir_x
!
!  Meshfile
!
!     (1) mesh type: multi-block structural grid
!     (2) file type: Ascii or Binary
!     (3) format:
!                    CGNS   (https://cgns.github.io/)
!                    Plot3D (https://ntrs.nasa.gov/citations/19900013774)
!     (4) feature:   ghost grid
!     (5) generator: OpenFOAM (foamToCGNS), Pointwise, Gmsh
!     (6) Post:      Paraview, Tecplot
!
!
!    ^ dir_y      Block 1         Block 2
!    |
!    |    6  ************************************
!            *    |    |    *    |    |    |    *
!         5  *----|----|----*----|----|----|----*
!            *    |    |    *    |    |    |    *
!         4  *----|----|----*----|----|----|----*
!            *    |    |    *    |    |    |    *
!         3  *----|----|----*----|----|----|----*
!            *    |    |    *    |    |    |    *
!         2  *----|----|----*----|----|----|----*
!            *    |    |    *    |    |    |    *
!     j   1  ************************************
!            1    2    3   4/1*  2*   3*   4*   5*
!
!            i                            ---> dir_x
!
!=============================================================================80

module class_geometry

  use kind_parameter, only : i4, i8, dp, len_short
  use kind_parameter, only : int, int8, double

  use global_type_defs, only : TypeDefs

  use global_class, only : ClassFile
  use global_class, only : terminal, error, mpi
  use global_class, only : string

  use method_gridgen,  only : ClassGridGenerate
  use method_gridfile, only : ClassGridFile

  use defs_block_geom, only : ClassGeomBlock, ClassGeomData, NDIR, NCOORD

  use global_devlog, only : devlog, LogLevel

  implicit none

!...Define scope
  private

  public :: ClassGeometry

!...Declare local struct
  type :: ClassGeomBlockList
    type(ClassGeomBlock),     pointer, private :: blk  => null()
    type(ClassGeomBlockList), pointer, private :: next => null()
  contains
    final :: delete_ClassGeomBlockList
  end type ClassGeomBlockList

!...Declare class
  type :: ClassGeometry
  !...Input variables
    character(len_short),     public :: type    = "gridfile"
      !< gridfile or uniform
    integer(i4),              public :: idimension = 0
    type(ClassGridGenerate),  private :: gridgen
    ! type(ClassGridFile),      private :: gridfile
    type(ClassFile),          private :: gridfile
    character(len_short),     private :: gridtype = "nodal-centered"
    logical,                  private :: writeGrid = .false.
  !...Internal variables
    type(ClassGeomData),      pointer, private :: block_data(:)
                          !< data pool of geometry,
                          !! this is static data,
                          !! not change by partition
    type(ClassGeomBlockList), pointer, private :: block_list => null()
                          !< dynamic block list
                          !! block can be cut and merged
  contains
  !...Public procedures
    procedure, public, pass :: input          => input_sub
    procedure, public, pass :: check_input    => check_input_sub
    ! procedure, public, pass :: gird_read      => read_gird_sub
    procedure, public, pass :: grid_generate  => generate_grid_sub
    procedure, public, pass :: grid_partition => grid_partition_sub
    procedure, public, pass :: grid_write     => write_grid_sub
    procedure, public, pass :: nblocks        => number_blocks_fun
    procedure, public, pass :: get_blocks     => get_blocks_sub
    procedure, public, pass :: block          => get_block_fun
  !...Private procedures
    procedure, private, pass :: add_block => add_block_sub
    final :: delete_ClassGeometry
  end type ClassGeometry

  !> Constructor of class ClassGeometry
  interface ClassGeometry
    module procedure new_ClassGeometry
  end interface ClassGeometry

contains

  !> Destructor of class ClassGeomBlockList
  subroutine delete_ClassGeomBlockList(this)
    type(ClassGeomBlockList), intent(inout) :: this
  continue
    if (associated(this%blk))  deallocate(this%blk)
    if (associated(this%next)) deallocate(this%next)
  end subroutine delete_ClassGeomBlockList

  !> Constructor of class ClassGeometry
  type(ClassGeometry) function new_ClassGeometry() result(this)
  continue
  end function new_ClassGeometry

  !> Destructor of class ClassGeometry
  subroutine delete_ClassGeometry(this)
    type(ClassGeometry), intent(inout) :: this
  continue
    if (associated(this%block_data)) deallocate(this%block_data)
    if (associated(this%block_list)) deallocate(this%block_list)
  end subroutine delete_ClassGeometry

  !> Input geometry information
  subroutine input_sub(this, vname, vvalue)
  !...Declare input/output variables
    class(ClassGeometry), intent(inout) :: this
    character(len=*),     intent(in)    :: vname  !< variable name
    character(len=*),     intent(in)    :: vvalue !< variable value
  continue
    select case (trim(vname))
      case ("dimension")
        this%idimension = TypeDefs%DimensionIndex(trim(vvalue))
      case ("type")
        this%type = trim(vvalue)
      case ("gridfile")
        call this%gridfile%new(trim(vvalue))
      case ("geoType")
        call this%gridgen%input(geoType=trim(vvalue))
      case ("gridType")
        call this%gridgen%input(gridType=trim(vvalue))
      case ("geoSize")
        call this%gridgen%input(geoSize=string%to_num(string=trim(vvalue), &
                                        mold=double, size=3, stat=error%iread))
        if ( error%read("geoSize") ) call mpi%stop(error%what())
      case ("gridSize")
        call this%gridgen%input(gridSize=string%to_num(string=trim(vvalue), &
                                        mold=int8, size=3, stat=error%iread))
        if ( error%read("gridSize") ) call mpi%stop(error%what())
      case ("writeGrid")
        this%writeGrid = string%to_logical(vvalue, error%iread)
        if ( error%read("writeGrid") ) call mpi%stop(error%what())
      case default
        call devlog%print(where="ClassGeometry::input_sub", &
                          message="Unknown variable name: "//trim(vname), &
                          level=LogLevel%error)
    end select
  end subroutine input_sub

  !> Check input information
  subroutine check_input_sub(this)
  !...Declare input/output variables
    class(ClassGeometry), intent(inout) :: this
  continue
    call terminal%print_no_wrap("- Check geometry parameters...")
    select case (trim(this%type))
      case ("gridfile")
        if (.not. this%gridfile%is_present()) then
          call mpi%stop("&Geometry -- gridfile not present.")
        end if
      case ("uniform")
        call this%gridgen%check_input(error)
        if ( error%occur() ) call mpi%stop(error%what())
      case default
        call mpi%stop("&Geometry -- error geometry type: " // trim(this%type))
    end select
    call terminal%print("Done.")
  end subroutine check_input_sub

  !> Add block to block list
  subroutine add_block_sub(this, blk)
  !...Declare input/output variables
    class(ClassGeometry), intent(inout) :: this
    type(ClassGeomBlock), pointer       :: blk
  !...Declare local variables
    type(ClassGeomBlockList), pointer :: new_node
  continue
    allocate(new_node, stat=error%ialloc)
    if (error%allocate("Grid Block List Node")) call mpi%stop(error%what())
  !...add new node to block list
    new_node%blk    => blk
    new_node%next   => this%block_list
    this%block_list => new_node
    nullify(new_node)
  end subroutine add_block_sub

  !> Get number of blocks
  integer function number_blocks_fun(this) result(nblocks)
  !...Declare input/output variables
    class(ClassGeometry), intent(in) :: this
  !...Declare local variables
    type(ClassGeomBlockList), pointer :: curr
  continue
    nblocks = 0
    curr => this%block_list
    do while (associated(curr))
      nblocks = nblocks + 1
      curr => curr%next
    end do
  end function number_blocks_fun

  !> Get blocks (read only)
  subroutine get_blocks_sub(this, blks)
  !...Declare input/output variables
    class(ClassGeometry), intent(inout) :: this
    type(ClassGeomBlock), intent(inout), allocatable :: blks(:)
  !...Declare local variables
    type(ClassGeomBlockList), pointer :: curr
    integer :: iblk
  continue
    if ( allocated(blks) ) then
      if ( this%nblocks() /= size(blks) ) then
        call mpi%stop("&Geometry -- number of blocks not match.")
      end if
    else
      allocate(blks(this%nblocks()), stat=error%ialloc)
      if ( error%allocate("Geometry Blocks") ) call mpi%stop(error%what())
    end if
    curr => this%block_list
    iblk = this%nblocks()
    do while (associated(curr))
      blks(iblk) = curr%blk
      iblk = iblk - 1
      curr => curr%next
    end do
  end subroutine get_blocks_sub

  !> Get block
  function get_block_fun(this, iblk) result(blk)
  !...Declare input/output variables
    class(ClassGeometry), intent(inout) :: this
    integer,              intent(in)    :: iblk
    type(ClassGeomBlock), pointer       :: blk
  !...Declare local variables
    type(ClassGeomBlockList), pointer :: curr
    integer :: cblk !< count blk
  continue
    curr => this%block_list
    cblk = 0
    do while (associated(curr))
      cblk = cblk + 1
      if ( cblk == iblk ) then
        blk => curr%blk
        return
      end if
      curr => curr%next
    end do
    call mpi%stop("&Geometry -- block not found.")
  end function get_block_fun

  !> Read grid file
  ! subroutine read_gird_sub(this)
  !   use global_class,    only : ClassCGNSMethod
  ! !...Declare input/output variables
  !   class(ClassGeometry), intent(inout) :: this
  ! !...Declare local variables
  !   type(ClassCGNSMethod)    :: cgns
  !   integer(i8), allocatable :: isize(:,:)
  !   integer(i4)              :: iblock
  ! continue
  !   call terminal%print(">> Read grid file...")
  !   call terminal%print("Grid file: " // this%gridfile%name())
  !   call terminal%print()
  ! !...Read grid file
  !   call cgns%read_mesh(this%gridfile%name(), error)
  !   if ( error%occur() ) call mpi%stop(error%what())
  !   call terminal%print(cgns%info())
  ! !...Check file dimension
  !   if ( this%idimension /= cgns%idimension() ) then
  !     call mpi%stop("&Geometry -- Dimension(" // &
  !                       string%from(this%idimension) // &
  !                      ") does not match with CGNS file(" // &
  !                       string%from(cgns%idimension()) // ").")
  !   end if
  ! !...Get grid information
  !   call cgns%size(this%nblocks, isize, error)
  !   if ( error%occur() ) call mpi%stop(error%what())
  !   allocate(this%blocks(this%nblocks), stat=error%ialloc)
  !   if ( error%allocate("Grid Blocks") ) call mpi%stop(error%what())
  !   do iblock = 1, this%nblocks
  !     this%blocks(iblock)%index = iblock
  !     this%blocks(iblock)%nvert(1:this%idimension) = &
  !                         isize(1:this%idimension, iblock)
  !     allocate(this%blocks(iblock)%coord( &
  !               this%blocks(iblock)%nvert(1), &
  !               this%blocks(iblock)%nvert(2), &
  !               this%blocks(iblock)%nvert(3), &
  !               NCOORD), stat=error%ialloc)
  !     if (error%allocate("Grid Blocks Coordinates")) call mpi%stop(error%what())
  !     this%blocks(iblock)%coord = 0
  !     this%blocks(iblock)%nbund = isize(4, iblock)
  !     allocate(this%blocks(iblock)%bund( &
  !               this%blocks(iblock)%nbund), stat=error%ialloc)
  !     if (error%allocate("Grid Blocks Boundaries")) call mpi%stop(error%what())
  !     this%blocks(iblock)%nconn = isize(5, iblock)
  !     allocate(this%blocks(iblock)%conn( &
  !               this%blocks(iblock)%nconn), stat=error%ialloc)
  !     if (error%allocate("Grid Blocks Connections")) call mpi%stop(error%what())
  !   end do
  ! !...Get grid coordinates
  !   do iblock = 1, this%nblocks
  !     this%blocks(iblock)%coord(:,:,:,:) = cgns%grid(iblock, error)
  !     if ( error%occur() ) call mpi%stop(error%what())
  !     if ( this%idimension == TypeDefs%Dimension%DIM2 ) then !< 2D
  !       this%blocks(iblock)%coord(:,:,2,:) = &
  !           this%blocks(iblock)%coord(:,:,1,:)
  !     end if
  !   end do
  ! !...Get boundary condition
  !   ! TODO
  ! !...End read grid file
  !   deallocate(isize)
  !   call terminal%print()
  ! end subroutine read_gird_sub

  !> Generate grid
  subroutine generate_grid_sub(this)
    class(ClassGeometry), intent(inout) :: this
  !...Declare local variables
    integer(i8) :: p1(3), p2(3)
    integer     :: dir
    integer     :: nblk
    type(ClassGeomBlock), pointer :: blk
    type(ClassGeomData),  pointer :: blk_data
    integer(i8) :: nvert(NDIR)
  continue
    call terminal%print(">> Generate " // trim(this%type) // " grid...")
  ! TODO: Only support one block yet
    nblk = 1
  !...Set grid blocks
    allocate(this%block_data(nblk), stat=error%ialloc)
    if ( error%allocate("Grid Data") ) call mpi%stop(error%what())
  !...Generate grid and save to static block
    if ( trim(this%type) == "uniform")  then
      this%block_data(1) = ClassGeomData()
      this%block_data(1)%range_min = 1
      call this%gridgen%uniform(this%idimension, &
                                this%block_data(1)%range_max, &
                                this%block_data(1)%coord, &
                                this%block_data(1)%cell_size, &
                                error)
      if ( error%occur() ) call mpi%stop(error%what())

    end if
  !...Create boundary
    ! initial 6 boundary, no connectivity
    allocate(this%block_data(1)%surface(6), stat=error%ialloc)
    if ( error%allocate("Grid Data") ) call mpi%stop(error%what())
    do dir = 1, NDIR
    !...Negative direction face
      p1 = this%block_data(1)%range_min
      p2 = this%block_data(1)%range_max
      p2(dir) = this%block_data(1)%range_min(dir)
      call this%block_data(1)%surface(dir)%set(p1, p2,                       &
                                              iblk=this%block_data(1)%index, &
                                              name="bundary",                &
                                              type=TypeDefs%BC%Boundary)

    !...Positive direction face
      p1 = this%block_data(1)%range_min
      p2 = this%block_data(1)%range_max
      p1(dir) = this%block_data(1)%range_max(dir)
      call this%block_data(1)%surface(dir+NDIR)%set(p1, p2,                  &
                                              iblk=this%block_data(1)%index, &
                                              name="bundary",                &
                                              type=TypeDefs%BC%Boundary)
    end do
  !...Create dynamic block and add to block list
    allocate(blk, stat=error%ialloc)
    if (error%allocate("Grid Block")) return
    blk_data => this%block_data(1)
    call blk%copy_from(blk_data, error%ialloc)
    if (error%allocate("Grid Block")) return
    call blk%set(id=1)
    call this%add_block(blk)
    nullify(blk)

  !...Print information
    call terminal%print("Number of blocks: " // string%from(this%nblocks()))
    blk => this%block(1)
    nvert = blk%nvert()
    call terminal%print("Grid size: " // string%from(nvert(1)) // "  " &
                                      // string%from(nvert(2)) // "  " &
                                      // string%from(nvert(3)))
    call terminal%print()
  end subroutine generate_grid_sub

  !> Partition multi-block grid for parallel computing
  subroutine grid_partition_sub(this)
    use method_partition
  !...Declare input/output variables
    class(ClassGeometry), intent(inout) :: this
  !...Declare local variables
    integer              :: iblock
    type(ClassPartition) :: partition
    type(ClassGeomBlockList), pointer :: curr
    type(ClassGeomBlock), pointer :: blk
    type(ClassGeomBlock), allocatable :: blks(:)
  continue
    call terminal%print(">> Partition grid for parallel computing...")
  !...Initial partition
    call terminal%print("- Initial partition")
    partition = ClassPartition(nprocessor=mpi%nprocess(), method=IF)
    call partition%set(data_size=real(dp*10, dp))
    ! Add blocks
    curr => this%block_list
    do while (associated(curr))
      call partition%add_block(curr%blk, error)
      if ( error%occur() ) call mpi%stop(error%what())
      curr => curr%next
    end do
    call terminal%print(partition%print("initial"))
    call terminal%print()

  !...Partition multi-block grid
    call terminal%print("- Execute partition")
    call partition%execute(error)
    if ( error%occur() ) call mpi%stop(error%what())
    call terminal%print(partition%print("final"))
    call terminal%print()

  !...Return partition blocks and maps
    call terminal%print("- Return partition blocks")
    call partition%return(blks, error)
    nullify(this%block_list)
    do iblock = 1, size(blks)
      allocate(blk, stat=error%ialloc)
      if ( error%allocate("Grid Block List Node") ) call mpi%stop(error%what())
      blk = blks(iblock)
      call this%add_block(blk)
      nullify(blk)
    end do
    ! Check return blocks information
    call terminal%print("  Number of blocks: " // string%from(this%nblocks()))
    call terminal%print()

  !...Write partition grid
    if ( this%writeGrid ) call this%grid_write()

  !...End grid partition
    call terminal%print()
  end subroutine grid_partition_sub

  !> Write grid
  subroutine write_grid_sub(this)
    use interface_cgns_write, only : ClassCGNSFile
  !...Declare input/output variables
    class(ClassGeometry), intent(inout) :: this
  !...Declare local variables
    type(ClassCGNSFile) :: cgnsFile
    integer             :: iblock
    type(ClassGeomBlock), allocatable :: blks(:)
  continue
    call terminal%print()
    call terminal%print(">> Write grid after parition...")
  !...Get ouput blocks
    call this%get_blocks(blks)
  !...Open write grid file
    cgnsFile = ClassCGNSFile("cutgrid.cgns")
    call terminal%print("- Open CGNS file")
      call cgnsFile%open(error)
      if ( error%occur() ) call mpi%stop(error%what())
  !...Write grid
    call terminal%print("- Create Base Node")
      call cgnsFile%add_base(this%idimension, this%nblocks(), error)
      if ( error%occur() ) call mpi%stop(error%what())
    call terminal%print("- Create Zone Node")
      do iblock = 1, this%nblocks()
        call cgnsFile%add_zone(iblock, blks(iblock)%nvert(), error)
        if ( error%occur() ) call mpi%stop(error%what())
      end do
    call terminal%print("- Create GridCoordinates Node")
      do iblock = 1, this%nblocks()
        call cgnsFile%add_coord(iblock, blks(iblock)%coord(), error)
        if ( error%occur() ) call mpi%stop(error%what())
      end do
  !...Close write grid file
    call terminal%print("- Close CGNS file")
      call cgnsFile%close(error)
      if ( error%occur() ) call mpi%stop(error%what())
    call terminal%print()
  end subroutine write_grid_sub

end module class_geometry
