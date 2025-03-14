!=============================================================================80
!
!> Define block geometry/mesh information.
!
!  coordinate: xyz
!  vector:     xyz
!  direction:  ijk
!
! Attention: NOT save flow feild data in these class
!
!=============================================================================80

module defs_block_geom

  use kind_parameter,   only : i2, i4, i8, dp, len_short
  use global_type_defs, only : TypeDefs
  use global_uuid,      only : UUID

  use global_devlog,  only : devlog, LogLevel
  use defs_face_geom, only : ClassGeomFace, ClassGeomPlane
  use interface_stdlib_string, only : ClassStringMethod

  implicit none

!...Define scope
  private

  public :: ClassGeomData
  public :: ClassGeomBlock
  public :: ClassGeomSurfaceList

  public :: NDIR, NCOORD

!...Declare parameters
  integer(i4), parameter :: NDIR   = 3 !< i, j, k
  integer(i4), parameter :: NCOORD = 3 !< x, y, z

  integer(i8), parameter :: ORIGIN_DIR(3) = 1 !< block origin (i,j,k)

!...Define Class
  !> geometry element: block surface
  type :: ClassGeomSurfaceList
    type(ClassGeomFace),        public, pointer :: surface   => null()
    type(ClassGeomBlock),       public, pointer :: donor_blk => null()
    type(ClassGeomSurfaceList), public, pointer :: next => null()
  contains
    final :: delete_ClassGeomSurfaceList
  end type ClassGeomSurfaceList

  !> Static data of block
  type :: ClassGeomData
    integer,              public :: index = 0
    character(len_short), public :: name = "blk"
    integer(i8),          public :: range_min(NDIR) = 1
                                  !< min of ijk direction
    integer(i8),          public :: range_max(NDIR) = 1
                                  !< max of ijk direction
    real(dp),             public :: cell_size(3) = 0.0_dp
    real(dp),             public, pointer     :: coord(:,:,:,:) => null()
    type(ClassGeomFace),  public, allocatable :: surface(:)
  contains
    final :: delete_ClassGeomData
  end type ClassGeomData
  !> Constructor of ClassGeomData
  interface ClassGeomData
    module procedure new_ClassGeomData
  end interface ClassGeomData

  !> basic mesh element: block
  type :: ClassGeomBlock
    integer,              private :: index = 0
    character(len_short), private :: name = "blk"
    integer(i8),          private :: range_min(NDIR) = 1
                                     !< min of ijk direction
    integer(i8),          private :: range_max(NDIR) = 1
                                     !< max of ijk direction
    type(ClassGeomData),        pointer, private :: data_pool    => null()
    type(ClassGeomSurfaceList), pointer, private :: surface_list => null()
  contains
  !...Block properties
    procedure, pass, public :: id    => block_id_fun
    procedure, pass, public :: size  => block_size_fun
    procedure, pass, public :: coord => block_coord_fun
    procedure, pass, public :: x     => block_coord_x_fun
    procedure, pass, public :: y     => block_coord_y_fun
    procedure, pass, public :: z     => block_coord_z_fun

    procedure, pass, public :: dr    => block_dr_fun

    procedure, pass, private :: block_nvert     => block_nvert_fun
    procedure, pass, private :: block_nvert_ijk => block_nvert_ijk_fun
    generic, public :: nvert => block_nvert, block_nvert_ijk

    procedure, pass, private :: block_ncell     => block_ncell_fun
    procedure, pass, private :: block_ncell_ijk => block_ncell_ijk_fun
    generic, public :: ncell => block_ncell, block_ncell_ijk

  !...Block Method
    procedure, pass, public :: set => block_set_para_sub

    procedure, pass, private :: copy_from_block => block_copy_from_block_sub
    procedure, pass, private :: copy_from_data  => block_copy_from_data_sub
    generic, public :: copy_from => copy_from_block, copy_from_data

    procedure, pass, public :: copy_to   => block_copy_to_sub

    procedure, pass, public :: cut_position   => block_cut_position_sub
    procedure, pass, public :: split_by_plane => block_split_by_plane_sub

    procedure, pass, private :: cut_by_plane => block_cut_by_plane_sub
    procedure, pass, private :: cut_uniform  => block_cut_uniform_sub
    generic, public :: cut => cut_by_plane, cut_uniform

    procedure, nopass, private :: cut_off_by_plane => block_cut_off_by_plane_sub
    procedure, nopass, private :: cut_off_uniform  => block_cut_off_uniform_sub
    generic, public :: cut_off => cut_off_by_plane, cut_off_uniform

    procedure, pass, public :: reconn_donor_blk_conn => &
                                  block_reconn_donor_blk_conn_sub
    procedure, pass, public :: deconn_donor_blk_conn => &
                                  block_deconn_donor_blk_conn_sub

  !...Surface properties
    procedure, pass, public :: nbund => block_nbund_fun
    procedure, pass, public :: nconn => block_nconn_fun

    procedure, pass, public :: get_bund => block_get_bund_sub
    procedure, pass, public :: get_conn => block_get_conn_sub

    procedure, pass, public :: bund => block_bund_index_fun
    procedure, pass, public :: conn => block_conn_index_fun

    procedure, pass, public :: conn_blk_id => block_conn_block_id_fun

  !...Surface Method
    procedure, pass, public :: add_surf => block_add_surface_sub
    procedure, pass, public :: add_conn => block_add_conn_sub

  !...Nopass Method
    procedure, nopass, public :: connect => block_connect_block_sub

  !...Destructor
    final :: delete_ClassGeomBlock
  end type ClassGeomBlock
  !> Constructor of class ClassGeomBlock
  interface ClassGeomBlock
    module procedure new_ClassGeomBlock
  end interface ClassGeomBlock

contains

!=============================================================================80
!
! Constructors and Destructors
!
!=============================================================================80

  !> Constructor of class ClassGeomData
  function new_ClassGeomData() result(this)
    type(ClassGeomData) :: this
    type(ClassStringMethod) :: string
  continue
    this%index = UUID%blk()
    this%name  = "blk-" // string%from(this%index)
  end function new_ClassGeomData

  !> Constructor of class ClassGeomBlock
  function new_ClassGeomBlock() result(this)
    type(ClassGeomBlock) :: this
    type(ClassStringMethod) :: string
  continue
    this%index = UUID%blk()
    this%name  = "blk-" // string%from(this%index)
  end function new_ClassGeomBlock

  !> Destructor of class ClassGeomBlockSurface
  subroutine delete_ClassGeomSurfaceList(this)
    type(ClassGeomSurfaceList), intent(inout) :: this
  continue
    if (associated(this%surface))   deallocate(this%surface)
    if (associated(this%donor_blk)) nullify(this%donor_blk)
    if (associated(this%next))      deallocate(this%next)
  end subroutine delete_ClassGeomSurfaceList

  !> Destructor of class ClassGeomBlockCoord
  subroutine delete_ClassGeomData(this)
    type(ClassGeomData), intent(inout) :: this
  continue
    if (associated(this%coord))  deallocate(this%coord)
    if (allocated(this%surface)) deallocate(this%surface)
  end subroutine delete_ClassGeomData

  !> Destructor of class ClassBlockMesh
  subroutine delete_ClassGeomBlock(this)
    type(ClassGeomBlock), intent(inout) :: this
  continue
    if (associated(this%data_pool))     nullify(this%data_pool)
    if (associated(this%surface_list))  deallocate(this%surface_list)
  end subroutine delete_ClassGeomBlock

!=============================================================================80
!
! block properties
!
!=============================================================================80

  !> GET number of vertices
  function block_nvert_fun(this) result(nvert)
    class(ClassGeomBlock), intent(inout) :: this
    integer(i8) :: nvert(NDIR)
  continue
    nvert = this%range_max - this%range_min + 1
  end function block_nvert_fun

  !> GET number of vertices
  integer(i8) function block_nvert_ijk_fun(this, dir) result(nvert)
    class(ClassGeomBlock), intent(inout) :: this
    integer,               intent(in)    :: dir
  continue
    nvert = this%range_max(dir) - this%range_min(dir) + 1
  end function block_nvert_ijk_fun

  !> GET number of cells
  function block_ncell_fun(this) result(ncell)
    class(ClassGeomBlock), intent(inout) :: this
    integer(i8) :: ncell(NDIR)
  continue
    ncell = this%range_max - this%range_min
  end function block_ncell_fun

  !> GET number of cells
  integer(i8) function block_ncell_ijk_fun(this, dir) result(ncell)
    class(ClassGeomBlock), intent(inout) :: this
    integer,               intent(in)    :: dir
  continue
    ncell = this%range_max(dir) - this%range_min(dir)
  end function block_ncell_ijk_fun

  !> Get index of block
  integer function block_id_fun(this) result(id)
    class(ClassGeomBlock), intent(inout) :: this
  continue
    id = this%index
  end function block_id_fun

  !> Get number of nodes
  integer(i8) function block_size_fun(this) result(nnode)
    class(ClassGeomBlock), intent(inout) :: this
  continue
    nnode = product(this%nvert())
  end function block_size_fun

  !> Get coordinate of block
  function block_coord_fun(this) result(coord)
    class(ClassGeomBlock), intent(inout) :: this
    real(dp), pointer :: coord(:,:,:,:)
  !...Declare local variables
    integer(i8) :: rmax(NDIR), rmin(NDIR)
  continue
    rmin = this%range_min
    rmax = this%range_max
    coord => this%data_pool%coord(rmin(1):rmax(1), rmin(2):rmax(2), &
                                  rmin(3):rmax(3), :)
  end function block_coord_fun

  !> Get coordinate x of block
  function block_coord_x_fun(this) result(x)
    class(ClassGeomBlock), intent(inout) :: this
    real(dp), pointer :: x(:,:,:)
  !...Declare local variables
    integer(i8) :: rmax(NDIR), rmin(NDIR)
  continue
    rmin = this%range_min
    rmax = this%range_max
    x => this%data_pool%coord(rmin(1):rmax(1), rmin(2):rmax(2), rmin(3):rmax(3), 1)
  end function block_coord_x_fun

  !> Get coordinate y of block
  function block_coord_y_fun(this) result(y)
    class(ClassGeomBlock), intent(inout) :: this
    real(dp), pointer :: y(:,:,:)
  !...Declare local variables
    integer(i8) :: rmax(NDIR), rmin(NDIR)
  continue
    rmin = this%range_min
    rmax = this%range_max
    y => this%data_pool%coord(rmin(1):rmax(1), rmin(2):rmax(2), rmin(3):rmax(3), 2)
  end function block_coord_y_fun

  !> Get coordinate z of block
  function block_coord_z_fun(this) result(z)
    class(ClassGeomBlock), intent(inout) :: this
    real(dp), pointer :: z(:,:,:)
  !...Declare local variables
    integer(i8) :: rmax(NDIR), rmin(NDIR)
  continue
    rmin = this%range_min
    rmax = this%range_max
    z => this%data_pool%coord(rmin(1):rmax(1), rmin(2):rmax(2), rmin(3):rmax(3), 3)
  end function block_coord_z_fun

  !> Get cell size
  ! TODO: only for uniform grid now
  !   * --- * -:-- * ---:- * --- *
  !            |<- dr ->|
  function block_dr_fun(this, i, j, k, dir) result(dr)
  !...Declare input/output variables
    class(ClassGeomBlock), intent(inout) :: this
    integer(i8),          intent(in)     :: i, j, k
    integer,              intent(in)     :: dir
    real(dp)                             :: dr
  continue
    dr = this%data_pool%cell_size(dir)
  end function block_dr_fun

!=============================================================================80
!
! block method
!
!=============================================================================80

  !> Set block parameters
  subroutine block_set_para_sub(this, id, name, range_min, range_max)
    class(ClassGeomBlock), intent(inout) :: this
    integer,          intent(in), optional :: id
    character(len=*), intent(in), optional :: name
    integer(i8),      intent(in), optional :: range_min(NDIR)
    integer(i8),      intent(in), optional :: range_max(NDIR)
  continue
    if (present(id))        this%index = id
    if (present(name))      this%name  = name
    if (present(range_min)) this%range_min = range_min
    if (present(range_max)) this%range_max = range_max
  end subroutine block_set_para_sub

  !> Copy block from data
  subroutine block_copy_from_data_sub(this, block_data, ialloc)
    class(ClassGeomBlock), intent(inout) :: this
    type(ClassGeomData),   pointer       :: block_data
    integer,               intent(out)   :: ialloc
  !...Declare local variables
    integer :: isurf
    type(ClassGeomFace), pointer :: surf
  continue
    this%index     = UUID%blk()
    this%name      = block_data%name
    this%range_min = block_data%range_min
    this%range_max = block_data%range_max

    this%data_pool => block_data

    do isurf = 1, size(block_data%surface)
      allocate(surf, stat=ialloc)
      if (ialloc /= 0) return
      call surf%copy_from(block_data%surface(isurf))
      call this%add_surf(surf, ialloc)
      if (ialloc /= 0) return
      nullify(surf)
    end do
  end subroutine block_copy_from_data_sub

  !> Copy block from new block
  subroutine block_copy_from_block_sub(this, blk)
    class(ClassGeomBlock), intent(inout) :: this
    type(ClassGeomBlock),  intent(inout) :: blk
  continue
    this%index      = UUID%blk()
    this%name       = blk%name
    this%range_min  = blk%range_min
    this%range_max  = blk%range_max
    this%data_pool  => blk%data_pool
  end subroutine block_copy_from_block_sub

  !> Copy block basic properties to new block, except face list
  subroutine block_copy_to_sub(this, blk)
    class(ClassGeomBlock), intent(inout) :: this
    type(ClassGeomBlock),  intent(inout) :: blk
  continue
    blk%index      = UUID%blk()
    blk%name       = this%name
    blk%range_max  = this%range_max
    blk%range_min  = this%range_min
    blk%data_pool  => this%data_pool
  end subroutine block_copy_to_sub

!=============================================================================80
!
! Method of surface properties
!
!=============================================================================80

  !> Get number of block surface
  function block_nsurf_fun(this) result(nsurf)
    class(ClassGeomBlock), intent(inout) :: this
    integer :: nsurf
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: curr
  continue
    nsurf = 0
    curr => this%surface_list
    do while (associated(curr))
      nsurf = nsurf + 1
      curr => curr%next
    end do
  end function block_nsurf_fun

  !> Get number of boundary nodes
  function block_nbund_fun(this) result(nbund)
    class(ClassGeomBlock), intent(inout) :: this
    integer :: nbund
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: curr
  continue
    nbund = 0
    curr => this%surface_list
    do while (associated(curr))
      if ( curr%surface%is_type(TypeDefs%BC%Boundary) ) then
        nbund = nbund + 1
      end if
      curr => curr%next
    end do
  end function block_nbund_fun

  !> Get number of connectivity nodes
  function block_nconn_fun(this) result(nconn)
    class(ClassGeomBlock), intent(inout) :: this
    integer :: nconn
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: curr
  continue
    nconn = 0
    curr => this%surface_list
    do while (associated(curr))
      if ( curr%surface%is_type(TypeDefs%BC%Connectivity) ) then
        nconn = nconn + 1
      end if
      curr => curr%next
    end do
  end function block_nconn_fun

  !> Get boundary (read only)
  subroutine block_get_bund_sub(this, bund, ialloc)
    class(ClassGeomBlock),            intent(inout) :: this
    type(ClassGeomFace), allocatable, intent(inout) :: bund(:)
    integer,                          intent(out)   :: ialloc
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: curr
    integer :: ibund
  continue
    if ( allocated(bund) ) ialloc = 1
    allocate(bund(this%nbund()), stat=ialloc)
    if ( ialloc /= 0 ) return
  !...get boundary
    curr => this%surface_list
    ibund = 0
    do while (associated(curr))
      if ( curr%surface%is_type(TypeDefs%BC%Boundary) ) then
        ibund = ibund + 1
        bund(ibund) = curr%surface
      end if
      curr => curr%next
    end do
  end subroutine block_get_bund_sub

  !> Get connectivity (read only)
  subroutine block_get_conn_sub(this, conn, ialloc)
    class(ClassGeomBlock),            intent(inout) :: this
    type(ClassGeomFace), allocatable, intent(inout) :: conn(:)
    integer,                          intent(out)   :: ialloc
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: curr
    integer :: iconn
  continue
    if ( allocated(conn) ) ialloc = 1
    allocate(conn(this%nconn()), stat=ialloc)
    if ( ialloc /= 0 ) return
  !...get connectivity
    curr => this%surface_list
    iconn = 0
    do while (associated(curr))
      if ( curr%surface%is_type(TypeDefs%BC%Connectivity) ) then
        iconn = iconn + 1
        conn(iconn) = curr%surface
      end if
      curr => curr%next
    end do
  end subroutine block_get_conn_sub

  !> Get boundary pointer
  function block_bund_index_fun(this, ibund) result(bund)
    class(ClassGeomBlock), intent(inout) :: this
    integer,               intent(in)    :: ibund
    type(ClassGeomFace),   pointer       :: bund
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: curr
    integer :: cbund !< counter of boundary
  continue
    curr => this%surface_list
    cbund = 0
    do while (associated(curr))
      if ( curr%surface%is_type(TypeDefs%BC%Boundary) ) then
        cbund = cbund + 1
        if ( cbund == ibund ) then
          bund => curr%surface
          return
        end if
      end if
      curr => curr%next
    end do
  end function block_bund_index_fun

  !> Get connectivity pointer
  function block_conn_index_fun(this, iconn) result(conn)
    class(ClassGeomBlock), intent(inout) :: this
    integer,               intent(in)    :: iconn
    type(ClassGeomFace),   pointer       :: conn
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: curr
    integer :: cconn !< counter of connectivity
  continue
    curr => this%surface_list
    cconn = 0
    do while (associated(curr))
      if ( curr%surface%is_type(TypeDefs%BC%Connectivity) ) then
        cconn = cconn + 1
        if ( cconn == iconn ) then
          conn => curr%surface
          return
        end if
      end if
      curr => curr%next
    end do
  end function block_conn_index_fun

  !> Get connect block id
  function block_conn_block_id_fun(this, ialloc) result(id)
    class(ClassGeomBlock), intent(inout) :: this
    integer,               intent(out)   :: ialloc
    integer, allocatable :: id(:)
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: curr
    integer :: iconn
  continue
    allocate(id(this%nconn()), stat=ialloc)
    if ( ialloc /= 0 ) return
  !...get connectivity
    curr => this%surface_list
    iconn = 0
    do while (associated(curr))
      if ( curr%surface%is_type(TypeDefs%BC%Connectivity) ) then
        iconn = iconn + 1
        id(iconn) = curr%donor_blk%id()
      end if
      curr => curr%next
    end do
  end function block_conn_block_id_fun

  !> Compute cut absolute position
  !      * ------------ *
  !      ^      ^       ^
  !    floor   wcut   ceiling
  integer(i8) function block_cut_position_sub(this, wcut, dir, side) result(pos)
    class(ClassGeomBlock), intent(inout) :: this
    real(dp),              intent(in)    :: wcut !< weight of cut (min -> max)
    integer,               intent(in)    :: dir  !< direction of cut
    integer,               intent(in)    :: side !< side of cut pointe =>
                                                 !! -1: floor, 1: ceiling
  continue
    select case (side)
      case (-1)
        pos = floor  (wcut * dble(this%ncell(dir))) + this%range_min(dir)
      case (1)
        pos = ceiling(wcut * dble(this%ncell(dir))) + this%range_min(dir)
      case default
        call devlog%print(where="ClassGeomBlock::block_cut_position_sub", &
                          message="side must be -1 or 1", &
                          level=LogLevel%error)
    end select
  end function block_cut_position_sub

  !> Split block by plane, return cut off block
  subroutine block_split_by_plane_sub(this, cut, blk_n, blk_p, ialloc)
    class(ClassGeomBlock), intent(inout) :: this
    class(ClassGeomFace),  intent(inout) :: cut
    type(ClassGeomBlock),  pointer       :: blk_p !< block (positive side)
    type(ClassGeomBlock),  pointer       :: blk_n !< block (negative side)
    integer,               intent(out)   :: ialloc
  !...Declare local variables
    integer     :: dir
    integer     :: ibund, nbund_n, nbund_p
    integer     :: iconn, nconn_n, nconn_p
    integer(i8) :: face_range(NDIR,2)
    type(ClassGeomFace) :: face_cut
    type(ClassGeomSurfaceList), pointer :: curr
  continue
  !...Check if block need to be split
    if ( cut%position() <= this%range_min(cut%normal()) .or. &
         cut%position() >= this%range_max(cut%normal()) ) return
  !...Inherit block basic properties
    allocate(blk_p, stat=ialloc)
    if ( ialloc /= 0 ) return

    allocate(blk_n, stat=ialloc)
    if ( ialloc /= 0 ) return

  !...Split block by plane
    call this%copy_to(blk_n)
    call this%copy_to(blk_p)

    blk_n%range_max(cut%normal()) = cut%position()
    blk_p%range_min(cut%normal()) = cut%position()

  !...Split surfaces
    ! Negative side
    curr => this%surface_list
    do while (associated(curr))
      face_cut = curr%surface%cut(cut, side=-1)
      if ( .not. face_cut%empty() ) then
        call blk_n%add_surf(face_cut, ialloc)
        if ( ialloc /= 0 ) return
        if ( face_cut%is_type(TypeDefs%BC%Connectivity) ) &
            blk_n%surface_list%donor_blk => curr%donor_blk
      end if
      curr => curr%next
    end do
    ! Positive side
    curr => this%surface_list
    do while (associated(curr))
      face_cut = curr%surface%cut(cut, side=1)
      if ( .not. face_cut%empty() ) then
        call blk_p%add_surf(face_cut, ialloc)
        if ( face_cut%is_type(TypeDefs%BC%Connectivity) ) &
            blk_n%surface_list%donor_blk => curr%donor_blk
        if ( ialloc /= 0 ) return
      end if
      curr => curr%next
    end do
    ! add cut connectivity between blocks
    face_range(:,1) = this%range_min(:)
    face_range(:,2) = this%range_max(:)
    face_range(cut%normal(),:) = cut%position()
    call this%connect(blk_n, face_range, blk_p, face_range, ialloc)
  end subroutine block_split_by_plane_sub

  !> Cut block by plane, return cut off block
  subroutine block_cut_by_plane_sub(this, cut, blk_cut, side, ialloc)
    class(ClassGeomBlock), intent(inout) :: this
    type(ClassGeomPlane),  intent(inout) :: cut
    type(ClassGeomBlock),  pointer       :: blk_cut  !< block (cut off)
    integer,               intent(in)    :: side
    integer,               intent(out)   :: ialloc
  !...Declare local variables
    type(ClassGeomBlock), pointer :: blk_n
    type(ClassGeomBlock), pointer :: blk_p
    type(ClassGeomBlock), pointer :: donor_blk
  continue
  !...Check if block need to be split
    if ( cut%position() <= this%range_min(cut%normal()) .or. &
         cut%position() >= this%range_max(cut%normal()) ) return
  !...Split block
    call this%split_by_plane(cut, blk_n, blk_p, ialloc)
    if ( ialloc /= 0 ) return

  !...Return cut off block
    if ( side == -1 ) then
      blk_cut => blk_n
      nullify(blk_n)
    else if ( side == 1 ) then
      blk_cut => blk_p
      nullify(blk_p)
    end if

  !...Deallocate memory
    if ( associated(blk_n) ) deallocate(blk_n)
    if ( associated(blk_p) ) deallocate(blk_p)
  end subroutine block_cut_by_plane_sub

  !> Cut off a block by plane
  subroutine block_cut_off_by_plane_sub(cut, blk_curr, blk_cut, side, ialloc)
    class(ClassGeomFace),  intent(inout) :: cut
    type(ClassGeomBlock),  pointer       :: blk_curr  !< block (current)
    type(ClassGeomBlock),  pointer       :: blk_cut   !< block (cut off)
    integer,               intent(in)    :: side      !< 1: positive, -1: negative
    integer,               intent(out)   :: ialloc
  !...Declare local variables
    type(ClassGeomBlock), pointer :: blk_n
    type(ClassGeomBlock), pointer :: blk_p

    type(ClassGeomSurfaceList), pointer :: curr_p, curr_n
  continue
  !...Check if block need to be split
    if ( cut%position() >= blk_curr%range_max(cut%normal()) .or. &
         cut%position() <= blk_curr%range_min(cut%normal()) ) return
  !...Split block
    call blk_curr%split_by_plane(cut, blk_n, blk_p, ialloc)
    if ( ialloc /= 0 ) return
  !...Change donor block connectivity
    call blk_curr%deconn_donor_blk_conn(blk_curr)
  !...Change block
    deallocate(blk_curr)
    if ( side == -1 ) then
      blk_curr => blk_p
      nullify(blk_p)
      blk_cut => blk_n
      nullify(blk_n)
    else if ( side == 1 ) then
      blk_curr => blk_n
      nullify(blk_n)
      blk_cut => blk_p
      nullify(blk_p)
    end if
  !...Change donor block connectivity
    call blk_curr%reconn_donor_blk_conn(blk_curr, ialloc)
    call blk_cut%reconn_donor_blk_conn(blk_cut, ialloc)
  !...Deallocate memory
    if ( associated(blk_n) ) deallocate(blk_n)
    if ( associated(blk_p) ) deallocate(blk_p)
  end subroutine block_cut_off_by_plane_sub

  !> Cut block uniform
  !                      --> j
  !  |  * --- * --- * --- *
  !  v  |  1  |  3  |  5  |
  !  i  * --- * --- * --- *
  !     |  2  |  4  |  6  |
  !     * --- * --- * --- *
  !
  subroutine block_cut_uniform_sub(this, ncut, blk_cut, ialloc)
    class(ClassGeomBlock), intent(inout) :: this
    integer(i8),           intent(in)    :: ncut(3)
    type(ClassGeomBlock),  pointer       :: blk_cut(:) !< block (cut off)
    integer,               intent(out)   :: ialloc
  !...Declare local variables
    integer     :: dir
    integer     :: icut, donor_icut !< index of cut
    integer(i8) :: ncut_remain  !< number of cuts remain
    integer     :: ncell_reamin !< number of cells remain
    integer     :: cuti, cutj, cutk !< index of cut
    integer     :: ncut_i, ncut_ij  !< number of cut
    integer(i8) :: face_range(NDIR, 2)
    integer(i8), allocatable :: pos(:,:) !< cut position at each direction
    type(ClassGeomFace)                 :: face_cut
    type(ClassGeomSurfaceList), pointer :: curr
    type(ClassGeomBlock),       pointer :: blk_, blk_donor_
  continue
  !...Check dimension
    if (product(ncut) == 1) return
    do dir = 1, NDIR
      if ( ncut(dir) > this%range_max(dir) - this%range_min(dir) ) return
    end do
  !...Allocate memory
    allocate(blk_cut(product(ncut)), stat=ialloc)
    if ( ialloc /= 0 ) return
    do icut = 1, product(ncut)
      call this%copy_to(blk_cut(icut))
    end do
    allocate(pos(NDIR, maxval(ncut)+1), stat=ialloc)
    if ( ialloc /= 0 ) return
    pos(:,:) = 1
  !...Compute cut position at each direction
    do dir = 1, NDIR
      pos(dir, 1) = this%range_min(dir)
      do icut = 2, ncut(dir)
        ncell_reamin = this%range_max(dir) - pos(dir, icut-1)
        ncut_remain  = ncut(dir) - icut + 2
        pos(dir, icut) = pos(dir, icut-1) + ncell_reamin / ncut_remain
      end do
      pos(dir, ncut(dir)+1) = this%range_max(dir)
    end do
  !...Compute origin and nvert of each cut block
    ncut_i  = ncut(1)
    ncut_ij = ncut(1) * ncut(2)
    do cutk = 1, ncut(3)
      do cutj = 1, ncut(2)
        do cuti = 1, ncut(1)
          icut = cuti + (cutj - 1) * ncut_i + (cutk - 1) * ncut_ij

          blk_cut(icut)%range_min(1) = pos(1, cuti)
          blk_cut(icut)%range_min(2) = pos(2, cutj)
          blk_cut(icut)%range_min(3) = pos(3, cutk)

          blk_cut(icut)%range_max(1) = pos(1, cuti+1)
          blk_cut(icut)%range_max(2) = pos(2, cutj+1)
          blk_cut(icut)%range_max(3) = pos(3, cutk+1)
        end do
      end do
    end do
  !...Cut Boundary and Connectivity
    ! There are 6 face for each block
    !
    !  position       positive    negative
    !  boundary       inherit     inherit
    !  connectivity     A-B      no operation
    !
    !    --> j                   * %%% *  * %%% *  * %%% *
    !  | * --- * --- * --- *     %  1  #  |  3  #  |  1  %
    !  v |  1  |  3  |  5  |     * ### *  * ### *  * ### *
    !  i * --- * --- * --- * =>
    !    |  2  |  4  |  6  |     * --- *  * --- *  * --- *
    !    * --- * --- * --- *     %  2  #  |  4  #  |  6  %
    !                            * %%% *  * %%% *  * %%% *
    !
    !    %%% => boundary
    !    ### => positive connectivity
    !    --- => negative connectivity
    !
    do icut = 1, product(ncut)
      do dir = 1, NDIR
      !...negative face
        face_range(:,1)   = blk_cut(icut)%range_min(:)
        face_range(:,2)   = blk_cut(icut)%range_max(:)
        face_range(dir,2) = blk_cut(icut)%range_min(dir)
        if ( face_range(dir,1) == this%range_min(dir) ) then !< boundary
          curr => this%surface_list
          do while (associated(curr))
            face_cut = curr%surface%cut(face_range(:,1), face_range(:,2))
            if ( .not. face_cut%empty() ) then
              call blk_cut(icut)%add_surf(face_cut, ialloc)
              if ( ialloc /= 0 ) return
              if ( face_cut%is_type(TypeDefs%BC%Connectivity) ) &
                blk_cut(icut)%surface_list%donor_blk => curr%donor_blk
            end if
            curr => curr%next
          end do
        end if
      !...positive face
        face_range(:,1)   = blk_cut(icut)%range_min(:)
        face_range(:,2)   = blk_cut(icut)%range_max(:)
        face_range(dir,1) = blk_cut(icut)%range_max(dir)
        if ( face_range(dir,2) == this%range_max(dir) ) then !< boundary
          curr => this%surface_list
          do while (associated(curr))
            face_cut = curr%surface%cut(face_range(:,1), face_range(:,2))
            if ( .not. face_cut%empty() ) then
              call blk_cut(icut)%add_surf(face_cut, ialloc)
              if ( ialloc /= 0 ) return
              if ( face_cut%is_type(TypeDefs%BC%Connectivity) ) &
                blk_cut(icut)%surface_list%donor_blk => curr%donor_blk
            end if
            curr => curr%next
          end do
        else !< connectivity
        !...Compute donor block index
          if ( dir == 1 ) then
            donor_icut = icut + 1
          else if (dir == 2) then
            donor_icut = icut + ncut_i
          else if (dir == 3) then
            donor_icut = icut + ncut_ij
          end if
      !...Create connectivity
          blk_       => blk_cut(icut)
          blk_donor_ => blk_cut(donor_icut)
          call this%connect(blk_, face_range, blk_donor_, face_range, ialloc)
          if ( ialloc /= 0 ) return
        end if
      end do
    end do
  !...Connect cut blocks
  end subroutine block_cut_uniform_sub

  !> Cut off block uniform
  !                      --> j
  !  |  * --- * --- * --- *
  !  v  |  1  |  3  |  5  |
  !  i  * --- * --- * --- *
  !     |  2  |  4  |  6  |
  !     * --- * --- * --- *
  !
  subroutine block_cut_off_uniform_sub(ncut, blk_curr, blk_cut, ialloc)
    integer(i8),           intent(in)    :: ncut(3)
    type(ClassGeomBlock),  pointer       :: blk_cut(:) !< block (cut off)
    type(ClassGeomBlock),  pointer       :: blk_curr   !< block (current)
    integer,               intent(out)   :: ialloc
  !...Declare local variables
    integer :: dir
    integer :: icut
    type(ClassGeomBlock),  pointer :: blk_cut_(:)
    type(ClassGeomBlock),  pointer :: blk_cut__
  continue
  !...Check dimension
    if (product(ncut) == 1) return
    do dir = 1, NDIR
      if ( ncut(dir) > blk_curr%range_max(dir) - blk_curr%range_min(dir) ) return
    end do
  !...Cut block
    call blk_curr%cut(ncut, blk_cut_, ialloc)
    if ( ialloc /= 0 ) return
  !...Change donor block connectivity
    call blk_curr%deconn_donor_blk_conn(blk_curr)
  !...Return cut off block
    deallocate(blk_curr)
    blk_curr => blk_cut_(1)
    blk_cut  => blk_cut_(2:product(ncut))
    nullify(blk_cut_)
  !...Change donor block connectivity
    call blk_curr%reconn_donor_blk_conn(blk_curr, ialloc)
    do icut = 1, size(blk_cut)
      blk_cut__ => blk_cut(icut)
      call blk_cut__%reconn_donor_blk_conn(blk_cut__, ialloc)
    end do
  !...Deallocate memory for safety
    if ( associated(blk_cut_) ) deallocate(blk_cut_)
  end subroutine block_cut_off_uniform_sub

  !> Add surface to list
  subroutine block_add_surface_sub(this, surface, ialloc)
    class(ClassGeomBlock), intent(inout) :: this
    type(ClassGeomFace),   intent(inout) :: surface
    integer,               intent(out)   :: ialloc
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: new_node
    type(ClassGeomFace),        pointer :: new_surf
  continue
    allocate(new_node, stat=ialloc)
    if ( ialloc /= 0 ) return
    allocate(new_surf, stat=ialloc)
    if ( ialloc /= 0 ) return
    call new_surf%copy_from(surface)
    new_node%surface => new_surf
  !...Add new node to list
    new_node%next => this%surface_list
    this%surface_list => new_node
    nullify(new_node)
  end subroutine block_add_surface_sub

  !> Add connectivity to block surfacelist
  subroutine block_add_conn_sub(this, conn, donor_blk, ialloc)
    class(ClassGeomBlock), intent(inout) :: this
    type(ClassGeomFace),   pointer       :: conn
    type(ClassGeomBlock),  pointer       :: donor_blk
    integer,               intent(out)   :: ialloc
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: new_node
  continue
    allocate(new_node, stat=ialloc)
    if ( ialloc /= 0 ) return
  !...Set donor block
    if ( .not. conn%is_type(TypeDefs%BC%Connectivity) ) then
      call devlog%print(where='ClassGeomBlock::block_add_surface_sub',  &
                        message='Add surface is not connectivity', &
                        level=LogLevel%Error)
    end if
    new_node%surface   => conn
    new_node%donor_blk => donor_blk
  !...Add new node to list
    new_node%next => this%surface_list
    this%surface_list => new_node
    nullify(new_node)
  end subroutine block_add_conn_sub

  !> Connect two blocks
  subroutine block_connect_block_sub(blk_n, range_n, blk_p, range_p, ialloc, trans)
    type(ClassGeomBlock),  pointer       :: blk_n
    integer(i8),           intent(in)    :: range_n(NDIR,2)
    type(ClassGeomBlock),  pointer       :: blk_p
    integer(i8),           intent(in)    :: range_p(NDIR,2)
    integer,               intent(out)   :: ialloc
    integer,               intent(in),   optional   :: trans(NDIR)
                                          !< direction transformation
                                          !! For same, this no need to be set
  !...Declare local variables
    type(ClassGeomFace), pointer :: conn_n, conn_p
  continue
    allocate(conn_n, stat=ialloc)
    if ( ialloc /= 0 ) return
    allocate(conn_p, stat=ialloc)
    if ( ialloc /= 0 ) return

    call conn_n%set(range_n(:,1), range_n(:,2), iblk=blk_n%index, name="conn_n", &
                    type=TypeDefs%BC%Connectivity)
    call conn_p%set(range_p(:,1), range_p(:,2), iblk=blk_p%index, name="conn_p", &
                    type=TypeDefs%BC%Connectivity)

    if ( present(trans) ) then
      call conn_n%connect_to(conn_p, trans)
      call conn_p%connect_to(conn_n, trans)
    else
      call conn_n%connect_to(conn_p)
      call conn_p%connect_to(conn_n)
    end if

    call blk_n%add_conn(conn_n, blk_p, ialloc)
    if ( ialloc /= 0 ) return
    call blk_p%add_conn(conn_p, blk_n, ialloc)
    if ( ialloc /= 0 ) return

    nullify(conn_n)
    nullify(conn_p)
  end subroutine block_connect_block_sub

  !> Disconnect donor block connectivity
  !! 1. For cut block, origin A <--> B
  !! 2. keep A and cut B(B1, B2, ...)
  !! 3. A -x-> B;  A <--- B
  !! 4. inherit A <--- B1, A <--- B2, A <--- B3
  !! 5. reconnect A ---> B1/B2... by reconn_donor_blk_conn
  subroutine block_deconn_donor_blk_conn_sub(this, curr_blk)
    class(ClassGeomBlock), intent(inout) :: this
    type(ClassGeomBlock),  pointer       :: curr_blk
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: curr
    type(ClassGeomSurfaceList), pointer :: donor
  continue
    curr => this%surface_list
    do while (associated(curr))
      if ( curr%surface%is_type(TypeDefs%BC%Connectivity) ) then
        donor => curr%donor_blk%surface_list
        do while (associated(donor))
          if ( associated(donor%donor_blk, curr_blk) ) then
            nullify(donor%donor_blk)
          end if
          donor => donor%next
        end do
      end if
      curr => curr%next
    end do
  end subroutine block_deconn_donor_blk_conn_sub

  !> Reconnect donor block connectivity (current block donor properties must be set)
  !! 1. change connect by deconn_donor_blk_conn first
  !! 2. origin A -x-> B;  A <--- B1, A <--- B2, A <--- B3
  !! 3. reconnect A ---> B1/B2...
  subroutine block_reconn_donor_blk_conn_sub(this, curr_blk, ialloc)
    class(ClassGeomBlock), intent(inout) :: this
    integer,               intent(out)   :: ialloc
    type(ClassGeomBlock),  pointer       :: curr_blk
  !...Declare local variables
    type(ClassGeomSurfaceList), pointer :: curr
    type(ClassGeomBlock),       pointer :: donor_blk
    type(ClassGeomSurfaceList), pointer :: donor
    type(ClassGeomFace),        pointer :: donor_conn
    logical :: flag_all_conn, flag_conn
  continue
    !             A <--> B
    !          connA -> B, connB
    !       A, connA <- connB
    !
    !   Cut       A <--> B1              A <--> B2        ....
    !          connA -> NULL, NULL    connA -> NULL, NULL ....
    !       A, connA <- connB1     A, connA <- connB2     ....
    !
    !  Method: Create a new donor face for donor block
    !         connA -> connA1  connA1 <- connB1
    !
    ! Attention: to avoid surfacelist operation, the node contains connectivity
    !            of A <-> B where shared to new connectivity of A <-> B1
    !
    curr => this%surface_list
    do while (associated(curr))
      if ( curr%surface%is_type(TypeDefs%BC%Connectivity) ) then
      !...Check connectivity relation (B1 <-> B2)
        donor_blk  => curr%donor_blk
        if ( .not. associated(donor_blk) ) then
          call devlog%print(where='ClassGeomBlock::block_change_donor_blk_conn_sub',  &
                            message='Donor block not set', &
                            level=LogLevel%Error)
        end if
        donor => donor_blk%surface_list
        flag_conn = .false.
        do while (associated(donor))
          if ( associated(curr%surface%donor_face(), donor%surface) .and. &
               associated(curr%surface, donor%surface%donor_face()) ) then
            flag_conn = .true.
            exit
          end if
          donor => donor%next
        end do

      !...Create new connectivity (A <-> B1, B2...)
        if ( .not. flag_conn ) then
        !...Create new donor face for donor block (connB1 -> connA1)
          call curr%surface%new_donor_face(donor_conn, curr%surface, ialloc)
          if ( ialloc /= 0 ) return
        !...Change donor block connectivity (connA -> connA1 -> B1, connB1)
          donor => donor_blk%surface_list
          flag_all_conn = .true.
          do while (associated(donor))
            ! use the connectivity has been split
            if ( donor%surface%is_type(TypeDefs%BC%Connectivity) ) then
              if ( .not. associated(donor%donor_blk) ) then
                flag_all_conn = .false.
                if (associated(donor%surface))  deallocate(donor%surface)
                donor%surface => donor_conn
                donor%donor_blk => curr_blk
                exit
              end if
            end if
            donor => donor%next
          end do
          if ( flag_all_conn ) then
            ! add connectivity to donor block
            call donor_blk%add_conn(donor_conn, curr_blk, ialloc)
            if ( ialloc /= 0 ) return
          end if
        end if
      end if
      curr => curr%next
    end do
  end subroutine block_reconn_donor_blk_conn_sub

end module defs_block_geom
