!=============================================================================80
!
!> Interface to metis library
!
! TODO: Not support multi-block partition yet, because there are some problem
!       to balance load of different blocks and group small blocks.
!
!=============================================================================80

module interface_metis

  use interface_metis_c

  use kind_parameter, only : i8, len_long
  use global_error, only : ClassError

  implicit none

!...Include metis library

!...Define scope
  private

  public  :: ClassMetis

!...Declare class
  type :: ClassMetisGraph
    integer(i8), public :: nvert(3) !< number of vertices at each dimension
  contains
  end type ClassMetisGraph

  type :: ClassMetisMesh
    integer(i8), public :: nvert(3) !< number of vertices at each dimension
    integer(i8), public, allocatable :: eptr(:) !< element pointer
    integer(i8), public, allocatable :: eind(:) !< element index
  contains
    procedure, public, pass :: nelements => nelements_fun
    procedure, public, pass :: nvertices => nvertices_fun
    procedure, public, pass :: nedges    => nedges_fun
    procedure, public, pass :: to_metis_fmt => to_metis_fmt_sub
    final :: delete_ClassMetisMesh
  end type ClassMetisMesh

  type :: ClassMetis
    integer,              private          :: nblocks = 0
    type(ClassMetisMesh), private, pointer :: block(:) => null()
  contains
    procedure, pass, public :: partition_mesh  => partition_mesh_sub
    procedure, pass, public :: partition_graph => partition_graph_sub
    procedure, pass, public :: set_partition   => set_partition_param
    procedure, pass, public :: add_mesh_block  => add_mesh_block_sub
    procedure, pass, public :: init_info       => print_init_info_fun
    final :: delete_ClassMetis
  end type ClassMetis

  !> Constructor of class ClassMetis
  interface ClassMetis
    module procedure new_ClassMetis
  end interface ClassMetis

contains

!================================ ClassMetisMesh =============================80
!
! Subroutine of ClassMetisMesh
!
!=============================================================================80

  !> Destructor of class ClassMetisMesh
  subroutine delete_ClassMetisMesh(this)
  !...Deallocating pointer attribute
    type(ClassMetisMesh), intent(inout) :: this
  continue
    if (allocated(this%eptr)) deallocate(this%eptr)
    if (allocated(this%eind)) deallocate(this%eind)
  end subroutine delete_ClassMetisMesh

  !> Return number of elements
  integer(i8) function nelements_fun(this) result(val)
  !...Declare input/output variables
    class(ClassMetisMesh), intent(in) :: this
  continue
    val = (this%nvert(1) - 1) * (this%nvert(2) - 1) * (this%nvert(3) - 1)
  end function nelements_fun

  !> Return number of vertices
  integer(i8) function nvertices_fun(this) result(val)
  !...Declare input/output variables
    class(ClassMetisMesh), intent(in) :: this
  continue
    val = this%nvert(1) * this%nvert(2) * this%nvert(3)
  end function nvertices_fun

  !> Return number of edges
  !  nEdges in I direction: (nI - 1) * nJ * nK
  !  nEdges in J direction: nI * (nJ - 1) * nK
  !  nEdges in K direction: nI * nJ * (nK - 1)
  integer(i8) function nedges_fun(this) result(val)
  !...Declare input/output variables
    class(ClassMetisMesh), intent(in) :: this
  !...Local variables
    integer(i8) :: ni, nj, nk
  continue
    ni = this%nvert(1)
    nj = this%nvert(2)
    nk = this%nvert(3)
    val = (ni - 1) * nj * nk + ni * (nj - 1) * nk + ni * nj * (nk - 1)
  end function nedges_fun

  !> Convert to metis format
  subroutine to_metis_fmt_sub(this, error)
  !...Declare input/output variables
    class(ClassMetisMesh), intent(inout)  :: this
    type(ClassError),      intent(inout)  :: error
  !...Declare local variables
    integer(i8), allocatable :: node(:,:,:)
    integer(i8) :: i, j, k
    integer(i8) :: inode, ielem
  continue
    allocate(node(this%nvert(1), this%nvert(2), this%nvert(3)), stat=error%ialloc)
    if (error%allocate("METIS NODE")) return
    inode = 0
    do k = 1, this%nvert(3) - 1
      do j = 1, this%nvert(2) - 1
        do i = 1, this%nvert(1) - 1

        end do
      end do
    end do
  end subroutine to_metis_fmt_sub

  !> Return element index
  !> !! NOT USE FOR CYCLE
  !  Rank: i => j => k
  !  PlaneIJ(k) => LineI(j) => Node(i)
  !  index = (k-1)*nPlaneIJ + (j-1)*nLineI + i
  !  nPlaneIJ = nNodeI * nNodeJ
  integer(i8) function ielement_fun(this, i, j, k) result(ielem)
  !...Declare input/output variables
    class(ClassMetisMesh), intent(in) :: this
    integer(i8),           intent(in) :: i
    integer(i8),           intent(in) :: j
    integer(i8),           intent(in) :: k
  continue
    ielem = i + (j - 1) * this%nvert(1) + (k - 1) * this%nvert(1) * this%nvert(2)
  end function ielement_fun

  !> Return element dimension
  !> !! NOT USE FOR CYCLE
  function element_dimension_fun(this, ielem) result(edim)
  !...Declare input/output variables
    class(ClassMetisMesh), intent(in) :: this
    integer(i8),           intent(in) :: ielem
    integer(i8)                       :: edim(3)
  !...Declare local variables
    integer(i8) :: res_elem
  continue
    res_elem = ielem
    edim(3)  = res_elem / (this%nvert(1) * this%nvert(2)) + 1
    res_elem = res_elem - (edim(3) - 1) * this%nvert(1) * this%nvert(2)
    edim(2)  = res_elem / this%nvert(1) + 1
    res_elem = res_elem - (edim(2) - 1) * this%nvert(1)
    edim(1)  = res_elem
  end function element_dimension_fun

  !> Return element node
  !> !! NOT USE FOR CYCLE
  integer(i8) function element_node_fun(this, ielem, inode) result(val)
  !...Declare input/output variables
    class(ClassMetisMesh), intent(in) :: this
    integer(i8),           intent(in) :: ielem
    integer(i8),           intent(in) :: inode
  continue
  end function element_node_fun

!================================== ClassMetis ================================80
!
! Subroutine of ClassMetis
!
!=============================================================================80

  !> Constructor of class ClassMetis
  function new_ClassMetis() result(this)
  !...Declare input/output variables
    type(ClassMetis)    :: this
  continue
    this%nblocks = 0
    this%block => null()
  end function new_ClassMetis

  !> Destructor of class ClassMetis
  subroutine delete_ClassMetis(this)
  !...Deallocating pointer attribute
    type(ClassMetis), intent(inout) :: this
  continue
    if (associated(this%block)) deallocate(this%block)
  end subroutine delete_ClassMetis

  !> Set partition parameter
  subroutine set_partition_param(this, type, nblocks, error)
  !...Declare input/output variables
    class(ClassMetis), intent(inout) :: this
    character(len=*),  intent(in)    :: type !< 'mesh' or 'graph'
    integer,           intent(in)    :: nblocks
    type(ClassError),  intent(inout) :: error
  continue
    if ( trim(type) /= "mesh") then
      call error%set(.true., "Only support METIS mesh partition yet.")
      return
    else if ( nblocks /= 1 ) then
      call error%set(.true., "Only support single block partition yet.")
      return
    end if
    this%nblocks = nblocks
    allocate(this%block(nblocks), stat=error%ialloc)
    if (error%allocate("METIS mesh")) return
  end subroutine set_partition_param

  !> Add mesh block
  subroutine add_mesh_block_sub(this, iblock, nvert, error)
  !...Declare input/output variables
    class(ClassMetis), intent(inout) :: this
    integer,           intent(in)    :: iblock
    integer(i8),       intent(in)    :: nvert(3)  !< size of mesh block
                                                  !< number of vertices
    type(ClassError),  intent(inout) :: error
  continue
    this%block(iblock)%nvert = nvert
    allocate(this%block(iblock)%eptr(this%block(iblock)%nelements() + 1), &
             stat=error%ialloc)
    if (error%allocate("METIS EPTR")) return
    this%block(iblock)%eptr = 0
    allocate(this%block(iblock)%eind(this%block(iblock)%nelements()*6), &
             stat=error%ialloc)
    if (error%allocate("METIS EIND")) return
    this%block(iblock)%eind = 0
    call this%block(iblock)%to_metis_fmt(error)
    if (error%occur()) return
  end subroutine add_mesh_block_sub

  !> Print initial information
  function print_init_info_fun(this) result(info)
    use interface_stdlib_string, only : ClassStringMethod
    use container_linklist,      only : linklist
  !...Declare input/output variables
    class(ClassMetis),   intent(in)  :: this
    character(len_long), allocatable :: info(:)
  !...Declare local variables
    type(ClassStringMethod) :: string
    type(linklist)          :: info_
    class(*), pointer       :: curr_
    integer                 :: stat_, iinfo, iblock
  continue
  !...Mesh partitioning information
    stat_ = info_%push_back("Metis -- Multi-Block Grids Partition")
    stat_ = info_%push_back("Number of blocks: " // string%from(this%nblocks))
    do iblock = 1, this%nblocks
      stat_ = info_%push_back("")
      stat_ = info_%push_back("- Block " // string%from(iblock))
      stat_ = info_%push_back("  Size  " // &
                          string%from(this%block(iblock)%nvert(1)) // "  " // &
                          string%from(this%block(iblock)%nvert(2)) // "  " // &
                          string%from(this%block(iblock)%nvert(3)))
      stat_ = info_%push_back("  n  m  size(eptr) size(eind)")
    end do
    ! if error, not output
    if ( stat_ /= 0 ) return
  !...Output information
    allocate(info(info_%size()), stat=stat_)
    if ( stat_ /= 0 ) return
    iinfo = 0
    info  = ""
    do while (info_%cycle())
      iinfo = iinfo + 1
      curr_ => info_%current()
      select type (curr_)
        type is (character(len=*))
          info(iinfo) = curr_
        class default
          return
      end select
    end do
    nullify(curr_)
  end function print_init_info_fun

  !> Partition mesh
  subroutine partition_mesh_sub(this)
  !...Declare input/output variables
    class(ClassMetis), intent(in) :: this
  continue
  end subroutine partition_mesh_sub

  !> Partition graph
  subroutine partition_graph_sub(this)
  !...Declare input/output variables
    class(ClassMetis), intent(in) :: this
  continue
  end subroutine partition_graph_sub

end module interface_metis
