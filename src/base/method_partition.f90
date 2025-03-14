!=============================================================================80
!
!> Partition Multi-Block Structural Grid.
!
! REF: Wang, Hengjie. Algorithm Design for High-Performance CFD Solvers on
!      Structured Grids. University of California, Irvine, 2021.
!
!  1. Split blocks into sub-blocks by REB and IF method
!  2. Map sub-block to graph and partition by METIS
!  3. Merge sub-blocks at each processor
!  4. Inherit connectivity between sub-blocks
!
!
! Example for Metric
!
!    Processor 1          Processor 2
!
!     * ---- *              * ---- *
!     |      |      30      |      |
!     |  50  |   <------>   |  70  |
!     |      |              |      |
!     * ---- *              |      |
!        |  10              * ---- *
!     * ---- *
!     |  30  |
!     * ---- *
!
!   Load Imbalance  = deviation / average workload
!                    = sqrt((80-75)^2 + (70-75)^2) / 2) / 75
!                    = 5 / 75
!
!       deviation = sqrt(sum((each - average)^2)/number of processors)
!       average workload = sum workload / number of processors
!
!   Edge Cuts = Communication edges * 2 = 2
!
!       Shared Edges        = Edge Cuts in processors
!       Communication Edges = Edge Cuts between processors
!
!   Communication Volume = sum of volume of Communication Edge Cuts
!                        = 30 * 2
!
!   Shared Memory Copy = sum of volume of shared edges
!                      = 10 * 2
!
!=============================================================================80

module method_partition

  use kind_parameter,     only : dp, dp_inf, dp_eps
  use kind_parameter,     only : i4, i8
  use kind_parameter,     only : len_string

  use global_error,    only : ClassError
  use defs_block_geom, only : ClassGeomBlock, NDIR
  use defs_face_geom,  only : ClassGeomFace, ClassGeomPlane

  implicit none

!...Define scope
  private

  public :: ClassPartition

  public :: REB, IF

!...Define Types for partition method
  integer, parameter :: REB = 1 !< recursive edge bisection
  integer, parameter :: IF  = 2 !< integer factorization

!...Declare partition method
  !> Class of partition map information
  type :: ClassPartitionMap
    integer,     private :: parent_block = 0
  end type ClassPartitionMap

  type :: ClassMetisGraph
    integer, private :: nvtxs = 0
    integer, private :: ncon  = 1
    integer, private, allocatable :: vert_id(:)
    integer, private, allocatable :: xadj(:)
    integer, private, allocatable :: adjncy(:)
    integer, private, allocatable :: vwgt(:)
    integer, private, allocatable :: vsize(:)
    integer, private, allocatable :: adjwgt(:)
    integer, private :: nparts = 0
    real(dp), private, allocatable :: tpwgts(:)
    real(dp), private, allocatable :: ubvec(:)
    integer,  private :: objval = 0
    integer,  private, allocatable :: epart(:)
  end type ClassMetisGraph

  !> Class of block cut setp
  type :: ClassBlockCutList
    integer,              private :: type  = 0
                !< 1 => cut by plane;
                !! 2 => cut by factorization
    type(ClassGeomPlane), private :: plane
    integer,              private :: side  = 0
                !< side of cut plane,
                !! -1: negative side; +1: positive side
    integer(i8),          private :: factor(3) = 1
                !< factorization: number of parition in each direction
    type(ClassBlockCutList), private, pointer :: head => null()
    type(ClassBlockCutList), private, pointer :: next => null()
  contains
    procedure, pass, public :: add    => cut_list_add_sub
    procedure, pass, public :: delete => cut_list_delete_node_sub
    procedure, pass, public :: is_empty  => cut_list_empty_sub
    final :: delete_ClassBlockCutList
  end type ClassBlockCutList

  !> Class of partition block list
  type :: ClassPartitionBlock
    type(ClassGeomBlock),      pointer, private :: blk => null()
    real(dp),                           private :: workload = 0.0_dp
                              !< workload of block
                              !! 3.5 = Allocate 3 process + Merge 0.5 other process

    type(ClassPartitionMap),            private :: map !< current block map
    type(ClassPartitionBlock), pointer, private :: head => null()
    type(ClassPartitionBlock), pointer, private :: next => null()
  contains
    procedure, pass   :: add_block     => list_add_block_sub
    procedure, pass   :: add_blocklist => list_add_blocklist_sub
    generic,   public :: add => add_block, add_blocklist
    procedure, public, pass :: size => list_size_fun
    ! procedure, public, pass   :: insert => list_insert_block_sub
    final :: delete_ClassPartitionBlock
  end type ClassPartitionBlock

  !> Class of partition method
  type :: ClassPartitionMethod
    real(dp), private :: alpha   = 1.0E-4 !< latency (s)
    real(dp), private :: beta    = 1.0E+9 !< bandwidth of network (bytes/s)
    real(dp), private :: epsilon = 0.05   !< tolerance for load imbalance
    real(dp), private :: data_size = 8.0  !< data size of each node or cell
              !< double precision: 8 bytes; single precision: 4 bytes
              !< exapmle, if each node has 5 variables, 5*8 = 40 bytes
    integer,  private :: nghost_layer = 2 !< number of ghost layers
  contains
    procedure, public, pass   :: set => set_method_sub
    procedure, public, pass   :: IF  => integer_factorize_block
    procedure, public, pass   :: REB => recursive_edge_bisection
    procedure, public, pass   :: find_min_cut  => find_min_cut_fun
    procedure, public, pass   :: t_b2b_if      => t_b2b_if_fun
    procedure, public, nopass :: factorization => factorize_number_sub
    procedure, public, nopass :: load_balance  => load_balance_sub
    procedure, public, nopass :: cut_block     => cut_block_sub
    procedure, public, nopass :: build_metis_graph => build_metis_graph_sub
    procedure, public, nopass :: partition_metis_graph => metis_graph_partition_sub
  end type ClassPartitionMethod

  !> Class of partition
  type :: ClassPartition
    integer,  private :: imethod = 0 !< partition method: REB, IF
    integer,  private :: nproc   = 0 !< number of processors
    type(ClassPartitionMethod), private :: method
    type(ClassPartitionBlock),  private :: blocks !< block list
    type(ClassMetisGraph),      private :: mgraph !< metis graph
    type(ClassPartitionMap),    private, allocatable :: maps(:) !< all block maps
    type(ClassBlockCutList),    private :: cutlist
  contains
    procedure, public, pass :: set        => set_method_para_sub
    procedure, public, pass :: add_block  => add_block_sub
    procedure, public, pass :: print      => print_partition_info_fun
    procedure, public, pass :: execute    => partition_blocks_sub
    procedure, public, pass :: rank_block => rank_block_sub
    procedure, public, pass :: return     => return_blocks_sub
    final :: delete_ClassPartition
  end type ClassPartition

!...Constructor of class ClassPartition
  interface ClassPartition
    module procedure new_ClassPartition
  end interface ClassPartition

contains

!=============================================================================80
!
! Method of ClassBlockCutList
!
!=============================================================================80

  !> Destructor of class ClassBlockCutList
  subroutine delete_ClassBlockCutList(this)
    type(ClassBlockCutList), intent(inout) :: this
  continue
    if (associated(this%next)) deallocate(this%next)
  end subroutine delete_ClassBlockCutList

  !> Add new cut to list
  subroutine cut_list_add_sub(this, type, ierr, plane, side, factor)
    class(ClassBlockCutList), intent(inout) :: this
    integer,                  intent(in)    :: type
                              !< 1 => cut by plane;
                              !! 2 => cut by factorization
    integer,                  intent(out)    :: ierr
    type(ClassGeomPlane),     intent(in),  optional :: plane
    integer,                  intent(in),  optional :: side
                              !< -1: negative side; +1: positive side
    integer(i8),              intent(in),  optional :: factor(NDIR)
  !...Declare local variables
    type(ClassBlockCutList), pointer :: new_cut
  continue
    ierr = 0
  !...Create new cut
    allocate(new_cut, stat=ierr)
    if ( ierr /= 0 ) return
    new_cut%type   = type
  !...Check cut information
    if ( new_cut%type == 1 ) then
      if (.not. present(plane))  ierr = 1
      if (.not. present(side))   ierr = 1
      if (      present(factor)) ierr = 1
    else if ( new_cut%type == 2 ) then
      if (      present(plane))  ierr = 1
      if (      present(side))   ierr = 1
      if (.not. present(factor)) ierr = 1
    end if
    if ( ierr == 1 ) return

  !...Save cut information
    if ( new_cut%type == 1 ) then
      if (present(plane)) new_cut%plane = plane
      if (present(side))  new_cut%side = side
    else if ( new_cut%type == 2 ) then
      if (present(factor)) new_cut%factor = factor
    end if
  !...Add new cut to list
    new_cut%next => this%head
    this%head => new_cut
    new_cut%head => this%head
    this%next => this%head
  end subroutine cut_list_add_sub

  !> Delete a node from the list
  subroutine cut_list_delete_node_sub(this, pos)
    class(ClassBlockCutList), intent(inout) :: this
    integer,                  intent(in)    :: pos
                              !< -1: previous node, 1: current top node
  !...Declare local variables
    type(ClassBlockCutList), pointer :: tmp
  continue
    if ( this%is_empty() ) return
    if ( pos == -1 ) then
      tmp => this%head%next
      this%head%next => tmp%next
      tmp%next => null()
      tmp%head => null()
      deallocate(tmp)
    else if ( pos == 1 ) then
      tmp       => this%head
      this%head => this%head%next
      tmp%next  => null()
      tmp%head  => null()
      deallocate(tmp)
    end if
    this%next => this%head
  end subroutine cut_list_delete_node_sub

  !> Check if the list is empty
  logical function cut_list_empty_sub(this)
    class(ClassBlockCutList), intent(in) :: this
  continue
    cut_list_empty_sub = .not. associated(this%head)
  end function cut_list_empty_sub

!=============================================================================80
!
! Method of ClassPartitionBlock
!
!=============================================================================80

  !> Destructor of class ClassPartitionBlock
  subroutine delete_ClassPartitionBlock(this)
    type(ClassPartitionBlock), intent(inout) :: this
  continue
    if (associated(this%blk))  deallocate(this%blk)
    if (associated(this%next)) deallocate(this%next)
  end subroutine delete_ClassPartitionBlock

  !> Add block to partition
  subroutine list_add_block_sub(this, blk, ialloc)
  !...Declare input/output variables
    class(ClassPartitionBlock), intent(inout) :: this
    type(ClassGeomBlock),       pointer       :: blk
    integer,                    intent(out)   :: ialloc
  !...Local variables
    type(ClassPartitionBlock),  pointer       :: new_block
  continue
    allocate(new_block, stat=ialloc)
    if (ialloc /= 0) return
    new_block%blk => blk
  !...Stack new block
    new_block%next => this%head
    this%head => new_block
    new_block%head => this%head
    this%next => this%head
  end subroutine list_add_block_sub

  !> Add blocklist
  subroutine list_add_blocklist_sub(this, blk)
    class(ClassPartitionBlock), intent(inout) :: this
    type(ClassPartitionBlock),  pointer       :: blk
  continue
    blk%next => this%head
    this%head => blk
    blk%head => this%head
    this%next => this%head
  end subroutine list_add_blocklist_sub

  !> Get size of block list
  integer function list_size_fun(this) result(size)
    class(ClassPartitionBlock), intent(in) :: this
    type(ClassPartitionBlock), pointer :: curr
  continue
    size = 0
    curr => this%head
    do while (associated(curr))
      size = size + 1
      curr => curr%next
    end do
  end function list_size_fun

!=============================================================================80
!
! Method of ClassPartition
!
!=============================================================================80

  !> Constructor of class ClassPartition
  function new_ClassPartition(nprocessor, method) result(this)
  !...Declare input/output variables
    integer, intent(in) :: nprocessor
    integer, intent(in) :: method
    type(ClassPartition) :: this
  continue
    this%nproc  = nprocessor
    this%imethod = method
  end function new_ClassPartition

  !> Destructor of class ClassPartition
  subroutine delete_ClassPartition(this)
    type(ClassPartition), intent(inout) :: this
  continue
    if ( allocated(this%maps) ) deallocate(this%maps)
  end subroutine delete_ClassPartition

  !> Set partition method parameters
  subroutine set_method_para_sub(this, alpha, beta, epsilon, data_size, nghost_layer)
  !...Declare input/output variables
    class(ClassPartition), intent(inout)          :: this
    real(dp),              intent(in),   optional :: alpha
                           !< latency (s)
    real(dp),              intent(in),   optional :: beta
                           !< bandwidth of network (bytes/s)
    real(dp),              intent(in),   optional :: epsilon
                           !< tolerance for load imbalance
    real(dp),              intent(in),   optional :: data_size
                           !< data size of each node or cell
    integer,               intent(in),   optional :: nghost_layer
                           !< number of ghost layers
  continue
    if ( present(alpha) )         call this%method%set(alpha=alpha)
    if ( present(beta) )          call this%method%set(beta=beta)
    if ( present(epsilon) )       call this%method%set(epsilon=epsilon)
    if ( present(data_size) )     call this%method%set(data_size=data_size)
    if ( present(nghost_layer) )  call this%method%set(nghost_layer=nghost_layer)
  end subroutine set_method_para_sub

  !> Add block to partition
  subroutine add_block_sub(this, blk, error)
  !...Declare input/output variables
    class(ClassPartition), intent(inout) :: this
    type(ClassGeomBlock),  pointer       :: blk
    type(ClassError),      intent(inout) :: error
  !...Local variables
    type(ClassPartitionBlock), pointer :: new_block
  continue
    call this%blocks%add(blk, error%ialloc)
    if ( error%allocate("Partition Block") ) return
  end subroutine add_block_sub

  !> Print partition information
  function print_partition_info_fun(this, type) result(info)
    use interface_stdlib_string, only : ClassStringMethod
  !...Declare input/output variables
    class(ClassPartition), intent(in)  :: this
    character(len=*),      intent(in)  :: type !< "initial", "final"
    character(len_string), allocatable :: info(:)
  !...Local variables
    type(ClassStringMethod) :: string
    type(ClassPartitionBlock), pointer :: curr
    integer :: ierr
    integer :: i
  continue
    select case (trim(type))
      case ("initial")
        allocate(info(3), stat=ierr)
        if ( ierr /= 0 ) return
        info = ""
        if ( this%imethod == REB ) then
          info(1) = "  Partition Method: REB"
        else if ( this%imethod == IF ) then
          info(1) = "  Partition Method: IF"
        else
          info(1) = "  Partition Method: NOT DEFINED!"
        end if
        info(2) = "  Number of Processors: " // string%from(this%nproc)
        info(3) = "  Number of Blocks: " // string%from(this%blocks%size())
      case ("final")
        allocate(info(5 + this%blocks%size()), stat=ierr)
        if ( ierr /= 0 ) return
        info = ""
        if (this%imethod == REB) then
          info(1) = "  Recursive Partition Times"
        else if ( this%imethod == IF ) then
          info(1) = "  Partition Factorization: " // &
                      string%from(this%cutlist%head%factor(1))  // &
                    " * " // &
                      string%from(this%cutlist%head%factor(2))
          if (associated(this%cutlist%head%next)) then
            info(1) = trim(info(1)) // " + 1"
          end if
        end if
        info(2) = ""
        info(3) = "  Processor  Block_ID  Parent_ID     Size   "
        info(4) = "  ---------  --------  ---------  ----------"
        curr => this%blocks%head
        do i = 1, this%blocks%size()
          info(4 + i) = "  " // &
                        string%padl(string%from(this%mgraph%epart(i)), 4) // &
                        "  " // &
                        string%padl(string%from(i), 9) // &
                        "  " // &
                        string%padl(string%from(curr%map%parent_block), 9) // &
                        "  " // &
                        string%padl(string%from(curr%blk%nvert(1)), 9) // &
                        " * " // &
                        string%padl(string%from(curr%blk%nvert(2)), 6)
          curr => curr%next
        end do
        info(5 + this%blocks%size()) = "  ---------  --------  ---------  ----------"
      case default
    end select
  end function print_partition_info_fun

  !> Partition blocks
  subroutine partition_blocks_sub(this, error)
  !...Declare input/output variables
    class(ClassPartition), intent(inout) :: this
    class(ClassError),     intent(inout) :: error
  !...Declear local variables
    type(ClassPartitionBlock), pointer :: curr
  continue
  !...Block cut
    select case (this%imethod)
      case (IF)
        ! compute workload for current block
        call this%method%load_balance(this%nproc, this%blocks, error)
        if ( error%occur() ) return
        curr => this%blocks%head
        do while (associated(curr))
        !...Compute cut map of current block
          call this%method%IF(curr, this%cutlist, t0_min=dp_inf, &
                              t1_cut=0.0_dp, error=error)
          if ( error%occur() ) return
          !...Cut block by cutlist
          call this%method%cut_block(curr, this%cutlist%head, curr%blk%id(), error)
          if ( error%occur() ) return
          this%blocks%head => curr%head
        !...Next block
          curr => curr%next
        end do

      case (REB)
        ! call this%method%REB()
        call error%set(.true., info="Not complete REB method yet!")
        return
      case default
        call error%set(.true., info="Not set partition method!")
        return
    end select
  !...Set metis graph and partition
    call this%rank_block(error)
    if ( error%occur() ) return

    call this%method%build_metis_graph(this%blocks, this%mgraph, error)
    if ( error%occur() ) return

    call this%method%partition_metis_graph(this%mgraph, this%nproc, error)
    if ( error%occur() ) return
  end subroutine partition_blocks_sub

  !> Rank block id for partition
  subroutine rank_block_sub(this, error)
    class(ClassPartition), intent(inout) :: this
    class(ClassError),     intent(inout) :: error
  !...Declare local variables
    type(ClassPartitionBlock), pointer :: curr
    integer :: nblk
  continue
    curr => this%blocks%head
    nblk = 0
    do while (associated(curr))
      nblk = nblk + 1
      curr => curr%next
    end do
  !...Allocate block map
    allocate(this%maps(nblk), stat=error%ialloc)
    if ( error%allocate("Partition Map") ) return
  !...Reset block id
    curr => this%blocks%head
    do while (associated(curr))
      call curr%blk%set(id=nblk)
      this%maps(nblk)%parent_block = curr%map%parent_block
      nblk = nblk - 1
      curr => curr%next
    end do
  end subroutine rank_block_sub

  !> Return blocks
  subroutine return_blocks_sub(this, blks, error)
  !...Declare input/output variables
    class(ClassPartition), intent(inout) :: this
    type(ClassGeomBlock),  intent(inout), allocatable:: blks(:)
    type(ClassError),      intent(inout) :: error
  !...Declare local variables
    integer :: nblk
    type(ClassPartitionBlock), pointer :: curr
  continue
    curr => this%blocks%head
    nblk = 0
    do while (associated(curr))
      nblk = nblk + 1
      curr => curr%next
    end do
  !...Allocate return blocks
    allocate(blks(nblk), stat=error%ialloc)
    if ( error%allocate("New Partition Block") ) return
  !...Save blocks
    curr => this%blocks%head
    do while (associated(curr))
      blks(nblk) = curr%blk
      nblk = nblk - 1
      curr => curr%next
    end do
  end subroutine return_blocks_sub

!=============================================================================80
!
! Method of ClassPartitionMethod
!
!=============================================================================80

  !> Set partition method parameters
  subroutine set_method_sub(this, alpha, beta, epsilon, data_size, nghost_layer)
  !...Declare input/output variables
    class(ClassPartitionMethod), intent(inout)          :: this
    real(dp),                    intent(in),   optional :: alpha
    real(dp),                    intent(in),   optional :: beta
    real(dp),                    intent(in),   optional :: epsilon
    real(dp),                    intent(in),   optional :: data_size
    integer,                     intent(in),   optional :: nghost_layer
  continue
    if ( present(alpha) )        this%alpha        = alpha
    if ( present(beta) )         this%beta         = beta
    if ( present(epsilon) )      this%epsilon      = epsilon
    if ( present(data_size) )    this%data_size    = data_size
    if ( present(nghost_layer) ) this%nghost_layer = nghost_layer
  end subroutine set_method_sub

  !> Partition block by Integer Factorization method (IF)
  recursive subroutine integer_factorize_block(this, curr, cutlist, t0_min, &
                                                t1_cut, error)
  !...Declare input/output variables
    class(ClassPartitionMethod), intent(inout) :: this
    type(ClassPartitionBlock),   pointer       :: curr
    type(ClassBlockCutList),     intent(inout) :: cutlist
    real(dp),                    intent(in)    :: t0_min
                                 !< minimum communication cost before,
                                 !! initial is infinity
    real(dp),                    intent(in)    :: t1_cut
                                 !< communication cost of sub-block cut off,
                                 !! initial is zero because no cut block
    type(ClassError),            intent(inout) :: error
  !...Declare local variables
    integer     :: nproc   = 1
  ! ********************* Factorization ********************* !
    integer(i8)              :: nfact, ifact
    integer(i8), allocatable :: fact(:,:)   !< Factorization
    integer(i8)              :: fact_(NDIR) !< temp
    real(dp)                 :: t1     !< current communication cost
    real(dp)                 :: t1_min !< current minimum communication cost
    real(dp)                 :: t2_cut !< new communication cost of sub-block cut off
  ! ************************ Cut Block ********************** !
    real(dp)                           :: wcut !< percent of workload to be cut (%)
    integer                            :: cut_dir !< cut direction
    integer(i8)                        :: cut_pos !< cut position
    type(ClassGeomPlane)               :: cut_plane
    integer                            :: side !< side of cut plane
    type(ClassPartitionBlock), pointer :: remain_ !< remaining block
  ! ********************************************************* !
  continue
  !...Base case 1
    ! Get number of graph to be partitioned (=> number of processors)
    nproc = ceiling(curr%workload)
    if ( nproc == 1 ) return
  !...Base case 2
    ! Faractorization
    call this%factorization(nproc, nfact, fact, error%ialloc)
    if (error%allocate("Factorize Number of Processor")) return
    ! Compute minimum communication cost of factorization
    t1_min = dp_inf
    do ifact = 1, nfact
      t1 = this%t_b2b_if(curr%blk, fact(:,ifact))
      if ( t1 < t1_min ) then
        t1_min = t1
        fact_ = fact(:,ifact)
      end if
    end do
    ! privious cut is better, end cut
    if ( t0_min < max(t1_min, t1_cut) ) then
      ! cut B by t0(nx ny nz)
      call cutlist%delete(pos=1)
      return
    end if
  !...Recursive case
    ! save current cut information
    call cutlist%delete(pos=-1)
    call cutlist%add(type=2, ierr=error%ialloc, factor=fact_)
    if ( error%allocate("Cut List: Integer Factorization") ) return
    ! new cut
    allocate(remain_, stat=error%ialloc)
    if (error%allocate("Cut Block Node")) return
    ! Cut off a small block
    ! etc: workload = 3.7, wcut = 0.7 / 3.7; workload = 4.0, wcut = 1 / 4
    ! (implement) wcut : wrem = 0.7 : 3.0
    remain_%workload = real(ceiling(curr%workload - 1.0_dp))
    wcut             = 1.0_dp - remain_%workload / curr%workload
    call this%find_min_cut(curr%blk, wcut, cut_dir, cut_pos, t2_cut)
    ! TODO: (NOT-implement) wcut : wrem = 3.0 : 0.7, compare this two cases
    ! wcut = 1 - wcut
    ! call this%find_min_cut(curr%blk, wcut, cut_dir, cut_pos)
    ! cut block by position finded
    cut_plane = ClassGeomPlane(dir=cut_dir, pos=cut_pos)
    if ( wcut < 0.5 ) then
      side = +1
    else
      side = -1
    end if
    call curr%blk%cut(cut_plane, remain_%blk, side, error%ialloc)
    if (error%allocate("Cut Block")) return
    ! save new cut information
    call cutlist%add(type=1, ierr=error%ialloc, plane=cut_plane, side=-side)
    if ( error%allocate("Cut List: Plane") ) return
    ! factorize remaining block
    call this%IF(remain_, cutlist, t0_min=max(t1_min, t1_cut), &
                  t1_cut= t2_cut, error=error)
  !...Deallocate pointer
    if (associated(remain_)) deallocate(remain_)
  end subroutine integer_factorize_block

  !> Partition block by recursive edge bisection method (REB)
  recursive subroutine recursive_edge_bisection(this)
    class(ClassPartitionMethod), intent(inout) :: this
  continue
  end subroutine recursive_edge_bisection

  !> Factorize number to 3 integers
  subroutine factorize_number_sub(num, nfact, fact, ialloc)
  !...Declare input/output variables
    integer(i4), intent(in)  :: num
    integer(i8), intent(out) :: nfact
    integer,     intent(out) :: ialloc
    integer(i8), intent(inout), allocatable :: fact(:,:)
  !...Declare local variables
    integer :: n1, n2, n3
  continue
    nfact = 0
    do n1 = 1, num
      if ( mod(num, n1) /= 0 ) cycle
      do n2 = 1, num/n1
        if ( mod(num/n1, n2) == 0 ) then
          n3 = num / (n1 * n2)
          nfact = nfact + 1
        end if
      end do
    end do
  !...Allocate fact and save
    if (allocated(fact)) deallocate(fact)
    allocate(fact(NDIR,nfact), stat=ialloc)
    if (ialloc /= 0) return
    nfact = 0
    do n1 = 1, num
      if ( mod(num, n1) /= 0 ) cycle
      do n2 = 1, num/n1
        if ( mod(num/n1, n2) == 0 ) then
          n3 = num / (n1 * n2)
          nfact = nfact + 1
          fact(1,nfact) = n1
          fact(2,nfact) = n2
          fact(3,nfact) = n3
        end if
      end do
    end do
  end subroutine factorize_number_sub

  !> Maximum communication cost among nx*ny*nz sub-blocks
  !  t_b2b = latency * edge cuts + communication volume / bandwidth
  !        = alpha   * ncut      + vol_comm / beta
  !  vol_comm = sum(face_size * ghost_layers * data_size)
  !           = sum(face_size) * ghost_layers * data_size
  !
  ! TODO: NOT considering the cases
  !    1. a face contains both cut and boundary
  !          => ignore case that the block containing max face size is not the mid block
  !    2. cut face connect multi-block
  !          => ignore latency
  !  The question is if considering, partition must be executed first.
  !
  function t_b2b_if_fun(this, blk, fact) result(t_b2b)
  !...Declare input/output variables
    class(ClassPartitionMethod), intent(inout) :: this
    type(ClassGeomBlock),        intent(inout) :: blk
    integer(i8),                 intent(inout) :: fact(NDIR)
                                 !< factorization: number of parition
    real(dp) :: t_b2b !< cost of factorization
  !...Declare local variables
    integer :: iconn, dir
    integer :: conn_curr !< count of current connectivity in block at direction
    integer :: ncut_ijk, ncut_tot
    integer(i8) :: part_verts(NDIR)
    integer(i8) :: A_cut   !< sum of cut face size
    integer(i8) :: V_block !< volume of block
    integer(i8) :: conn_pos  !< position of connectivity
    type(ClassGeomFace), pointer :: conn !< connectivity
  continue
    t_b2b = dp_inf
  !...Check if block need to be cut off
    do dir = 1, NDIR
      ! ncell < npart
      if ( blk%nvert(dir) - 1 < fact(dir) ) return
    end do
  !...Compute max partition
    do dir = 1, NDIR
      ! nodal-centered grid, ncell = nvert - 1, part_vert = ncell / npart + 1
      part_verts(dir) = ceiling(real(blk%ncell(dir)) / real(fact(dir))) + 1
    end do
    V_block = part_verts(1) * part_verts(2) * part_verts(3)
  !...Compute max sum face size of single cut-block
    A_cut = 0
    ncut_tot  = 0
    do dir = 1, NDIR
      ! count number of present connectivity at direction
      conn_curr = 0
      conn_pos  = 0
      do iconn = 1, blk%nconn()
        conn => blk%conn(iconn)
        if ( conn%normal() == dir ) then
          if ( conn%position() /= conn_pos  ) then
            conn_curr = conn_curr + 1
            conn_pos  = conn%position()
            if ( conn_curr == 2 ) exit
          end if
        end if
      end do
      ! Compute total cut face at current direction
      ncut_ijk = conn_curr + fact(dir) - 1
      ! Compute max number of cut face in block
      if ( ncut_ijk > 2 ) ncut_ijk = 2
      ! compute max sum face size in block
      ncut_tot  = ncut_tot  + ncut_ijk
      A_cut = A_cut + ncut_ijk * V_block / part_verts(dir)
    end do
  !...Compute max communication cost
    t_b2b = this%alpha * dble(ncut_tot) + &
            dble(A_cut * this%nghost_layer) * this%data_size / this%beta
  end function t_b2b_if_fun

  !> Find min cut:
  !  dt_cut = t_b2b_cut - t_b2b_shared
  subroutine find_min_cut_fun(this, blk, wcut, cut_dir, cut_pos, t_b2b, part)
  !...Declare input/output variables
    class(ClassPartitionMethod), intent(inout) :: this
    type(ClassGeomBlock),        intent(inout) :: blk      !< block to be cut
    real(dp),             intent(in)  :: wcut
                          !< Workload to be cut off (%),
                          !! this is different from literature for multi-block.
                          !! if block allocated 3.5 processors,
                          !! there are 0.5 processors task to be cut,
                          !! the workload is 0.5 / 3.5
    integer,              intent(out) :: cut_dir  !< cut direction
    integer(i8),          intent(out) :: cut_pos  !< cut position
    real(dp),             intent(out) :: t_b2b    !< communication cost
    integer,              intent(in), optional :: part !< current partition
  !...Declare local variables
    real(dp) :: dt_cut          !< communication cost of cutting a block
    real(dp) :: dt_cut_l, dt_cut_r
    real(dp) :: dt_min  !< minimum communication cost
    integer  :: dir, iconn
    integer  :: ccut_l, ccut_r !< count of left and right cuts
    integer(i8) :: A_cut, A_l, A_r !< size of cut face
    integer(i8) :: V_block         !< volume of block
    integer(i8) :: pos, posFloor, posCeil !< cut position
    type(ClassGeomPlane)  :: cut_plane
    type(ClassGeomFace)   :: conn_c   !< connectivity after cut
    type(ClassGeomFace), pointer :: conn !< connectivity
  continue
    dt_min = dp_inf
  !...Cut workload = 0
    if ( wcut < dp_eps ) return
  !...Loop over directions
    V_block = blk%size()
    do dir = NDIR, 1, -1
      A_cut = V_block / blk%nvert(dir)
    !...Compute position of cut
      if ( blk%nvert(dir) == 2 ) cycle
      posFloor = blk%cut_position(wcut * (1 - this%epsilon), dir, side=-1)
      posCeil  = blk%cut_position(wcut * (1 + this%epsilon), dir, side=+1)
      do pos = posFloor, posCeil
      !...Cut connectivity and count cuts
        cut_plane = ClassGeomPlane(dir=dir, pos=pos)
        ccut_l = 1
        A_l    = A_cut
        do iconn = 1, blk%nconn()
          conn => blk%conn(iconn)
          conn_c = conn%cut(cut_plane, side=-1)
          if ( conn_c%empty() ) cycle
          ccut_l = ccut_l + 1
          A_l    = A_l + conn_c%size()
        end do
        ccut_r = 1
        A_r    = A_cut
        do iconn = 1, blk%nconn()
          conn => blk%conn(iconn)
          conn_c = conn%cut(cut_plane, side=+1)
          if ( conn_c%empty() ) cycle
          ccut_r = ccut_r + 1
          A_r    = A_r + conn_c%size()
        end do
      !...Compute communication cost
        dt_cut_l = this%alpha * dble(ccut_l) + dble(A_l * this%nghost_layer) &
                                             * this%data_size / this%beta
        dt_cut_r = this%alpha * dble(ccut_r) + dble(A_r * this%nghost_layer) &
                                             * this%data_size / this%beta
        dt_cut = max(dt_cut_l, dt_cut_r)
      !...Find min cut
        if ( dt_cut < dt_min ) then
          dt_min = dt_cut
          cut_dir = dir
          cut_pos = pos
        end if
      end do
    end do
  !...Output minimum communication cost
    t_b2b = dt_min
  end subroutine find_min_cut_fun

  !> Load balance
  !  TODO: this algorithm is not good
  !   For multi-block, all blocks cutted off will be gather to one processor
  !   and result in imbalance because of too many edges cuts
  subroutine load_balance_sub(nproc, block_list, error)
  !...Declare input/output variables
    integer,                     intent(in)    :: nproc
    type(ClassPartitionBlock),   intent(inout) :: block_list
    type(ClassError),            intent(inout) :: error
  !...Declare local variables
    type(ClassPartitionBlock), pointer :: curr
    real(dp) :: v_total = 0.0_dp !< total volume of all blocks
    real(dp) :: w_cut   = 0.0_dp !< workload to be cut off
    integer  :: dir !< direction
    integer  :: proc_alloc   = 0 !< number of processors allocated
    logical  :: flag_balance = .false.
    real(dp) :: workload_sum = 0.0_dp
  continue
    curr => block_list%head
    do while (associated(curr))
      v_total = v_total + curr%blk%size()
      curr => curr%next
    end do
    curr => block_list%head
    do while (associated(curr))
    !...Compute workload of current block
      curr%workload = real(nproc) * real(curr%blk%size()) / real(v_total)
    !...Check workload
      !
      ! At least one direction, min number of cell to be cut off > 0
      !
      !   * -- * -- * -- * ...   nvert = 101, ncell = 100
      !     workload  wcut      cellcut                 balance workload
      !        3.98,  0.02,   100 * 0.02 / 3.98 = 0.50 => 0 = 0    4.0
      !        3.5,   0.5,    100 * 0.5  / 3.5  = 14.3 => 14 > 0   3.5
      !        3.03,  0.03,   100 * 0.01 / 3.03 = 0.99 => 0 = 0    3.0
      !
      w_cut = min(curr%workload - real(floor(curr%workload)), &
                  real(ceiling(curr%workload) - curr%workload))
      flag_balance = .true.
      do dir = 1, NDIR
        if ( int(real(curr%blk%ncell(dir)) * w_cut / curr%workload) > 0 ) &
          flag_balance = .false.
      end do
      if ( flag_balance ) curr%workload = real(nint(curr%workload))
    !...Next block
      curr => curr%next
    end do
    !...Check total processor allocated
    curr => block_list%head
    do while (associated(curr))
      workload_sum = workload_sum + curr%workload
      curr => curr%next
    end do
    ! total graph to be cut = ceiling(workload_sum)
    if ( ceiling(workload_sum) < nproc ) then
      call error%set(.true., info="Graphs cut for processors NOT enough!")
    end if
  end subroutine load_balance_sub

  !> Cut block by CutList
  recursive subroutine cut_block_sub(curr, cutlist, parent_block, error)
  !...Declare input/output variables
    type(ClassPartitionBlock), intent(inout) :: curr
    type(ClassBlockCutList),   pointer       :: cutlist
    integer,                   intent(in)    :: parent_block
    type(ClassError),          intent(inout) :: error
  !...Declare local variables
    integer     :: iblk
    type(ClassPartitionBlock), pointer :: cut_       !< new node of partition block list
    type(ClassGeomBlock),      pointer :: cut_blk(:) !< new node of partition block list
  continue
    cut_    => null()
    cut_blk => null()
  !...Recursive Case
    if ( associated(cutlist%next) ) then
      call cut_block_sub(curr, cutlist%next, parent_block, error)
    end if
  !...Base Case
    select case (cutlist%type)
      case (1) !< cut block by plane
        allocate(cut_, stat=error%ialloc)
        if (error%allocate("Cut Block Node")) return
      !...Cut off a small block
        call curr%blk%cut_off(cutlist%plane, curr%blk, cut_%blk, cutlist%side, &
                              error%ialloc)
        if (error%allocate("Cut Block")) return
      !...Save map information
        curr%map%parent_block = parent_block
        cut_%map%parent_block = parent_block
      !...Save new cut information (stack)
        cut_%next => curr%head
        curr%head => cut_
        cut_%head => curr%head
        nullify(cut_)

      case (2) !< cut block by factorization
        call curr%blk%cut_off(cutlist%factor, curr%blk, cut_blk, &
                              error%ialloc)
        curr%map%parent_block  = parent_block
        ! curr%map%parent_origin not change
        do iblk = 1, size(cut_blk)
          allocate(cut_, stat=error%ialloc)
          if (error%allocate("Cut Block Node")) return
          cut_%blk => cut_blk(iblk)

          cut_%map%parent_block  = parent_block
        !...Save new cut information (stack)
          cut_%next => curr%head
          curr%head => cut_
          cut_%head => curr%head
          nullify(cut_)
        end do
        nullify(cut_blk)

      case default
        call error%set(.true., info="Invalid cutlist type!")
        return
    end select
  !...Deallocate local variables
    if ( associated(cut_) )    deallocate(cut_)
    if ( associated(cut_blk) ) deallocate(cut_blk)
  end subroutine cut_block_sub

  !> Build metis graph
  subroutine build_metis_graph_sub(block_list, mgraph, error)
    type(ClassPartitionBlock), intent(inout) :: block_list
    type(ClassMetisGraph),     intent(inout) :: mgraph
    class(ClassError),         intent(inout) :: error
  !...Declare local variables
    type(ClassPartitionBlock),  pointer :: curr
    integer              :: nadj, iblk
    integer, allocatable :: adj_id(:)
  continue
  !...Set metis graph default value
    mgraph%ncon = 1
  !...Count number of vertex and adjacent
    curr => block_list%head
    nadj = 0
    mgraph%nvtxs = 0
    do while (associated(curr))
      mgraph%nvtxs = mgraph%nvtxs + 1
      ! Count number of adjacent
      ! TODO: delete repeat connection
      nadj = nadj + curr%blk%nconn()
      curr => curr%next
    end do
  !...Allocate metis graph
    allocate(mgraph%vert_id(mgraph%nvtxs), stat=error%ialloc)
    if ( error%allocate("Metis Graph Vertex ID") ) return

    allocate(mgraph%xadj(mgraph%nvtxs+1), stat=error%ialloc)
    if ( error%allocate("Metis Graph Xadj") ) return

    allocate(mgraph%adjncy(nadj), stat=error%ialloc)
    if ( error%allocate("Metis Graph Adjncy") ) return

    allocate(mgraph%epart(mgraph%nvtxs), stat=error%ialloc)
    if ( error%allocate("Metis Graph Partition Vector") ) return

  !...Set adjacent
    mgraph%xadj(1) = 1

    curr => block_list%head
    iblk = 0
    do while (associated(curr))
      iblk = iblk + 1
      mgraph%vert_id(iblk) = curr%blk%id()
      mgraph%xadj(iblk+1) = mgraph%xadj(iblk) + curr%blk%nconn()
      mgraph%adjncy(mgraph%xadj(iblk):mgraph%xadj(iblk+1)-1) = &
              curr%blk%conn_blk_id(error%ialloc)
      if (error%allocate("Donor block ID")) return
      curr => curr%next
    end do
  end subroutine build_metis_graph_sub

  !> Partition block by metis graph
  subroutine metis_graph_partition_sub(mgraph, nproc, error)
    use interface_metis_c
  !...Declare input/output variables
    type(ClassMetisGraph), intent(inout) :: mgraph
    integer(idx_t),        intent(in)    :: nproc
    type(ClassError),      intent(inout) :: error
  !...Local variables
    integer(idx_t) :: options(METIS_NOPTIONS)

    integer(idx_t)              :: nvtxs     !< number of vertices
    integer(idx_t)              :: ncon      !< number of balancing constraints
    integer(idx_t), allocatable :: xadj(:)   !< from mgraph
    integer(idx_t), allocatable :: adjncy(:) !< from mgraph
    integer(idx_t)              :: nparts    !< number of parts to partition the graph
    integer(idx_t), allocatable :: epart(:)
                                !< output: stores the partition vector of the graph
    integer(idx_t)              :: objval
                                !< output: stores the edge-cut or
                                !<         the total communication volume

    integer(idx_t), allocatable :: vwgt(:)   !< NULL
    integer(idx_t), allocatable :: vsize(:)  !< NULL
    integer(idx_t), allocatable :: adjwgt(:) !< NULL
    real(real_t),   allocatable :: tpwgts(:) !< NULL
    real(real_t),   allocatable :: ubvec(:)  !< NULL

    integer(idx_t) :: ierr
    integer(idx_t) :: iproc
  continue
  !...Set METIS parameters
    ! number of vertices in the graph
    nvtxs  = int(mgraph%nvtxs, kind=idx_t)
    ! number of balancing constraints (>= 1)
    !    = 1   not consider data difference of each vertice
    ncon   = int(mgraph%ncon, kind=idx_t)
    ! adjacency structure of the graph
    xadj   = int(mgraph%xadj, kind=idx_t)
    adjncy = int(mgraph%adjncy, kind=idx_t)
    ! number of parts to partition the graph
    nparts = int(nproc, kind=idx_t)
    ! allocate memory for partition vector
    allocate(epart(nparts), stat=error%ialloc)
    if ( error%allocate("Metis Graph Partition Vector") ) return
  !...METIS Graph Partition
    ierr = METIS_SetDefaultOptions(options)
    if (ierr /= METIS_OK) then
      call error%set(.true., info="Set METIS Default Options Error!")
      return
    end if
    ! Set numbering to Fortran style (begins at 1)
    options(METIS_OPTION_NUMBERING) = 1
    ! Force contiguous partitions
    options(METIS_OPTION_CONTIG) = 1
    ! Explicitly minimize the maximum connectivity
    options(METIS_OPTION_MINCONN) = 1
    ! Method: k-way partitioning
    ! Weight: non-weighted
    ! Min:    communication volume
    options(METIS_OPTION_OBJTYPE) = METIS_OBJTYPE_CUT
    ! Execution
    ! TODO: there are some bug when nparts = nproc
    ! ierr = metis_partgraphkway(nvtxs,ncon,xadj,adjncy,vwgt,vsize, &
    !                            adjwgt,nparts,tpwgts,ubvec,options, &
    !                            objval,epart)
    if (ierr /= METIS_OK) then
      call error%set(.true., info="Error METIS Graph Partition!")
      return
    end if
  !...Save partition information
    ! TODO: modify code after bug fixed
    objval = 0
    do iproc = 1, nproc
      epart(iproc) = iproc - 1
    end do
    mgraph%objval = objval
    mgraph%epart  = epart
  end subroutine metis_graph_partition_sub

end module method_partition
