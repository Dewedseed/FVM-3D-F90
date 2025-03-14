!=============================================================================80
!
!> Simple grid generator.
!
!=============================================================================80

module method_gridgen

  use kind_parameter,   only : len_short, dp, i4, i8
  use global_type_defs, only : TypeDefs

  use global_error,    only : ClassError


  implicit none

!...Define scope
  private

  public :: ClassGridGenerate

!...Declare local variables
  integer(i4), parameter :: NDIR   = 3 !< i, j, k
  integer(i4), parameter :: NCOORD = 3 !< x, y, z

!...Declare class
  type :: ClassGridGenerate
    character(len_short), private :: geoType = "square"
    real(dp),             private :: geoSize(NCOORD) = 1.0
    character(len_short), private :: gridType = "nodal-centered"
    integer(i8),          private :: gridSize(NDIR) = 2
  contains
    procedure, public, pass :: input       => gridgen_set_sub
    procedure, public, pass :: check_input => gridgen_check_input_sub
    procedure, public, pass :: uniform     => gridgen_uniform_sub
    final :: delete_ClassGridGenerate
  end type ClassGridGenerate

contains

  !> Destructor of class ClassGridGenerate
  subroutine delete_ClassGridGenerate(this)
  !...Declare input/output variables
    type(ClassGridGenerate), intent(inout) :: this
  continue
  end subroutine delete_ClassGridGenerate

  !> Set grid generator parameters
  subroutine gridgen_set_sub(this, geoType, geoSize, gridType, gridSize)
  !...Declare input/output variables
    class(ClassGridGenerate), intent(inout) :: this
    character(len=*),  intent(in), optional :: geoType
    real(dp),          intent(in), optional :: geoSize(NDIR)
    character(len=*),  intent(in), optional :: gridType
    integer(i8),       intent(in), optional :: gridSize(NDIR)
  continue
    if ( present(geoType) )   this%geoType  = geoType
    if ( present(geoSize) )   this%geoSize  = geoSize
    if ( present(gridType) )  this%gridType = gridType
    if ( present(gridSize) )  this%gridSize = gridSize
  end subroutine gridgen_set_sub

  !> Check input parameters
  subroutine gridgen_check_input_sub(this, error)
  !...Declare input/output variables
    class(ClassGridGenerate), intent(inout) :: this
    type(ClassError),         intent(out)   :: error
  continue
    if ( trim(this%geoType) .ne. "square" ) then
      call error%set(.true., "Invalid geometry type: " // trim(this%geoType))
      return
    end if
    if ( .not. all(this%geoSize >= -1.0E-6) ) then
      call error%set(.true., "Invalid geometry size.")
      return
    end if
    if ( .not. all(this%gridSize >= 0) ) then
      call error%set(.true., "Invalid grid size.")
      return
    end if
  end subroutine gridgen_check_input_sub

  !> Uniform grid generator
  subroutine gridgen_uniform_sub(this, ndim, gsize, coord, cell_size, error)
  !...Declare input/output variables
    class(ClassGridGenerate), intent(inout) :: this
    integer(i4),              intent(in)    :: ndim
      !< number of dimensions
    integer(i8),              intent(out)   :: gsize(NDIR)
      !< grid size of each ijk directions
    real(dp),                 intent(inout), pointer :: coord(:,:,:,:)
      !< coordinates
    real(dp),                 intent(inout) :: cell_size(NDIR)
    type(ClassError),         intent(out)   :: error
  !...Declare local variables
    integer(i8) :: i, j, k
    real(dp)    :: dx, dy, dz
  continue
  !...Get grid size
    gsize         = 2
    gsize(1:ndim) = this%gridSize(1:ndim)
  !...Allocate coordinates
    if ( associated(coord) ) then
      call error%set(.true., "Grid coordinates already allocated.")
      return
    end if
    allocate(coord(gsize(1), gsize(2), gsize(3), NCOORD), stat=error%ialloc)
    if ( error%allocate("Grid Coordinates") ) return
    coord = 0.0
  !...Set geometry
    select case (trim(this%geoType))
      case ("square")
        this%geoSize(2) = this%geoSize(1)
        this%geoSize(3) = 1.0_dp
      case default
        call error%set(.true., "Error geometry type: " // trim(this%geoType))
        return
    end select
  !...Generate coordinates
    dx = this%geoSize(1) / real(gsize(1)-1,dp)
    dy = this%geoSize(2) / real(gsize(2)-1,dp)
    dz = this%geoSize(3) / real(gsize(3)-1,dp)
    do k = 1, gsize(3)
      do j = 1, gsize(2)
        do i = 1, gsize(1)
          coord(i,j,k,1) = dx * real(i-1,dp)
          coord(i,j,k,2) = dy * real(j-1,dp)
          coord(i,j,k,3) = dz * real(k-1,dp)
        end do
      end do
    end do
  !...Save Cell size
    cell_size(1) = dx
    cell_size(2) = dy
    cell_size(3) = dz
  end subroutine gridgen_uniform_sub

end module method_gridgen
