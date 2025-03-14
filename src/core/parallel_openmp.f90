!=============================================================================80
!
!> Parallel computing: OpenMP
!
!=============================================================================80

module parallel_openmp

  use kind_parameter, only : dp

  implicit none

!...Define scope
  private
  public  :: ParallelOpenMP

!...Declare class
  type :: ParallelOpenMP
    ! integer, private  :: ncores
    ! integer, private  :: nthreads
  contains
    procedure, public, pass :: is_host => is_host_process
                       !< Check if thread is master for printing information
    final :: delete_ParallelOpenMP  !< Destructor
  end type ParallelOpenMP

  !> Constructor of class ParallelOpenMP
  interface ParallelOpenMP
    module procedure new_ParallelOpenMP
  end interface ParallelOpenMP

contains

  !> Constructor of class ParallelOpenMP
  function new_ParallelOpenMP() result(this)
  !...Declare input/output variables
    type(ParallelOpenMP) :: this
  continue
    ! this%next = null()
  end function new_ParallelOpenMP

  !> Destructor of class ParallelOpenMP
  subroutine delete_ParallelOpenMP(this)
  !...Deallocating pointer attribute
    type(ParallelOpenMP), intent(inout) :: this
  continue
    ! if (associated(this%next)) deallocate(this%next)
  end subroutine delete_ParallelOpenMP

  !> Check if process is master
  logical function is_host_process(this)
  !...Declare input/output variables
    class(ParallelOpenMP), intent(in) :: this
  continue
    is_host_process = .true.
  end function is_host_process

end module parallel_openmp
