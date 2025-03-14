!=============================================================================80
!
!> Parallel compute: OpenMPI
!
! OpenMPI 5.0 API
!   https://docs.open-mpi.org/en/v5.0.x/man-openmpi/index.html
! OpenMPI Types Definitions
!
!
!=============================================================================80

module parallel_mpi

  use global_devlog, only : devlog, LogLevel

  implicit none

! include openmpi
  include "mpif.h"

!...Define scope
  private

  public :: ParallelMPI

!...Declare local class
  type :: ParallelMPI
    integer, private  :: unit = 6
    integer, private  :: nproc = 0 !< number of process
    integer, private  :: irank = 0 !< process rank
    integer, private  :: ierr  = MPI_SUCCESS !< error
  contains
    procedure, pass, public :: start     => start_process
    procedure, pass, public :: end       => end_process
    procedure, pass, public :: is_host   => is_host_process
    !< Check if process is master for printing information
    procedure, pass, public :: is_rank   => is_rank_process
    procedure, pass, public :: stop      => stop_process
    procedure, pass, public :: barrier   => barrier_sub
    procedure, pass, public :: nprocess  => nprocess_fun
    procedure, pass, private :: check_error => check_error_sub
    procedure, pass, private :: occur_error => occur_error_fun
    procedure, pass, private :: error_msg   => error_msg_fun
    final :: delete_ParallelMPI  !< Destructor
  end type ParallelMPI

  !> Constructor of class ParallelMPI
  interface ParallelMPI
    module procedure new_ParallelMPI
  end interface ParallelMPI

contains

  !> Constructor of class ParallelMPI
  function new_ParallelMPI(terminal_unit) result(mpi)
    integer, intent(in), optional  :: terminal_unit
    type(ParallelMPI)              :: mpi
  continue
    mpi%unit = terminal_unit
  end function new_ParallelMPI

  !> Destructor of class ParallelMPI
  subroutine delete_ParallelMPI(mpi)
    type(ParallelMPI), intent(inout) :: mpi
  end subroutine delete_ParallelMPI

  !> Start parallel computing
  subroutine start_process(this)
    class(ParallelMPI), intent(inout) :: this
  continue
    call mpi_init(this%ierr)
    if (this%occur_error()) write (this%unit,*) "** " // trim(this%error_msg())
    call mpi_comm_rank(MPI_COMM_WORLD, this%irank, this%ierr)
    call mpi_comm_size(MPI_COMM_WORLD, this%nproc, this%ierr)
    call this%check_error()
  !...Synchronize all process
    if (this%is_host()) write (this%unit,*)
    if (this%is_host()) write (this%unit,*)
    if (this%is_host()) write (this%unit,*) "OpenMPI Version: 5.0.6"
    if (this%is_host()) write (this%unit,*)
    call mpi_barrier(MPI_COMM_WORLD, this%ierr)
    write (this%unit,*) "-- From Process ", this%irank, " of ", this%nproc
    call flush(this%unit)
  end subroutine start_process

  !> End parallel computing
  subroutine end_process(this)
    class(ParallelMPI), intent(inout) :: this
  continue
    call mpi_finalize(this%ierr)
  end subroutine end_process

  !> Check if process is master for printing information
  function is_host_process(this) result(is_host)
    class(ParallelMPI), intent(in) :: this
    logical :: is_host
  continue
    is_host = (this%irank == 0)
  end function is_host_process

  !> Check if process is master for printing information
  function is_rank_process(this, rank) result(is_rank)
    class(ParallelMPI), intent(in) :: this
    integer,            intent(in) :: rank
    logical :: is_rank
  continue
    is_rank = (this%irank == rank)
  end function is_rank_process

  !> stop all process
  subroutine stop_process(this, info)
    class(ParallelMPI), intent(inout) :: this
    character(len=*),   intent(in)    :: info !< stop information
  continue
    call this%barrier()
  !...Print stop information
    call flush(this%unit)
    write (this%unit,*)
    write (this%unit,*) "** " // trim(info)
    write (this%unit,*)
    call flush(this%unit)
  !...Close logfile
    if (this%is_host()) then
      call devlog%print(where = "parallel_mpi::stop_process", &
                        message = info, &
                        level = LogLevel%message)
      call devlog%end()
    end if
  !...Abort
    call mpi_abort(MPI_COMM_WORLD, this%irank, this%ierr)
    call mpi_finalize(this%ierr)
    stop
  end subroutine stop_process

  !> Check if error occur
  logical function occur_error_fun(this) result(occur)
    class(ParallelMPI), intent(in) :: this
  continue
    occur = (this%ierr /= MPI_SUCCESS)
  end function occur_error_fun

  !> Get error message
  function error_msg_fun(this) result(errmsg)
    class(ParallelMPI), intent(inout) :: this
    character(MPI_MAX_ERROR_STRING)   :: errmsg
    integer :: resultlen, ierr
  continue
    call mpi_error_string(this%ierr, errmsg, resultlen, this%ierr)
  end function error_msg_fun

  !> Check error
  subroutine check_error_sub(this)
    class(ParallelMPI), intent(inout) :: this
    character(MPI_MAX_ERROR_STRING) :: errmsg = ""
    integer :: resultlen, ierr
  continue
    if ( this%occur_error() ) call this%stop(this%error_msg())
  end subroutine check_error_sub

  !> Synchronize all process
  subroutine barrier_sub(this)
    class(ParallelMPI), intent(inout) :: this
  continue
    call mpi_barrier(MPI_COMM_WORLD, this%ierr)
    call this%check_error()
  end subroutine barrier_sub

  !> Print number of process for safety
  function nprocess_fun(this) result(nproc)
    class(ParallelMPI), intent(in) :: this
    integer :: nproc
  continue
    nproc = this%nproc
  end function nprocess_fun

end module parallel_mpi
