!=============================================================================80
!
!> Define iteration information.
!
!=============================================================================80

module class_iteration

  use kind_parameter,  only : i8, dp, len_short
  use kind_parameter,  only : double
  use global_type_defs, only : TypeDefs

  use global_class, only : error, mpi, terminal
  use global_class, only : string

  use global_devlog, only : devlog, LogLevel

  implicit none

!...Define scope
  private

  public  :: ClassIteration

!...Declare local variables
  type :: ClassIteration
    integer(i8), public :: step = 0       !< current step
    real(dp),    public :: time = 0.0_dp  !< current real time
    integer,     public :: nres = 0       !< number of residuals
                                          !< = number of equations + total
    character(len_short), public, allocatable :: res_name(:)    !< residual name
    real(dp),    public, allocatable :: res_abs(:)     !< absolute residual
    real(dp),    public, allocatable :: res_abs0(:)     !< absolute residual
    real(dp),    public, allocatable :: res_relat(:)   !< relative residual
  !...Private variables
    real(dp),    private :: res_abs_min   = 0.0_dp  !< minimum absolute residual
    real(dp),    private :: res_abs_max   = 1.0E10_dp  !< max absolute residual
    real(dp),    private :: res_relat_min = 0.0_dp  !< minimum relative residual
    real(dp),    private :: max_time      = 0.0_dp  !< total real time
    integer(i8), private :: max_step      = 1E10_i8 !< maximum step
    real(dp),    private :: time_tol      = 1.0E-10_dp !< time tolerance
  contains
    procedure, public, pass :: input            => input_sub
    procedure, public, pass :: check_input      => check_input_sub
    procedure, public, pass :: allocate_memory  => allocate_memory_sub
    procedure, public, pass :: converged        => converged_fun
    procedure, public, pass :: reset_residual   => reset_residual_sub
    procedure, public, pass :: compute_residual => compute_residual_sub
    procedure, public, pass :: update           => update_sub
    final :: delete_ClassIteration  !< Destructor
  end type ClassIteration

contains

  !> Constructor of class ClassIteration
  type(ClassIteration) function new_ClassIteration() result(this)
  continue
  end function new_ClassIteration

  !> Destructor of class ClassIteration
  subroutine delete_ClassIteration(this)
    type(ClassIteration), intent(inout) :: this
  continue
    if ( allocated(this%res_name) ) deallocate(this%res_name)
    if ( allocated(this%res_abs) ) deallocate(this%res_abs)
    if ( allocated(this%res_abs0) ) deallocate(this%res_abs0)
    if ( allocated(this%res_relat) ) deallocate(this%res_relat)
  end subroutine delete_ClassIteration

  !> Input iteration information
  subroutine input_sub(this, vname, vvalue)
  !...Declare input/output variables
    class(ClassIteration), intent(inout) :: this
    character(len=*),      intent(in)    :: vname  !< variable name
    character(len=*),      intent(in)    :: vvalue !< variable value
  continue
    select case (trim(vname))
      case ("absolute_residual")
        this%res_abs_min = string%to_num(vvalue, double, error%iread)
        if ( error%read("absolute_residual") ) call mpi%stop(error%what())
      case ("relative_residual")
        this%res_relat_min = string%to_num(vvalue, double, error%iread)
        if ( error%read("relative_residual") ) call mpi%stop(error%what())
      case ("total_time")
        this%max_time = string%to_num(vvalue, double, error%iread)
        if ( error%read("total_time") ) call mpi%stop(error%what())
      case default
        call devlog%print(where="ClassIteration::input_sub", &
                          message="Unknown variable name: "//trim(vname), &
                          level=LogLevel%error)
    end select
  end subroutine input_sub

  !> Check input information
  subroutine check_input_sub(this)
  !...Declare input/output variables
    class(ClassIteration), intent(inout) :: this
  continue
    call terminal%print_no_wrap("- Check iteration parameters...")
    if ( this%res_abs_min < 0.0_dp ) then
      call mpi%stop("Invalid minimum absolute residual.")
    end if
    if ( this%res_relat_min < 0.0_dp ) then
      call mpi%stop("Invalid minimum relative residual.")
    end if
    if ( this%max_time < -1.0E-6 ) then
      call mpi%stop("Invalid total time.")
    end if
    call terminal%print("Done.")
  end subroutine check_input_sub

  !> Allocate memory
  subroutine allocate_memory_sub(this, system)
    use class_equation,     only : ClassPhysicalSystem
  !...Declare input/output variables
    class(ClassIteration),     intent(inout) :: this
    type(ClassPhysicalSystem), intent(inout) :: system
    integer :: i
  continue
  !...Allocate memory
    call terminal%print(">> Allocate memory for interation...")
    this%nres = system%size() + 1
    if (this%nres ==  0) call mpi%stop("No physical system created .")

    allocate(this%res_name(this%nres), stat=error%ialloc)
    if ( error%allocate("Residual Names") ) call mpi%stop(error%what())
    this%res_name(1:this%nres-1) = TypeDefs%EqnResName( &
                                   TypeDefs%EqnResIndex(system%eqn_type()))
    this%res_name(this%nres) = "R_total"

    allocate(this%res_abs(this%nres), stat=error%ialloc)
    if ( error%allocate("Absolute Residuals") ) call mpi%stop(error%what())
    this%res_abs = 0.0_dp

    allocate(this%res_abs0(this%nres), stat=error%ialloc)
    if ( error%allocate("Absolute Residuals") ) call mpi%stop(error%what())
    this%res_abs0 = 0.0_dp

    allocate(this%res_relat(this%nres), stat=error%ialloc)
    if ( error%allocate("Relative Residuals") ) call mpi%stop(error%what())
    this%res_relat = 0.0_dp
  end subroutine allocate_memory_sub

  !> Check if the solver has converged
  logical function converged_fun(this) result(converged)
    class(ClassIteration), intent(in) :: this
  continue
    converged = .false.
    if (this%step == 0) return
  !...Case 1: reached maximum step
    if ( this%step == this%max_step ) then
      converged = .true.
      call terminal%print()
      call terminal%print(">> Maximum Step: " // &
                              string%from(this%max_step) // &
                          "  Current Step: " // &
                              string%from(this%step))
  !...Case 2: reached maximum time
    else if ( this%time > this%max_time - this%time_tol ) then
      converged = .true.
      call terminal%print()
      call terminal%print(">> Maximum Time: " // &
                              string%from(this%max_time) // &
                          "  Current Time: " // &
                              string%from(this%time))
  !...Case 3: reached absolute residual
    else if ( this%res_abs(this%nres) < this%res_abs_min ) then
      converged = .true.
      call terminal%print()
      call terminal%print(">> Minimum Absolute Residual: " // &
                              string%from(this%res_abs_min) // &
                          "  Absolute Residual: " // &
                              string%from(this%res_abs(this%nres)))
  !...Case 4: reached relative residual
    else if ( this%res_relat(this%nres) < this%res_relat_min ) then
      converged = .true.
      call terminal%print()
      call terminal%print(">> Minimum Relative Residual: " // &
                              string%from(this%res_relat_min) // &
                          "  Relative Residual: " // &
                              string%from(this%res_relat(this%nres)))
  !...Case 5: error residual reach threshold
    else if ( this%res_abs(this%nres) > this%res_abs_max ) then
      call terminal%print()
      call mpi%stop("Error: Absolute Residual Exceeds Threshold 1E10. ABSRES " // &
                              string%from(this%res_abs(this%nres)))
  !...Case 6: NAN residual
    else if ( isnan(this%res_abs(this%nres)) ) then
      call terminal%print()
      call mpi%stop("Error: ABSRES " // string%from(this%res_abs(this%nres)))
    end if
  end function converged_fun

  !> Reset residual before each time step
  subroutine reset_residual_sub(this)
  !...Declare input/output variables
    class(ClassIteration), intent(inout) :: this
  !...Declare local variable
  continue
    this%res_abs   = 0.0_dp
    this%res_relat = 0.0_dp
  end subroutine reset_residual_sub

  !> Compute residuals
  !   R(ieqn) = sqrt(sum(R(ipoint, ieqn)^2) / npoint)
  !   R_total = sqrt(sum(R(ieqn)^2) / neqn)
  !   R_relate = R / R0   (R0 = residual at the first step)
  ! Question: rhs is one block, so the result now is
  !   R = sqrt(sum(R(blk)^2))
  subroutine compute_residual_sub(this, rhs)
  !...Declare input/output variables
    class(ClassIteration),        intent(inout) :: this
    real(dp), dimension(:,:,:,:), intent(inout) :: rhs !< right hand side = residual
  !...Declare local variables
    integer :: ieqn, neqn
    integer(i8) :: ni, nj, nk
    integer(i8) :: i, j, k
    real(dp) :: res2(this%nres - 1)
    real(dp) :: res2_t !< total residual
  continue
    call devlog%print("Compute residual")
    ni   = size(rhs, 1)
    nj   = size(rhs, 2)
    nk   = size(rhs, 3)
    neqn = this%nres - 1
    res2   = 0.0_dp
    res2_t = 0.0_dp
  !...Compute residuals of each equation
    do ieqn = 1, neqn
      do k = 1, nk
        do j = 1, nj
          do i = 1, ni
            res2(ieqn) = res2(ieqn) + rhs(i, j, k, ieqn)**2
          end do
        end do
      end do

      res2(ieqn) = res2(ieqn) / real(ni * nj * nk, dp)
      res2_t = res2_t + res2(ieqn)
    end do

    res2_t = res2_t / real(neqn, dp)
  !...Update residual
    do ieqn = 1, neqn
      this%res_abs(ieqn) = sqrt(this%res_abs(ieqn)**2 + res2(ieqn))
    end do
    this%res_abs(this%nres) = sqrt(this%res_abs(this%nres)**2 + res2_t)
  end subroutine compute_residual_sub

  !> Update relative residual and clear
  subroutine update_sub(this)
  !...Declare local variable
    class(ClassIteration), intent(inout) :: this
  continue
    if (this%step == 1) then
      this%res_abs0 = this%res_abs
    end if
    this%res_relat = this%res_abs / this%res_abs0
  end subroutine update_sub

end module class_iteration
