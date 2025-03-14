!=============================================================================80
!
!> Main data container.
!
!=============================================================================80

module class_solver

  use class_solver_info,   only : ClassSolverInfo
  use class_case,          only : ClassCase
  use class_geometry,      only : ClassGeometry
  use class_flow_field,    only : ClassFlowField
  use class_space_discret, only : ClassSpaceDiscret
  use class_time_discret,  only : ClassTimeDiscret
  use class_iteration,     only : ClassIteration

  use global_class, only : terminal, string

  implicit none

!...Define scope
  private

  public  :: ClassSolver

!...Declare local variables
  type :: ClassSolver
    type(ClassSolverInfo),   public :: info
    type(ClassCase),         public :: case
    type(ClassGeometry),     public :: geometry
    type(ClassFlowField),    public :: flowfield
    type(ClassSpaceDiscret), public :: spaceDiscr
    type(ClassTimeDiscret),  public :: timeDiscr
    type(ClassIteration),    public :: iteration
  contains
    final :: delete_ClassSolver  !< Destructor
  end type ClassSolver

  !> Constructor of class ClassSolver
  interface ClassSolver
    module procedure new_ClassSolver
  end interface ClassSolver

contains

  !> Constructor of class ClassSolver
  type(ClassSolver) function new_ClassSolver() result(this)
  !...Declare input/output variables
  continue
    this%case = ClassCase()
  end function new_ClassSolver

  !> Destructor of class ClassSolver
  subroutine delete_ClassSolver(this)
  !...Deallocating pointer attribute
    type(ClassSolver), intent(inout) :: this
  continue
    ! if (associated(this%next)) deallocate(this%next)
  end subroutine delete_ClassSolver

end module class_solver
