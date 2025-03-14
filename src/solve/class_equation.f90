!=============================================================================80
!
!> Equation System.
!
!                   |  Equations             | Terms
! -----------------------------------------------------------------
!  Physical System  |  Mass-Conservation     | Time-Dependence
!                   |  Momentum-Conservation | Inviscid Flux
!                   |  Energy-Conservation   | Viscous Flux
!                   |  Turbulence-Model      | Source
!                   |  Chemistry-Model       |
! -----------------------------------------------------------------
!  Math System      |  PDE                   | Time-Dependence
!                   |  Dependent Variable    | Spatial Derivative
!                   |  Constant Coefficient  | Lambda
!                   |  Dependent Coefficient |
!
!=============================================================================80

module class_equation

  use kind_parameter,     only : dp
  use container_linklist, only : linklist
  use global_type_defs,    only : TypeDefs

  use global_class, only : error, mpi, terminal

  use global_devlog, only : devlog, LogLevel

  implicit none

!...Define scope
  private

  public :: ClassPhysicalSystem

!...Declare Class
  type :: ClassEquationTerm
    integer, private :: type = 0
  end type ClassEquationTerm

  type :: ClassEquation
    integer, private :: type   = 0
    integer, private :: varDep = 0 !< dependent variable
    type(ClassEquationTerm), pointer :: timeDep  => null()
    type(ClassEquationTerm), pointer :: inviscid => null()
    type(ClassEquationTerm), pointer :: viscous  => null()
    type(ClassEquationTerm), pointer :: source   => null()
  contains
    final :: delete_ClassEquation
  end type ClassEquation

!...Constructor of class ClassEquation
  interface ClassEquation
    module procedure new_ClassEquation
  end interface ClassEquation

  type :: ClassPhysicalSystem
    type(linklist)  :: equations
  contains
    procedure, public, pass :: add_equation => add_equation_sub
    procedure, public, pass :: size         => size_fun
    procedure, public, pass :: eqn_index    => eqn_index_fun
    procedure, public, pass :: eqn_type     => eqn_type_fun
    procedure, private, nopass :: assign_eqn__ => assign_eqn_fun__
    final :: delete_ClassPhysicalSystem
  end type ClassPhysicalSystem

!...Constructor of class ClassPhysicalSystem
  interface ClassPhysicalSystem
    module procedure new_ClassPhysicalSystem
  end interface ClassPhysicalSystem

contains

  !> Constructor of class ClassEquation
  function new_ClassEquation(eqn_type) result(this)
    integer, intent(in) :: eqn_type
    type(ClassEquation) :: this
  continue
  !...Initialize
    this%type = eqn_type
    if (associated(this%timeDep))  deallocate(this%timeDep)
    if (associated(this%inviscid)) deallocate(this%inviscid)
    if (associated(this%viscous))  deallocate(this%viscous)
    if (associated(this%source))   deallocate(this%source)
    this%timeDep  => null()
    this%inviscid => null()
    this%viscous  => null()
    this%source   => null()
  !...Pre-define equation
    this%varDep = TypeDefs%EqnVarIndex(this%type)
    if ( this%varDep == TypeDefs%Equation%VarNull ) then
      call devlog%print(where="ClassEquation::new_ClassEquation", &
                        message="Unknown equation type.",  &
                        level=LogLevel%error)
    end if
  end function new_ClassEquation

  !> Destructor of class ClassEquation
  subroutine delete_ClassEquation(this)
  !...Deallocating pointer attribute
    type(ClassEquation), intent(inout) :: this
  continue
    if (associated(this%timeDep))  deallocate(this%timeDep)
    if (associated(this%inviscid)) deallocate(this%inviscid)
    if (associated(this%viscous))  deallocate(this%viscous)
    if (associated(this%source))   deallocate(this%source)
  end subroutine delete_ClassEquation

  !> Constructor of class ClassPhysicalSystem
  function new_ClassPhysicalSystem() result(this)
    type(ClassPhysicalSystem) :: this
  continue
  end function new_ClassPhysicalSystem

  !> Destructor of class ClassPhysicalSystem
  subroutine delete_ClassPhysicalSystem(this)
  !...Deallocating pointer attribute
    type(ClassPhysicalSystem), intent(inout) :: this
  continue
  end subroutine delete_ClassPhysicalSystem

  !> Assign value to equation pointer
  function assign_eqn_fun__(eqn__) result(eqn)
  !...Declare input/output variables
    class(*),             pointer :: eqn__
    class(ClassEquation), pointer :: eqn
  continue
    select type (eqn__)
      type is (ClassEquation)
        eqn => eqn__
      class default
        call devlog%print(where="ClassArg::assign_sub", &
                          message="Argument type error",  &
                          level=LogLevel%error)
    end select
  end function assign_eqn_fun__

  !> Add equation
  subroutine add_equation_sub(this, eqn_type, flow_state)
    class(ClassPhysicalSystem), intent(inout) :: this
    character(len=*),           intent(in)    :: eqn_type
    character(len=*),           intent(in), optional :: flow_state
  !...Declare local variables
    integer :: ieqn_type
  continue
  !...Parse equation type
    ieqn_type = TypeDefs%EquationIndex(eqn_type)
    if (ieqn_type == TypeDefs%Equation%NULL) then
      call devlog%print(where="ClassPhysicalSystem::add_equation_sub", &
                        message="Unknown equation type.",  &
                        level=LogLevel%error)
    end if
  !...Add equation
    error%ialloc = this%equations%push_back(ClassEquation(ieqn_type))
    if (error%allocate("Physical Equations")) call mpi%stop(error%what())
  end subroutine add_equation_sub

  !> Get equation size
  integer function size_fun(this) result(size)
    class(ClassPhysicalSystem), intent(inout) :: this
  continue
    size = this%equations%size()
  end function size_fun

  !> Get equation index
  integer function eqn_index_fun(this, eqn_type) result(index)
    class(ClassPhysicalSystem), intent(inout) :: this
    character(len=*),           intent(in) :: eqn_type
  !...Declare local variables
    integer                       :: ieqn_type
    class(ClassEquation), pointer :: eqn__
  continue
    ieqn_type = TypeDefs%EquationIndex(eqn_type)
    index     = 0
    do while (this%equations%cycle())
      index = index + 1
      eqn__ => this%assign_eqn__(this%equations%current())
      if (eqn__%type == ieqn_type) then
        call this%equations%exit_cycle()
        return
      end if
    end do
    if ( index == this%equations%size() ) index = 0
  end function eqn_index_fun

  !> Get equation type
  function eqn_type_fun(this) result(eqn_type)
    class(ClassPhysicalSystem), intent(inout) :: this
    integer, allocatable :: eqn_type(:)
  !...Declare local variables
    integer                       :: index
    class(ClassEquation), pointer :: eqn__
  continue
    allocate(eqn_type(this%equations%size()), stat=error%ialloc)
    if (error%allocate("Physical Equations")) call mpi%stop(error%what())
    index = 0
    do while (this%equations%cycle())
      index = index + 1
      eqn__ => this%assign_eqn__(this%equations%current())
      eqn_type(index) = eqn__%type
    end do
  end function eqn_type_fun

end module class_equation
