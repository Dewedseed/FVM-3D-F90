!=============================================================================80
!
!> Class of case information
!
!=============================================================================80

module class_case

  use kind_parameter, only : len_string

  use global_type_defs, only : TypeDefs

  use global_class, only : error, mpi, terminal
  use global_class, only : string

  use global_devlog, only : devlog, LogLevel

  implicit none

!...Define scope
  private

  public :: ClassCase

!...Declare local variables
  type :: ClassCase
    character(len_string), public :: name          = ""
    character(len_string), public :: dimension     = ""
    logical,               public :: dimensionless = .true.
    character(len_string), public :: path          = "" !< executable path
  contains
    procedure, public, pass :: input => input_sub
    procedure, public, pass :: check_input => check_input_sub
    final :: delete_ClassCase  !< Destructor
  end type ClassCase

  !> Constructor of class ClassCase
  interface ClassCase
    module procedure new_ClassCase
  end interface ClassCase

contains

  !> Constructor of class ClassCase
  type(ClassCase) function new_ClassCase() result(this)
  continue
  end function new_ClassCase

  !> Destructor of class ClassCase
  subroutine delete_ClassCase(this)
    type(ClassCase), intent(inout) :: this
  continue
  end subroutine delete_ClassCase

  !> Input case information
  subroutine input_sub(this, vname, vvalue)
  !...Declare input/output variables
    class(ClassCase), intent(inout) :: this
    character(len=*), intent(in)    :: vname  !< variable name
    character(len=*), intent(in)    :: vvalue !< variable value
  continue
    select case (trim(vname))
      case ("name")
        this%name = trim(vvalue)
      case ("dimension")
        this%dimension = trim(vvalue)
      case ("dimensionless")
        this%dimensionless = string%to_logical(vvalue, error%iread)
        if ( error%read("dimensionless") ) call mpi%stop(error%what())
      case ("path")
        this%path = trim(vvalue)
      case default
        call devlog%print(where="ClassCase::input_sub", &
                          message="Invalid variable name: " // trim(vname), &
                          level=LogLevel%error)
    end select
  end subroutine input_sub

  !> Check input information
  subroutine check_input_sub(this)
  !...Declare input/output variables
    class(ClassCase), intent(inout) :: this
  continue
  !...Check dimension
    call terminal%print_no_wrap("- Check case parameters...")
    if ( TypeDefs%DimensionIndex(trim(this%dimension)) == &
         TypeDefs%Dimension%NULL ) then
      call mpi%stop("Invalid dimension: " // trim(this%dimension))
    end if
    call terminal%print("Done.")
  end subroutine check_input_sub

end module class_case
