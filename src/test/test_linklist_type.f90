!=============================================================================80
!
!> Description
!
!=============================================================================80

module test_linklist

  implicit none

!...Define scope
  private
  public :: ClassGroup

!...Declare local variables
  type :: ClassStudent
    integer,            private :: id   = 0
    character(len=128), public  :: name = "None"
    type(ClassStudent), pointer :: next => null()
  contains
    final :: delete_ClassStudent  !< Destructor
  end type ClassStudent
  !> Constructor of class ClassStudent
  interface ClassStudent
    module procedure new_ClassStudent_fun
  end interface ClassStudent

  type :: ClassGroup
    integer,                     private :: nstudents = 0
    type(ClassStudent), pointer, private :: head => null()
  contains
    procedure, public, pass :: add => add_student
    final :: delete_ClassGroup  !< Destructor
  end type ClassGroup

contains

  !> Constructor of class ClassStudent
  type(ClassStudent) function new_ClassStudent_fun(id, name) result(new_Student)
    integer,          intent(in) :: id
    character(len=*), intent(in) :: name
  continue
    new_Student%id   = id
    new_Student%name = trim(name)
    new_Student%next => null()
  end function new_ClassStudent_fun

  !> Destructor of class ClassStudent
  subroutine delete_ClassStudent(this)
    type(ClassStudent), intent(inout) :: this
  continue
    write (*,*) "Deleting Student ", this%id, ": ", trim(this%name)
    if ( associated(this%next) ) deallocate(this%next)
  end subroutine delete_ClassStudent

  !> Destructor of class ClassGroup
  subroutine delete_ClassGroup(this)
    type(ClassGroup), intent(inout) :: this
  continue
    write (*,*) "Deleting ClassGroup"
    if ( associated(this%head) ) deallocate(this%head)
  end subroutine delete_ClassGroup

  !> Add new student to group
  subroutine add_student(this, name)
    class(ClassGroup), intent(inout) :: this
    character(len=*),  intent(in)    :: name
  !...Define local variables
    type(ClassStudent), pointer :: new_Student
    integer :: ierr
  continue
    allocate(new_Student, stat=ierr)
    this%nstudents = this%nstudents + 1
    new_Student    = ClassStudent(this%nstudents, name)
    if ( associated(this%head) ) then
      new_Student%next => this%head
      this%head        => new_Student
    else
      this%head => new_Student
    end if
    nullify(new_Student)
  end subroutine add_student

end module test_linklist
