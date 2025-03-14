!=============================================================================80
!
!> Description
!
!=============================================================================80

module test_linklist_deallocate

  implicit none

!...Define scope
  private
  public :: test_process

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

  subroutine test_process()
    type(ClassStudent), pointer :: Students => null()
    type(ClassStudent), pointer :: new_Student1 => null()
    type(ClassStudent), pointer :: new_Student2 => null()
    type(ClassStudent), pointer :: new_Student3 => null()
  continue
    allocate(new_Student1)
    new_Student1 = ClassStudent(1, "John")
    allocate(new_Student2)
    new_Student2 = ClassStudent(2, "Bob")
    allocate(new_Student3)
    new_Student3 = ClassStudent(3, "Tom")
    Students           => new_Student1
    Students%next      => new_Student2
    Students%next%next => new_Student3
    write (*,*) "Deallocate new_Student1."
    ! nullify(new_Student1)
    ! deallocate(new_Student1)
    new_Student1 => null()
    write (*,*) "This is test_process."
    deallocate(Students)
  end subroutine test_process

end module test_linklist_deallocate
