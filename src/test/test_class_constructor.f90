!=============================================================================80
!
!> Description
!
!=============================================================================80

module test_class_constructor
  implicit none
!...Define scope
  private
  public  :: new_ClassStudent_sub, ClassStudent
!...Declare class
  type :: ClassStudent
    integer,            private :: id   = 0
    character(len=128), public  :: name = "None"
    type(ClassStudent), pointer :: next => null()
  contains
    procedure, public, pass :: init => new_ClassStudent_sub2
    final :: delete_ClassStudent  !< Destructor
  end type ClassStudent
  !> Constructor of class ClassStudent
  interface ClassStudent
    module procedure new_ClassStudent_fun
  end interface ClassStudent
contains
  !> Constructor of class ClassStudent
  function new_ClassStudent_fun(id, name) result(new_Student)
    integer,          intent(in) :: id
    character(len=*), intent(in) :: name
    type(ClassStudent), pointer  :: new_Student
    integer :: ierr
  continue
    allocate(new_Student, stat=ierr)
    write (*,*) "Initial Student ", new_Student%id, ": ", &
                                    trim(new_Student%name)
    new_Student%id   = id
    new_Student%name = trim(name)
    new_Student%next => null()
    write (*,*) "Creating Student ", new_Student%id, ": ", &
                                    trim(new_Student%name)
  end function new_ClassStudent_fun
  !> Destructor of class ClassStudent
  subroutine delete_ClassStudent(this)
    type(ClassStudent), intent(inout) :: this
  continue
    write (*,*) "Deleting Student ", this%id, ": ", trim(this%name)
    if ( associated(this%next) ) deallocate(this%next)
  end subroutine delete_ClassStudent
  !> create new_student by subroutine
  subroutine new_ClassStudent_sub(id, name, new_Student)
    integer,            intent(in)  :: id
    character(len=*),   intent(in)  :: name
    type(ClassStudent), intent(out) :: new_Student
  continue
    write (*,*) "Initial Student ", new_Student%id, ": ", &
                                    trim(new_Student%name)
    new_Student%id   = id
    new_Student%name = trim(name)
    write (*,*) "Creating Student ", new_Student%id, ": ", &
                                      trim(new_Student%name)
  end subroutine new_ClassStudent_sub
  !> create new_student by subroutine
  subroutine new_ClassStudent_sub2(this, id, name)
    integer,            intent(in)  :: id
    character(len=*),   intent(in)  :: name
    class(ClassStudent), intent(out) :: this
  continue
    write (*,*) "Initial Student ", this%id, ": ", trim(this%name)
    this%id   = id
    this%name = trim(name)
    write (*,*) "Creating Student ", this%id, ": ", trim(this%name)
  end subroutine new_ClassStudent_sub2
end module test_class_constructor
