module test_obj_file
  implicit none
  public :: ClassFileTest
  !! Declare Class
  type ClassFileTest
    character(len=233) :: name = ""
    integer, public :: unit = 2333
    integer, public :: iostat = 0
  contains
    procedure, pass, public :: open => open_file
    procedure, pass, public :: read => read_file
  end type ClassFileTest
  !! Constructor of ClassFileTest
  interface ClassFileTest
    module procedure new_ClassFileTest
  end interface
contains
  !! Constructor of ClassFileTest
  function new_ClassFileTest(file_name) result(this)
    character(len=*), intent(in) :: file_name
    type(ClassFileTest) :: this
    this%name = trim(file_name)
    this%unit = 2333
    this%iostat = 0
  end function new_ClassFileTest
  !! Open file
  subroutine open_file(this, status, action)
    class(ClassFileTest), intent(inout) :: this
    character(len=*), intent(in)    :: status
    character(len=*), intent(in)    :: action
  continue
    open(unit=this%unit, file=this%name, status=status, &
         action=action, iostat=this%iostat)
    if ( this%iostat /= 0 ) then
      write (*,*) "Error open file: ", trim(this%name)
      stop
    end if
  end subroutine open_file
  !! Read file
  function read_file(this) result(line)
    class(ClassFileTest), intent(inout) :: this
    character(len=50) :: line
  continue
    read(this%unit, '(a)', iostat=this%iostat) line
    if ( this%iostat > 0 ) then
      write (*,*) "Error read file: ", trim(line)
      stop
    else if ( this%iostat == -1 ) then
      write (*,*) "End of file"
      stop
    end if
  end function read_file
end module test_obj_file
