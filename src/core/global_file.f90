!=============================================================================80
!
!> Method for file.
!
!=============================================================================80

module global_file

  use kind_parameter, only : len_long

  implicit none

!...Define scope
  private
  public  :: ClassFile, ClassFileLine

!...Declare Class
  type :: ClassFileLine
    integer,             public :: index   = 0
    character(len_long), public :: content = ""
  contains
    procedure, public, pass :: is_blank  => is_blank_fun
    procedure, public, pass :: head_char => head_char_fun
    procedure, public, pass :: end_char  => end_char_fun
    final :: delete_ClassFileLine
  end type ClassFileLine
  !> Constructor of class ClassFile
  interface ClassFileLine
    module procedure new_ClassFileLine
  end interface

  type :: ClassFile
    character(len_long), private :: file = ""
    integer,             private :: unit = 0
    integer,             private :: iostat = 0
    type(ClassFileLine), private :: line
  contains
    procedure, public, pass :: new        => new_file_sub
    procedure, public, pass :: open       => open_file_sub
    procedure, public, pass :: close      => close_file_sub
    procedure, public, pass :: name       => print_fun
    procedure, public, pass :: get_name   => get_name_fun
    procedure, public, pass :: to_linux   => to_linux_fun
    procedure, public, pass :: get_path   => get_path_fun
    procedure, public, pass :: add_path   => add_path_sub
    procedure, public, pass :: is_present => is_present_fun
    procedure, public, pass :: is_open    => is_open_fun
    procedure, public, pass :: is_end     => is_end_fun
    procedure, public, pass :: read_error => read_error_fun
    procedure, public, pass :: read       => read_line_sub
    procedure, public, pass :: rewind     => rewind_sub
    procedure, public, pass :: write      => write_sub
    final :: delete_ClassFile  !< Destructor
  end type ClassFile
  !* Attention: when create a new file use ClassFile(),
  !*            the file opened will be closed.
  !*            this is a bug but not recurrent in test_obj_file.f90
  !> Constructor of class ClassFile
  ! interface ClassFile
  !   module procedure new_ClassFile
  ! end interface

contains

!================================= ClassFileLine =============================80
!
! Method for ClassFileLine
!
!=============================================================================80

  !> Constructor of class ClassFileLine
  function new_ClassFileLine(index, content) result(this)
  !...Declare input variables
    integer,          intent(in) :: index
    character(len=*), intent(in) :: content
  !...Declare output variables
    type(ClassFileLine) :: this
  continue
    this%index   = index
    this%content = content
  end function new_ClassFileLine

  !> Destructor of class ClassFileLine
  subroutine delete_ClassFileLine(this)
  !...Deallocating pointer attribute
    type(ClassFileLine), intent(inout) :: this
  continue
  end subroutine delete_ClassFileLine

  !> Check if line is blank
  logical function is_blank_fun(this) result(is_blank)
    class(ClassFileLine), intent(in) :: this
  continue
    is_blank = (len_trim(this%content) == 0)
  end function is_blank_fun

  !> Get head character
  character(len=1) function head_char_fun(this) result(head_char)
    class(ClassFileLine), intent(in) :: this
  continue
    head_char = this%content(1:1)
  end function head_char_fun

  !> Get end character
  character(len=1) function end_char_fun(this) result(end_char)
    class(ClassFileLine), intent(in) :: this
  continue
    end_char = this%content(len_trim(this%content):len_trim(this%content))
  end function end_char_fun

!================================== ClassFile ================================80
!
! Method for ClassFile
!
!=============================================================================80

  !> Constructor of class ClassFile
  function new_ClassFile(file_name) result(this)
  !...Declare input variables
    character(len=*), intent(in) :: file_name
  !...Declare output variables
    type(ClassFile) :: this
  continue
    this%file   = trim(file_name)
    this%unit   = 2333
    this%iostat = 0
    this%line   = ClassFileLine(0, "")
  end function new_ClassFile

  !> Destructor of class ClassFile
  subroutine delete_ClassFile(this)
  !...Deallocating pointer attribute
    type(ClassFile), intent(inout) :: this
  continue
    if ( this%is_open() ) close(this%unit)
  end subroutine delete_ClassFile

  !> Create new file
  subroutine new_file_sub(this, file_name)
    class(ClassFile), intent(inout) :: this
    character(len=*), intent(in)    :: file_name
  continue
    if (this%is_open()) call this%close()
    this%file   = trim(file_name)
    this%unit   = 2333
    this%iostat = 0
    this%line   = ClassFileLine(0, "")
  end subroutine new_file_sub

  !...Set file unit
  subroutine open_file_sub(this, status, action, error)
    use global_error, only : ClassError
  !...Declare input/output variables
    class(ClassFile), intent(inout) :: this
    character(len=*), intent(in)    :: status
    character(len=*), intent(in)    :: action
    type(ClassError), intent(inout) :: error
  continue
  !...Get available unit
    do while (this%is_open())
      this%unit = this%unit + 1
    end do
  !...Open file
    open(unit=this%unit, file=trim(this%file), status=status, &
         action=action,  iostat=this%iostat)
    if ( error%openfile(this%file, this%iostat) ) return
  end subroutine open_file_sub

  !> Close file
  subroutine close_file_sub(this)
  !...Declare input/output variables
    class(ClassFile), intent(inout) :: this
  continue
    if ( this%is_open() ) close(this%unit)
  end subroutine close_file_sub

  !> Print file name
  function print_fun(this) result(name)
    class(ClassFile), intent(inout) :: this
    character(len=:), allocatable :: name
  continue
    name = trim(this%file)
  end function print_fun

  !> Get name of file
  function get_name_fun(this) result(name)
  !...Declare input variables
    class(ClassFile), intent(inout) :: this
  !...Declare output variables
    character(len_long) :: name
  !...Declare local variables
    integer :: last_slash
  continue
    last_slash = index(this%file, "/", back=.true.)
    name = this%file(last_slash+1:)
  end function get_name_fun

  !> get path of file
  function get_path_fun(this) result(path)
  !...Declare input variables
    class(ClassFile), intent(inout) :: this
  !...Declare output variables
    character(len_long) :: path
  !...Declare local variables
    integer :: last_slash
  continue
    last_slash = index(this%file, "/", back=.true.)
    path = this%file(1:last_slash-1)
  end function get_path_fun

  !> add path to file
  subroutine add_path_sub(this, path)
  !...Declare input/output variables
    class(ClassFile), intent(inout) :: this
    character(len=*), intent(in)    :: path
  continue
    this%file = trim(path) // "/" // trim(this%file)
  end subroutine add_path_sub

  !> Windows path to linux
  function to_linux_fun(this) result(file_linux)
  !...Declare input variables
    class(ClassFile), intent(in) :: this
  !...Declare output variables
    character(len_long) :: file_linux
  !...Declare local variables
    integer :: i
  continue
    file_linux = this%file
    do i = 1, len_trim(this%file)
      if ( this%file(i:i) == "\" ) then
        file_linux(i:i) = "/"
      end if
    end do
  end function to_linux_fun

  !> file is present?
  function is_present_fun(this) result(is_present)
  !...Declare input variables
    class(ClassFile), intent(in) :: this
  !...Declare output variables
    logical :: is_present
  continue
    inquire(file=trim(this%file), exist=is_present)
  end function is_present_fun

  !> file is open?
  function is_open_fun(this) result(is_open)
  !...Declare input variables
    class(ClassFile), intent(in) :: this
  !...Declare output variables
    logical :: is_open
  continue
    inquire(unit=this%unit, opened=is_open)
  end function is_open_fun

  !> file is end?
  logical function is_end_fun(this) result(is_end)
  !...Declare input variables
    class(ClassFile), intent(in) :: this
  continue
    if ( this%iostat == -1 ) then
      is_end = .true.
    else
      is_end = .false.
    end if
  end function is_end_fun

  !> error when reading file
  logical function read_error_fun(this) result(is_error)
  !...Declare input variables
    class(ClassFile), intent(in) :: this
  continue
    if ( this%iostat > 0 ) then
      is_error = .true.
    else
      is_error = .false.
    end if
  end function read_error_fun

  !> read line from file
  function read_line_sub(this) result(line)
    class(ClassFile), intent(inout) :: this
    type(ClassFileLine) :: line
    character(len_long) :: line_content
  continue
    read(this%unit, '(a)', iostat=this%iostat) line_content
    this%line%content = line_content
    this%line%index = this%line%index + 1
    line%content = adjustl(this%line%content)
    line%index   = this%line%index
  end function read_line_sub

  !> rewind file
  subroutine rewind_sub(this)
  !...Declare input/output variables
    class(ClassFile), intent(inout) :: this
  continue
    rewind(this%unit)
  end subroutine rewind_sub

  !> write line to file
  subroutine write_sub(this, line)
  !...Declare input/output variables
    class(ClassFile), intent(inout) :: this
    character(len=*), intent(in)    :: line
  continue
    write(this%unit, '(a)') trim(line)
  end subroutine write_sub

end module global_file
