!=============================================================================80
!
!> NOT FINISH! This class is for catching system errors.
!
!=============================================================================80

module global_error

  use kind_parameter, only : len_long, i1

  implicit none

!...Define scope
  private
  public :: ClassError

!...Declare class
  type :: ClassError
    logical,                 private :: flag    = .false.
    character(len=len_long), private :: info    = ""
    integer,                 public  :: ialloc  = 0
    integer,                 public  :: inml    = 0
    integer,                 public  :: iread   = 0
  contains
    procedure, public, pass :: set  => set_error
    procedure, public, pass :: occur => occur_error
    procedure, public, pass :: what => print_error
    procedure, public, pass :: allocate => allocate_error
    procedure, public, pass :: openfile => openfile_error
    procedure, public, pass :: readnml => readnml_error
    procedure, public, pass :: read => read_error
    ! final :: delete_error_class  !< Destructor
  end type ClassError

  !> Constructor of class ClassError
  interface ClassError
    module procedure new_ClassError
  end interface ClassError

contains

  !> Constructor of class ClassError
  function new_ClassError() result(new_error)
    type(ClassError) :: new_error
  continue
    new_error%flag = .false.
    new_error%info = ""
  end function new_ClassError

  !> set error information
  subroutine set_error(this, occur, info)
  !...Declare input/output variables
    logical,          intent(in) :: occur
    character(len=*), intent(in) :: info
    class(ClassError), intent(inout) :: this
  continue
    this%flag = occur
    this%info = trim(info)
  end subroutine set_error

  !> check if error
  function occur_error(this) result(occur)
  !...Declare input variables
    class(ClassError), intent(inout) :: this
  !...Declare output variables
    logical :: occur
  continue
    occur = this%flag
  end function occur_error

  !> print error information
  function print_error(this) result(info)
  !...Declare input variables
    class(ClassError), intent(inout) :: this
  !...Declare output variables
    character(len=256) :: info
  continue
    info = trim(this%info)
  end function print_error

  !> check if error when allocating array
  function allocate_error(this, array_name) result(is_error)
  !...Declare input variables
    class(ClassError), intent(inout) :: this
    character(len=*),  intent(in)    :: array_name
  !...Declare output variables
    logical :: is_error
  continue
    if ( this%ialloc /= 0 ) then
      is_error = .true.
      call this%set(.true., "Error Allocate Memory for " // trim(array_name))
    else
      is_error = .false.
    end if
  end function allocate_error

  !> check if error when opening file
  function openfile_error(this, file_name, iostat) result(is_error)
  !...Declare input variables
    class(ClassError), intent(inout) :: this
    character(len=*),  intent(in)    :: file_name
    integer,           intent(in)    :: iostat
  !...Declare output variables
    logical :: is_error
  continue
    if ( iostat /= 0 ) then
      is_error = .true.
      call this%set(.true., "Error Open File " // trim(file_name))
    else
      is_error = .false.
    end if
  end function openfile_error

  !> check if error when reading namelist
  function readnml_error(this, nml_name) result(is_error)
  !...Declare input variables
    class(ClassError), intent(inout) :: this
    character(len=*),  intent(in)    :: nml_name
  !...Declare output variables
    logical :: is_error
  continue
    if ( this%inml /= 0 ) then
      is_error = .true.
      call this%set(.true., "Error Read Namelist &" // trim(nml_name))
    else
      is_error = .false.
    end if
  end function readnml_error

  !> check if error when reading
  function read_error(this, content) result(is_error)
  !...Declare input variables
    class(ClassError), intent(inout) :: this
    character(len=*),  intent(in)    :: content
  !...Declare output variables
    logical :: is_error
  continue
    if ( this%iread == 0 ) then
      is_error = .false.
    else if ( this%iread == -1 ) then !< read end
      is_error = .false.
    else
      is_error = .true.
      call this%set(.true., 'Error Read "' // trim(content) // '"')
    end if
  end function read_error

end module global_error
