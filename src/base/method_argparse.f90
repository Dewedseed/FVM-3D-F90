!=============================================================================80
!
!> Command argument parsing library
!
!=============================================================================80

module method_argparse

  use kind_parameter,          only : len_long
  use global_error,            only : ClassError
  use container_linklist,      only : linklist
  use interface_stdlib_string, only : ClassStringMethod

  use global_devlog,           only : devlog, LogLevel

  implicit none

!...Define scope
  private

  public  :: ClassArgParse

!...Declare local variables
  type :: ClassArg
    integer,             public :: index = 0
    character(len_long), public :: cmd  = ""
    character(len_long), public :: val  = ""
    logical,             public :: set  = .false.
  contains
    final :: delete_ClassArg  !< Destructor
  end type ClassArg
  !> Constructor of class ClassArg
  interface ClassArg
    module procedure new_ClassArg
  end interface ClassArg

  type :: ClassArgParse
    integer,                  private :: narg = 0
    class(linklist), pointer, private :: args => null()
  contains
    procedure, public, pass :: get_arguments => get_arguments_fun
    procedure, public, pass :: nargs         => nargs_fun
    procedure, public, pass :: print         => print_list_fun
    procedure, public, pass :: parse_args    => parse_args_fun
    procedure, public, pass :: check         => check_list_fun
    procedure, public, nopass :: assign_arg__  => assign_arg_sub__
    final :: delete_ClassArgparse  !< Destructor
  end type ClassArgParse
  !> Constructor of class ClassArg
  interface ClassArgParse
    module procedure new_ClassArgparse
  end interface ClassArgParse

contains

  !> Constructor of class ClassArg
  type(ClassArg) function new_ClassArg(cmd, val)
  !...Declare input/output variables
    character(len=*), intent(in) :: cmd
    character(len=*), intent(in) :: val
  continue
    new_ClassArg%cmd = trim(cmd)
    new_ClassArg%val = trim(val)
    new_ClassArg%set = .false.
  end function new_ClassArg
  !> Destructor of class ClassArg
  subroutine delete_ClassArg(this)
    type(ClassArg), intent(inout) :: this
  continue
  end subroutine delete_ClassArg

  !> Constructor of class ClassArgParse
  type(ClassArgParse) function new_ClassArgParse() result(this)
  !...Declare input/output variables
  continue
    this%narg = 0
    if ( (associated(this%args)) ) deallocate(this%args)
    this%args => null()
  end function new_ClassArgParse
  !> Destructor of class ClassArg
  subroutine delete_ClassArgparse(this)
  !...Declare input/output variables
    type(ClassArgParse), intent(inout) :: this
  continue
    if ( associated(this%args) ) deallocate(this%args)
  end subroutine delete_ClassArgparse

  !> Assign value to argument pointer
  function assign_arg_sub__(arg__) result(arg)
  !...Declare input/output variables
    class(*),        pointer :: arg__
    class(ClassArg), pointer :: arg
  !...Declare local variables
    type(ClassError) :: error
  continue
    select type (arg__)
      type is (ClassArg)
        arg => arg__
      class default
        call devlog%print(where="ClassArg::assign_sub",   &
                          message="Argument type error",  &
                          level=LogLevel%error)
    end select
  end function assign_arg_sub__

  !> Add argument
  integer function get_arguments_fun(this) result(stat_alloc)
    use interface_stdlib_string, only : ClassStringMethod
  !...Declare input/output variables
    class(ClassArgParse), intent(inout) :: this
  !...Declare local variables
    integer                 :: iarg
    character(len_long)     :: arg
    type(ClassArg), pointer :: new_arg
    type(ClassStringMethod) :: string
  continue
  !...Initialize
    stat_alloc = 0
  !... Get number of arguments
    this%narg = command_argument_count()
    if (this%narg == 0) return
  !...Allocate arguments
    allocate(this%args, stat=stat_alloc)
    if ( stat_alloc /= 0 ) return
  !...Get arguments
    do iarg = 1, this%narg
      call get_command_argument(iarg, arg)
      ! create new argument
      allocate(new_arg, stat=stat_alloc)
      if ( stat_alloc /= 0 ) return
      new_arg%index = iarg
      if ( string%find(arg, "=") > 0 ) then
        new_arg%cmd = trim(arg(1:string%find(arg, "=")-1))
        new_arg%val = trim(arg(string%find(arg, "=")+1:))
      else
        new_arg%cmd = trim(arg)
      end if
      ! store argument
      stat_alloc = this%args%push_back(new_arg)
      if ( stat_alloc /= 0 ) return
      ! delete pointer
      nullify(new_arg)
    end do
  end function get_arguments_fun

  !> Get number of arguments
  integer function nargs_fun(this) result(nargs)
    class(ClassArgParse), intent(in) :: this
  continue
    nargs = this%narg
  end function nargs_fun

  !> Print list of arguments
  function print_list_fun(this) result(arg_list)
    class(ClassArgParse), intent(inout) :: this
    character(len_long),  allocatable :: arg_list(:)
  !...Declare local variables
    integer                  :: ierr, iarg
    type(ClassStringMethod)  :: string
    class(ClassArg), pointer :: arg
  continue
    allocate(arg_list(this%narg), stat=ierr)
    if ( ierr /= 0 ) return
    do iarg = 1, this%narg
      arg => this%assign_arg__(this%args%get_data(iarg))
      arg_list(iarg) = "Argument " // trim(string%from(iarg, "(I4)")) &
                                   // ": " &
                                   // trim(arg%cmd) &
                                   // "=" &
                                   // trim(arg%val)
    end do
  end function print_list_fun

  !> Parse command arguments
  character(len_long) function parse_args_fun(this, cmd) result(val)
  !...Declare input/output variables
    class(ClassArgParse), intent(inout) :: this
    character(len=*),     intent(in)    :: cmd
  !...Declare local variables
    class(ClassArg), pointer :: arg
    integer :: iarg, ierr
  continue
  !...Initialize
    val = ""
  !...Parse command
    do iarg = 1, this%args%size()
      arg => this%assign_arg__(this%args%get_data(iarg))
      if ( trim(arg%cmd) .eq. trim(cmd) ) then
        val = trim(arg%val)
        arg%set = .true.
        exit
      end if
    end do
  end function parse_args_fun

  !> Check if argument is set
  type(ClassError) function check_list_fun(this) result(error)
  !...Declare input/output variables
    class(ClassArgParse), intent(inout) :: this
  !...Declare local variables
    class(ClassArg), pointer :: arg
    type(ClassStringMethod)  :: string
    integer :: iarg
  continue
    error = ClassError()
    do iarg = 1, this%args%size()
      arg => this%assign_arg__(this%args%get_data(iarg))
      if ( .not. arg%set) then
        call error%set(occur=.true., &
                       info="Argument " &
                            // trim(string%from(arg%index)) &
                            // " '" &
                            // trim(arg%cmd) &
                            // "' is not set")
        exit
      end if
    end do
  end function check_list_fun

end module method_argparse
