!=============================================================================80
!
!> Customize the namelist method to simplify the reading process.
!
!=============================================================================80

module method_namelist

  use kind_parameter, only : len_long, len_short
  use kind_parameter, only : dp, sp, double, float, int, logic, str

  use container_linklist,      only : linklist
  use interface_stdlib_string, only : ClassStringMethod

  use global_error,  only : ClassError
  use global_devlog, only : devlog, LogLevel

  implicit none

!...Define scope
  private

  public  :: ClassNamelist

!...Declare local variables
  type :: ClassMember
    character(len=len_short), public :: name = ""
    character(len=len_short), public :: val  = ""
    logical,                  public :: exist  = .false.
  contains
    final :: delete_ClassMember  !< Destructor
  end type ClassMember
  !> Constructor of class ClassMember
  interface ClassMember
    module procedure new_ClassMember
  end interface ClassMember

  type :: ClassNamelist
    character(len=len_short), private :: name = ""
    type(linklist), pointer,  private :: members => null()
  contains
    procedure, private, nopass :: assign_member__  => assign_member_fun__

    procedure, public,  pass :: read  => read_nml_sub
    procedure, private, pass :: add   => add_member_sub
    procedure, public,  pass :: info  => nml_info_fun
    procedure, public,  pass :: check => check_member_sub
    procedure, public,  pass :: get   => get_string_fun

    final :: delete_ClassNamelist
  end type ClassNamelist
  !> Constructor of class ClassNamelist
  interface ClassNamelist
    module procedure new_ClassNamelist
  end interface ClassNamelist

contains

  !> Constructor of class ClassMember
  type(ClassMember) function new_ClassMember(name, val) result(this)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: val
  continue
    this%name = name
    this%val  = val
    this%exist  = .false.
  end function new_ClassMember
  !> Destructor of class ClassMember
  subroutine delete_ClassMember(this)
  !...Deallocating pointer attribute
    type(ClassMember), intent(inout) :: this
  continue
  end subroutine delete_ClassMember

  !> Constructor of class ClassNamelist
  type(ClassNamelist) function new_ClassNamelist(name) result(this)
    character(*), intent(in) :: name
  continue
    this%name = name
    if (associated(this%members)) deallocate(this%members)
    this%members => null()
  end function new_ClassNamelist
  !> Destructor of class ClassNamelist
  subroutine delete_ClassNamelist(this)
  !...Deallocating pointer attribute
    type(ClassNamelist), intent(inout) :: this
  continue
    if (associated(this%members)) deallocate(this%members)
  end subroutine delete_ClassNamelist

  !> Assign member
  function assign_member_fun__(mem__) result(mem)
    class(*),           pointer :: mem__
    class(ClassMember), pointer :: mem
  !...Declare local variables
    type(ClassError) :: error
  continue
    select type (mem__)
      type is (ClassMember)
        mem => mem__
      class default
        call devlog%print(where="ClassNamelist::assign_member", &
                          message="Invalid type of member",     &
                          level=LogLevel%error)
    end select
  end function assign_member_fun__

  !> Add new member
  subroutine add_member_sub(this, assignment, stat_alloc)
  !...Declare input/output variables
    class(ClassNamelist), intent(inout) :: this
    character(len=*),     intent(in)    :: assignment
    integer,              intent(out)   :: stat_alloc
  !...Declare local variables
    type(ClassMember)       :: new_member
    type(ClassStringMethod) :: string
  continue
  !...Create new member
    new_member%name = trim(adjustl(assignment(1:string%find(assignment, "=")-1)))
    new_member%val  = trim(adjustl(assignment(string%find(assignment, "=")+1:)))
  !...Add new member
    stat_alloc = this%members%push_back(new_member)
  end subroutine add_member_sub

  !> Print namelist information
  function nml_info_fun(this) result(info)
  !...Declare input/output variables
    class(ClassNamelist), intent(in) :: this
  !...Declare local variables
    character(len_long), allocatable :: info(:)
    class(ClassMember),  pointer     :: member
    integer :: ierr, imem
  continue
    allocate(info(this%members%size()+2), stat=ierr)
    if ( ierr /= 0 ) return
    info = ""
  !...Ignore empty namelist
    if ( this%members%size() == 0 ) then
      info(1) = "Ignore namelist " // trim(this%name)
      return
    end if
  !...Traverse the namelist members
    info(1) = "&" // trim(this%name)
    do imem = 1, this%members%size()
      member => this%assign_member__(this%members%get_data(imem))
      info(imem+1) = "  " // trim(member%name) // " = " // trim(member%val)
    end do
    info(this%members%size()+2) = "/"
  end function nml_info_fun

  !> Read namelist
  subroutine read_nml_sub(this, file, error)
    use global_error,   only : ClassError
    use global_file,    only : ClassFile, ClassFileLine
    use interface_stdlib_string, only : ClassStringMethod
  !...Declare input/output variables
    class(ClassNamelist), intent(inout) :: this
    type(ClassFile),      intent(inout) :: file
    type(ClassError),     intent(inout) :: error
  !...Declare local variables
    character, parameter    :: nml_end = "/"
    type(ClassStringMethod) :: string
    logical                 :: is_nml
    type(ClassFileLine)     :: line
    character(len=len_long) :: nml_line
  continue
  !...Check file
    if (file%is_end()) call file%rewind()
  !...Initialize
    allocate(this%members, stat=error%ialloc)
    if ( error%allocate("namelist &"// trim(this%name))) return
    is_nml   = .false.
    nml_line = ""
  !...Read namelist
    do while (.true.)
    !...Read line
      line = file%read()
      if (file%read_error()) then ! Error
        call error%set(occur=.true., info="Error read line " // &
                trim(string%from(line%index)) // ": " // trim(line%content))
        return
      end if
      if (file%is_end()) return
    !...Skip empty line and comment
      if ( line%is_blank() .or. line%head_char() == "!" ) cycle
    !...Parse line
      if ( trim(line%content) == "&" // trim(this%name) ) then
        is_nml = .true.
      else if ( is_nml .and. trim(line%content) == nml_end) then
        is_nml = .false.
        exit
      else if (is_nml) then
        nml_line = trim(nml_line) // trim(line%content)
      !...Multiple line
        if ( line%end_char() == "&" ) cycle
      !...Not assignment
        if ( string%find(nml_line, "=") == 0 ) then
          call error%set(occur=.true., &
                         info="Format Error -- line " &
                            //  trim(string%from(line%index)) &
                            // ": " // trim(line%content))
          return
        end if
      !...Parse assignment
        nml_line = string%replace_all(nml_line, "&", " ")
        call this%add(trim(nml_line), error%ialloc)
        if (error%allocate("namelist "//trim(this%name))) return
      !...Reset
        nml_line = ""
      end if
    end do
  end subroutine read_nml_sub

  !> check namelist
  subroutine check_member_sub(this, error)
    use global_error, only : ClassError
  !...Declare input/output variables
    class(ClassNamelist), intent(inout) :: this
    type(ClassError),     intent(inout) :: error
  !...Declare local variables
    type(ClassMember), pointer :: current
  continue

    do while (this%members%cycle())
      current => this%assign_member__(this%members%current())
      if (.not. current%exist) then
        call error%set(occur=.true., &
                       info="Variable not defined in namelist: " &
                            // trim(current%name) &
                            // " = " &
                            // trim(current%val))
        return
      end if
    end do

  end subroutine check_member_sub

  !> get string variable from namelist
  function get_string_fun(this, var) result(val)
  !...Declare input/output variables
    class(ClassNamelist), intent(in) :: this
    character(len=*),     intent(in) :: var  !< variable name
    character(len=:),    allocatable :: val
  !...Declare local variables
    type(ClassMember), pointer :: member
    type(ClassStringMethod)    :: string
  continue
    val = ""
    do while (this%members%cycle())
      member => this%assign_member__(this%members%current())
      if (trim(member%name) == trim(var)) then
        val = trim(string%replace_all(member%val, '"', ""))
        member%exist = .true.
        call this%members%exit_cycle()
        exit
      end if
    end do
  end function get_string_fun

  !> get integer variable from namelist
  integer function get_integer_fun(this, var, type, stat) result(val)
  !...Declare input/output variables
    class(ClassNamelist), intent(inout) :: this
    character(len=*),     intent(in)    :: var  !< variable name
    integer,              intent(in)    :: type !< variable type
    integer,              intent(out)   :: stat !< transfer status
  !...Declare local variables
    type(ClassMember), pointer :: current
    type(ClassStringMethod)    :: string
  continue
    val = 0
    do while (this%members%cycle())
      current => this%assign_member__(this%members%current())
      if (trim(current%name) == trim(var)) then
        read(current%val, *, iostat=stat) val
        current%exist = .true.
        call this%members%exit_cycle()
        exit
      end if
    end do
  end function get_integer_fun

  !> get integer variable from namelist
  function get_integer_array_fun(this, var, type, dim, stat) result(val)
  !...Declare input/output variables
    class(ClassNamelist), intent(inout) :: this
    character(len=*),     intent(in)    :: var  !< variable name
    integer,              intent(in)    :: type !< variable type
    integer,              intent(in)    :: dim  !< dimension
    integer,              intent(out)   :: stat !< transfer status
    integer                             :: val(dim)
  !...Declare local variables
    type(ClassMember), pointer :: current
    type(ClassStringMethod)    :: string
  continue
    val = 0
    do while (this%members%cycle())
      current => this%assign_member__(this%members%current())
      if (trim(current%name) == trim(var)) then
        read(current%val, *, iostat=stat) val
        current%exist = .true.
        call this%members%exit_cycle()
        exit
      end if
    end do
  end function get_integer_array_fun

  !> get float variable from namelist
  real(kind=sp) function get_float_fun(this, var, type, stat) result(val)
  !...Declare input/output variables
    class(ClassNamelist), intent(inout) :: this
    character(len=*),     intent(in)    :: var  !< variable name
    real(sp),             intent(in)    :: type !< variable type
    integer,              intent(out)   :: stat !< transfer status
  !...Declare local variables
    type(ClassMember), pointer :: current
    real(kind=sp)              :: tolerance = 1.0E-4
  continue
    val = 0.0_sp
    do while (this%members%cycle())
      current => this%assign_member__(this%members%current())
      if (trim(current%name) == trim(var)) then
        read(current%val, *, iostat=stat) val
        current%exist = .true.
        call this%members%exit_cycle()
        exit
      end if
    end do
  end function get_float_fun

  !> get double variable from namelist
  real(kind=dp) function get_double_fun(this, var, type, stat) result(val)
  !...Declare input/output variables
    class(ClassNamelist), intent(inout) :: this
    character(len=*),     intent(in)    :: var  !< variable name
    real(dp),             intent(in)    :: type !< variable type
    integer,              intent(out)   :: stat !< transfer status
  !...Declare local variables
    type(ClassMember), pointer :: current
    real(kind=dp)              :: tolerance = 1.0D-8
  continue
    val = 0.0_dp
    do while (this%members%cycle())
      current => this%assign_member__(this%members%current())
      if (trim(current%name) == trim(var)) then
        read(current%val, *, iostat=stat) val
        current%exist = .true.
        call this%members%exit_cycle()
        exit
      end if
    end do
  end function get_double_fun

  !> get logical variable from namelist
  logical function get_logical_fun(this, var, type, stat) result(val)
  !...Declare input/output variables
    class(ClassNamelist), intent(inout) :: this
    character(len=*),     intent(in)    :: var  !< variable name
    logical,              intent(in)    :: type !< variable type
    integer,              intent(out)   :: stat !< transfer status
  !...Declare local variables
    type(ClassMember), pointer :: current
    type(ClassStringMethod)    :: string
  continue
    val = .false.
    do while (this%members%cycle())
      current => this%assign_member__(this%members%current())
      if (trim(current%name) == trim(var)) then
        ! val = string%to_logical(current%val, stat)
        current%exist = .true.
        call this%members%exit_cycle()
        exit
      end if
    end do
  end function get_logical_fun

end module method_namelist
