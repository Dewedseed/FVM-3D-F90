!=============================================================================80
!
!> Class for terminal
!
!=============================================================================80

module global_terminal

  implicit none

!...Define scope
  private

  public  :: ClassTerminal

!...Declare Class
  type :: ClassTerminal
    integer, private :: unit = 6
    logical, private :: open =  .false.
    logical, private :: debug = .false.
  contains
    procedure, pass, public :: flush => flush_sub
    procedure, pass :: print_blank        => print_blank_sub
    procedure, pass :: print_info         => print_info_sub
    procedure, pass :: print_infos        => print_infos_sub
    procedure, pass :: print_info_format  => print_info_format_sub
    procedure, pass :: print_infos_format => print_infos_format_sub
    generic, public :: print => print_blank,        &
                                print_info,         &
                                print_infos,        &
                                print_info_format,  &
                                print_infos_format

    procedure, pass :: print_no_wrap__      => print_no_wrap_sub
    procedure, pass :: print_no_wrap_fromat => print_no_wrap_fromat_sub
    generic, public :: print_no_wrap => print_no_wrap__, print_no_wrap_fromat

    procedure :: print_debug   => print_debug_sub
    procedure :: print_refresh => print_refresh_sub
    final :: delete_ClassTerminal
  end type ClassTerminal

  !> Constructor of class ClassTerminal
  interface ClassTerminal
    module procedure new_ClassTerminal
  end interface ClassTerminal

contains

  !> Constructor of class ClassTerminal
  function new_ClassTerminal(open, unit) result(this)
    logical, intent(in) :: open
    integer, intent(in), optional :: unit
    type(ClassTerminal) :: this
  continue
    this%open = open
    if ( present(unit) ) then
      this%unit = unit
    end if
  end function new_ClassTerminal

  !> Destructor of class ClassTerminal
  subroutine delete_ClassTerminal(this)
    type(ClassTerminal), intent(inout) :: this
  continue
    this%unit = 6
    this%open = .false.
    ! close(this%unit)
  end subroutine delete_ClassTerminal

  !> Flush terminal
  subroutine flush_sub(this)
    class(ClassTerminal), intent(inout) :: this
  continue
    flush(this%unit)
  end subroutine flush_sub

  !> Print blank information to terminal
  subroutine print_blank_sub(this)
    class(ClassTerminal), intent(inout) :: this
  continue
    if ( this%open ) write(this%unit,*)
  end subroutine print_blank_sub

  !> Print single information to terminal
  subroutine print_info_sub(this, info)
    class(ClassTerminal), intent(inout) :: this
    character(len=*),     intent(in)    :: info
  continue
    if ( this%open ) write(this%unit,*) trim(info)
  end subroutine print_info_sub

  !> Print multiple information to terminal
  subroutine print_infos_sub(this, info)
    class(ClassTerminal), intent(in) :: this
    character(len=*),     intent(in) :: info(:)
    integer :: i
  continue
    if ( this%open ) then
      do i = 1, size(info)
        write(this%unit,*) trim(info(i))
      end do
    end if
  end subroutine print_infos_sub

  !> Print information with format to terminal
  subroutine print_info_format_sub(this, infos, format)
    class(ClassTerminal), intent(inout) :: this
    character(len=*),     intent(in)    :: infos
    character(len=*),     intent(in)    :: format
  continue
    if ( this%open ) write(this%unit, format) trim(infos)
  end subroutine print_info_format_sub

  !> Print multiple information with format to terminal
  subroutine print_infos_format_sub(this, info, format)
    class(ClassTerminal), intent(inout) :: this
    character(len=*),     intent(in)    :: info(:)
    character(len=*),     intent(in)    :: format
  !...Declare local variables
    integer :: i
  continue
    if ( this%open ) write(this%unit,format) (trim(info(i)), i=1, size(info))
  end subroutine print_infos_format_sub

  !> Print but no wrap
  subroutine print_no_wrap_sub(this, info)
    class(ClassTerminal), intent(inout) :: this
    character(len=*),     intent(in)    :: info
  continue
    if ( this%open ) write(this%unit,'(1x,a)',advance='no') trim(info)
  end subroutine print_no_wrap_sub

  !> Print but no wrap with format
  subroutine print_no_wrap_fromat_sub(this, info, format)
    class(ClassTerminal), intent(inout) :: this
    character(len=*),     intent(in)    :: info
    character(len=*),     intent(in)    :: format
  continue
    if ( this%open ) write(this%unit,format,advance='no') trim(info)
  end subroutine print_no_wrap_fromat_sub

  !> Print for debug (this may effect efficiency)
  subroutine print_debug_sub(this, info)
    class(ClassTerminal), intent(inout) :: this
    character(len=*),     intent(in)    :: info
  continue
    if (this%open .and. this%debug) then
      call this%print(info)
    end if
  end subroutine print_debug_sub

  !> Print no advance
  subroutine print_refresh_sub(this, info)
  !...Declare input/output variables
    class(ClassTerminal), intent(inout) :: this
    character(len=*),     intent(in)    :: info
  continue
    if ( this%open ) then
      ! write (this%unit, '(a,1x,80x)', advance='no') char(13)
      write (this%unit, '(a,1x,a)',   advance='no') char(13), trim(info)
      write (this%unit, '(a)',        advance='no') char(13)
      call this%flush()
    end if
  end subroutine print_refresh_sub

end module global_terminal
