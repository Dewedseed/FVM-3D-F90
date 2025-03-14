!=============================================================================80
!
!> Ouput log information to file for developer.
!
!=============================================================================80

module global_devlog

  implicit none

!...Define scope
  private

  public :: ClassDevLog, devlog, LogLevel

!...Declare Class
  type :: ClassDevLog
    integer, public  :: unit = 2727
    logical, private :: open = .false.  !< for parallel, output to file or not;
                                        !! avoid io block
    character(len=256), private :: logfile = ""
  contains
    procedure :: start => start_sub
    procedure :: end   => end_sub

    procedure, private :: print_msg   => print_message_sub
    procedure, private :: print_where => print_where_sub
    generic :: print => print_msg, print_where
  end type ClassDevLog

  type :: ClassDevLogLevel
    integer, public :: error   = 1
    integer, public :: message = 2
    integer, public :: debug   = 3
    integer, public :: info    = 4
    integer, public :: warning = 5
  end type ClassDevLogLevel

  interface ClassDevLog
    module procedure new_ClassDevLog
  end interface ClassDevLog

  type(ClassDevLog)      :: devlog
  type(ClassDevLogLevel) :: LogLevel

contains

  !> Constructor of ClassDevLog
  function new_ClassDevLog(open, path, unit) result(self)
    logical,          intent(in) :: open
    character(len=*), intent(in) :: path
    integer,          intent(in), optional :: unit
    type(ClassDevLog) :: self
  continue
    self%open    = open
    self%logfile = trim(path) // "/" // trim("dev.log")
    if ( present(unit) ) self%unit = unit
  end function new_ClassDevLog

  !> Start logging
  subroutine start_sub(this)
    class(ClassDevLog), intent(inout) :: this
    integer                           :: iostat
  continue
    if ( this%open ) then
      open(newunit=this%unit, file=this%logfile, status="replace", iostat=iostat)
      if ( iostat /= 0 ) then
        write (*,*) "Error open log file: ", trim(this%logfile)
      end if
    end if
  end subroutine start_sub

  !> End logging
  subroutine end_sub(this)
    class(ClassDevLog), intent(inout) :: this
  continue
    if ( this%open ) close(this%unit)
  end subroutine end_sub

  !> Print information to logfile
  subroutine print_message_sub(this, message)
    class(ClassDevLog), intent(inout) :: this
    character(len=*),   intent(in), optional :: message
  continue
    if ( this%open ) write(this%unit,*) "-- ", trim(message)
  end subroutine print_message_sub

  !> Print information to logfile
  subroutine print_where_sub(this, where, message, level)
    class(ClassDevLog), intent(inout) :: this
    character(len=*),   intent(in)    :: where
    character(len=*),   intent(in)    :: message
    integer,            intent(in)    :: level
  continue
    if ( this%open ) then
      if ( level == LogLevel%error ) then
        write(this%unit,*)
        write(this%unit,*) "------------------------------------------------"
        write(this%unit,*) " ** ERROR: ", trim(where)
        write(this%unit,*) "      ",      trim(message)
        write(this%unit,*) "------------------------------------------------"
        write(this%unit,*)
        flush(this%unit)
        close(this%unit)
        write (6,*)
        write (6,*) "DEV-ERROR -- Refer to ", trim(this%logfile)
        write (6,*)
        stop
      else
        write(this%unit,*)
        write(this%unit,*) trim(where), " -- ", trim(message)
        write(this%unit,*)
      end if
    end if
  end subroutine print_where_sub

end module global_devlog
