!=============================================================================80
!
!> https://www.onlinegdb.com/online_fortran_compiler
!
!=============================================================================80

module global_terminal_Test

  implicit none

!...Define scope
  private
  public  :: terminal

!...Declare local variables
  type ClassTerminal
    class(*), pointer :: info__
  contains
    procedure, pass, public :: print => terminal_print
  end type

  type(ClassTerminal) :: terminal

contains

  subroutine terminal_print(this, info)
    class(ClassTerminal), intent(inout) :: this
    class(*), intent(in) :: info
  continue
    select type (info)
      type is (character(len=*))
        write (*,*) trim(info)
      type is (integer)
        write (*,*) info
      class default
    end select
  end subroutine terminal_print

end module global_terminal_Test


program test
  use global_terminal_Test, only : terminal
  implicit none
continue
  write (*,*)
  write (*,*) ">>>>>>> Test <<<<<<<<"
  call terminal%print("Hello")
  call terminal%print(1)
end program test
