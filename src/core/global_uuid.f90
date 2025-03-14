!=============================================================================80
!
!> uuid module
!
!=============================================================================80

module global_uuid

  use kind_parameter, only : i4, i4_inf

  implicit none

!...Define scope
  private

  public :: UUID

!...Declare Class
  type :: ClassUUID
    integer(i4), private :: id_blk = 0
  contains
    procedure, public, pass :: blk => uuid_blk_fun
  end type ClassUUID

  type(ClassUUID) :: UUID

contains

  !> Create new id for block
  integer(i4) function uuid_blk_fun(this) result(blk_id)
    class(ClassUUID), intent(inout) :: this
  continue
    this%id_blk = this%id_blk + 1
    if ( blk_id >= i4_inf ) then
      write (*,*)
      write (*,*) " *********************** "
      write (*,*) " **   UUID OVERFLOW   ** "
      write (*,*) " *********************** "
      write (*,*)
      stop
    end if
    blk_id      = this%id_blk
  end function uuid_blk_fun

end module global_uuid
