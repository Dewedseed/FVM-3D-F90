!=============================================================================80
!
!> Simple inviscid flux reconstruction.
!
!=============================================================================80

module scheme_reconstruct_simple

  use kind_parameter, only : dp, i8
  use global_devlog,  only : devlog, LogLevel

  implicit none
  private

  public :: reconstruct_simple_sub

contains

!=============================================================================80
!
!> Simple inviscid flux reconstruction.
!
!  FDM:        :
!     j+1 * ---:--- *
!         |    :    |  qp = q(i, j+1)
!     .. .. .. : .. .. ..
!         |    :    |  qm = q(i, j)
!      j  * ---:--- *
!        i-1   :    i
!
!  FVM (cell-centered):
!        ---- ----
!   j+1  | * | * |   qp = q(i, j+1)
!        ---- ----  ........
!     j  | * | * |   qm = q(i, j)
!        ---- ----
!        i-1   i
!
!=============================================================================80

  !> compute on ghost cell
  subroutine reconstruct_simple_sub(nface, dghost, p_ghost, p_r, wind)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    integer(i8),              intent(in)    :: nface
    integer(i8),              intent(in)    :: dghost
    type(Primitive_Variable), intent(inout) :: p_ghost(:)
                              !< privative variables on ghost mesh
    type(Primitive_Variable), intent(inout) :: p_r(:)
                              !< reconstruct variables on real mesh face
    integer,                  intent(in)    :: wind !< -1: left, 1: right
  !...Declare local variables
    integer(i8) :: iface
    integer(i8) :: dw !< coefficient for wind stencil
  continue
    !
    ! real-mesh:               * --- * --- * --- *
    ! real-index:              1     2     3     4
    ! face-index:           1     2     3     4     5
    ! ghost-mesh:  + ... + ... * --- * --- * --- * ... + ... +
    ! p-index:     1     2     3     4     5     6     7     8
    !                      2.5   3.5   4.5   5.5   6.5
    !
    !     iface   left   right
    !       1      2       3
    !       2      3       4
    !       :      :       :
    !       5      6       7
    !         iface+2-1  iface+2
    !
    select case (wind)
      case (-1) !< left
        dw = -1
      case (1) !< right
        dw = 0
      case default
        call devlog%print(where='scheme_reconstruct_simple::reconstruct_simple_sub', &
                          message='wind is not -1 or 1', &
                          level=LogLevel%Error)
    end select
  !...Reconstruction
    do iface = 1, nface
      p_r(iface) = p_ghost(iface + dghost + dw)
    end do
  end subroutine reconstruct_simple_sub

end module scheme_reconstruct_simple
