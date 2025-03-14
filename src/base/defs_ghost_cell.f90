!=============================================================================80
!
!> Ghost cell definitions
!
! FDM (1-layer)
!
!            + ... + ... +  <------- ghost layer
!            :     :     :           + ghost node
!      + ... * --- * --- * ... +     : ghost layer edge
!      :     |     |     |     :
!      + ... * --- * --- * ... +
!      :     |     |     | <-------- real mesh
!      + ... * --- * --- * ... +     * mesh node
!            :     :     :           | mesh edge
!            + ... + ... +
!
! FVM (1-layer) cell-centered
!
!             ..... .....
!            :  +  :  +  :  <------- ghost cell layer
!       ..... ----- ----- .....      + ghost node
!      :  +  |  *  |  *  |  +  :     : ghost layer edge
!       ..... ----- ----- .....
!      :  +  |  *  |  *  | <-------- real mesh
!       ..... ----- ----- .....      * mesh node
!            :  +  :  +  :           | mesh edge
!             ..... .....
!
!=============================================================================80

module defs_ghost_cell
  use kind_parameter,  only : dp, i8
  use defs_block_geom, only : NDIR

  implicit none
  private

  public :: ClassGhostMesh, ClassRealMesh

  type :: ClassGhostMesh
    private
    integer     :: nGhost_layers = 3 !< number of ghost cell layers each side
    integer     :: nGhost_region = 6 !< number of ghost cells region
    integer(i8) :: range_min(NDIR) = 1
                   !< min of ijk direction
    integer(i8) :: range_max(NDIR) = 1
                   !< max of ijk direction
  contains
    procedure :: init    => init_ghost_mesh
    procedure :: nvert   => ghost_nvert_ijk_fun
    procedure :: nregion => ghost_nregion_fun
    procedure :: nlayer  => ghost_nlayer_fun
    procedure :: layer   => ghost_layer_fun
  end type ClassGhostMesh

  type :: ClassRealMesh
    private
    integer(i8) :: range_min(NDIR) = 1
                   !< min of ijk direction
    integer(i8) :: range_max(NDIR) = 1
                   !< max of ijk direction
  contains
    procedure :: init    => init_real_mesh
    procedure :: nvert   => real_nvert_ijk_fun
    procedure :: nface   => real_nface_ijk_fun
    procedure :: range   => real_ijk_range_fun
    procedure :: map_ijk => map_ijk_to_ghost_ijk
  end type ClassRealMesh

contains

!=============================================================================80
!
! ClassGhostMesh method
!
!=============================================================================80

  !> Initialize ghost cell mesh
  subroutine init_ghost_mesh(this, nvert)
  !...Declare input/output variables
    class(ClassGhostMesh), intent(inout) :: this
    integer(i8),           intent(in)    :: nvert(:)
  !...Declare local variable
    integer :: dir
  continue
    do dir = 1, NDIR
      this%range_min(dir) = 1;
      this%range_max(dir) = nvert(dir) + 2 * this%nGhost_layers
    end do
  end subroutine init_ghost_mesh

  !> Get number of vertices in ijk direction
  integer(i8) function ghost_nvert_ijk_fun(this, dir) result(nvert)
  !...Declare input/output variables
    class(ClassGhostMesh), intent(inout) :: this
    integer,               intent(in)    :: dir
  continue
    nvert = this%range_max(dir) - this%range_min(dir) + 1
  end function ghost_nvert_ijk_fun

  !> Get number of ghost regions
  integer function ghost_nregion_fun(this) result(nghost)
  !...Declare input/output variables
    class(ClassGhostMesh), intent(inout) :: this
  continue
    nghost = this%nGhost_region
  end function ghost_nregion_fun

  !> Get number of ghost layers at each region
  integer function ghost_nlayer_fun(this, iregion) result(nlayer)
  !...Declare input/output variables
    class(ClassGhostMesh), intent(inout) :: this
    integer,               intent(in)    :: iregion
  continue
    nlayer = 0
    if (iregion <= this%nGhost_region) then
      nlayer = this%nGhost_layers
    end if
  end function ghost_nlayer_fun

  !> Get ghost layer at each region
  function ghost_layer_fun(this, iregion, ilayer) result(layer)
  !...Declare input/output variables
    class(ClassGhostMesh), intent(inout) :: this
    integer,               intent(in)    :: iregion
    integer,               intent(in)    :: ilayer
    integer(i8) :: layer(3,3) !< ((imin,imax,norm),(jmin,jmax,norm),(kmin,kmax,norm))
  continue
    !                                     j
    !    * --- * --- *  0               4 +    + 5
    !    :     :     :                    :  .          Left 1  Right 2
    !    + ... + ... +  1           + ... * ... +  i    Bottom 3  Top 4
    !    :     :     :              1  .  :     2       Back 5  Front 6
    !    + ... + ... +  2            + 6  + 3
    !       Layer                   k   Region
    !
    layer(:,1) = this%range_min(:) + this%nGhost_layers
    layer(:,2) = this%range_max(:) - this%nGhost_layers
    layer(:,3) = 0
    select case (iregion)
      case (1) !< Region 1 (left, direction-i, negative)
        layer(1,1) = this%range_min(1) + this%nGhost_layers - ilayer
        layer(1,2) = layer(1,1)
        layer(1,3) = -1
      case (2) !< Region 2 (right, direction-i, positive)
        layer(1,2) = this%range_max(1) - this%nGhost_layers + ilayer
        layer(1,1) = layer(1,2)
        layer(1,3) = 1
      case (3) !< Region 3 (bottom, direction-j, negative)
        layer(2,1) = this%range_min(2) + this%nGhost_layers - ilayer
        layer(2,2) = layer(2,1)
        layer(2,3) = -1
      case (4) !< Region 4 (top, direction-j, positive)
        layer(2,2) = this%range_max(2) - this%nGhost_layers + ilayer
        layer(2,1) = layer(2,2)
        layer(2,3) = 1
      case (5) !< Region 5 (back, direction-k, negative)
        layer(3,1) = this%range_min(3) + this%nGhost_layers - ilayer
        layer(3,2) = layer(3,1)
        layer(3,3) = -1
      case (6) !< Region 6 (front, direction-k, positive)
        layer(3,2) = this%range_max(3) - this%nGhost_layers + ilayer
        layer(3,1) = layer(3,2)
        layer(3,3) = 1
    end select
  end function ghost_layer_fun

!=============================================================================80
!
! ClassRealMesh method
!
!=============================================================================80

  !> Initialize real cell
  subroutine init_real_mesh(this, ghost_mesh)
  !...Declare input/output variables
    class(ClassRealMesh),  intent(inout) :: this
    class(ClassGhostMesh), intent(in)    :: ghost_mesh
  !...Declare local variable
    integer :: dir
  continue
    do dir = 1, NDIR
      this%range_min(dir) = ghost_mesh%range_min(dir) + ghost_mesh%nGhost_layers
      this%range_max(dir) = ghost_mesh%range_max(dir) - ghost_mesh%nGhost_layers
    end do
  end subroutine init_real_mesh

  !> Get number of mesh vertices in ijk direction
  integer(i8) function real_nvert_ijk_fun(this, dir) result(nvert)
  !...Declare input/output variables
    class(ClassRealMesh),  intent(inout) :: this
    integer,               intent(in)    :: dir
  continue
    nvert = this%range_max(dir) - this%range_min(dir) + 1
  end function real_nvert_ijk_fun

  !> Get number of mesh faces in ijk direction
  integer(i8) function real_nface_ijk_fun(this, dir) result(nface)
  !...Declare input/output variables
    class(ClassRealMesh),  intent(inout) :: this
    integer,               intent(in)    :: dir
  continue
    nface = this%nvert(dir) + 1
  end function real_nface_ijk_fun

  !> Get ijk range on real mesh
  integer(i8) function real_ijk_range_fun(this, dir, pos) result(idx)
  !...Declare input/output variables
    class(ClassRealMesh),  intent(inout) :: this
    integer,               intent(in)    :: dir
    integer,               intent(in)    :: pos !< 1: begin, 2: end
  continue
    if (pos == 1) then
      idx = this%range_min(dir)
    else
      idx = this%range_max(dir)
    end if
  end function real_ijk_range_fun

  !> Map geometry mesh vertex ijk index to ghost mesh vertex index
  integer(i8) function map_ijk_to_ghost_ijk(this, dir) result(dijk)
  !...Declare input/output variables
    class(ClassRealMesh),  intent(inout) :: this
    integer,               intent(in)    :: dir
  continue
    dijk = this%range_min(dir) - 1
  end function map_ijk_to_ghost_ijk

end module defs_ghost_cell
