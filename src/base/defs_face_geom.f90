!=============================================================================80
!
!> Define face geometry/mesh information.
!
!  coordinate: xyz
!  vector:     xyz
!  direction:  ijk
!
! Attention: NOT save flow feild data in these class
!
!=============================================================================80

module defs_face_geom

  use kind_parameter,   only : i2, i4, i8, dp, len_short
  use global_type_defs, only : TypeDefs

  use global_devlog, only : devlog, LogLevel

  implicit none

!...Define scope
  private

  public :: ClassGeomFace
  public :: ClassGeomPlane

  public :: NDIR, NCOORD

  integer(i4), parameter :: NDIR   = 3 !< i, j, k
  integer(i4), parameter :: NCOORD = 3 !< x, y, z

!...Declare private class
  !> basic mesh element (NOT use yet)
  type :: ClassGeomNode
    integer(i8), private :: i, j, k
    real(dp),    private :: x, y, z
  end type ClassGeomNode

!...Declare public class
  !> basic geometry element: Face,
  !! define by two points (i,j,k)
  type :: ClassGeomFace
    character(len_short),  private :: name = ""
    integer,               private :: type = 0
    integer,               private :: dir  = 0
                          !< face direction
    integer(i8),           private :: range_min(NDIR) = 0
                          !< min of ijk direction (min digonal node)
    integer(i8),           private :: range_max(NDIR) = 0
                          !< max of ijk direction (max digonal node)
    integer,               private :: donor_type(NDIR) = (/1,2,3/)
                          !< direction transformation
                          !< 1: donor (min, max) -- (min, max);
                          !< -1: donor (min, max) -- (max, min)
    integer(i8),           private :: donor_range_min(NDIR) = 0
                          !< min of ijk direction (min digonal node)
    integer(i8),           private :: donor_range_max(NDIR) = 0
                          !< max of ijk direction (max digonal node)
    class(ClassGeomFace),  private, pointer :: donor_face_ => null()
                          !< for connectivity, donor face
  contains
    procedure, public,  pass :: set        => face_set_fun
    procedure, public,  pass :: normal     => face_normal_fun
    procedure, public,  pass :: position   => face_position_fun
    procedure, public,  pass :: size       => face_size_fun
    procedure, public,  pass :: empty      => face_empty_fun
    procedure, public,  pass :: clear      => face_clear_sub
    procedure, public,  pass :: is_type    => face_is_type_fun
    procedure, public,  pass :: copy_from  => face_copy_from_face_sub
    procedure, public,  pass :: copy_to    => face_copy_to_face_sub
    procedure, public,  pass :: connect_to => face_connect_to_face_sub
    procedure, public,  pass :: donor_face => face_donor_face_fun
    procedure, public,  pass :: new_donor_face => face_new_donor_face_sub
    procedure, public,  pass :: trans_donor_type => face_transform_donor_type_fun

    procedure, private,  pass :: cut_by_plane => face_cut_by_plane_fun
    procedure, private,  pass :: cut_by_range => face_cut_by_range_fun
    generic,   public :: cut => cut_by_plane, cut_by_range

    final :: delete_ClassGeomFace
  end type ClassGeomFace

  !> geometry element: plane (a very big face)
  type, extends(ClassGeomFace) :: ClassGeomPlane
  contains
  end type ClassGeomPlane
  interface ClassGeomPlane
    module procedure new_ClassGeomPlane
  end interface ClassGeomPlane

contains

  !> Constructor of class geometry plane
  function new_ClassGeomPlane(dir, pos) result(plane)
    type(ClassGeomPlane) :: plane
    integer,     intent(in) :: dir
    integer(i8), intent(in) :: pos
  continue
    plane%name = "Plane"
    plane%range_min(:) = - 1E4
    plane%range_max(:) = + 1E4
    ! set position
    plane%range_min(dir) = pos
    plane%range_max(dir) = pos
  end function new_ClassGeomPlane

!=============================================================================80
!
! Method for face
!
!=============================================================================80

  !> Destructor of class ClassGeomFace
  subroutine delete_ClassGeomFace(this)
    type(ClassGeomFace), intent(inout) :: this
  continue
    if ( associated(this%donor_face_) ) nullify(this%donor_face_)
  end subroutine delete_ClassGeomFace

  !> Set face
  subroutine face_set_fun(this, range_min, range_max, iblk, name, type)
    class(ClassGeomFace), intent(inout) :: this
    integer(i8),          intent(in)    :: range_min(NDIR)
    integer(i8),          intent(in)    :: range_max(NDIR)
    integer,              intent(in),  optional :: iblk  !< parent block
    character(len=*),     intent(in),  optional :: name
    integer,              intent(in),  optional :: type  !< from type define
  continue
    this%range_min = range_min
    this%range_max = range_max
    if ( present(name) ) this%name = name
    if ( present(type) ) this%type = type
  end subroutine face_set_fun

  !> Compute normal direction of face
  integer function face_normal_fun(this) result(dir)
    class(ClassGeomFace), intent(inout) :: this
  continue
    do dir = 1, 3
      if ( this%range_min(dir) == this%range_max(dir) ) exit
    end do
  end function face_normal_fun

  !> Compute position of face (absolute position)
  integer function face_position_fun(this) result(pos)
    class(ClassGeomFace), intent(inout) :: this
  continue
    pos = this%range_min(this%normal())
  end function face_position_fun

  !> Compute size of face
  integer function face_size_fun(this) result(size)
    class(ClassGeomFace), intent(inout) :: this
    integer :: dir
  continue
    size = 1
    do dir = 1, NDIR
      size = size * (this%range_max(dir) - this%range_min(dir) + 1)
    end do
  end function face_size_fun

  !> If face is empty
  logical function face_empty_fun(this) result(empty)
    class(ClassGeomFace), intent(inout) :: this
    integer :: dir
  continue
    empty = .false.
    do dir = 1, NDIR
      if (dir == this%normal()) cycle
      if ( this%range_min(dir) >= this%range_max(dir) ) then
        empty = .true.
      end if
    end do
  end function face_empty_fun

  !> Clear face
  subroutine face_clear_sub(this)
    class(ClassGeomFace), intent(inout) :: this
  continue
    this%name      = ""
    this%type      = 0
    this%dir       = 0
    this%range_min = 0
    this%range_max = 0

    this%donor_type      = 0
    this%donor_range_min = 0
    this%donor_range_max = 0
    this%donor_face_ => null()
  end subroutine face_clear_sub

  !> Cut face and return left and right face
  ! TODO: test this function
  type(ClassGeomFace) function face_cut_by_plane_fun(this, cut, side) result(face)
    class(ClassGeomFace),  intent(inout) :: this
    class(ClassGeomFace),  intent(inout) :: cut  !< absolute cut plane
    integer,               intent(in)    :: side !< 1: positive, -1: negative
  !...Declare local variables
    integer     :: donor_dir
    integer(i8) :: donor_pos
  continue
    ! inherit face properties
    call this%copy_to(face)
  !...Save face
    if ( side == -1 ) then
      face%range_max(cut%normal()) = min(face%range_max(cut%normal()), cut%position())
    else if ( side == 1 ) then
      face%range_min(cut%normal()) = max(face%range_min(cut%normal()), cut%position())
    else
      call devlog%print(where='ClassGeomFace::face_cut_fun',  &
                        message='side should be -1 or 1',    &
                        level=LogLevel%Error)
    end if
  !...cut donor face
    !
    ! donor:     (+)                       (-)
    !     * ---|----------*        * ---|----------*
    !     x1   pos        x2       x1   pos        x2
    !     * ---|----------*        * ----------|---*
    !     y1   dpos       y2       y1        dpos  y2
    !
    !   (+): dpos = y1 + pos - x1
    !   (-): dpos = y1 + x2 - pos
    !
    if ( this%type /= TypeDefs%BC%Connectivity ) return
    donor_dir = this%donor_type(cut%normal())
    if ( donor_dir > 0 ) then
      donor_pos = this%donor_range_min(abs(donor_dir)) + this%position() &
                - this%range_min(cut%position())
    else
      donor_pos = this%donor_range_min(abs(donor_dir)) - this%position() &
                + this%range_max(cut%position())
    end if
    if ( side == -1 ) then
      if ( donor_dir > 0 ) then
        face%donor_range_max(abs(donor_dir)) = donor_pos
      else
        face%donor_range_min(abs(donor_dir)) = donor_pos
      end if
    else if ( side == 1 ) then
      if ( donor_dir > 0 ) then
        face%donor_range_min(abs(donor_dir)) = donor_pos
      else
        face%donor_range_max(abs(donor_dir)) = donor_pos
      end if
    else
      call devlog%print(where='ClassGeomFace::face_cut_fun',  &
                        message='side should be -1 or 1',    &
                        level=LogLevel%Error)
    end if
  end function face_cut_by_plane_fun

  !> Face cut by range
  function face_cut_by_range_fun(this, range_min, range_max) result(face)
    class(ClassGeomFace), intent(inout) :: this
    integer(i8),          intent(in)    :: range_min(NDIR)
    integer(i8),          intent(in)    :: range_max(NDIR)
    type(ClassGeomFace)                 :: face
  !...Declare local variables
    integer     :: dir, donor_dir
    integer(i8) :: donor_range_min(NDIR)
    integer(i8) :: donor_range_max(NDIR)
  continue
  !...Face not in current face
    if ( range_min(this%normal()) /= this%position() .or. &
         range_max(this%normal()) /= this%position() ) then
      call face%clear()
      return
    end if
  !...Cut face
    call this%copy_to(face)
    do dir = 1, NDIR
      face%range_min(dir) = max(range_min(dir), face%range_min(dir))
      face%range_max(dir) = min(range_max(dir), face%range_max(dir))
    end do
    if ( face%empty() ) then
      call face%clear()
      return
    end if
  !...Cut donor face
    !
    ! donor:     (+)                       (-)
    !     * ---|----------*        * ---|----------*
    !     x1   pos        x2       x1   pos        x2
    !     * ---|----------*        * ----------|---*
    !     y1   dpos       y2       y1        dpos  y2
    !
    !   (+): dpos = y1 + pos - x1
    !   (-): dpos = y1 + x2 - pos
    !
    if ( this%type /= TypeDefs%BC%Connectivity ) return
    donor_dir = this%donor_type(this%normal())
    if ( donor_dir > 0 ) then
      do dir = 1, NDIR
        if (this%normal() == dir) cycle
        donor_range_min(dir) = this%donor_range_min(abs(donor_dir)) + &
                               range_min(dir) - this%range_min(dir)
        donor_range_max(dir) = this%donor_range_min(abs(donor_dir)) + &
                               range_max(dir) - this%range_min(dir)
      end do
    else
      do dir = 1, NDIR
        if (this%normal() == dir) cycle
        donor_range_max(dir) = this%donor_range_min(abs(donor_dir)) + &
                               this%range_max(dir) - range_min(dir)
        donor_range_min(dir) = this%donor_range_min(abs(donor_dir)) + &
                               this%range_max(dir) - range_max(dir)
      end do
    end if
    ! Set donor face
    do dir = 1, NDIR
      face%donor_range_min(dir) = max(donor_range_min(dir), face%donor_range_min(dir))
      face%donor_range_max(dir) = min(donor_range_max(dir), face%donor_range_max(dir))
    end do
  end function face_cut_by_range_fun

  !> If face is type
  logical function face_is_type_fun(this, itype) result(flag)
    class(ClassGeomFace), intent(inout) :: this
    integer,              intent(in)    :: itype
  continue
    flag = ( this%type == itype )
  end function face_is_type_fun

  !> Copy face from another face (for safety)
  subroutine face_copy_from_face_sub(this, face)
    class(ClassGeomFace), intent(inout) :: this
    class(ClassGeomFace), intent(in)    :: face
  continue
    this%name      = face%name
    this%type      = face%type
    this%dir       = face%dir
    this%range_min = face%range_min
    this%range_max = face%range_max

    this%donor_type      = face%donor_type
    this%donor_range_min = face%donor_range_min
    this%donor_range_max = face%donor_range_max
    this%donor_face_ => face%donor_face_
  end subroutine face_copy_from_face_sub

  !> Copy face from another face (for safety)
  subroutine face_copy_to_face_sub(this, face)
    class(ClassGeomFace), intent(inout) :: this
    type(ClassGeomFace),  intent(inout) :: face
  continue
    face%name      = this%name
    face%type      = this%type
    face%dir       = this%dir
    face%range_min = this%range_min
    face%range_max = this%range_max

    face%donor_type      = this%donor_type
    face%donor_range_min = this%donor_range_min
    face%donor_range_max = this%donor_range_max
    face%donor_face_ => this%donor_face_
  end subroutine face_copy_to_face_sub

  !> Get donor_type of donor face from current donor_type
  function face_transform_donor_type_fun(this) result(new_donor_type)
    class(ClassGeomFace), intent(inout) :: this
    integer :: new_donor_type(NDIR)
  !...Declare local variables
    integer :: dir, donor_dir
  continue
    do dir = 1, NDIR
      donor_dir = this%donor_type(dir)
      if ( donor_dir > 0 ) then
        new_donor_type(donor_dir) = dir
      else
        new_donor_type(abs(donor_dir)) = -dir
      end if
    end do
  end function face_transform_donor_type_fun

  !> Face connectivity
  subroutine face_connect_to_face_sub(this, donor_face, donor_type)
    class(ClassGeomFace), intent(inout) :: this
    type(ClassGeomFace),  pointer       :: donor_face
    integer,              intent(in),   optional :: donor_type(NDIR)
  !...Declare local variables
    integer :: dir
  continue
    this%donor_face_ => donor_face
    if ( present(donor_type) ) then
      this%donor_type = donor_type
      do dir = 1, NDIR
        if ( donor_type(dir) > 0 ) then
          this%donor_range_min(dir) = donor_face%range_min(donor_type(dir))
          this%donor_range_max(dir) = donor_face%range_max(donor_type(dir))
        else
          this%donor_range_min(dir) = donor_face%range_max(abs(donor_type(dir)))
          this%donor_range_max(dir) = donor_face%range_min(abs(donor_type(dir)))
        end if
      end do
    else
      this%donor_type      = donor_face%trans_donor_type()
      this%donor_range_min = donor_face%range_min
      this%donor_range_max = donor_face%range_max
    end if
  end subroutine face_connect_to_face_sub

  !> Get current donor face
  function face_donor_face_fun(this) result(donor_face)
    class(ClassGeomFace), intent(inout) :: this
    type(ClassGeomFace),  pointer       :: donor_face
  continue
    donor_face => this%donor_face_
  end function face_donor_face_fun

  !> Create new donor face, and delete old donor face, new donor face not link to current face
  subroutine face_new_donor_face_sub(this, new_donor_face, curr_face, ialloc)
    class(ClassGeomFace), intent(inout) :: this
    type(ClassGeomFace),  pointer       :: new_donor_face
    type(ClassGeomFace),  pointer       :: curr_face
    integer,              intent(out)   :: ialloc
  continue
    allocate(new_donor_face, stat=ialloc)
    if ( ialloc /= 0 ) return
    if ( associated(this%donor_face_)) nullify(this%donor_face_)
  !...Set donor face from current face
    new_donor_face%type       = this%type
    new_donor_face%range_min  = this%donor_range_min
    new_donor_face%range_max  = this%donor_range_max

    new_donor_face%donor_type = this%trans_donor_type()
    new_donor_face%donor_range_min = this%range_min
    new_donor_face%donor_range_max = this%range_max
    new_donor_face%donor_face_ => curr_face
  end subroutine face_new_donor_face_sub

end module defs_face_geom
