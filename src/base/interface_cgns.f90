!=============================================================================80
!
!> Interface module for IO of CGNS Format meshfile.
!
! User Guide:
!   https://cgns.github.io/cgns-modern.github.io/doc/userguide.html
! Library Interface:
!   https://cgns.github.io/cgns-modern.github.io/standard/MLL/CGNS_MLL.html#standardmll
! File Node Library:
!   https://cgns.github.io/cgns-modern.github.io/standard/FMM/nodes.html#
! Type Define:
!   https://cgns.github.io/CGNS_docs_current/midlevel/general.html#typedefs
! CFL3D Implement:
!   https://github.com/nasa/CFL3D/blob/master/source/tools/cgns_to_cfl3dinput.F
!
!=============================================================================80

module interface_cgns

  use kind_parameter, only : len_short, len_string, sp, dp, i4, i8

  use global_error, only : ClassError

  use cgns

  implicit none

!...Define scope
  private

  public  :: ClassCGNSMethod

!...Declare local variables
  type :: ClassCGNSZoneGrid
    integer(cgsize_t)     :: isize(3,3) = 1
      !< Number of vertices, cells, and boundary vertices in each (index)-dimension
      !<
      !< 3D structured: NVertex, NCell, NBoundaryVertex (IJK)
      !<
      !< 2D structured: NVertex, NCell, NBoundaryVertex (IJ)
      !<
      !< 3D unstructured: NVertex, NCell3D, NBoundVertex
      !<
      !< 2D unstructured: NVertex, NCell2D, NBoundVertex
      !!
    integer(cgsize_t)     :: NVertex(3) = 1
      !< Number of vertices in each (index)-dimension
    integer(cgsize_t)     :: NCell(3) = 1
      !< Number of cells in each (index)-dimension
    integer(cgsize_t)     :: NBoundaryVertex(3) = 0
      !< Number of boundary vertices in each (index)-dimension
      !<
      !< structured = 0
    integer               :: index_dim = 0
      !< Structured zones = base cell dimension,
      !< Unstructured zones = 1
    integer(cgsize_t)     :: irmin(3) = 1
      !< lower range index
    integer(cgsize_t)     :: irmax(3) = 1
      !< upper range index
    integer               :: ncoords = 0
      !< Number of coordinate arrays for zone Z
    character(len_string) :: data_type = ""
    integer               :: datatype(3) = 0
      !< RealSingle or RealDouble
    character(len_string) :: region = ""
      !< X-Y or X-Z or Y-Z or X-Y-Z
    character(len_string) :: coordname(3) = ""
      !< Name of coordinate: CoordinateX, CoordinateY, CoordinateZ
    real(sp), allocatable :: coord_s(:,:,:,:)
    real(dp), allocatable :: coord_d(:,:,:,:)
  end type ClassCGNSZoneGrid

  type :: ClassCGNSZoneBC
    character(len_short) :: name = ""
    integer              :: itype = 0
    character(len_short) :: type = ""
      !< The admissible types are FamilySpecified
    character(len_short) :: fam_name = ""
    character(len_short) :: phy_type = "Wall"
      !< physical type
    integer              :: iptset = 0
      !< PointSet type; PointRange or PointList
    character(len_short) :: ptset_type = ""
      !< PointSet type; PointRange or PointList
    integer(cgsize_t)    :: npnts = 0
      !< Number of boundary points or elements;
      !< PointRange => 2; PointList => npts
    integer              :: NormalIndex(3) = 0
      !< coordinate direction of the boundary condition patch normal
    integer(cgsize_t)    :: NormalListFlag = 0
      !< NormalList defined = 1; not = 0
    integer              :: NormalDataType = 0
      !< RealSingle or RealDouble
    integer              :: ndataset = 0
      !< Number of boundary condition datasets
    integer(cgsize_t)    :: pnts(3,2) = 0
      !< (ndim, npnts);
      !<
      !< ndim: unstructured = 1; structured = 2 or 3
      !<
      !< npnts: PointRange = 2; PointList = npts
  end type ClassCGNSZoneBC

  type :: ClassCGNSZoneConn
    character(len_short) :: name = ""
    character(len_short) :: donor_name = ""
      !< Name of the zone interfacing with the current zone
    character(len_short) :: type = "Conn-1to1"
    integer(cgsize_t)    :: pnts(3,2) = 0
    integer(cgsize_t)    :: donor_pnts(3,2) = 0
    integer              :: transform(3) = 0
      !< Short hand notation for the transformation matrix defining
      !< the relative orientation of the two zones.
      !< https://cgns.github.io/cgns-modern.github.io/standard/SIDS/multizone.html
  end type ClassCGNSZoneConn

  type :: ClassCGNSZone
    character(len_short)  :: name = ""
      !< Zone0001...
    character(len_short)  :: type = ""
      !< The admissible types are Structured and Unstructured
    integer               :: nbocos = 0
      !< Number of boundary conditions
    integer               :: nconns = 0
      !< Number of connectivity
    type(ClassCGNSZoneGrid)              :: grid
      !< Grid information
    type(ClassCGNSZoneBC),   allocatable :: boco(:)
      !< Boundary conditions
    type(ClassCGNSZoneConn), allocatable :: conn(:)
      !< Connectivity
  contains
    final :: delete_ClassCGNSZone
  end type ClassCGNSZone

  type :: ClassCGNSFam
    character(len_short) :: name = ""
    integer              :: iBC = 1
      !< Family boundary condition index number = 1
    integer              :: nBC = 0
    character(len_short) :: BCName = ""
    integer              :: iBCType = 0
    character(len_short) :: BCType = ""
    integer              :: nGeo = 0
  end type ClassCGNSFam

  type :: ClassCGNSBase
    character(len_string) :: name     = ""
    integer               :: cell_dim = 0
      !< Dimension of the cells; 3 for volume cells,
      !< 2 for surface cells; 1 for line cells.
    integer               :: phys_dim = 0
      !< Number of coordinates required to define a vector in the field
    integer               :: nfamilies  = 0
    !< number of family names under Family_t
    integer               :: nzone    = 0
    type(ClassCGNSZone),  pointer :: zone(:) => null()
    type(ClassCGNSFam),   pointer :: family(:) => null()
  contains
    final :: delete_ClassCGNSBase
  end type ClassCGNSBase

  type :: ClassCGNSMethod
    integer,               private :: ier       = 0
    character(len_string), private :: file_type = ""
    real(sp),              private :: version   = 0.0
    character(len_string), private :: precision = ""
    integer,               private :: nbase     = 0
      !< Number of bases present
    type(ClassCGNSBase),   private, allocatable :: base(:)
  contains
  !...Public procedures
    procedure, public,  pass :: read_mesh  => read_mesh_sub
    procedure, public,  pass :: info       => print_info_fun
    procedure, public,  pass :: idimension => print_idimension_fun
    procedure, public,  pass :: size       => print_size_sub
    procedure, public,  pass :: grid       => print_grid_fun
    ! procedure, public,  pass :: boundary   => print_zone_bc_sub
  !...Private procedures
    procedure, private, pass :: occur     => occur_error_fun
    procedure, private, pass :: is_cgns_file => is_cgns_file_fun
    procedure, private, pass :: get_description => get_description_sub
    procedure, private, pass :: get_file_info => get_file_info_sub
    procedure, private, nopass :: get_base_info => get_base_info_sub
    procedure, private, nopass :: get_zone_info => get_zone_info_sub
    procedure, private, nopass :: get_zone_coord_info => get_zone_coord_info_sub
    procedure, private, nopass :: check_zone_datatype => check_zone_datatype_sub
    procedure, private, nopass :: get_zone_coord => get_zone_coord_sub
    procedure, private, nopass :: get_zone_bc => get_zone_bc_sub
    procedure, private, nopass :: get_zone_conn => get_zone_conn_sub
    procedure, private, nopass :: get_family_info => get_family_info_sub
    procedure, private, nopass :: get_family_bc => get_family_bc_sub
    procedure, private, nopass :: set_bc_form_family => set_bc_form_family_sub
    final :: delete_ClassCGNSMethod
  end type ClassCGNSMethod

contains

  !> Destructor of class ClassCGNSZone
  subroutine delete_ClassCGNSZone(this)
    type(ClassCGNSZone), intent(inout) :: this
  continue
    if (allocated(this%grid%coord_s)) deallocate(this%grid%coord_s)
    if (allocated(this%grid%coord_d)) deallocate(this%grid%coord_d)
    if (allocated(this%boco))         deallocate(this%boco)
    if (allocated(this%conn))         deallocate(this%conn)
  end subroutine delete_ClassCGNSZone
  !> Destructor of class ClassCGNSBase
  subroutine delete_ClassCGNSBase(this)
    type(ClassCGNSBase), intent(inout) :: this
  continue
    if (associated(this%zone))   deallocate(this%zone)
    if (associated(this%family)) deallocate(this%family)
  end subroutine delete_ClassCGNSBase
  !> Destructor of class ClassCGNSMethod
  subroutine delete_ClassCGNSMethod(this)
    type(ClassCGNSMethod), intent(inout) :: this
  continue
    if (allocated(this%base)) deallocate(this%base)
  end subroutine delete_ClassCGNSMethod

  !> Read meshfile
  subroutine read_mesh_sub(this, file_name, error)
    class(ClassCGNSMethod), intent(inout) :: this
    character(len=*),       intent(in)    :: file_name
        !< name of meshfile
    type(ClassError),       intent(inout) :: error
        !< error when reading cgns meshfile
  !...Declare local variables
    integer :: precision
    integer :: index_file, index_base, index_zone, index_bc, index_fam
    type(ClassCGNSZone), pointer :: zone_i
  continue
  !...Check if file is cgns
    if ( .not. this%is_cgns_file(file_name) ) then
      call error%set(occur=.true., info="File is not cgns format.")
      return
    end if
  !...Open file
    call cg_open_f(trim(file_name), CG_MODE_READ, index_file, this%ier)
    if (this%occur(error)) return
  !...Get basic information
    call this%get_file_info(index_file, this%ier)
    if (this%occur(error)) return
  !...Get number of base nodes
    call cg_nbases_f(index_file, this%nbase, this%ier)
    if (this%occur(error)) return
  !...Check number of base
    if ( this%nbase > 1 ) then
      call error%set(occur=.true., info="Not support multiple base.")
      return
    end if
    allocate(this%base(this%nbase), stat=error%ialloc)
    if ( error%allocate("CGNS Base") ) return
  !...Loop over base nodes
    do index_base = 1, this%nbase
    !...Get base node information
      call this%get_base_info(index_file, index_base, &
                              this%base(index_base), this%ier)
      if (this%occur(error)) return
    !...Get families
      call cg_nfamilies_f(index_file, index_base, &
                          this%base(index_base)%nfamilies, this%ier)
      if (this%occur(error)) return
      allocate(this%base(index_base)%family(this%base(index_base)%nfamilies), &
               stat=error%ialloc)
      if ( error%allocate("CGNS Family") ) return
      do index_fam = 1, this%base(index_base)%nfamilies
        call this%get_family_info(index_file, index_base, index_fam, &
                                  this%base(index_base)%family(index_fam), &
                                  this%ier)
        if (this%occur(error)) return
        call this%get_family_bc(index_file, index_base, index_fam, &
                                this%base(index_base)%family(index_fam), &
                                this%ier)
        if (this%occur(error)) return
      end do
    !...Get number of zones
      call cg_nzones_f(index_file, index_base,      &
                       this%base(index_base)%nzone, &
                       this%ier)
      if (this%occur(error)) return
      allocate(this%base(index_base)%zone(this%base(index_base)%nzone), &
               stat=error%ialloc)
      if ( error%allocate("CGNS Zone") ) return
    !...Loop over zones
      do index_zone = 1, this%base(index_base)%nzone
        zone_i => this%base(index_base)%zone(index_zone)
      !...Get zone information
        call this%get_zone_info(index_file, index_base, index_zone, &
                                zone_i, this%ier)
        if (this%occur(error)) return
      !...Read Grid
        call this%get_zone_coord_info(index_file, index_base, index_zone, &
                                      zone_i%grid, this%ier)
        if (this%occur(error)) return
        call this%check_zone_datatype(zone_i%grid,&
         error)
        if ( error%occur() ) return
        call this%get_zone_coord(index_file, index_base, index_zone, &
                                 zone_i%grid, this%ier)
        if (this%occur(error)) return
      !...Get zone boundary condition
        call cg_nbocos_f(index_file, index_base, index_zone, &
                         zone_i%nbocos, this%ier)
        if (this%occur(error)) return
        if ( zone_i%nbocos > 0 ) then
          allocate(zone_i%boco(zone_i%nbocos), stat=error%ialloc)
          if ( error%allocate("CGNS Zone BC") ) return
          do index_bc = 1, zone_i%nbocos
            call this%get_zone_bc(index_file, index_base, index_zone, index_bc, &
                                  zone_i%boco(index_bc), this%ier, error)
            ! TODO: this add "error" because some choices are not implemented
            if (error%occur())     return
            if (this%occur(error)) return
            if ( zone_i%boco(index_bc)%itype == FamilySpecified ) then
              call this%set_bc_form_family(index_file, index_base, &
                                           index_zone, index_bc,   &
                                           this%base(index_base)%family, &
                                           zone_i%boco(index_bc),  &
                                           this%ier)
              if (this%occur(error)) return
            end if
          end do
        end if
        if (this%occur(error)) return
      !...Get zone connectivity
        call cg_n1to1_f(index_file, index_base, index_zone, &
                        zone_i%nconns, this%ier)
        if (this%occur(error)) return
        if ( zone_i%nconns > 0 ) then
          allocate(zone_i%conn(zone_i%nconns), stat=error%ialloc)
          if ( error%allocate("CGNS Zone Conn") ) return
          do index_bc = 1, zone_i%nconns
            call this%get_zone_conn(index_file, index_base, index_zone, index_bc, &
                                    zone_i%conn(index_bc), this%ier)
            if (this%occur(error)) return
          end do
        end if
      end do
    end do
    nullify(zone_i)
  !...Close file
    call cg_close_f(index_file, this%ier)
    if (this%occur(error)) return
  end subroutine read_mesh_sub

  !> Get error message
  logical function occur_error_fun(this, error) result(is_error)
    class(ClassCGNSMethod), intent(inout) :: this
    type(ClassError),      intent(inout) :: error
    character(len_string) :: error_message = ""
  continue
    if (this%ier .ne. CG_OK) then
      call cg_get_error_f(error_message)
      call error%set(occur=.true., info=error_message)
      return
    end if
  end function occur_error_fun

  !> Print CGNS file information
  function print_info_fun(this) result(info)
    use interface_stdlib_string, only : ClassStringMethod
    use container_linklist,      only : linklist
  !...Declare input/output variables
    class(ClassCGNSMethod), intent(inout) :: this
    character(len_string),  allocatable   :: info(:)
  !...Declare local variables
    type(ClassStringMethod) :: string
    integer                 :: ninfo, iinfo
    integer                 :: ibase, izone, ifam, iboco, iconn
    integer                 :: ierr
    type(linklist)          :: info_
    integer                 :: stat_
    class(*),       pointer :: curr__
    type(ClassCGNSZone), pointer :: zone_i

  continue
  !...Add information to linklist
    stat_ = info_%push_back("CGNS Format:    " // trim(this%file_type))
    stat_ = info_%push_back("CGNS Version:   " // string%from(this%version))
    stat_ = info_%push_back("Data Precision: " // trim(this%precision))
    stat_ = info_%push_back("Number of Base: " // string%from(this%nbase))
    do ibase = 1, this%nbase
      stat_ = info_%push_back("")
    !...Write base information
      stat_ = info_%push_back("Base " // string%from(ibase))
      stat_ = info_%push_back("Base Name: " // trim(this%base(ibase)%name))
      stat_ = info_%push_back("Cell Dimension: " // &
                    string%from(this%base(ibase)%cell_dim))
      stat_ = info_%push_back("Physical Dimension: " // &
                    string%from(this%base(ibase)%phys_dim))
    !...Write families information
      stat_ = info_%push_back("Number of BC Families: " // &
                    string%from(this%base(ibase)%nfamilies))
      stat_ = info_%push_back("  " // &
                              string%padr("Family", 8) // &
                              string%padr("Name", 8)   // &
                              string%padr("Type", 8))
      do ifam = 1, this%base(ibase)%nfamilies
        stat_ = info_%push_back("  " // &
                      string%from(ifam, '(I4)') // &
                      " " // &
                      string%padr(trim(this%base(ibase)%family(ifam)%name), 12) // &
                      " " // &
                      trim(this%base(ibase)%family(ifam)%BCType))
      end do
    !...Write zone information
      stat_ = info_%push_back("Number of Zones: " // &
                    string%from(this%base(ibase)%nzone))
      stat_ = info_%push_back("   " // &
                              string%padr("Zone", 8) // &
                              string%padr("Name", 8)   // &
                              string%padr("Type", 7)   // &
                              string%padr("Index_Dim", 12)   // &
                              string%padr("DataType", 10)   // &
                              string%padr("Region", 8)   // &
                              string%padr("Vertices", 16)   // &
                              string%padr("Cells", 8))
      do izone = 1, this%base(ibase)%nzone
        zone_i => this%base(ibase)%zone(izone)
        stat_ = info_%push_back("  " // &
                      string%from(izone, '(I4)') // &
                      "  " // &
                      string%padr(trim(zone_i%name), 8) // &
                      "  " // &
                      trim(zone_i%type) // &
                      "  " // &
                      string%from(zone_i%grid%index_dim) // &
                      "    " // &
                      trim(zone_i%grid%data_type)  // &
                      "    " // &
                      trim(zone_i%grid%region) // &
                      "    " // &
                      string%from(zone_i%grid%NVertex(1)) // &
                      " x " // &
                      string%from(zone_i%grid%NVertex(2)) // &
                      " x " // &
                      string%from(zone_i%grid%NVertex(3)) // &
                      "    " // &
                      string%from(zone_i%grid%NCell(1)) // &
                      " x " // &
                      string%from(zone_i%grid%NCell(2)) // &
                      " x " // &
                      string%from(zone_i%grid%NCell(3)))
      end do
    !...Write zone boundary condition and connectivity information
      do izone = 1, this%base(ibase)%nzone
        stat_ = info_%push_back("Boundary of Zone " // string%from(izone))
        zone_i => this%base(ibase)%zone(izone)
        stat_ = info_%push_back("   " // &
                                string%padr("Bound", 8) // &
                                string%padr("Name", 8)   // &
                                string%padr("Type", 14)   // &
                                string%padr("PointSet", 13)   // &
                                string%padr("Npoints", 10)   // &
                                string%padr("Normal", 8))
        do iboco = 1, zone_i%nbocos
          stat_ = info_%push_back("   " // &
                        string%from(iboco, '(I3)') // &
                        "  " // &
                        string%padr(trim(zone_i%boco(iboco)%name),8) // &
                        "  " // &
                        string%padr(trim(zone_i%boco(iboco)%phy_type), 12) // &
                        "  " // &
                        trim(zone_i%boco(iboco)%ptset_type) // &
                        "    " // &
                        string%from(zone_i%boco(iboco)%npnts, '(I6)') // &
                        "     " // &
                        string%from(zone_i%boco(iboco)%NormalListFlag))
        end do
      !...Write connectivity information
        do iconn = 1, zone_i%nconns
          stat_ = info_%push_back("   " // &
                        string%from(iconn+zone_i%nbocos, '(I3)') // &
                        "  " // &
                        trim(zone_i%conn(iconn)%name) // &
                        "  " // &
                        trim(zone_i%conn(iconn)%type) // &
                        " ==> " // &
                        trim(zone_i%conn(iconn)%donor_name))
        end do
      end do
      stat_ = info_%push_back("End Base")
    end do
    if ( stat_ /= 0 ) return
  !...Allocate memory for cgns file information
    ninfo = info_%size()
    allocate(info(ninfo), stat=ierr)
    if ( ierr /= 0 ) return
  !...Print cgns file information
    iinfo = 0
    do while (info_%cycle())
      iinfo = iinfo + 1
      curr__ => info_%current()
      select type (curr__)
        type is (character(len=*))
          info(iinfo) = curr__
        class default
          return
      end select
    end do
  !...Deallocate
    nullify(zone_i)
    nullify(curr__)
  end function print_info_fun

  !> Check if file is cgns format
  logical function is_cgns_file_fun(this, file_name) result(is_cgns)
    class(ClassCGNSMethod), intent(inout) :: this
    character(len=*),       intent(in)    :: file_name
  !...Declare local variables
    integer :: file_type
  continue
    call cg_is_cgns_f(trim(file_name), file_type, this%ier)
    select case (file_type)
      case (CG_FILE_HDF5)
        is_cgns = .true.
        this%file_type = "HDF5"
      case (CG_FILE_ADF)
        is_cgns = .true.
        this%file_type = "ADF"
      case (CG_FILE_ADF2)
        is_cgns = .true.
        this%file_type = "ADF2 (Version 2.5)"
      case default
        is_cgns = .false.
    end select
  end function is_cgns_file_fun

  !> Get CGNS file infomation
  subroutine get_file_info_sub(this, index_file, ier)
    class(ClassCGNSMethod), intent(inout) :: this
    integer,                intent(in)    :: index_file
    integer,                intent(out)   :: ier
  !...Declare local variables
    integer :: precision
  continue
    call cg_version_f(index_file, this%version, ier)
    if (ier .ne. CG_OK) return
    call cg_precision_f(index_file, precision, ier)
    if (ier .ne. CG_OK) return
    select case (precision)
      case (32)
        this%precision = "32-bit"
      case (64)
        this%precision = "64-bit"
      case (0)
        this%precision = "unknown"
    end select
  end subroutine get_file_info_sub

  !> Get CGNS file description
  subroutine get_description_sub(this, index_file, ier)
    class(ClassCGNSMethod), intent(inout) :: this
    integer,                intent(in)    :: index_file
    integer,                intent(out)   :: ier
  !...Declare local variables
    integer :: ndescriptors
            !< Number of Descriptor_t nodes under the current node
    integer :: size
            !< Size of the descriptor data (Fortran interface only)
    character(len_string) :: name
            !< Name of the descriptor
    character(len_string) :: text
            !< Description held in the Descriptor_t node
    integer :: ides
  continue
    call cg_ndescriptors_f(ndescriptors, ier)
    do ides = 1, ndescriptors
      call cg_descriptor_size_f(ides, size, ier)
      write (*,*) size
      call cg_descriptor_read_f(ides, name, text, ier)
      write (*,*) name, text
    end do
  end subroutine get_description_sub

  !> Get base_t information
  subroutine get_base_info_sub(index_file, index_base,base, ier)
  !...Declare input/output variables
    integer, intent(in) :: index_file
    integer, intent(in) :: index_base
    type(ClassCGNSBase), intent(inout) :: base
    integer, intent(out) :: ier
  !...Declare local variables
  continue
    call cg_base_read_f(index_file, index_base, base%name, base%cell_dim, &
                                                base%phys_dim, ier)
    if (ier .ne. CG_OK) return
  end subroutine get_base_info_sub

  !> Get zone_t information
  subroutine get_zone_info_sub(index_file, index_base, index_zone, zone, ier)
  !...Declare input/output variables
    integer, intent(in) :: index_file
    integer, intent(in) :: index_base
    integer, intent(in) :: index_zone
    type(ClassCGNSZone), intent(inout) :: zone
    integer, intent(out) :: ier
  !...Declare local variables
    integer           :: zone_type
    integer           :: index_dim
    integer(cgsize_t) :: isize(9)
  continue
  !...Get zone basic information
    call cg_zone_read_f(index_file, index_base, index_zone, &
                        zone%name, zone%grid%isize, ier)
    if (ier .ne. CG_OK) return
  !...Get zone type
    call cg_zone_type_f(index_file, index_base, index_zone, &
                        zone_type, ier)
    if (ier .ne. CG_OK) return
    zone%type = ZoneTypeName(zone_type)
  !...Get zone index dimension
    call cg_index_dim_f(index_file, index_base, index_zone, &
                                   zone%grid%index_dim, ier)
    if (ier .ne. CG_OK) return
  !...Set zone size
    isize     = reshape(zone%grid%isize, (/9/))
    index_dim = zone%grid%index_dim
    zone%grid%NVertex(1:index_dim)         = isize(1             :   index_dim)
    zone%grid%NCell(1:index_dim)           = isize(  index_dim+1 : 2*index_dim)
    zone%grid%NBoundaryVertex(1:index_dim) = isize(2*index_dim+1 : 3*index_dim)
  !...Set coord range
    zone%grid%irmin = 1
    zone%grid%irmax = zone%grid%NVertex
  end subroutine get_zone_info_sub

  !> Get zone_t coordinates
  subroutine get_zone_coord_info_sub(index_file, index_base, index_zone, &
                                     grid, ier)
    use interface_stdlib_string, only : ClassStringMethod
  !...Declare input/output variables
    integer, intent(in) :: index_file
    integer, intent(in) :: index_base
    integer, intent(in) :: index_zone
    type(ClassCGNSZoneGrid), intent(inout) :: grid
    integer, intent(out) :: ier
  !...Declare local variables
    integer                 :: icoord
    type(ClassStringMethod) :: string
  continue
    call cg_ncoords_f(index_file, index_base, index_zone, &
                      grid%ncoords, ier)
    if (ier .ne. CG_OK) return
    do icoord = 1, grid%ncoords
      call cg_coord_info_f(index_file, index_base, index_zone, icoord, &
                        grid%datatype(icoord), grid%coordname(icoord), ier)
      if (ier .ne. CG_OK) return
    end do
    select case (grid%ncoords)
      case (2)
        grid%region = &
          trim(string%replace_all(grid%coordname(1), "Coordinate", "")) // &
          "-" // &
          trim(string%replace_all(grid%coordname(2), "Coordinate", ""))
      case (3)
        grid%region = &
          trim(string%replace_all(grid%coordname(1), "Coordinate", "")) // &
          "-" // &
          trim(string%replace_all(grid%coordname(2), "Coordinate", "")) // &
          "-" // &
          trim(string%replace_all(grid%coordname(3), "Coordinate", ""))
    end select
  end subroutine get_zone_coord_info_sub

  !> Allocate coordinates
  subroutine check_zone_datatype_sub(grid, error)
    type(ClassCGNSZoneGrid), intent(inout) :: grid
    type(ClassError),        intent(inout) :: error
  continue
    if (sum(grid%datatype(1:grid%ncoords)) == grid%ncoords*RealSingle) then
      ! allocate(zone%coord_s(zone%NVertex(1), zone%NVertex(2), &
      !                       zone%NVertex(3), zone%ncoords),   &
      !                       stat=error%ialloc)
      grid%data_type = "RealSingle"
      allocate(grid%coord_d(grid%NVertex(1), grid%NVertex(2), &
                            grid%NVertex(3), 3),   &
                            stat=error%ialloc)
    else if (sum(grid%datatype(1:grid%ncoords)) == grid%ncoords*RealDouble) then
      grid%data_type = "RealDouble"
      allocate(grid%coord_d(grid%NVertex(1), grid%NVertex(2), &
                            grid%NVertex(3), 3),   &
                            stat=error%ialloc)
    else
      call error%set(.true., "The precision in each direction of &
                            & the CGNS grid is inconsistent")
      return
    end if
    if ( error%allocate("CGNS Coordinates") ) return
  end subroutine check_zone_datatype_sub

  !> Get zone coordinates
  subroutine get_zone_coord_sub(index_file, index_base, index_zone, grid, ier)
    integer,             intent(in)    :: index_file
    integer,             intent(in)    :: index_base
    integer,             intent(in)    :: index_zone
    type(ClassCGNSZoneGrid), intent(inout) :: grid
    integer,             intent(out)   :: ier
  !...Declare local variables
    integer :: icoord
    integer :: phys_coord
    ! real(sp), allocatable :: coord_s(:,:,:,:)
    ! real(dp), allocatable :: coord_d(:,:,:,:)
  continue
  !...Read coordinates
    do icoord = 1, grid%ncoords
      select case (trim(grid%coordname(icoord)))
        case ("CoordinateX")
          phys_coord = 1
        case ("CoordinateY")
          phys_coord = 2
        case ("CoordinateZ")
          phys_coord = 3
      end select
      ! if (zone%datatype(icoord) == RealSingle) then
        ! call cg_coord_read_f(index_file, index_base, index_zone, &
        !                      zone%coordname(icoord), zone%datatype(icoord), &
        !                      zone%irmin, zone%irmax, &
        !                      coord_s(:,:,:,phys_coord), ier)
      ! else if (zone%datatype(icoord) == RealDouble) then
        call cg_coord_read_f(index_file, index_base, index_zone, &
                             grid%coordname(icoord), grid%datatype(icoord), &
                             grid%irmin, grid%irmax, &
                             grid%coord_d(:,:,:,phys_coord), ier)
      ! end if
      if (ier .ne. CG_OK) return
    end do
  end subroutine get_zone_coord_sub

  !> Get zone boundary condition
  subroutine get_zone_bc_sub(index_file, index_base, index_zone, &
                             ibc, boco, ier, error)
    integer,             intent(in)    :: index_file
    integer,             intent(in)    :: index_base
    integer,             intent(in)    :: index_zone
    integer,             intent(in)    :: ibc
    type(ClassCGNSZoneBC), intent(inout) :: boco
    integer,             intent(out)   :: ier
    type(ClassError),    intent(inout) :: error
  !...Declare local variables
    integer           :: NormalList

  continue
  !...Get zone boundary condition information
    call cg_boco_info_f(index_file, index_base, index_zone, ibc, &
                        boco%name,            &
                        boco%itype,           &
                        boco%iptset,          &
                        boco%npnts,           &
                        boco%NormalIndex,     &
                        boco%NormalListFlag,  &
                        boco%NormalDataType,  &
                        boco%ndataset,        &
                        ier)
    if (ier .ne. CG_OK) return

  !...Check boundary condition
    boco%type = trim(BCTypeName(boco%itype))
    if ( boco%itype .ne. FamilySpecified ) then
      call error%set(.true., "CGNS: Not supported BC type" // trim(boco%type))
      return
    end if

    boco%ptset_type = trim(PointSetTypeName(boco%iptset))
    select case (boco%iptset)
      case (PointRange)
        continue
      case (PointList)
        call error%set(.true., "CGNS: Not supported PointSet type" // &
                               trim(boco%ptset_type))
        return
      case default
        call error%set(.true., "CGNS: Not supported PointSet type" // &
                               trim(boco%ptset_type))
        return
    end select

    if (  boco%NormalListFlag /= 0 ) then
      call error%set(.true., "CGNS: Not supported NormalList of BC")
      return
    end if

    if ( boco%ndataset /= 0 ) then
      call error%set(.true., "CGNS: Not supported Dataset of BC")
      return
    end if

  !...Get boundary condition points
    call cg_boco_read_f(index_file, index_base, index_zone, ibc, &
                        boco%pnts, NormalList, ier)
    if (ier .ne. CG_OK) return
  !...Get boundary condition family

  end subroutine get_zone_bc_sub

  !> Get zone connectivity
  subroutine get_zone_conn_sub(index_file, index_base, index_zone, iconn,&
                               conn, ier)
    integer,             intent(in)    :: index_file
    integer,             intent(in)    :: index_base
    integer,             intent(in)    :: index_zone
    integer,             intent(in)    :: iconn
    type(ClassCGNSZoneConn), intent(inout) :: conn
    integer,             intent(out)   :: ier
  continue
  !...Get zone connectivity
    call cg_1to1_read_f(index_file, index_base, index_zone, iconn,&
                        conn%name, &
                        conn%donor_name, &
                        conn%pnts, &
                        conn%donor_pnts, &
                        conn%transform, &
                        ier)
    if (ier .ne. CG_OK) return
  end subroutine get_zone_conn_sub

  !> Get base family
  subroutine get_family_info_sub(index_file, index_base, ifam, family, ier)
    integer,             intent(in)    :: index_file
    integer,             intent(in)    :: index_base
    integer,             intent(in)    :: ifam
    type(ClassCGNSFam),  intent(inout) :: family
    integer,             intent(out)   :: ier
  continue
  !...Get family information
    call cg_family_read_f(index_file, index_base, ifam, &
                          family%name, family%nBC, family%nGeo, ier)
    if (ier .ne. CG_OK) return
  end subroutine get_family_info_sub

  !> Get family boundary condition
  subroutine get_family_bc_sub(index_file, index_base, ifam, family, ier)
    integer,             intent(in)    :: index_file
    integer,             intent(in)    :: index_base
    type(ClassCGNSFam),  intent(inout) :: family
    integer,             intent(out)   :: ier
  !...Declare local variables
    integer :: ibc, ifam
  continue
    if ( family%nBC > 0 ) then
      call cg_fambc_read_f(index_file, index_base, ifam, family%iBC, &
                            family%BCName, family%iBCType, ier)
      if (ier .ne. CG_OK) return
    end if
    family%BCType = trim(BCTypeName(family%iBCType))
  end subroutine get_family_bc_sub

  !> Set BC for family
  subroutine set_bc_form_family_sub(index_file, index_base, &
                                    index_zone, index_bc, &
                                    family, boco, ier)
    integer,             intent(in)    :: index_file
    integer,             intent(in)    :: index_base
    integer,             intent(in)    :: index_zone
    integer,             intent(in)    :: index_bc
    type(ClassCGNSFam),  intent(inout) :: family(:)
    type(ClassCGNSZoneBC), intent(inout) :: boco
    integer,             intent(out)   :: ier
  !...Declare local variables
    integer :: ifam
  continue
    call cg_goto_f(index_file, index_base, ier, &
                   "Zone_t", index_zone, &
                   "ZoneBC_t", 1, &
                   "BC_t", index_bc, &
                   "end")
    if (ier .ne. CG_OK) return
    call cg_famname_read_f(boco%fam_name, ier)
    if (ier .ne. CG_OK) return
    do ifam = 1, size(family)
      if ( trim(family(ifam)%name) == trim(boco%fam_name) ) then
        boco%phy_type = trim(family(ifam)%BCType)
        return
      end if
    end do
  end subroutine set_bc_form_family_sub

  !> Print dimension
  integer function print_idimension_fun(this) result(idimension)
    class(ClassCGNSMethod), intent(in) :: this
  continue
    idimension = this%base(1)%cell_dim
  end function print_idimension_fun

  !> Print grid
  function print_grid_fun(this, izone, error) result(coord)
    class(ClassCGNSMethod), intent(in)    :: this
    integer,                intent(in)    :: izone
    type(ClassError),       intent(inout) :: error
    real(dp), allocatable :: coord(:,:,:,:)
  !...Declare local variables
    integer :: i, j, k
  continue
    i = this%base(1)%zone(izone)%grid%NVertex(1)
    j = this%base(1)%zone(izone)%grid%NVertex(2)
    k = this%base(1)%zone(izone)%grid%NVertex(3)
    allocate(coord(i, j, k, 3), stat=error%ialloc)
    if ( error%allocate("Grid") ) return
    coord = this%base(1)%zone(izone)%grid%coord_d
  end function print_grid_fun

  !> Print grid size
  subroutine print_size_sub(this, nzone, isize, error)
    class(ClassCGNSMethod),   intent(in)    :: this
    integer(i4),              intent(out)   :: nzone
    integer(i8), allocatable, intent(out)   :: isize(:,:)
      !< (x, nzone); x: 1 = i, 2 = j, 3 = k, 4 = nbnd, 5 = nconn
    type(ClassError),         intent(inout) :: error
  !...Declare local variables
    integer :: iblock
  continue
  !...Allocate size
    nzone = this%base(1)%nzone
    allocate(isize(5, nzone), stat=error%ialloc)
    if ( error%allocate("Grid Size") ) return
    isize = 0
  !...Get grid size
    do iblock = 1, nzone
      isize(1:3, iblock) = this%base(1)%zone(iblock)%grid%NVertex
      isize(4, iblock)   = this%base(1)%zone(iblock)%nbocos
      isize(5, iblock)   = this%base(1)%zone(iblock)%nconns
    end do
  end subroutine print_size_sub

  !> Print zone boundary condition
  subroutine print_zone_bc_sub(this, izone, ibc, bc_name, bc_type)
    class(ClassCGNSMethod), intent(in)  :: this
    integer,                intent(in)  :: izone
    integer,                intent(in)  :: ibc
    character(len=*),       intent(out) :: bc_name
    character(len=*),       intent(out) :: bc_type
  continue
    bc_name = this%base(1)%zone(izone)%boco(ibc)%name
  end subroutine print_zone_bc_sub

end module interface_cgns
