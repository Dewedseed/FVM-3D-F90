!=============================================================================80
!
!> Spatial discretization variables and methods.
!
!=============================================================================80

module class_space_discret

  use kind_parameter, only : dp, i8, double

  use global_class, only : error, terminal, mpi
  use global_class, only : string

  use global_devlog, only : devlog, LogLevel

  use global_type_defs, only : TypeDefs

  implicit none

!...Define scope
  private

  public  :: ClassSpaceDiscret

!...Declare local variables
  type :: ClassSpaceDiscret
    integer,  public  :: reconstruct = 0   !< reconstruct scheme
    integer,  public  :: flux_split = 0    !< flux split scheme
    real(dp), private :: kappa   = -1.0_dp !< kappa for MUSCL scheme (1/3)
    integer,  private :: limiter = -1      !< limiter for MUSCL scheme
    ! real(dp), private :: limiter_coeff(5)  = 1.0_dp !< limiter coefficient (Attension: not used)
    real(dp), private :: entropy_fix_coeff = 0.05   !< entropy correction coefficient
  contains
    procedure :: input           => input_sub
    procedure :: check_input     => check_input_sub
    procedure :: allocate_memory => allocate_memory_sub
    procedure :: inviscid_flux   => inviscid_flux_sub
    procedure :: viscous_flux    => viscous_flux_sub
    procedure, nopass, private :: reconstruct_simple => reconstruct_simple_sub
    procedure, pass,   private :: reconstruct_MUSCL  => reconstruct_MUSCL_sub
    procedure, nopass, private :: reconstruct_WENO   => reconstruct_WENO_sub
    procedure, pass,   private :: FDS_Roe            => FDS_Roe_sub
    procedure, nopass, private :: FVS_VanLeer        => FVS_VanLeer_sub
    procedure, nopass, private :: FVS_AUSM           => FVS_AUSM_sub
    procedure, nopass, private :: update_residual    => update_residual_sub
    final :: delete_ClassSpaceDiscret  !< Destructor
  end type ClassSpaceDiscret

contains

  !> Constructor of class ClassSpaceDiscret
  function new_ClassSpaceDiscret() result(this)
    type(ClassSpaceDiscret) :: this
  continue
    ! if (associated(this%rhs)) deallocate(this%rhs)
    ! this%rhs => null()
  end function new_ClassSpaceDiscret

  ! > Destructor of class ClassSpaceDiscret
  subroutine delete_ClassSpaceDiscret(this)
    type(ClassSpaceDiscret), intent(inout) :: this
  continue
    ! if (associated(this%rhs)) deallocate(this%rhs)
  end subroutine delete_ClassSpaceDiscret

  !> Input space discretization information
  subroutine input_sub(this, vname, vvalue)
  !...Declare input/output variables
    class(ClassSpaceDiscret), intent(inout) :: this
    character(len=*),         intent(in)    :: vname  !< variable name
    character(len=*),         intent(in)    :: vvalue !< variable value
  continue
    select case(trim(vname))
      case("reconstruct")
        this%reconstruct = TypeDefs%SchemeIndex(trim(vvalue))
      case("kappa")
        this%kappa = string%to_num(vvalue, double, error%iread)
      case("limiter")
        this%limiter = TypeDefs%SchemeIndex(trim(vvalue))
      case("limiter_coeff")
        continue
      case("flux_split")
        this%flux_split = TypeDefs%SchemeIndex(trim(vvalue))
      case("entropy_fix")
        this%entropy_fix_coeff = string%to_num(vvalue, double, error%iread)
      case default
        call devlog%print(where="ClassSpaceDiscret::input_sub", &
                          message="Unknown variable name: "//trim(vname), &
                          level=LogLevel%error)
    end select
  end subroutine input_sub

  !> Check space discretization information
  subroutine check_input_sub(this)
  !...Declare input/output variables
    class(ClassSpaceDiscret), intent(inout) :: this
  continue
    call terminal%print_no_wrap("- Check spatial discretization parameters...")
    if ( this%reconstruct == TypeDefs%Scheme%NULL ) then
      call mpi%stop("Invalid reconstruct scheme.")
    end if
    if ( this%flux_split == TypeDefs%Scheme%NULL ) then
      call mpi%stop("Invalid flux split scheme.")
    end if
    call terminal%print("Done.")
  end subroutine check_input_sub

  !> Allocate memory
  subroutine allocate_memory_sub(this, system, geometry)
    use class_equation, only : ClassPhysicalSystem
    use class_geometry, only : ClassGeometry
  !...Declare input/output variables
    class(ClassSpaceDiscret),  intent(inout) :: this
    type(ClassPhysicalSystem), intent(in)    :: system
    type(ClassGeometry),       intent(in)    :: geometry
  continue
  end subroutine allocate_memory_sub

  !> Compute inviscid flux
  subroutine inviscid_flux_sub(this, geometry, flowfield)
    use class_geometry,   only : ClassGeometry
    use class_flow_field, only : ClassFlowField
  !...Declare input/output variables
    class(ClassSpaceDiscret), intent(inout) :: this
    type(ClassGeometry),      intent(inout) :: geometry
    type(ClassFlowField),     intent(inout) :: flowfield
  !...Declare local variables
    integer :: iblk, dir
  continue
  !...Loop over blocks
    do iblk = 1, geometry%nblocks()
      call devlog%print(message="Compute inviscid flux Block: "// string%from(iblk))
    !...Loop over directions
      ! 1: i-direction, jk-face
      ! 2: j-direction, ik-face
      ! 3: k-direction, ij-face
      do dir = 1, geometry%idimension
      ! do dir = 2, 2
        call devlog%print(message="Direction: "// string%from(dir))
        call flowfield%block(iblk)%reset_pool()
      !...Reconstruct primitive variables on cell face
        if ( this%reconstruct == TypeDefs%Scheme%simple ) then
          call devlog%print(message="Reconstruction (Simple)")
          call this%reconstruct_simple(flowfield%block(iblk), dir)
        else if ( this%reconstruct == TypeDefs%Scheme%MUSCL ) then
          call devlog%print(message="Reconstruction (MUSCL)")
          call this%reconstruct_MUSCL(flowfield%block(iblk), dir)
        else if ( this%reconstruct == TypeDefs%Scheme%WENO ) then
          call devlog%print(message="Reconstruction (WENO)")
          call this%reconstruct_WENO(flowfield%block(iblk), dir, &
                                     flowfield%nconserved_var(), &
                                     flowfield)
        end if
      !...Flux split method
        if ( this%flux_split == TypeDefs%Scheme%Roe ) then
          call devlog%print(message="Flux split (Roe)")
          call this%FDS_Roe(flowfield%block(iblk), dir, &
                            flowfield%nconserved_var(), &
                            flowfield%gamma)
        else if ( this%flux_split == TypeDefs%Scheme%VanLeer ) then
          call devlog%print(message="Flux split (VanLeer)")
          call this%FVS_VanLeer(flowfield%block(iblk), dir, &
                                flowfield%nconserved_var(), &
                                flowfield%gamma)
        else if ( this%flux_split == TypeDefs%Scheme%AUSM ) then
          call devlog%print(message="Flux split (AUSM)")
          call this%FVS_AUSM(flowfield%block(iblk), dir, &
                             flowfield%nconserved_var(), &
                             flowfield%gamma)
        end if
      !...Add invisid flux to residual
        call devlog%print(message="Update RHS")
        call this%update_residual(flowfield%block(iblk), dir)
      end do
    end do
  end subroutine inviscid_flux_sub

  !> Reconstruct primitive variables on face
  subroutine reconstruct_simple_sub(blk, dir)
    use defs_block_data,  only : ClassBlockData
    use defs_fluid_props, only : Primitive_Variable
    use numerical_scheme, only : SpaceScheme
  !...Declare input/output variables
    type(ClassBlockData), intent(inout) :: blk
    integer,              intent(in)    :: dir
  !...Declare local variable
    type(Primitive_Variable), pointer :: pg(:,:,:)       !< primitive variable
    type(Primitive_Variable), pointer :: p_minus(:,:,:) !< primitive variable (minus)
    type(Primitive_Variable), pointer :: p_plus(:,:,:)  !< primitive variable (plus)
    integer(i8) :: i, j, k
    integer(i8) :: ig, jg, kg !< ghost index
    integer(i8) :: nface
    integer(i8) :: dghost(3)
    integer(i8) :: imax, jmax, kmax
  continue
  !...Get reconstructed primitive variable
    ! real-mesh:               * --- * --- * --- *
    ! real-index:              1     2     3     4
    ! face-index:           1     2     3     4     5
    ! ghost-mesh:  + ... + ... * --- * --- * --- * ... + ... +
    ! p-index:     1     2     3     4     5     6     7     8
    !                      2.5   3.5   4.5   5.5   6.5
    pg      => blk%get_qp_ghost()
    p_minus => blk%get_qp_minus()
    p_plus  => blk%get_qp_plus()
    dghost(1) = blk%real_mesh%map_ijk(1)
    dghost(2) = blk%real_mesh%map_ijk(2)
    dghost(3) = blk%real_mesh%map_ijk(3)
  !...Get range
    imax = blk%real_mesh%nvert(1)
    jmax = blk%real_mesh%nvert(2)
    kmax = blk%real_mesh%nvert(3)
  !...Loop over faces
    nface = blk%real_mesh%nface(dir)
    select case (dir)
      case (1) !< i-direction
        do k = 1, kmax
          do j = 1, jmax
            jg = j + dghost(2)
            kg = k + dghost(3)
            call SpaceScheme%reconstruct_simple(nface, dghost(dir), &
                pg(:,jg,kg), p_minus(1:nface,j,k), wind = -1)
            call SpaceScheme%reconstruct_simple(nface, dghost(dir), &
                pg(:,jg,kg), p_plus(1:nface,j,k), wind = 1)
          end do
        end do
      case (2) !< j-direction
        do k = 1, kmax
          do i = 1, imax
            ig = i + dghost(1)
            kg = k + dghost(3)
            call SpaceScheme%reconstruct_simple(nface, dghost(dir), &
                pg(ig,:,kg), p_minus(i,1:nface,k), wind = -1)
            call SpaceScheme%reconstruct_simple(nface, dghost(dir), &
                pg(ig,:,kg), p_plus(i,1:nface,k), wind = 1)
          end do
        end do

      case (3) !< k-direction
        do j = 1, jmax
          do i = 1, imax
            ig = i + dghost(1)
            jg = j + dghost(2)
            call SpaceScheme%reconstruct_simple(nface, dghost(dir), &
                    pg(ig,jg,:), p_minus(i,j,1:nface), wind = -1)
            call SpaceScheme%reconstruct_simple(nface, dghost(dir), &
                    pg(ig,jg,:), p_plus(i,j,1:nface), wind = 1)
          end do
        end do

      case default
        call devlog%print(where="class_space_discret::reconstruct_simple_sub", &
                          message="Error direction ", &
                          level=LogLevel%error)
    end select
  end subroutine reconstruct_simple_sub

  !> Reconstruct primitive variables on face using MUSCL scheme
  subroutine reconstruct_MUSCL_sub(this, blk, dir)
    use defs_block_data,  only : ClassBlockData
    use defs_fluid_props, only : Primitive_Variable
    use numerical_scheme, only : SpaceScheme
  !...Declare input/output variables
    class(ClassSpaceDiscret), intent(inout) :: this
    type(ClassBlockData),     intent(inout) :: blk
    integer,                  intent(in)    :: dir
  !...Declare local variable
    type(Primitive_Variable), pointer :: pg(:,:,:)       !< primitive variable
    type(Primitive_Variable), pointer :: p_minus(:,:,:) !< primitive variable (minus)
    type(Primitive_Variable), pointer :: p_plus(:,:,:)  !< primitive variable (plus)
    integer(i8) :: i, j, k
    integer(i8) :: ig, jg, kg
    integer(i8) :: nface
    integer(i8) :: dghost(3)
    integer(i8) :: imax, jmax, kmax
  continue
  !...Get reconstructed primitive variable
    ! real-mesh:               * --- * --- * --- *
    ! real-index:              1     2     3     4
    ! face-index:           1     2     3     4     5
    ! ghost-mesh:  + ... + ... * --- * --- * --- * ... + ... +
    ! p-index:     1     2     3     4     5     6     7     8
    !                      2.5   3.5   4.5   5.5   6.5
    pg      => blk%get_qp_ghost()
    p_minus => blk%get_qp_minus()
    p_plus  => blk%get_qp_plus()
    dghost(1) = blk%real_mesh%map_ijk(1)
    dghost(2) = blk%real_mesh%map_ijk(2)
    dghost(3) = blk%real_mesh%map_ijk(3)
  !...Get range
    imax = blk%real_mesh%nvert(1)
    jmax = blk%real_mesh%nvert(2)
    kmax = blk%real_mesh%nvert(3)
  !...Loop over faces
    nface = blk%real_mesh%nface(dir)
    select case (dir)
      case (1) !< direction-i => nface (i+1/2, i-1/2), face-jk (j, k)
        do k = 1, kmax
          do j = 1, jmax
            jg = j + dghost(2)
            kg = k + dghost(3)
            call SpaceScheme%reconstruct_MUSCL(nface, dghost(dir), this%kappa, &
                  this%limiter, pg(:,jg,kg), p_minus(1:nface,j,k), wind = -1)
            call SpaceScheme%reconstruct_MUSCL(nface, dghost(dir), this%kappa, &
                  this%limiter, pg(:,jg,kg), p_plus(1:nface,j,k),  wind =  1)
          end do
        end do
      case (2) !< direction-j => nface (j+1/2, j-1/2), face-ik (i, k)
        do k = 1, kmax
          do i = 1, imax
            ig = i + dghost(1)
            kg = k + dghost(3)
            call SpaceScheme%reconstruct_MUSCL(nface, dghost(dir), this%kappa, &
                  this%limiter, pg(ig,:,kg), p_minus(i,1:nface,k), wind = -1)
            call SpaceScheme%reconstruct_MUSCL(nface, dghost(dir), this%kappa, &
                  this%limiter, pg(ig,:,kg), p_plus(i,1:nface,k),  wind =  1)
          end do
        end do
      case (3) !< direction-k => nface (k+1/2, k-1/2), face-ij (i, j)
        do j = 1, jmax
          do i = 1, imax
            ig = i + dghost(1)
            jg = j + dghost(2)
            call SpaceScheme%reconstruct_MUSCL(nface, dghost(dir), this%kappa, &
                  this%limiter, pg(ig,jg,:), p_minus(i,j,1:nface), wind = -1)
            call SpaceScheme%reconstruct_MUSCL(nface, dghost(dir), this%kappa, &
                  this%limiter, pg(ig,jg,:), p_plus(i,j,1:nface),  wind =  1)
          end do
        end do
      case default
        call devlog%print(where="class_space_discret::reconstruct_MUSCL_sub", &
                          message="Error direction ", &
                          level=LogLevel%error)
    end select
  end subroutine reconstruct_MUSCL_sub

  !> Reconstruct primitive variables using WENO scheme
  subroutine reconstruct_WENO_sub(blk, dir, neq, flow)
    use class_flow_field, only : ClassFlowField
    use defs_block_data,  only : ClassBlockData
    use defs_fluid_props, only : Primitive_Variable
    use numerical_scheme, only : SpaceScheme
  !...Declare input/output variables
    type(ClassBlockData),     intent(inout) :: blk
    integer,                  intent(in)    :: dir
    integer,                  intent(in)    :: neq
    type(ClassFlowField),     intent(inout) :: flow
  !...Declare local variable
    type(Primitive_Variable), pointer :: p_plus(:,:,:)  !< primitive variable (plus)
    type(Primitive_Variable), pointer :: p_minus(:,:,:) !< primitive variable (minus)
    real(dp), pointer :: qg(:,:,:,:) !< convective variable (ghost)
    real(dp), pointer :: qr(:,:,:,:) !< reconstructed convective variable
    integer(i8) :: i, j, k
    integer(i8) :: ig, jg, kg !< ghost index
    integer(i8) :: nface
    integer(i8) :: dghost(3)
    integer(i8) :: imax, jmax, kmax
  continue
  !...Get reconstructed primitive variable
    ! real-mesh:               * --- * --- * --- *
    ! real-index:              1     2     3     4
    ! face-index:           1     2     3     4     5
    ! ghost-mesh:  + ... + ... * --- * --- * --- * ... + ... +
    ! p-index:     1     2     3     4     5     6     7     8
    !                      2.5   3.5   4.5   5.5   6.5
    qg      => blk%get_q_ghost()
    qr      => blk%get_q_recon()
    p_minus => blk%get_qp_minus()
    p_plus  => blk%get_qp_plus()
    dghost(1) = blk%real_mesh%map_ijk(1)
    dghost(2) = blk%real_mesh%map_ijk(2)
    dghost(3) = blk%real_mesh%map_ijk(3)
  !...Get range
    imax = blk%real_mesh%nvert(1)
    jmax = blk%real_mesh%nvert(2)
    kmax = blk%real_mesh%nvert(3)
  !...Loop over faces
    nface = blk%real_mesh%nface(dir)
    select case (dir)
      case (1) !< direction-i => nface (i+1/2, i-1/2), face-jk (j, k)
        do k = 1, kmax
          do j = 1, jmax
            jg = j + dghost(2)
            kg = k + dghost(3)
            call SpaceScheme%reconstruct_WENO(nface, neq, dghost(dir), &
                      qg(:,jg,kg,:), qr(1:nface,j,k,:), wind = -1)
            call flow%convert(qr(1:nface,j,k,:), p_minus(1:nface,j,k))
            call SpaceScheme%reconstruct_WENO(nface, neq, dghost(dir), &
                      qg(:,jg,kg,:), qr(1:nface,j,k,:), wind =  1)
            call flow%convert(qr(1:nface,j,k,:), p_plus(1:nface,j,k))
          end do
        end do
      case (2) !< direction-j => nface (j+1/2, j-1/2), face-ik (i, k)
        do k = 1, kmax
          do i = 1, imax
            ig = i + dghost(1)
            kg = k + dghost(3)
            call SpaceScheme%reconstruct_WENO(nface, neq, dghost(dir), &
                      qg(ig,:,kg,:), qr(i,1:nface,k,:), wind = -1)
            call flow%convert(qr(i,1:nface,k,:), p_minus(i,1:nface,k))
            call SpaceScheme%reconstruct_WENO(nface, neq, dghost(dir), &
                      qg(ig,:,kg,:), qr(i,1:nface,k,:), wind =  1)
            call flow%convert(qr(i,1:nface,k,:), p_plus(i,1:nface,k))
          end do
        end do
      case (3) !< direction-k => nface (k+1/2, k-1/2), face-ij (i, j)
        do j = 1, jmax
          do i = 1, imax
            ig = i + dghost(1)
            jg = j + dghost(2)
            call SpaceScheme%reconstruct_WENO(nface, neq, dghost(dir), &
                      qg(ig,jg,:,:), qr(i,j,1:nface,:), wind = -1)
            call flow%convert(qr(i,j,1:nface,:), p_minus(i,j,1:nface))
            call SpaceScheme%reconstruct_WENO(nface, neq, dghost(dir), &
                      qg(ig,jg,:,:), qr(i,j,1:nface,:), wind =  1)
            call flow%convert(qr(i,j,1:nface,:), p_plus(i,j,1:nface))
          end do
        end do
      case default
        call devlog%print(where="class_space_discret::reconstruct_WENO_sub", &
                          message="Error direction ", &
                          level=LogLevel%error)
    end select
  end subroutine reconstruct_WENO_sub

  !> Compute inviscid flux using Roe scheme
  subroutine FDS_Roe_sub(this, blk, dir, neq, gamma)
    use defs_block_data,  only : ClassBlockData
    use defs_block_geom,  only : ClassGeomBlock, NCOORD
    use defs_fluid_props, only : Primitive_Variable
    use numerical_scheme, only : SpaceScheme
  !...Declare input/output variables
    class(ClassSpaceDiscret), intent(inout) :: this
    type(ClassBlockData),     intent(inout) :: blk
    integer,                  intent(in)    :: dir
    integer,                  intent(in)    :: neq !< number of equations
    real(dp),                 intent(in)    :: gamma !< ratio of specific heat
  !...Declare local variables
    real(dp), allocatable :: norm(:,:)
    type(ClassGeomBlock),     pointer :: mesh
    type(Primitive_Variable), pointer :: qp_minus(:,:,:)
    type(Primitive_Variable), pointer :: qp_plus(:,:,:)
    real(dp),                 pointer :: flux(:,:,:,:)
    integer(i8) :: i, j, k
    integer(i8) :: nface
    integer(i8) :: imax, jmax, kmax
  continue
  !...Compute face normal vector
    !
    ! Attension: face (half) is not geometry face
    !
    !     * --- * 3
    !     |     |
    !   1 * -:- * 2
    !       half face(:) normal = node 1 -> node 2
    !       face/surface(---) normal = normal of face (1, 2)
    !
    allocate(norm(blk%real_mesh%nface(dir), NCOORD), stat=error%ialloc)
    if (error%allocate("Normal of half face")) call mpi%stop(error%what())
    norm        = 0.0_dp
    norm(:,dir) = 1.0_dp
  !...Get block mesh
    mesh     => blk%get_geom()
    qp_minus => blk%get_qp_minus()
    qp_plus  => blk%get_qp_plus()
    flux     => blk%get_flux()
  !...Roe scheme
    imax = blk%real_mesh%nvert(1)
    jmax = blk%real_mesh%nvert(2)
    kmax = blk%real_mesh%nvert(3)
    nface = blk%real_mesh%nface(dir)
    select case (dir)
      case (1) !< i-direction face
        do k = 1, kmax
          do j = 1, jmax
            call SpaceScheme%FDS_Roe(nface, neq, gamma, norm, &
                                     qp_minus(1:nface, j, k), &
                                     qp_plus (1:nface, j, k), &
                                     flux    (1:nface, j, k, 1:neq), &
                                     this%entropy_fix_coeff)
          end do
        end do
      case (2) !< j-direction face
        do k = 1, kmax
          do i = 1, imax
            call SpaceScheme%FDS_Roe(nface, neq, gamma, norm, &
                                     qp_minus(i, 1:nface, k), &
                                     qp_plus (i, 1:nface, k), &
                                     flux    (i, 1:nface, k, 1:neq), &
                                     this%entropy_fix_coeff)
          end do
        end do
      case (3) !< k-direction face
        do j = 1, jmax
          do i = 1, imax
            call SpaceScheme%FDS_Roe(nface, neq, gamma, norm, &
                                     qp_minus(i, j, 1:nface), &
                                     qp_plus (i, j, 1:nface), &
                                     flux    (i, j, 1:nface, 1:neq), &
                                     this%entropy_fix_coeff)
          end do
        end do
      case default
        call devlog%print(where="class_space_discret::FDS_Roe_sub", &
                          message="Invalid direction", &
                          level=LogLevel%error)
    end select
  end subroutine FDS_Roe_sub

  !> Compute inviscid flux using Van Leer scheme
  subroutine FVS_VanLeer_sub(blk, dir, neq, gamma)
    use defs_block_data, only : ClassBlockData
    use defs_block_geom,  only : ClassGeomBlock, NCOORD
    use defs_fluid_props, only : Primitive_Variable
    use numerical_scheme, only : SpaceScheme
  !...Declare input/output variables
    type(ClassBlockData), intent(inout) :: blk
    integer,              intent(in)    :: dir
    integer,              intent(in)    :: neq
    real(dp),             intent(in)    :: gamma
  !...Declare local variables
    real(dp), allocatable :: norm(:,:)
    type(ClassGeomBlock),     pointer :: mesh
    type(Primitive_Variable), pointer :: qp_minus(:,:,:)
    type(Primitive_Variable), pointer :: qp_plus(:,:,:)
    real(dp),                 pointer :: flux(:,:,:,:)
    integer(i8) :: i, j, k
    integer(i8) :: nface
    integer(i8) :: imax, jmax, kmax
  continue
  !...Compute face normal vector
    !
    ! Attension: face (half) is not geometry face
    !
    !     * --- * 3
    !     |     |
    !   1 * -:- * 2
    !       half face(:) normal = node 1 -> node 2
    !       face/surface(---) normal = normal of face (1, 2)
    !
    allocate(norm(blk%real_mesh%nface(dir), NCOORD), stat=error%ialloc)
    if (error%allocate("Normal of half face")) call mpi%stop(error%what())
    norm        = 0.0_dp
    norm(:,dir) = 1.0_dp
  !...Get block mesh
    mesh     => blk%get_geom()
    qp_minus => blk%get_qp_minus()
    qp_plus  => blk%get_qp_plus()
    flux     => blk%get_flux()
  !...Vanleer scheme
    imax = blk%real_mesh%nvert(1)
    jmax = blk%real_mesh%nvert(2)
    kmax = blk%real_mesh%nvert(3)
    nface = blk%real_mesh%nface(dir)
    select case (dir)
      case (1) !< i-direction
        do k = 1, kmax
          do j = 1, jmax
            call SpaceScheme%FVS_VanLeer(nface, neq, gamma, norm, &
                                         qp_minus(1:nface, j, k), &
                                         qp_plus (1:nface, j, k), &
                                         flux    (1:nface, j, k, 1:neq))
          end do
        end do
      case (2) !< j-direction
        do k = 1, kmax
          do i = 1, imax
            call SpaceScheme%FVS_VanLeer(nface, neq, gamma, norm, &
                                         qp_minus(i, 1:nface, k), &
                                         qp_plus (i, 1:nface, k), &
                                         flux    (i, 1:nface, k, 1:neq))
          end do
        end do
      case (3) !< k-direction
        do j = 1, jmax
          do i = 1, imax
            call SpaceScheme%FVS_VanLeer(nface, neq, gamma, norm, &
                                         qp_minus(i, j, 1:nface), &
                                         qp_plus (i, j, 1:nface), &
                                         flux    (i, j, 1:nface, 1:neq))
          end do
        end do
      case default
        call devlog%print(where="class_space_discret::FVS_VanLeer_sub", &
                          message="Invalid direction", &
                          level=LogLevel%error)
    end select
  end subroutine FVS_VanLeer_sub

  !> Compute inviscid flux using Van Leer scheme
  subroutine FVS_AUSM_sub(blk, dir, neq, gamma)
    use defs_block_data, only : ClassBlockData
    use defs_block_geom,  only : ClassGeomBlock, NCOORD
    use defs_fluid_props, only : Primitive_Variable
    use numerical_scheme, only : SpaceScheme
  !...Declare input/output variables
    type(ClassBlockData), intent(inout) :: blk
    integer,              intent(in)    :: dir
    integer,              intent(in)    :: neq
    real(dp),             intent(in)    :: gamma
  !...Declare local variables
    real(dp), allocatable :: norm(:,:)
    type(ClassGeomBlock),     pointer :: mesh
    type(Primitive_Variable), pointer :: qp_minus(:,:,:)
    type(Primitive_Variable), pointer :: qp_plus(:,:,:)
    real(dp),                 pointer :: flux(:,:,:,:)
    integer(i8) :: i, j, k
    integer(i8) :: nface
    integer(i8) :: imax, jmax, kmax
  continue
  !...Compute face normal vector
    !
    ! Attension: face (half) is not geometry face
    !
    !     * --- * 3
    !     |     |
    !   1 * -:- * 2
    !       half face(:) normal = node 1 -> node 2
    !       face/surface(---) normal = normal of face (1, 2)
    !
    allocate(norm(blk%real_mesh%nface(dir), NCOORD), stat=error%ialloc)
    if (error%allocate("Normal of half face")) call mpi%stop(error%what())
    norm        = 0.0_dp
    norm(:,dir) = 1.0_dp
  !...Get block mesh
    mesh     => blk%get_geom()
    qp_minus => blk%get_qp_minus()
    qp_plus  => blk%get_qp_plus()
    flux     => blk%get_flux()
  !...Vanleer scheme
    imax = blk%real_mesh%nvert(1)
    jmax = blk%real_mesh%nvert(2)
    kmax = blk%real_mesh%nvert(3)
    nface = blk%real_mesh%nface(dir)
    select case (dir)
      case (1) !< i-direction
        do k = 1, kmax
          do j = 1, jmax
            call SpaceScheme%FVS_AUSM(nface, neq, gamma, norm, &
                                      qp_minus(1:nface, j, k), &
                                      qp_plus (1:nface, j, k), &
                                      flux    (1:nface, j, k, 1:neq))
          end do
        end do
      case (2) !< j-direction
        do k = 1, kmax
          do i = 1, imax
            call SpaceScheme%FVS_AUSM(nface, neq, gamma, norm, &
                                      qp_minus(i, 1:nface, k), &
                                      qp_plus (i, 1:nface, k), &
                                      flux    (i, 1:nface, k, 1:neq))
          end do
        end do
      case (3) !< k-direction
        do j = 1, jmax
          do i = 1, imax
            call SpaceScheme%FVS_AUSM(nface, neq, gamma, norm, &
                                      qp_minus(i, j, 1:nface), &
                                      qp_plus (i, j, 1:nface), &
                                      flux    (i, j, 1:nface, 1:neq))
          end do
        end do
      case default
        call devlog%print(where="class_space_discret::FVS_AUSM_sub", &
                          message="Invalid direction", &
                          level=LogLevel%error)
    end select
  end subroutine FVS_AUSM_sub

  !> Compute viscous flux
  subroutine viscous_flux_sub(this)
  !...Declare input/output variables
    class(ClassSpaceDiscret),  intent(inout) :: this
  continue
  end subroutine viscous_flux_sub

  !> Update residual
  subroutine update_residual_sub(blk, dir)
    use defs_block_data, only : ClassBlockData
    use defs_block_geom, only : ClassGeomBlock
  !...Declare input/output variables
    class(ClassBlockData),  intent(inout) :: blk
    integer,                intent(in)    :: dir
  !...Declare local variable
    type(ClassGeomBlock), pointer :: mesh
    real(dp), pointer :: flux(:,:,:,:)
    real(dp), pointer :: rhs(:,:,:,:)
    integer(i8) :: i, j, k
    integer(i8) :: imax, jmax, kmax
  continue
  !...Get flux
    flux => blk%get_flux()
    rhs  => blk%get_rhs()
    mesh => blk%get_geom()
  !...Add to residual (inviscid flux)
    imax = blk%real_mesh%nvert(1)
    jmax = blk%real_mesh%nvert(2)
    kmax = blk%real_mesh%nvert(3)
    select case (dir)
      case (1) !< i-direction
        do k = 1, kmax
          do j = 1, jmax
            do i = 1, imax
              rhs(i,j,k,:) = rhs(i,j,k,:) - (flux(i + 1,j,k,:) - flux(i,j,k,:)) / &
                                            mesh%dr(i,j,k,dir)
            end do
          end do
        end do
      case (2) !< j-direction
        do k = 1, kmax
          do i = 1, imax
            do j = 1, jmax
              rhs(i,j,k,:) = rhs(i,j,k,:) - (flux(i,j + 1,k,:) - flux(i,j,k,:)) / &
                                            mesh%dr(i,j,k,dir)
            end do
          end do
        end do
      case (3) !< k-direction
        do j = 1, jmax
          do i = 1, imax
            do k = 1, kmax
              rhs(i,j,k,:) = rhs(i,j,k,:) - (flux(i,j,k + 1,:) - flux(i,j,k,:)) / &
                                            mesh%dr(i,j,k,dir)
            end do
          end do
        end do
      case default
        call devlog%print(where="class_space_discret::update_residual_sub", &
                          message="Invalid direction", &
                          level=LogLevel%error)
    end select
  end subroutine update_residual_sub

end module class_space_discret
