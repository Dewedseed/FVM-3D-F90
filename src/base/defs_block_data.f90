!=============================================================================80
!
!> Define block information.
!
!=============================================================================80

module defs_block_data

  use kind_parameter, only : i2, i4, i8, dp, len_short

  use defs_fluid_props, only : Primitive_Variable, Dependent_Variable
  use defs_block_geom,  only : ClassGeomBlock
  use defs_ghost_cell,  only : ClassGhostMesh, ClassRealMesh

  implicit none

!...Define scope
  private

  public :: ClassBlockData

!...Declare local variables
  !> block data container
  !     defined on ghost mesh: q, p  (for reconstruction and boundary conditions)
  !     defined on real mesh:  other
  type :: ClassBlockData
  !...Public variables
    private
    integer(i4)                       :: index = 0
    type(ClassGhostMesh),     public  :: ghost_mesh
    type(ClassRealMesh),      public  :: real_mesh
    type(ClassGeomBlock),     pointer :: mesh => null()
                                      !< map to geometry
    real(dp),                 pointer :: q(:,:,:,:)   => null()
                                      !< conserved variables (ghost mesh)
    real(dp),                 pointer :: q0(:,:,:,:)  => null()
                                      !< temporary conserved variables
    real(dp),                 pointer :: rhs(:,:,:,:) => null()
                                      !< right hand side
                                      !! residual
    type(Primitive_Variable), pointer :: p(:,:,:)     => null()
                                      !< primitive variables (ghost mesh)
    type(Dependent_Variable), pointer :: qd(:,:,:)    => null()
                                      !< dependent variables
    real(dp),                 pointer :: flux(:,:,:,:) => null()
                                      !< face fluxes
    type(Primitive_Variable), pointer :: p_plus(:,:,:) => null()
                                      !< primitive variables on positive side of face
    type(Primitive_Variable), pointer :: p_minus(:,:,:) => null()
                                      !< primitive variables on negative side of face
    real(dp),                 pointer :: qr(:,:,:,:) => null()
                                      !< reconstructed conserved variables on face
  contains
    procedure :: link_geom         => link_geom_sub
    procedure :: allocate_memory   => allocate_memory_sub
    procedure :: init_value        => init_value_sub
    procedure :: reset_pool        => reset_pool_sub
    procedure :: update_ghost_cell => update_ghost_cell_sub

    procedure :: get_qp       => get_primitive_fun
    procedure :: get          => get_variables_fun
    procedure :: get_q        => get_conserved_fun
    procedure :: get_q_ghost  => get_conserved_ghost_fun
    procedure :: get_q_recon  => get_conserved_reconstructed_fun
    procedure :: get_q0       => get_conserved_0_fun
    procedure :: get_qd       => get_dependent_fun
    procedure :: get_rhs      => get_rhs_fun
    procedure :: get_flux     => get_face_flux_fun
    procedure :: get_qp_plus  => get_face_qp_plus_fun
    procedure :: get_qp_minus => get_face_qp_minus_fun
    procedure :: get_geom     => get_block_geom_fun

    procedure, private :: get_pg_blk  => get_primitive_ghost_blk_fun
    generic :: get_qp_ghost => get_pg_blk

    procedure, nopass, private :: reset_p => reset_primitive_variables_sub
    generic :: reset => reset_p

    final :: delete_ClassBlockData
  end type ClassBlockData

contains

  !> Destructor of class ClassBlockData
  subroutine delete_ClassBlockData(this)
    type(ClassBlockData), intent(inout) :: this
  continue
    if (associated(this%mesh))    deallocate(this%mesh)
    if (associated(this%q))       deallocate(this%q)
    if (associated(this%q0))      deallocate(this%q0)
    if (associated(this%rhs))     deallocate(this%rhs)
    if (associated(this%p))       deallocate(this%p)
    if (associated(this%qd))      deallocate(this%qd)
    if (associated(this%flux))    deallocate(this%flux)
    if (associated(this%p_plus))  deallocate(this%p_plus)
    if (associated(this%p_minus)) deallocate(this%p_minus)
    if (associated(this%qr))      deallocate(this%qr)
  end subroutine delete_ClassBlockData

  !> Set geometry
  subroutine link_geom_sub(this, mesh)
    class(ClassBlockData), intent(inout) :: this
    type(ClassGeomBlock),  pointer       :: mesh
  continue
    this%mesh => mesh
    call this%ghost_mesh%init(mesh%nvert())
    call this%real_mesh%init(this%ghost_mesh)
  end subroutine link_geom_sub

  !> Allocate memory for block data
  subroutine allocate_memory_sub(this, neq, error)
    use global_error, only : ClassError
  !...Declear input/output variables
    class(ClassBlockData), intent(inout) :: this
    integer,               intent(in)    :: neq
                           !< number of equations
                           !< number of conserved variables
    type(ClassError),      intent(inout) :: error
  !...Declear local variables
    integer(i8) :: ni, nj, nk
  continue
  !...Allocate memory for conserved variables of real mesh
    ni = this%real_mesh%nvert(1)
    nj = this%real_mesh%nvert(2)
    nk = this%real_mesh%nvert(3)

    allocate(this%q0(ni, nj, nk, neq), stat=error%ialloc)
    if (error%allocate("Conservative Variables")) return

    allocate(this%rhs(ni, nj, nk, neq), stat=error%ialloc)
    if (error%allocate("RHS")) return

    allocate(this%qd(ni, nj, nk), stat=error%ialloc)
    if (error%allocate("Dependent variables")) return

  !...Allocate memory for variables of ghost mesh
    ni = this%ghost_mesh%nvert(1)
    nj = this%ghost_mesh%nvert(2)
    nk = this%ghost_mesh%nvert(3)

    allocate(this%q(ni, nj, nk, neq), stat=error%ialloc)
    if (error%allocate("Conservative Variables")) return

    allocate(this%p(ni, nj, nk), stat=error%ialloc)
    if (error%allocate("Primitive variables")) return

  !...Allocate memory for mesh face (half node)
    ni = this%real_mesh%nface(1)
    nj = this%real_mesh%nface(2)
    nk = this%real_mesh%nface(3)

    allocate(this%flux(ni, nj, nk, neq), stat=error%ialloc)
    if (error%allocate("Face flux")) return

    allocate(this%p_plus(ni, nj, nk), stat=error%ialloc)
    if (error%allocate("Face primitive variables (plus)")) return

    allocate(this%p_minus(ni, nj, nk), stat=error%ialloc)
    if (error%allocate("Face primitive variables (minus)")) return

    allocate(this%qr(ni, nj, nk, neq), stat=error%ialloc)
    if (error%allocate("Face conserved variables")) return
  end subroutine allocate_memory_sub

  !> Initialize block values (Primitive variables)
  subroutine init_value_sub(this, p_set, region)
  !...Declare input/output variables
    class(ClassBlockData), intent(inout) :: this
    type(Primitive_Variable), intent(in) :: p_set
                           !< primitive variables to be set
    real(dp),                 intent(in) :: region(2,3)
                           !< region to be set
                           !< i: [region(1,1), region(2,1)]
                           !< j: [region(1,2), region(2,2)]
                           !< k: [region(1,3), region(2,3)]
  !...Declare local variable
    real(dp), pointer :: x(:,:,:)
    real(dp), pointer :: y(:,:,:)
    real(dp), pointer :: z(:,:,:)
    integer(i8) :: i, j, k, di, dj, dk
  continue
  !...Get mesh coordinates
    x => this%mesh%x()
    y => this%mesh%y()
    z => this%mesh%z()
    di = this%real_mesh%map_ijk(1)
    dj = this%real_mesh%map_ijk(2)
    dk = this%real_mesh%map_ijk(3)
  !...Set primitive variables
    do k = 1, this%mesh%nvert(3)
      do j = 1, this%mesh%nvert(2)
        do i = 1, this%mesh%nvert(1)
          if (x(i,j,k) >= region(1,1) .and. x(i,j,k) <= region(2,1) .and. &
              y(i,j,k) >= region(1,2) .and. y(i,j,k) <= region(2,2) .and. &
              z(i,j,k) >= region(1,3) .and. z(i,j,k) <= region(2,3)) then
            this%p(i+di,j+dj,k+dk) = p_set
          end if
        end do
      end do
    end do
  end subroutine init_value_sub

  !> reset block data pool
  subroutine reset_pool_sub(this)
    class(ClassBlockData), intent(inout) :: this
  continue
    this%qr   = 0.0_dp
    this%flux = 0.0_dp
    call this%reset(this%p_minus(:,:,:))
    call this%reset(this%p_plus(:,:,:))
  end subroutine reset_pool_sub

  !> Reset primitive variables
  subroutine reset_primitive_variables_sub(p)
    type(Primitive_Variable), intent(inout) :: p(:,:,:)
  continue
    p%rho   = 0.0_dp
    p%u     = 0.0_dp
    p%v     = 0.0_dp
    p%w     = 0.0_dp
    p%press = 0.0_dp
  end subroutine reset_primitive_variables_sub

  !> Update ghost cell values (extrapolation)
  !
  subroutine update_ghost_cell_sub(this)
  !...Declare input/output variables
    class(ClassBlockData), intent(inout) :: this
  !...Declare local variable
    integer(i8) :: layer(3,3)
    integer(i8) :: i, j, k, di, dj, dk
    integer :: iregion, ilayer
  continue
    do iregion = 1, this%ghost_mesh%nregion()
      do ilayer = 1, this%ghost_mesh%nlayer(iregion)
        layer = this%ghost_mesh%layer(iregion, ilayer)
        di = layer(1,3) * ilayer
        dj = layer(2,3) * ilayer
        dk = layer(3,3) * ilayer
        do k = layer(3,1), layer(3,2)
          do j = layer(2,1), layer(2,2)
            do i = layer(1,1), layer(1,2)
              !
              !        <- -1           di = -1
              !    + ... + ... * --- * i
              !    2     1     0
              ! (i,j,k)      (i-di,j-dj,k-dk)
              !
              this%p(i,j,k)   = this%p(i-di,j-dj,k-dk)
              this%q(i,j,k,:) = this%q(i-di,j-dj,k-dk,:)
            end do
          end do
        end do
      end do
    end do
  end subroutine update_ghost_cell_sub

!=============================================================================80
!
! Expose internal variables
!
!=============================================================================80

  !> Get variables
  function get_variables_fun(this, var_type) result(var)
    class(ClassBlockData), intent(inout) :: this
    integer(i2),           intent(in)    :: var_type !< variable type
    real(dp),              pointer       :: var(:,:,:)
  continue
    var => this%q(:,:,:,var_type)
  end function get_variables_fun

  !> Get primitive variables (real mesh)
  function get_primitive_fun(this) result(p_ptr)
    class(ClassBlockData), intent(inout) :: this
    type(Primitive_Variable), pointer :: p_ptr(:,:,:)
  !...Declare local variable
    integer(i8) :: ibeg, iend, jbeg, jend, kbeg, kend
  continue
    ibeg = this%real_mesh%range(1, 1)
    iend = this%real_mesh%range(1, 2)
    jbeg = this%real_mesh%range(2, 1)
    jend = this%real_mesh%range(2, 2)
    kbeg = this%real_mesh%range(3, 1)
    kend = this%real_mesh%range(3, 2)
  !...Real range of primitive variables
    p_ptr => this%p(ibeg:iend, jbeg:jend, kbeg:kend)
  end function get_primitive_fun

  !> Get primitive variables (ghost mesh)
  function get_primitive_ghost_blk_fun(this) result(p_ptr)
    class(ClassBlockData), intent(inout) :: this
    type(Primitive_Variable), pointer :: p_ptr(:,:,:)
  continue
    p_ptr => this%p(:,:,:)
  end function get_primitive_ghost_blk_fun

  !> Get conserved variables (real mesh)
  function get_conserved_fun(this) result(q_ptr)
    class(ClassBlockData), intent(inout) :: this
    real(dp), pointer :: q_ptr(:,:,:,:)
  !...Declare local variable
    integer(i8) :: ibeg, iend, jbeg, jend, kbeg, kend
  continue
    ibeg = this%real_mesh%range(1, 1)
    iend = this%real_mesh%range(1, 2)
    jbeg = this%real_mesh%range(2, 1)
    jend = this%real_mesh%range(2, 2)
    kbeg = this%real_mesh%range(3, 1)
    kend = this%real_mesh%range(3, 2)
    q_ptr => this%q(ibeg:iend, jbeg:jend, kbeg:kend, :)
  end function get_conserved_fun

  !> Get conserved variables (ghost mesh)
  function get_conserved_ghost_fun(this) result(q_ptr)
    class(ClassBlockData), intent(inout) :: this
    real(dp), pointer :: q_ptr(:,:,:,:)
  continue
    q_ptr => this%q(:,:,:,:)
  end function get_conserved_ghost_fun

  !> Get reconstructed variables
  function get_conserved_reconstructed_fun(this) result(qr_ptr)
    class(ClassBlockData), intent(inout) :: this
    real(dp), pointer :: qr_ptr(:,:,:,:)
  continue
    qr_ptr => this%qr(:,:,:,:)
  end function get_conserved_reconstructed_fun

  !> Get conserved variables at initial time
  function get_conserved_0_fun(this) result(q0_ptr)
    class(ClassBlockData), intent(inout) :: this
    real(dp), pointer :: q0_ptr(:,:,:,:)
  continue
    q0_ptr => this%q0(:,:,:,:)
  end function get_conserved_0_fun

  !> Get dependent variables
  function get_dependent_fun(this) result(qd_ptr)
    class(ClassBlockData), intent(inout) :: this
    type(Dependent_Variable), pointer :: qd_ptr(:,:,:)
  continue
    qd_ptr => this%qd(:,:,:)
  end function get_dependent_fun

  !> Get right-hand side
  function get_rhs_fun(this) result(rhs_ptr)
    class(ClassBlockData), intent(inout) :: this
    real(dp), pointer :: rhs_ptr(:,:,:,:)
  continue
    rhs_ptr => this%rhs(:,:,:,:)
  end function get_rhs_fun

  !> Get face flux
  function get_face_flux_fun(this) result(flux_ptr)
    class(ClassBlockData), intent(inout) :: this
    real(dp), pointer :: flux_ptr(:,:,:,:)
  continue
    flux_ptr => this%flux(:,:,:,:)
  end function get_face_flux_fun

  !> Get face qp_plus
  function get_face_qp_plus_fun(this) result(qp_plus_ptr)
    class(ClassBlockData), intent(inout) :: this
    type(Primitive_Variable), pointer :: qp_plus_ptr(:,:,:)
  continue
    qp_plus_ptr => this%p_plus(:,:,:)
  end function get_face_qp_plus_fun

  !> Get face qp_minus
  function get_face_qp_minus_fun(this) result(qp_minus_ptr)
    class(ClassBlockData), intent(inout) :: this
    type(Primitive_Variable), pointer :: qp_minus_ptr(:,:,:)
  continue
    qp_minus_ptr => this%p_minus(:,:,:)
  end function get_face_qp_minus_fun

  !> Get block geometry
  function get_block_geom_fun(this) result(mesh_ptr)
    class(ClassBlockData), intent(inout) :: this
    type(ClassGeomBlock), pointer :: mesh_ptr
  continue
    mesh_ptr => this%mesh
  end function get_block_geom_fun

end module defs_block_data
