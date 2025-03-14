!=============================================================================80
!
!> Inviscid FLux Reconstruction using MUSCL
!
!=============================================================================80

module scheme_reconstruct_MUSCL

  use kind_parameter,   only : dp, i8
  use global_devlog,    only : devlog, LogLevel

  implicit none
  private

  public :: reconstruct_MUSCL_sub

  integer  :: limiter_ = -1 !< MUSCL limiter type
  real(dp) :: kappa_   = 0.3333_dp !< kappa = -1, 0, 1, 1/3 ...

contains

!========================= RECONSTRUCT_MUSCL_SUB =============================80
!
!> Inviscid FLux Reconstruction using MUSCL
!
!   q_l(i + 1/2) = q(i)     + 0.25 * [(1 - k) D(-) + (1 + k) D(+)]_(i)
!   q_r(i + 1/2) = q(i + 1) - 0.25 * [(1 - k) D(+) + (1 + k) D(-)]_(i + 1)
!
!   D(-)_(i) = q(i + 1) - q(i)
!   D(+)_(i) = q(i + 1) - q(i)
!
!   kappa = -1, 0, 1, 1/3 ...
!
!  j^ * --- * --- * --- *
!   | |     |     |     |
!     * --- * --- * --- *
!     |     |     |     |    face(i + 1/2, j, k)_L = MUSCL(q(i - 1), q(i), q(i + 1))
!     * --- * --- * --- *
!     |     |     |     |    face(i + 1/2, j, k)_R = MUSCL(q(i), q(i + 1), q(i + 2))
!     * --- * --- * --- *
!    i-1    i  ^  i+1   i+2
!            L |  R
!           i + 1/2
!
!=============================================================================80

  subroutine reconstruct_MUSCL_sub(nface, dghost, kappa, limiter, pg, pr, wind)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    integer(i8), intent(in) :: nface
    integer(i8), intent(in) :: dghost
    real(dp),    intent(in) :: kappa
    integer,     intent(in) :: limiter
    type(Primitive_Variable), intent(inout) :: pg(:) !< primitive variables (ghost)
    type(Primitive_Variable), intent(inout) :: pr(:) !< reconstruct variables (real)
    integer,     intent(in) :: wind !< -1: left, 1: right
  !...Declare local variable
    integer(i8) :: stencil(3)
    integer(i8) :: iface
    integer(i8) :: ig, ig1, ig2, ig3 !< ghost mesh index
  continue
    kappa_   = kappa
    limiter_ = limiter
  !...Get reconstructed primitive variable
    ! real-mesh:               * --- * --- * --- *
    ! real-index:              1     2     3     4
    ! face-index:           1     2     3     4     5
    ! ghost-mesh:  + ... + ... * --- * --- * --- * ... + ... +
    ! p-index:     1     2     3     4     5     6     7     8
    !                      2.5   3.5   4.5   5.5   6.5
    !
    !   real mesh   |  ghost mesh (stencil)
    !     iface     |    left      right    ig-1, ig, ig+1, ig+2
    !       1       |  (1,2,3)    (2,3,4)     1    2    3     4
    !       2       |  (2,3,4)    (3,4,5)     2    3    4     5
    !       :       |     :          :        :    :    :     :
    !       5       |  (5,6,7)    (6,7,8)     5    6    7     8
    !
    !     ig = iface + dghost - 1  (-1 because index start from 1)
    !
    select case (wind)
      case (-1)
        stencil = (/-1, 0, 1/)
      case (1)
        stencil = (/ 0, 1, 2/)
      case default
        call devlog%print(where="scheme_reconstruct_MUSCL::reconstruct_MUSCL_sub", &
                          message="Invalid wind direction",&
                          level=LogLevel%error)
    end select
  !...Loop over all face
    do iface = 1, nface
      ig  = iface + dghost - 1
      ig1 = ig + stencil(1)
      ig2 = ig + stencil(2)
      ig3 = ig + stencil(3)
    !...Reconstruct primitive variable
      pr(iface)%rho   = MUSCL(pg(ig1)%rho,   pg(ig2)%rho,   pg(ig3)%rho,   wind)
      pr(iface)%u     = MUSCL(pg(ig1)%u,     pg(ig2)%u,     pg(ig3)%u,     wind)
      pr(iface)%v     = MUSCL(pg(ig1)%v,     pg(ig2)%v,     pg(ig3)%v,     wind)
      pr(iface)%w     = MUSCL(pg(ig1)%w,     pg(ig2)%w,     pg(ig3)%w,     wind)
      pr(iface)%press = MUSCL(pg(ig1)%press, pg(ig2)%press, pg(ig3)%press, wind)
    end do
  end subroutine reconstruct_MUSCL_sub

  !> Compute reconstructed primitive variable
  !
  !  mesh           * --- * -:- * --- * --- *
  !  vert index                 i
  !  face index      L(-) <- i -> R(+)
  !  stencil(-)     i-2   i-1   i
  !  stencil(+)           i-1   i   i+1
  !
  function MUSCL(p1, p2, p3, wind) result(pr)
    use global_type_defs, only : TypeDefs
  !...Declare input/output variables
    real(dp) , intent(in) :: p1 !< primitive variable stencil(1)
    real(dp) , intent(in) :: p2 !< primitive variable stencil(2)
    real(dp) , intent(in) :: p3 !< primitive variable stencil(3)
    integer,   intent(in) :: wind !< wind direction (-1: minus, 1: plus)
    real(dp) :: pr !< reconstructed primitive variable
  !...Declare local variable
    real(dp) :: D_p, Db_p !< difference of primitive variable D(+)
    real(dp) :: D_m, Db_m !< difference of primitive variable D(-)
    real(dp) :: b !< limiter coefficient (minmod)
  continue
  !...Compute difference
    D_m = p2 - p1
    D_p = p3 - p2
  !...Perform limiter
    if (limiter_ == TypeDefs%Scheme%limiter_none .or. &
        limiter_ == TypeDefs%Scheme%NULL) then
      Db_m = D_m
      Db_p = D_p
    else if (limiter_ == TypeDefs%Scheme%limiter_minmod) then
      ! b = (1 + (3 - k) / (1 - k)) / 2
      ! b = 1.0_dp + 1.0_dp / (1.0_dp - kappa_)
      b = 1.0_dp
      Db_m = minmod(D_m, D_p, b)
      Db_p = minmod(D_p, D_m, b)
    else if (limiter_ == TypeDefs%Scheme%limiter_vanleer) then
      Db_m = VanLeer(D_m, D_p)
      Db_p = Db_m
    else if (limiter_ == TypeDefs%Scheme%limiter_vanAlbada) then
      Db_m = VanAlbada(D_m, D_p)
      Db_p = Db_m
    end if
  !...MUSCL Reconstruction
    !   q_l(i + 1/2) = q(i)     + 0.25 * [(1 - k) D(-) + (1 + k) D(+)]_(i)
    !   q_r(i + 1/2) = q(i + 1) - 0.25 * [(1 - k) D(+) + (1 + k) D(-)]_(i + 1)
    !   q_wind(i + 1/2) = q_wind - 0.25 * wind * [(1 + wind * k) D(-) + (1 - wind * k) D(+)]
    select case (wind)
    case (-1) !< left
      pr = p2 + 0.25_dp * ((1 - kappa_) * Db_m + (1 + kappa_) * Db_p)
    case (1) !< right
      pr = p2 - 0.25_dp * ((1 - kappa_) * Db_p + (1 + kappa_) * Db_m)
    end select
    ! pr = p2 - 0.25_dp * wind * ((1.0_dp + wind * kappa_) * Db_m + (1.0_dp - wind * kappa_) * Db_p)
  end function MUSCL

!================================= MINMOD ====================================80
!
!> MUSCL limiter: minmod
!
!  D(-) = minmod(D(-), b * D(+))
!  D(+) = minmod(D(+), b * D(-))
!
!    1 < b < (3 - k) / (1 - k)
!
!  minmod(x, y) = 0.5 * (sign(x) + sign(y)) * min(abs(x), abs(y))
!
!   sign(x) = 1,  if x >= 0
!   sign(x) = -1, if x <  0
!
!=============================================================================80

  pure elemental function minmod(x, y, b) result(z)
    real(dp), intent(in) :: x
    real(dp), intent(in) :: y
    real(dp), intent(in) :: b !< limiter coefficient: 1 <= b <= (3 - k) / (1 - k)
    real(dp) :: z
  continue
    z = 0.5 * (sign(1.0_dp, x) + sign(1.0_dp, y)) * min(abs(x), abs(b * y))
  end function minmod

!================================ VANLEER ====================================80
!
!> VanLeer limiter (kappa = -1.0)
!
!  D(-) = D(+) = VanLeer(D(-), D(+))
!
!  VanLeer(x, y) = (sign(x) + sign(y)) * x * y / (|x| + |y| + eps)
!
!  eps = 1.0E-6
!
!=============================================================================80

  pure elemental function VanLeer(x, y) result(z)
    real(dp), intent(in) :: x
    real(dp), intent(in) :: y
    real(dp) :: z
    real(dp), parameter :: eps = 1.0E-6_dp
  continue
    z = (sign(1.0_dp, x) + sign(1.0_dp, y)) * x * y / (abs(x) + abs(y) + eps)
  end function VanLeer

!================================ VanAlbada ==================================80
!
!> Van Albada limiter (kappa = -1.0)
!
!  D(-) = D(+) = VanAlbada(D(-), D(+))
!
!  VanAlbada(x, y) = (x * (y^2 + eps) + y * (x^2 + eps)) / (x**2 + y**2 + eps)
!
!  eps = 1.0E-6
!
!=============================================================================80

  pure elemental function VanAlbada(x, y) result(z)
    real(dp), intent(in) :: x
    real(dp), intent(in) :: y
    real(dp) :: z
    real(dp), parameter :: eps = 1.0E-6_dp
  continue
    z = (x * (y**2 + eps) + y * (x**2 + eps)) / (x**2 + y**2 + eps)
  end function VanAlbada

end module scheme_reconstruct_MUSCL
