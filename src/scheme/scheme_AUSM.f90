!=============================================================================80
!
!> Flux splitting scheme for the AUSM (AUSM+-up)
!
!   AUSM: Advection Upstream Splitting Method
!
!=============================================================================80

module scheme_AUSM

  use kind_parameter, only : dp, i8
  use global_devlog,  only : devlog, LogLevel

  implicit none
  private

  public :: FVS_AUSM_sub

contains

!================================ FVS_AUSM ===================================80
!
!> Flux splitting using AUSM scheme.
!
!=============================================================================80

  subroutine FVS_AUSM_sub(nface, neq, gamma, norm, q_left, q_right, flux)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    integer(i8),  intent(in) :: nface !< number of faces
    integer,      intent(in) :: neq   !< number of equations
    real(dp),     intent(in) :: gamma !< specific heat ratio
    real(dp),                 intent(inout) :: norm(nface,3)
                          !< unit normal vector of each face (nface, ncoord)
    type(Primitive_Variable), intent(inout) :: q_left(nface)
                          !< primitive variables on the positive side of cell face
    type(Primitive_Variable), intent(inout) :: q_right(nface)
                          !< primitive variables on the negative side of cell face
    real(dp),                 intent(inout) :: flux(nface,neq) !< flux (nface, neq)
  !...Declare local variable
    integer(i8) :: iface, ieq
    ! face primitive variable
    real(dp), dimension(nface) :: rho_l, rho_r
    real(dp), dimension(nface) :: u_l, u_r
    real(dp), dimension(nface) :: v_l, v_r
    real(dp), dimension(nface) :: w_l, w_r
    real(dp), dimension(nface) :: H_l, H_r !< total enthalpy: H = E + p / rho
    real(dp), dimension(nface) :: p_l, p_r !< pressure
    real(dp), dimension(nface) :: Vc_l, Vc_r !< contravariant velocity: V = u * nx + v * ny + w * nz
    real(dp), dimension(nface) :: c_l, c_r !< speed of sound
    real(dp), dimension(nface) :: M_l, M_r !< Mach number
    ! AUSM split variable
    real(dp), dimension(nface) :: Mn_    !< AUSM split Mach number
    real(dp), dimension(nface) :: Mn_abs !< AUSM numerical dissipation Mach number
    real(dp), dimension(nface, neq) :: Phi_l  !< Convective term
    real(dp), dimension(nface, neq) :: Phi_r  !< Convective term
    real(dp), dimension(nface) :: p_    !< AUSM split pressure
    real(dp), dimension(nface, neq) :: norm_ !< prssure term normal vector
  continue
  !...Get primitive variables of left and right states
    rho_l = q_left%rho
    u_l   = q_left%u
    v_l   = q_left%v
    w_l   = q_left%w
    p_l   = q_left%press

    rho_r = q_right%rho
    u_r   = q_right%u
    v_r   = q_right%v
    w_r   = q_right%w
    p_r   = q_right%press
  !...Compute total enthalpy
    Vc_l = u_l * norm(:,1) + v_l * norm(:,2) + w_l * norm(:,3)
    Vc_r = u_r * norm(:,1) + v_r * norm(:,2) + w_r * norm(:,3)
    H_l  = gamma / (gamma - 1) * p_l / rho_l + &
            0.5_dp * (u_l * u_l + v_l * v_l + w_l * w_l)
    H_r  = gamma / (gamma - 1) * p_r / rho_r + &
            0.5_dp * (u_r * u_r + v_r * v_r + w_r * w_r)
  !...Compute speed of sound and Mach number
    c_l = sqrt(gamma * p_l / rho_l)
    c_r = sqrt(gamma * p_r / rho_r)
    M_l = Vc_l / c_l
    M_r = Vc_r / c_r
  !...Convective term
    Mn_ = Mach_Split_AUSM(M_l, wind_ =  1) + &
          Mach_Split_AUSM(M_r, wind_ = -1)
    Mn_abs = Artificial_Dissipation_AUSM(abs(Mn_))

    Phi_l(:,1) = rho_l * c_l
    Phi_l(:,2) = Phi_l(:,1) * u_l
    Phi_l(:,3) = Phi_l(:,1) * v_l
    Phi_l(:,4) = Phi_l(:,1) * w_l
    Phi_l(:,5) = Phi_l(:,1) * H_l

    Phi_r(:,1) = rho_r * c_r
    Phi_r(:,2) = Phi_r(:,1) * u_r
    Phi_r(:,3) = Phi_r(:,1) * v_r
    Phi_r(:,4) = Phi_r(:,1) * w_r
    Phi_r(:,5) = Phi_r(:,1) * H_r
  !...Pressure term
    p_ = Pressure_Split_AUSM(p_l, M_l, wind_ =  1) + &
         Pressure_Split_AUSM(p_r, M_r, wind_ = -1)
    norm_(:,1)   = 0.0_dp
    norm_(:,2:4) = norm(:,1:3)
    norm_(:,5)   = 0.0_dp
  !...Compute flux
    do ieq = 1, neq
      flux(:,ieq) = 0.5_dp * (Mn_ + Mn_abs) * Phi_l(:,ieq) + &
                    0.5_dp * (Mn_ - Mn_abs) * Phi_r(:,ieq) + &
                    p_ * norm_(:,ieq)
    end do
  end subroutine FVS_AUSM_sub

!=============================================================================80
!
! Splitting function for AUSM scheme class
!
! Liou M S. Progress towards an improved CFD method-AUSM+[C].
!           12th Computational Fluid Dynamics Conference. 1995: 1701.
!
!=============================================================================80

  pure elemental function Mach_Split_AUSM(Ma, wind_) result(Ma_s)
    real(dp), intent(in) :: Ma !< Mach number
    integer,  intent(in) :: wind_ !< wind direction (-1: minus, 1: plus)
    real(dp) :: Ma_s !< split Mach number
  !...Declare local variable
    ! real(dp), parameter :: beta = 0.125_dp !< 1/8
    real(dp), parameter :: beta = 0.0_dp !< 1/8
    real(dp) :: wind
  continue
    wind = real(wind_, dp)
    if (abs(Ma) >= 1.0_dp) then
      Ma_s = 0.5_dp * (Ma + wind * abs(Ma))
    else
      Ma_s = wind * (0.25_dp * (Ma + wind)**2 + beta * (Ma**2 - 1.0_dp)**2)
    end if
  end function Mach_Split_AUSM

  pure elemental function Pressure_Split_AUSM(p, Ma, wind_) result(p_s)
    real(dp), intent(in) :: p !< pressure
    real(dp), intent(in) :: Ma !< Mach number
    integer,  intent(in) :: wind_ !< wind direction (-1: minus, 1: plus)
    real(dp) :: p_s !< split pressure
  !...Declare local variable
    ! real(dp), parameter :: alpha = 0.1875_dp !< 3/16
    real(dp), parameter :: alpha = 0.0_dp !< 3/16
    real(dp) :: wind
  continue
    wind = real(wind_, dp)
    if (abs(Ma) >= 1.0_dp) then
      p_s = 0.5_dp * (1.0_dp + wind * sign(1.0_dp, Ma))
    else
      p_s = 0.25_dp * (Ma + wind)**2 * (2.0_dp - wind * Ma) + &
            wind * alpha * Ma * (Ma**2 - 1)**2
    end if
    p_s = p_s * p
  end function Pressure_Split_AUSM

  pure elemental function Artificial_Dissipation_AUSM(Mn_abs) result(Mn_fix)
    real(dp), intent(in) :: Mn_abs !< AUSM Mach number |Mn|
    real(dp) :: Mn_fix !< AUSM correction Mach number
  !...Declare local variable
    real(dp), parameter :: delta = 0.1 !< 0 < delta <= 0.5
  continue
    if (Mn_abs > delta) then
      Mn_fix = Mn_abs
    else
      Mn_fix = 0.5_dp * (Mn_abs**2 + delta**2) / delta
    end if
  end function Artificial_Dissipation_AUSM

end module scheme_AUSM
