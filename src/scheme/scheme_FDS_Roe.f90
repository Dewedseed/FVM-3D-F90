!=============================================================================80
!
!> Flux Difference Splitting (FDS) Scheme with Roe's Approximate Riemann Solver
!
!=============================================================================80

module scheme_FDS_Roe

  use kind_parameter, only : dp, i8
  use global_devlog,  only : devlog, LogLevel

  implicit none
  private

  public :: FDS_Roe_sub

contains

!=============================== FDS_ROE_SUB =================================80
!
!> Flux Difference Splitting (FDS) Scheme with Roe's Approximate Riemann Solver
!
!=============================================================================80

  subroutine FDS_Roe_sub(nface, neq, gamma, norm, q_left, q_right, flux, entp_fix)
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
    real(dp),     intent(in) :: entp_fix !< entropy correction coefficient
  !...Declare local variables
    integer(i8) :: iface
    ! flux
    real(dp), dimension(nface, neq) :: flux_left, flux_right
    real(dp), dimension(nface, neq) :: dflux_1, dflux_234, dflux_5
    ! face primitive variable
    real(dp), dimension(nface) :: rho_l, rho_r
    real(dp), dimension(nface) :: u_l, u_r
    real(dp), dimension(nface) :: v_l, v_r
    real(dp), dimension(nface) :: w_l, w_r
    real(dp), dimension(nface) :: H_l, H_r !< total enthalpy: H = E + p / rho
    real(dp), dimension(nface) :: p_l, p_r !< pressure
    real(dp), dimension(nface) :: Vc_l, Vc_r !< contravariant velocity: V = u * nx + v * ny + w * nz
    ! Roe-averaged variables
    real(dp), dimension(nface) :: rho_b, u_b, v_b, w_b, H_b, Vl_b
    real(dp), dimension(nface) :: c_b !< sound speed
    real(dp), dimension(nface) :: q2_b !< q^2 = u^2 + v^2 + w^2
    ! eigenvalues of Roe matrix
    real(dp), dimension(nface, neq) :: lambda
    ! difference of primitive variables
    real(dp), dimension(nface) :: drho, du, dv, dw, dpress, dVl
    real(dp), dimension(nface) :: delta_harten
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
  !...Compute velocity and total enthalpy
    Vc_l = u_l * norm(:,1) + v_l * norm(:,2) + w_l * norm(:,3)
    Vc_r = u_r * norm(:,1) + v_r * norm(:,2) + w_r * norm(:,3)
    ! H_l  = gamma / (gamma - 1) * p_l / rho_l + 0.5 * Vl_l * Vl_l
    ! H_r  = gamma / (gamma - 1) * p_r / rho_r + 0.5 * Vl_r * Vl_r
    H_l  = gamma / (gamma - 1) * p_l / rho_l + 0.5_dp * (u_l * u_l + v_l * v_l + w_l * w_l)
    H_r  = gamma / (gamma - 1) * p_r / rho_r + 0.5_dp * (u_r * u_r + v_r * v_r + w_r * w_r)
  !...Compute difference
    drho   = rho_r - rho_l
    du     = u_r - u_l
    dv     = v_r - v_l
    dw     = w_r - w_l
    dpress = p_r - p_l
    dVl    = Vc_r - Vc_l
  !...Compute Roe-averaged variables
    rho_b = sqrt(rho_l * rho_r)
    u_b   = Roe_Avarage(rho_l, rho_b, u_r, u_l)
    v_b   = Roe_Avarage(rho_l, rho_b, v_r, v_l)
    w_b   = Roe_Avarage(rho_l, rho_b, w_r, w_l)
    H_b   = Roe_Avarage(rho_l, rho_b, H_r, H_l)
    Vl_b  = u_b * norm(:,1) + v_b * norm(:,2) + w_b * norm(:,3)
    q2_b  = u_b * u_b + v_b * v_b + w_b * w_b
    c_b   = sqrt((gamma - 1.0_dp) * (H_b - 0.5_dp * q2_b))
  !...Entropy correction (Harten's)
    delta_harten = entp_fix * abs(Vl_b + c_b) !< from Blazek 2D-project
    ! delta_harten = entp_fix * c_b !< from Blazek book (local sound of speed)
    lambda(:,1) = entropy_correct(abs(Vl_b - c_b), delta_harten)
    lambda(:,2) = entropy_correct(abs(Vl_b), delta_harten)
    lambda(:,3) = entropy_correct(abs(Vl_b), delta_harten)
    lambda(:,4) = entropy_correct(abs(Vl_b), delta_harten)
    lambda(:,5) = entropy_correct(abs(Vl_b + c_b), delta_harten)
  !...Compute Roe flux
    dflux_1(:,1) = lambda(:,1) * (dpress - rho_b * c_b * dVl) / (2.0_dp * c_b * c_b)
    dflux_1(:,2) = dflux_1(:,1) * (u_b - c_b * norm(:,1))
    dflux_1(:,3) = dflux_1(:,1) * (v_b - c_b * norm(:,2))
    dflux_1(:,4) = dflux_1(:,1) * (w_b - c_b * norm(:,3))
    dflux_1(:,5) = dflux_1(:,1) * (H_b - c_b * Vl_b)

    dflux_234(:,1) = lambda(:,2) * (drho - dpress / (c_b * c_b))
    dflux_234(:,2) = dflux_234(:,1) * u_b + lambda(:,2) * rho_b * (du - dVl * norm(:,1))
    dflux_234(:,3) = dflux_234(:,1) * v_b + lambda(:,2) * rho_b * (dv - dVl * norm(:,2))
    dflux_234(:,4) = dflux_234(:,1) * w_b + lambda(:,2) * rho_b * (dw - dVl * norm(:,3))
    dflux_234(:,5) = dflux_234(:,1) * 0.5_dp * q2_b + &
                     lambda(:,2) * rho_b * (u_b * du + v_b * dv + w_b * dw - Vl_b * dVl)

    dflux_5(:,1) = lambda(:,5) * (dpress + rho_b * c_b * dVl) / (2.0_dp * c_b * c_b)
    dflux_5(:,2) = dflux_5(:,1) * (u_b + c_b * norm(:,1))
    dflux_5(:,3) = dflux_5(:,1) * (v_b + c_b * norm(:,2))
    dflux_5(:,4) = dflux_5(:,1) * (w_b + c_b * norm(:,3))
    dflux_5(:,5) = dflux_5(:,1) * (H_b + c_b * Vl_b)
  !...Compute left and right fluxes
    flux_left(:,1) = rho_l * Vc_l
    flux_left(:,2) = rho_l * u_l * Vc_l + p_l * norm(:,1)
    flux_left(:,3) = rho_l * v_l * Vc_l + p_l * norm(:,2)
    flux_left(:,4) = rho_l * w_l * Vc_l + p_l * norm(:,3)
    flux_left(:,5) = rho_l * H_l * Vc_l

    flux_right(:,1) = rho_r * Vc_r
    flux_right(:,2) = rho_r * u_r * Vc_r + p_r * norm(:,1)
    flux_right(:,3) = rho_r * v_r * Vc_r + p_r * norm(:,2)
    flux_right(:,4) = rho_r * w_r * Vc_r + p_r * norm(:,3)
    flux_right(:,5) = rho_r * H_r * Vc_r
  !...Compute flux
    flux = 0.5 * ((flux_left + flux_right) - (dflux_1 + dflux_234 + dflux_5))
  end subroutine FDS_Roe_sub

!=============================== ROE_AVARAGE =================================80
!
!> Roe-averaged variables
!
!   rho_b = sqrt(rho_l * rho_r)
!   q_b = (sqrt(rho_l) * q_l + sqrt(rho_r) * q_r) / (sqrt(rho_l) + sqrt(rho_r))
!       = (rho_l * q_l + rho_b * q_r) / (rho_l + rho_b)
!       = (rho_b * q_l + rho_r * q_r) / (rho_b + rho_r)
!
!=============================================================================80

  pure elemental function Roe_Avarage(rho_l, rho_b, q_r, q_l) result(q_b)
    real(dp), intent(in) :: rho_l  !< density left
    real(dp), intent(in) :: rho_b  !< Roe averaged density
    real(dp), intent(in) :: q_r    !< right state
    real(dp), intent(in) :: q_l    !< left state
    real(dp) :: q_b !< Roe averaged state
  continue
    q_b = (rho_l * q_l + rho_b * q_r) / (rho_l + rho_b)
  end function Roe_Avarage

!============================= ENTROPY_CORRECT ===============================80
!
!> Harten's entropy correction
!
!=============================================================================80

  pure elemental function entropy_correct(lambda, delta) result(lambda_c)
    real(dp), intent(in) :: lambda !< initial eigenvalues
    real(dp), intent(in) :: delta  !< threshold value of the correction
    real(dp) :: lambda_c !< entropy correction eigenvalues
  continue
    if ( lambda > delta ) then
      lambda_c = lambda
    else
      lambda_c = 0.5_dp * (lambda**2 + delta**2) / delta
    end if
  end function entropy_correct

end module scheme_FDS_Roe
