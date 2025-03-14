!=============================================================================80
!
!> Flux Vector Splitting (FVS) Van Leer Scheme
!
!=============================================================================80

module scheme_FVS_VanLeer

  use kind_parameter, only : dp, i8
  use global_devlog,  only : devlog, LogLevel

  implicit none
  private

  public :: FVS_VanLeer_sub

contains

!================================ SUB_NAME ===================================80
!
!> Flux Vector Splitting (FVS) Van-Leer Scheme
!
!=============================================================================80

  subroutine FVS_VanLeer_sub(nface, neq, gamma, norm, q_left, q_right, flux)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/ouput variables
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
  !...Declare local variables
    integer(i8) :: i
    ! face primitive variables
    real(dp), dimension(nface) :: rho_l, rho_r
    real(dp), dimension(nface) :: u_l, u_r
    real(dp), dimension(nface) :: v_l, v_r
    real(dp), dimension(nface) :: w_l, w_r
    real(dp), dimension(nface) :: p_l, p_r !< pressure
    ! real(dp), dimension(nface) :: H_l, H_r !< total enthalpy: H = E + p / rho
    real(dp), dimension(nface) :: q2_l, q2_r !< total enthalpy: q2 = u^2 + v^2 + w^2
    real(dp), dimension(nface) :: Vc_l, Vc_r !< contravariant velocity: V = u * nx + v * ny + w * nz
    ! Mach number and speed of sound
    real(dp), dimension(nface) :: c_l, c_r !< speed of sound
    real(dp), dimension(nface) :: M_l, M_r !< Mach number
    real(dp), dimension(nface) :: M_n !< Mach number of normal component
    ! flux plus and minus
    real(dp), dimension(nface,neq) :: flux_plus, flux_minus
  continue
  !...Compute primitive variables of left and right states
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
    q2_l = u_l * u_l + v_l * v_l + w_l * w_l
    q2_r = u_r * u_r + v_r * v_r + w_r * w_r
  !...Compute Mach number
    c_l = sqrt(gamma * p_l / rho_l)
    c_r = sqrt(gamma * p_r / rho_r)
    M_l = Vc_l / c_l
    M_r = Vc_r / c_r
    M_n = M_l + M_r
  !...Compute VanLeer flux
    ! M_l = VanLeer_Mach_Split(M_l, M_n, wind = -1)
    ! M_r = VanLeer_Mach_Split(M_r, M_n, wind =  1)
    M_l = VanLeer_Mach_Split(M_l, M_l, wind = -1)
    M_r = VanLeer_Mach_Split(M_r, M_r, wind =  1)
    flux_plus(:,1) = rho_l * c_l * M_l
    flux_plus(:,2) = flux_plus(:,1) * (norm(:,1) * (-Vc_l + 2.0_dp * c_l) / gamma + u_l)
    flux_plus(:,3) = flux_plus(:,1) * (norm(:,2) * (-Vc_l + 2.0_dp * c_l) / gamma + v_l)
    flux_plus(:,4) = flux_plus(:,1) * (norm(:,3) * (-Vc_l + 2.0_dp * c_l) / gamma + w_l)
    flux_plus(:,5) = flux_plus(:,1) * (((gamma - 1) * Vc_l + 2.0_dp * c_l)**2 / &
                      (2.0_dp * (gamma**2 - 1)) + (q2_l - Vc_l**2) / 2.0_dp)
    flux_minus(:,1) = rho_r * c_r * M_r
    flux_minus(:,2) = flux_minus(:,1) * (norm(:,1) * (-Vc_r - 2.0_dp * c_r) / gamma + u_r)
    flux_minus(:,3) = flux_minus(:,1) * (norm(:,2) * (-Vc_r - 2.0_dp * c_r) / gamma + v_r)
    flux_minus(:,4) = flux_minus(:,1) * (norm(:,3) * (-Vc_r - 2.0_dp * c_r) / gamma + w_r)
    flux_minus(:,5) = flux_minus(:,1) * (((gamma - 1) * Vc_r - 2.0_dp * c_r)**2 / &
                      (2.0_dp * (gamma**2 - 1)) + (q2_r - Vc_r**2) / 2.0_dp)
  !...Compute FVS flux
    flux = flux_plus + flux_minus
  end subroutine FVS_VanLeer_sub

  !> Mach number split function
  pure elemental function VanLeer_Mach_Split(Ma, Mn, wind) result(Ma_s)
  !...Declare input/output variables
    real(dp), intent(in) :: Ma   !< Mach number
    real(dp), intent(in) :: Mn   !< Reference Mach number
    integer,  intent(in) :: wind !< -1: left, 1: right
    real(dp) :: Ma_s  !< split Mach number
  continue
    select case (wind)
      case (-1) !< left side
        if (Mn >= 1.0_dp) then !< supersonic
          Ma_s = Ma
        else if (Mn <= -1.0_dp) then !< supersonic
          Ma_s = 0
        else !< subsonic
          Ma_s = 0.25_dp * (Ma + 1.0_dp) * (Ma + 1.0_dp)
        end if
      case (1) !< right side
        if (Mn >= 1.0_dp) then !< supersonic
          Ma_s = 0
        else if (Mn <= -1.0_dp) then !< supersonic
          Ma_s = Ma
        else !< subsonic
          Ma_s = -0.25_dp * (Ma - 1.0_dp) * (Ma - 1.0_dp)
        end if
    end select
  end function VanLeer_Mach_Split

end module scheme_FVS_VanLeer
